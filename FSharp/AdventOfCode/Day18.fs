namespace AdventOfCode

open System
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day18 =

    type Operator =
        | Plus
        | Times

    type Token =
        | LeftParen
        | RightParen
        | Operator of Operator
        | Number of int
        override this.ToString () =
            match this with
            | Number i -> sprintf "%i" i
            | RightParen -> ")"
            | LeftParen -> "("
            | Operator Operator.Plus -> "+"
            | Operator Operator.Times -> "*"

    let (|Digit|_|) (i : char) : int option =
        match Int32.TryParse (string i) with
        | true, i -> Some i
        | false, _ -> None

    let lex (s : string) : Token list =
        let rec consumeUntilNonInt (soFar : int) (pos : int) : int * int =
            if pos >= s.Length then soFar, pos else
            match s.[pos] with
            | Digit digit ->
                consumeUntilNonInt (10 * soFar + digit) (pos + 1)
            | _ -> soFar, pos

        let rec go (tokens : Token list) (pos : int) =
            if pos >= s.Length then List.rev tokens else
            match s.[pos] with
            | ')' -> go (RightParen :: tokens) (pos + 1)
            | '(' -> go (LeftParen :: tokens) (pos + 1)
            | '+' -> go (Operator Plus :: tokens) (pos + 1)
            | '*' -> go (Operator Times :: tokens) (pos + 1)
            | ' ' -> go tokens (pos + 1)
            | Digit digit ->
                let int, pos = consumeUntilNonInt digit (pos + 1)
                go (Number int :: tokens) pos
            | c -> failwithf "Unexpected character during parse: %c at position %i" c pos

        go [] 0

    type Expression =
        | Times of Expression * Expression
        | Const of int
        | Sum of Expression * Expression

    let parse1 (tokens : Token list) : Expression =
        // Parse the first expression encountered;
        // return the unprocessed tokens and a stack of expressions for subsequent expressions to consume
        let rec go (stack : Expression list) (tail : Token list) : Expression * Expression list * Token list =
            match stack, tail with
            | head :: tail, Token.Operator Operator.Times :: rest ->
                let parsed, newStack, rest = go tail rest
                Expression.Times (head, parsed), newStack, rest
            | head :: tail, Token.Operator Operator.Plus :: rest ->
                let parsed, newStack, rest = go tail rest
                Expression.Sum (head, parsed), newStack, rest
            | [head], [] -> head, [], []
            | [], Token.Number i :: Token.Operator Operator.Times :: rest ->
                let parsed, newStack, rest = go [] rest
                Expression.Times (Expression.Const i, parsed), newStack, rest
            | [], Token.Number i :: Token.Operator Operator.Plus :: rest ->
                let parsed, newStack, rest = go [] rest
                Expression.Sum (Expression.Const i, parsed), newStack, rest
            | [], [Token.Number i] ->
                Expression.Const i, [], []
            | stack, Token.LeftParen :: rest ->
                let contents, stack, rest = go stack rest
                go (contents :: stack) rest
            | [], Token.Number i :: Token.RightParen :: rest ->
                Expression.Const i, [], rest
            | head :: tail, Token.RightParen :: rest ->
                head, tail, rest
            // Error conditions
            | _, Token.Number _ :: Token.Number _ :: _ ->
                failwith "Unexpected parse error: did not expect a number to follow a number"
            | _, Token.Number _ :: Token.LeftParen :: _ ->
                failwith "Unexpected parse error: a left paren should not follow a number"
            | [], Token.RightParen :: _ ->
                failwith "Unexpectedly started an expression with a right-paren"
            | [], [] -> failwith "Unexpectedly empty expression"
            | _ :: _, Token.Number _ :: _ ->
                failwith "Saw a number while stack was nonempty"
            | [], Token.Operator _ :: _ ->
                failwith "Cannot start with an operator"
            | _, [] -> failwith "Stack was not empty at end of expression"

        let expr, stack, remaining =
            tokens
            |> List.fold (fun state ->
                // I'm an absolute chump and constructed the parser right-associative
                // instead of left-associative. Reverse the list.
                function
                | LeftParen -> RightParen :: state
                | RightParen -> LeftParen :: state
                | x -> x :: state
            ) []
            |> go []

        match remaining with
        | _ :: _ -> failwith "Unexpectedly had tokens still to consume after parse"
        | [] ->

        match stack with
        | _ :: _ -> failwith "Unbalanced stack"
        | [] ->

        expr

    let rec eval (e : Expression) : int64 =
        match e with
        | Expression.Const i -> int64 i
        | Expression.Times (t, u) -> eval t * eval u
        | Expression.Sum (t, u) -> eval t + eval u

    let part1 () =
        Utils.readResource "Day18Input.txt"
        |> List.map lex
        |> List.map parse1
        |> List.map eval
        |> List.sum

    // Life's too short to write another parser. Just use a proper library like I would
    // have done from the beginning if I'd realised the problem was going to change completely
    // for part 2.

    [<RequireQualifiedAccess>]
    type PolishOperator =
        | LeftParen
        | Plus
        | Times

        static member ToOperator (s : PolishOperator) =
            match s with
            | PolishOperator.Plus -> Operator.Plus
            | PolishOperator.Times -> Operator.Times
            | PolishOperator.LeftParen -> failwith "Still had a left-paren on the stack"

    let parse2 (tokens : Token list) : Expression =
        // Convert the token stream to reverse-Polish notation.
        let rec toRpn (stack : PolishOperator list) (answer : Token list) (rest : Token list) : Token list =
            match rest with
            | [] ->
                // We have consumed all input tokens. There may still be outstanding operations;
                // put them at the end in the order they were added.
                (List.rev answer) @ (List.map (PolishOperator.ToOperator >> Operator) stack)
            | Token.Number i :: rest ->
                // We will definitely be emitting this number next (RPN doesn't change the order of the numbers).
                toRpn stack (Number i :: answer) rest
            | Token.LeftParen :: rest ->
                // Effectively enter a sub-mode. Nothing will ever look past LeftParen on the stack.
                toRpn (PolishOperator.LeftParen :: stack) answer rest
            | Token.RightParen :: rest ->
                // Exit the sub-mode by unwinding the stack in the order it was constructed.
                let rec go acc (stack : PolishOperator list) =
                    match stack with
                    | [] -> failwith "Expected a left-paren"
                    | PolishOperator.LeftParen :: rest -> acc, rest
                    | PolishOperator.Plus :: rest -> go (Token.Operator Operator.Plus :: acc) rest
                    | PolishOperator.Times :: rest -> go (Token.Operator Operator.Times :: acc) rest
                let answer, stack = go answer stack
                toRpn stack answer rest
            | Token.Operator Operator.Times :: rest ->
                // The lowest-priority operator, so if there's already an operator on the stack, we pop it before adding
                // the Times we just consumed.
                match stack with
                | PolishOperator.Times :: stack ->
                    toRpn (PolishOperator.Times :: stack) (Operator Operator.Times :: answer) rest
                | PolishOperator.Plus :: stack ->
                    toRpn (PolishOperator.Times :: stack) (Operator Operator.Plus :: answer) rest
                // In these two cases, it's like the stack is empty (or the stack literally is empty), so we just
                // put Times onto the stack (so that we can continue and see whether any addition will take place).
                | PolishOperator.LeftParen :: _
                | [] ->
                    toRpn (PolishOperator.Times :: stack) answer rest
            | Token.Operator Operator.Plus :: rest ->
                // The higher-priority operator.
                match stack with
                | PolishOperator.Plus :: stack ->
                    // Essentially: pop the existing Plus from the stack, and push another one.
                    toRpn (PolishOperator.Plus :: stack) (Operator Operator.Plus :: answer) rest
                | PolishOperator.Times :: stack ->
                    // The Plus is higher-priority than the Times, so we need to wait and see what is going to
                    // come next.
                    toRpn (PolishOperator.Plus :: PolishOperator.Times :: stack) answer rest
                // In these two cases, it's like the stack is empty (or the stack literally is empty), so we just
                // put Plus onto the stack (so that we can continue and see whether any addition will take place).
                | PolishOperator.LeftParen :: _
                | [] -> toRpn (PolishOperator.Plus :: stack) answer rest

        let inPostfix = toRpn [] [] tokens

        let rec eval (stack : Expression list) (tokens : Token list) : Expression =
            match tokens with
            | [] ->
                match stack with
                | [x] -> x
                | _ -> failwith "unterminated"
            | Number i :: tokens -> eval (Const i :: stack) tokens
            | Operator Operator.Plus :: tokens ->
                match stack with
                | x :: y :: stack -> eval (Sum (x, y) :: stack) tokens
                | _ -> failwith "needed two args for Plus"
            | Operator Operator.Times :: tokens ->
                match stack with
                | x :: y :: stack -> eval (Times (x, y) :: stack) tokens
                | _ -> failwith "needed two args for Times"
            | _ -> failwith "no leftparens allowed"

        eval [] inPostfix

    let part2 () =
        Utils.readResource "Day18Input.txt"
        |> List.map lex
        |> List.map parse2
        |> List.map eval
        |> List.sum
