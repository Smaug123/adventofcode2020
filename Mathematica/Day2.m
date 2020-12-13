(* ::Package:: *)

part1[lines_List] :=
  Select[lines,
    With[{split = StringSplit[#, " "]},
      With[{char = StringTake[split[[2]], {1}], password = split[[-1]], desired = FromDigits /@ StringSplit[split[[1]], "-"]},
        desired[[1]] <= StringCount[password, char] <= desired[[2]]
      ]
    ]&
  ] // Length


part2[lines_List] :=
  Select[lines,
    With[{split = StringSplit[#, " "]},
      With[{char = StringTake[split[[2]], {1}], password = split[[-1]], desired = FromDigits /@ StringSplit[split[[1]], "-"]},
        Xor @@ Thread[StringTake[password, List /@ desired] == char]
      ]
    ]&
  ] // Length
