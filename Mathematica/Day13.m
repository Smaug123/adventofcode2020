(* ::Package:: *)

part1[lines_List]:=
  With[{earliest=FromDigits@lines[[1]],gaps=FromDigits/@(StringSplit[lines[[2]],","]/."x"->Nothing)},
    Times@@First@MinimalBy[{#-Mod[earliest,#],#}&/@gaps,First]
  ]


(*Take only the second line of input.*)
part2[str_String]:=
  ChineseRemainder@@Transpose@MapIndexed[If[#1=="x",Nothing,{-#2[[1]]+1,FromDigits[#1]}]&,StringSplit[str,","]]
