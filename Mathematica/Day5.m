(* ::Package:: *)

seats[lines_]:=FromDigits[StringReplace[#,{"F"->"0","B"->"1","L"->"0","R"->"1"}],2]&/@lines;


part1[lines_]:=Max[seats[lines]]


part2[lines_]:=Split[ReverseSort[seats[lines]],#1-#2==1&][[2]][[1]]+1
