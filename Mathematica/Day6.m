(* ::Package:: *)

parse[lines_]:=
  Select[
    Split[lines, StringLength[#1]>0&&StringLength[#2]>0&],
    #=!={""}&
  ]


part1[parsed_]:=Length/@(Union@@@(Characters/@parsed))//Total


part2[parsed_]:=Length/@(Intersection@@@(Characters/@parsed))//Total
