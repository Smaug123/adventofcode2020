(* ::Package:: *)

part1[numbers_List] := Times @@ First@Select[Tuples[numbers, {2}], Plus @@ # == 2020 &, 1]


part2[numbers_List] := Times @@ First@Select[Tuples[numbers, {3}], Plus @@ # == 2020 &, 1]
