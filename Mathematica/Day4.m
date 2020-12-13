(* ::Package:: *)

parse[lines_]:=
  Select[
    Split[lines,StringLength[#1]>0&&StringLength[#2]>0&],
    #=!={""}&
  ]


toPassport[sublines_]:=StringSplit[#,":"]&/@Flatten[StringSplit[#," "]&/@sublines]


part1[lines_List]:=
  Select[lines,
    Select[Sort[First/@toPassport[#]],#=!="cid"&]
      ===
    {"byr","ecl","eyr","hcl","hgt","iyr","pid"}
    &
  ]


validators[parsed_List]:=
  parsed/.{
"byr"->Function[{s},StringLength[s]==4&&AllTrue[Characters@s,MemberQ[CharacterRange["0","9"],#]&]&&1920<=FromDigits[s]<=2002],
"iyr"->Function[{s},StringLength[s]==4&&AllTrue[Characters@s,MemberQ[CharacterRange["0","9"],#]&]&&2010<=FromDigits[s]<=2020],
"eyr"->Function[{s},StringLength[s]==4&&AllTrue[Characters@s,MemberQ[CharacterRange["0","9"],#]&]&&2020<=FromDigits[s]<=2030],
"hgt"->
  Function[{s},
    Switch[StringTake[s,-2;;],
      "cm",
        With[{st=StringTake[s,;;-3]},
          AllTrue[Characters@st,MemberQ[CharacterRange["0","9"],#]&]
          &&
          150<=FromDigits[st]<=193
        ],
      "in",
        With[{st=StringTake[s,1;;-3]},
          AllTrue[Characters@st,MemberQ[CharacterRange["0","9"],#]&]
          &&
          59<=FromDigits[st]<=76
        ],
      _,False
    ]
  ],
"hcl"->
  Function[{s},
    StringTake[s,{1}]=="#"
    &&
    StringLength[s]==7
    &&
    AllTrue[
      Characters@StringTake[s,2;;],
      MemberQ[CharacterRange["a","f"],#]||MemberQ[CharacterRange["0","9"],#]&
    ]
  ],
"ecl"->Function[{s},MemberQ[{"amb","blu","brn","gry","grn","hzl","oth"},s]],
"pid"->Function[{s},StringLength[s]==9&&AllTrue[Characters@s,MemberQ[CharacterRange["0","9"],#]&]],
"cid"->(True&)
}


part2[passports_]:=Length@Select[Construct@@@validators[toPassport[#]]&/@part1[passports],(And@@#)&]
