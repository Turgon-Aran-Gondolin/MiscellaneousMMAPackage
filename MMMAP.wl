(* ::Package:: *)

BeginPackage["MMMAP`"]


ListPackages::usage;


Begin["Private`"]


Print[Directory[]];
(*Print[FileNames["*.wl"]];*)
ListPackages[]:=TogglerBar[Dynamic[x,{If[MatchQ[#1,{"Packages"}],Null,Get[FileNameJoin[{Directory[],#1//First}]]]&,(x=#1)&}],{"Packages"}~Join~FileNames["*.wl"],Appearance->"Vertical"]


End[]


EndPackage[]
