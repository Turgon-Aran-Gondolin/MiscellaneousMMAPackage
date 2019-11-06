(* ::Package:: *)

BeginPackage["MMMAP`"]


ListPackages::usage;
ListAndLoadPackages::usage;


Begin["Private`"]


(*Print[Directory[]];*)
(*Print[FileNames["*.wl"]];*)
ListPackages[]:=FileBaseName/@FileNames["*.wl",DirectoryName[FindFile["MMMAP`"]]];
ListAndLoadPackages[]:=TogglerBar[Dynamic[x,{If[MatchQ[#1,{"Packages"}],Null,Print[#1//First];Get[FileNameJoin[{DirectoryName[FindFile["MMMAP`"]],#1//First}]<>".wl"]]&,x=#1&}],{"Packages"}~Join~(FileBaseName/@FileNames["*.wl",DirectoryName[FindFile["MMMAP`"]]]),Appearance->"Vertical"]


End[]


EndPackage[]
