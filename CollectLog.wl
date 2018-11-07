(* ::Package:: *)

BeginPackage["CollectLog`"];


CollectLog::usage=
"CollectLog[expr]";
CollectAllLog::usage=
"CollectAllLog[expr]";


Begin["`Private`"]


(* ::Input::Initialization:: *)
CollectLog[expr_]:=Module[{rule1,rule2,a,b,x},rule1=Log[a_]+Log[b_]->Log[a*b];
rule2=x_*Log[a_]->Log[a^x];
(expr/.rule1)/.rule2/.rule1/.rule2];
CollectAllLog[expr_]:=Nest[collectLog,expr,Length[expr]]/.Log[o_]:>FullSimplify@Log[o];


End[]


EndPackage[]
