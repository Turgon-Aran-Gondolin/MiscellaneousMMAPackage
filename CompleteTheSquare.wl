(* ::Package:: *)

BeginPackage["MMMAP`CompleteTheSquare`"];


CompleteTheSquare::usage=
"CompleteTheSquare[expr]\n CompleteTheSquare[expr, Variables[expr]]";
CompleteTheSquare::notquad = "The expression is not quadratic in the variables `1`";


Begin["`Private`"]


Unprotect[Dot];
ClearAttributes[Dot,Orderless];
CompleteTheSquare[expr_] := CompleteTheSquare[expr, Variables[expr]]
CompleteTheSquare[expr_, Vars_Symbol] := CompleteTheSquare[expr, {Vars}]
CompleteTheSquare[expr_, Vars : {__Symbol}] := Module[{array, A, B, Cc, s, vars, sVars},
  vars = Intersection[Vars, Variables[expr]];
  Check[array = CoefficientArrays[expr, vars], Return[expr], CoefficientArrays::poly];
  If[Length[array] != 3, Message[CompleteTheSquare::notquad, vars]; Return[expr]];
  {Cc, B, A} = array; A = Symmetrize[A];
  s = Simplify[1/2 Inverse[A].B, Trig -> False];
  sVars = Hold /@ (vars + s); A = Map[Hold, A, {2}];
  Expand[A.sVars.sVars] + Simplify[Cc - s.A.s, Trig -> False] // ReleaseHold
  ]


End[]


EndPackage[]
