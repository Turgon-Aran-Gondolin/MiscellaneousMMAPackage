(* ::Package:: *)

BeginPackage["MMMAP`Jacobian`"];

JacobianMatrix::usage="JacobianMatrix[f_List?VectorQ, x_List]";
JacobianDeterminant::usage="JacobianDeterminant[f_List?VectorQ, x_List]";


Begin["`Private`"]



JacobianMatrix[f_List?VectorQ, x_List] :=
    Outer[D, f, x] /; Equal@@(Dimensions/@{f,x})

JacobianDeterminant[f_List?VectorQ, x_List] :=
    Det[JacobianMatrix[f, x]] /;
      Equal @@ (Dimensions /@ {f, x})


End[]


EndPackage[]
