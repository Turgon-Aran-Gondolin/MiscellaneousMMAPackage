(* ::Package:: *)

BeginPackage["MMMAP`ErrorBar`"];


ePlot;
EBPlot::usage=
"ErrorBarPlot[data,color]";
$Colors;


Begin["`Private`"]


plusMinusMean[a_,b_]:={a+b,a-b,a};
ePlot[plotFun_,dataX_,plusMinList_,color_,opts:OptionsPattern[]]:=Block[{f},f[y_]:=Transpose[{dataX,y}];
plotFun[{f[plusMinList[[All,1]]],f[plusMinList[[All,2]]],f[plusMinList[[All,3]]]},Filling->{1->{2}},Joined->{True,True,True},PlotStyle->{Opacity[0],Opacity[0],Darker@color},PlotMarkers->{Graphics@{Disk[]},0.01},FillingStyle->Directive[Opacity[0.2],color],Frame->True,Axes->False,PlotRange->All,FilterRules[{opts},Options[ListPlot]]]]


EBPlot[data_,color_,opts:OptionsPattern[]]:=Module[{dataY,dataX,errorY,plusMinList},dataY=data[[All,2]];
dataX=data[[All,1]];
errorY=data[[All,3]];
plusMinList=Thread[plusMinusMean[dataY,errorY]];
ePlot[ListPlot,dataX,plusMinList,color,opts]]

$Colors=ColorData[98,"ColorList"];


End[]


EndPackage[]
