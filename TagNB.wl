(* ::Package:: *)

BeginPackage["MMMAP`TagNB`"];


makeTabbedNotebook::usage=
"makeTabbedNotebook[]\nmakeTabbedNotebook[nameList_List]";


Begin["`Private`"]


(*make a single page of the notebook*)page[tag_String]:=Cell@CellGroupData[{Cell["","SlideShowNavigationBar",CellTags->{tag}],Cell[tag,"Title"]}];
(*make a single tab-like button which selects the page*)
button[tag_String]:=Button[Dynamic[Setter[Dynamic[CurrentValue[EvaluationNotebook[],{TaggingRules,"page"},tag]],tag]],CurrentValue[EvaluationNotebook[],{TaggingRules,"page"}]=tag;
NotebookLocate[tag],Appearance->None];
(*make a notebook based upon a list of strings which are names of tabs*)
makeTabbedNotebook[nameList_List]:=NotebookPut[Notebook[page/@nameList,DockedCells->ToBoxes[ExpressionCell[Row[button/@nameList],"DockedCell"]][[1]],ScreenStyleEnvironment->"SlideShow"],SelectedNotebook[]];

makeTabbedNotebook[]:=makeTabbedNotebook[{"First","Second","Third"}]


End[]


EndPackage[]
