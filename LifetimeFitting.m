(* ::Package:: *)

LifetimeFittingHelp[]:=Print["
A small set of tools to fit lifetime data. Works well with SpectraFitting package.
\!\(\*
StyleBox[\"Instructions\",\nFontVariations->{\"Underline\"->True}]\):
1. Import spectra, e.g. with GetSpecDir command.
2. Groom spectra, e.g. with LTGroom command, which can be used on the output of GetSpecDir.
3. Fit spectra, e.g. with LifetimeFit[data, window, minimum background level]."]


LifetimeFittingHelp[]


(* ::Text:: *)
(*Copying from spectra*)
(*fitting code. Could also call as dependency, but doesn't make sense for just one simple function.*)


MaxPos[data_,filter_:(1 #1&)]:=(#1[[Position[#1,Max[Transpose[#1][[2]]]][[1,1]]]][[1]]&)[filter[data]]


(* ::Text:: *)
(*Simple command with a few options to fit lifetime data*)


LifetimeFit[data_,window_,bkgrdconstr_]:=
Module[{a,b,x,\[Tau]0,lifetimeData,model,lifetimeFit,windowtemp=window},
	If[window==0,windowtemp=MaxPos[data]+{0.4,4}];
	Print[windowtemp];
	lifetimeData=Select[data,windowtemp[[1]]<#[[1]]<windowtemp[[2]]&];
	model=a+b*Exp[-(x-windowtemp[[1]])/\[Tau]0];
	lifetimeFit=NonlinearModelFit[lifetimeData,{model,a>bkgrdconstr},{{a,0.5},{b,1},{\[Tau]0,1}},x];
	Print[
		Show[
			ListPlot[{data,lifetimeData},PlotRange->{{windowtemp[[1]]-3,windowtemp[[2]]+5},All},Joined->True,ImageSize->500],
			Plot[lifetimeFit[x],{x,windowtemp[[1]],windowtemp[[2]]},PlotStyle->{Red,Thick},PlotRange->All]
		],
		Show[
			ListLogPlot[{data,lifetimeData},PlotRange->{{windowtemp[[1]]-3,windowtemp[[2]]+5},All},Joined->True,ImageSize->500],
			LogPlot[lifetimeFit[x],{x,windowtemp[[1]],windowtemp[[2]]},PlotStyle->{Red,Thick}]
		]
	];
	Return[lifetimeFit["ParameterTable"]]
]


LifetimeFit[data_]:=LifetimeFit[data,0,0]


(* ::Text:: *)
(*Command to groom lifetime spectrum. Basically trivial. Nice to apply after GetSpecDir command.*)


LTGroom[spec_]:=spec[[All,{1,3}]];
LTGroom::usage="Grooms a single spectrum e.g. from the output of GetSpecDir.";
