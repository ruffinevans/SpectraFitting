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
(*Simple command with a few options to fit lifetime data*)


LifetimeFit[data_,window_,bkgrdconstr_]:=
Module[{a,b,x,\[Tau]0,lifetimeData,model,lifetimeFit},
lifetimeData=Select[data,window[[1]]<#[[1]]<window[[2]]&];
model=a+b*Exp[-(x-window[[1]])/\[Tau]0];
lifetimeFit=NonlinearModelFit[lifetimeData,{model,a>0.05},{{a,0.5},{b,1},{\[Tau]0,1}},x];
Print[Show[ListPlot[{data,lifetimeData},PlotRange->{{window[[1]]-3,window[[2]]+5},All},Joined->True,ImageSize->500],Plot[lifetimeFit[x],{x,window[[1]],window[[2]]},PlotStyle->{Red,Thick},PlotRange->All]],
Show[ListLogPlot[{data,lifetimeData},PlotRange->{{window[[1]]-3,window[[2]]+5},All},Joined->True,ImageSize->500],LogPlot[lifetimeFit[x],{x,window[[1]],window[[2]]},PlotStyle->{Red,Thick}]]
];
Return[lifetimeFit["ParameterTable"]]
]


(* ::Text:: *)
(*Command to groom lifetime spectrum. Basically trivial. Nice to apply after GetSpecDir command.*)


LTGroom[spec_]:=spec[[All,{1,3}]];
LTGroom::usage="Grooms a single spectrum e.g. from the output of GetSpecDir.";
