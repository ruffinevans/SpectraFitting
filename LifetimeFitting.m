(* ::Package:: *)

(* ::Input:: *)
(*LifetimeFit[data_,window_,bkgrdconstr_]:=*)
(*Module[{a,b,x,\[Tau]0,lifetimeData,model,lifetimeFit},*)
(*lifetimeData=Select[data,window[[1]]<#[[1]]<window[[2]]&];*)
(*model=a+b*Exp[-(x-window[[1]])/\[Tau]0];*)
(*lifetimeFit=NonlinearModelFit[lifetimeData,{model,a>0.05},{{a,0.5},{b,1},{\[Tau]0,1}},x];*)
(*Print[Show[ListPlot[{data,lifetimeData},PlotRange->{{window[[1]]-3,window[[2]]+5},All},Joined->True,ImageSize->500],Plot[lifetimeFit[x],{x,window[[1]],window[[2]]},PlotStyle->{Red,Thick},PlotRange->All]],*)
(*Show[ListLogPlot[{data,lifetimeData},PlotRange->{{window[[1]]-3,window[[2]]+5},All},Joined->True,ImageSize->500],LogPlot[lifetimeFit[x],{x,window[[1]],window[[2]]},PlotStyle->{Red,Thick}]]*)
(*];*)
(*Return[lifetimeFit["ParameterTable"]]*)
(*]*)
