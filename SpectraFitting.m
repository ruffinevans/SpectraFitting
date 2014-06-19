(* ::Package:: *)

(* ::Text:: *)
(*Here are a set of tools to fit spectra effectively.*)


(* ::Section:: *)
(*File I/O and grooming*)


(* ::Text:: *)
(*First, define a fast duplicate removal function that we will apply to all spectra because they should be single valued.*)


QuickDD[list_]:=First/@GatherBy[list,First]
QuickDD::usage="In a list of points, deletes any points where a previous point with the same first value (x value) exists. Necessary for many plotting and fitting routines that expect single-valued functions.";


(* ::Text:: *)
(*Import Spectrum from File. Assume CSV but allow for other options.*)


GetSpec[path_,format_:"csv"]:=QuickDD[Import[path,format]]
GetSpec::usage="Imports a spectrum from a text file. Assumes a standard {energy, amplitude} formatting like a csv file. If the file is organized in essentially the same way but is formatted differently, an optional segcond argument can be included that is passed directly to the Mathematica \"Import[]\" function as a format string. See the usage notes for that function. Sometimes \"Table\" is a useful choice for this argument.";


(* ::Text:: *)
(*Import all files that are not mathematica files from a folder.*)


GetSpecDir[path_,format_:"csv"]:=Module[{spectralist=Import[path]},SetDirectory[path];
spectralist=DeleteCases[spectralist,_?(StringMatchQ[#,"*.nb"]||StringMatchQ[#,"*.m"]&)]; (* Remove mathematica files from the list of possible spectra.*)
Return[Table[GetSpec[spectralist[[i]],format],{i,1,Length[Import[path]]}]];
]
GetSpecDir::usage="Imports all the files as spectra from a directory, ignoring Mathematica .nb and .m files.";


(* ::Text:: *)
(*Function to correct a spectrum based on a flat-field spectrum.*)


SpecDivide[spec_,ffspec_]:=Module[{ffinterp=Interpolation[ffspec]},
Return[Table[{spec[[i,1]],spec[[i,2]]/ffinterp[spec[[i,1]]]},{i,1,Length[spec]}]]]
SpecDivide::usage="Divides the first argument by an interpolated version of the second argument. Useful for flat-field corrections. Works best if the arguments are taken at the same x values, otherwise the function will do its best.";


SpectraHelp[]:=Print["Usage Instructions:\n
1. Get a list of spectra from a directory by using the GetSpecDir[path] function.\n
2. (Optional) Get an individual spectrum for flatfield correction using the GetSpec[path] function. Divide each spectrum by this flat-field using the SpecDivide[spec,ffspec] function.\n
3. Initialize a list of guesses using the CoordInit[Spectra,center,d] function, where center is a very preliminary guess for the center point of a generic spectrum and d is the overall spread to zoom in on for further fitting. You should save this guess as a variable.\n
4. Use the PickGuesses[Spectra,CoordList] command to interactively refine these guesses. CoordList should be the variable you defined in the previous step.\n
5. Run SingleLorentzianFit[Spectra, Coordlist] to fit each function to a single Lorentzian. Save the output to a variable. More options are coming soon.\n
6. Use the ShowFits[Spectra, CoordList, Fits, Path] where Fits is the list of fits and Path is the original directory to display all of the fits and important parameters.\n\n
If you need additional help, all of the functions have usage instructions that can be queried with two question marks and the name of the function, e.g. ??ShowFits ."];


(* ::Section:: *)
(*Spectra Manipulation and Fitting*)


(* ::Text:: *)
(*Convert list of elements to four coordinates for user input. Because the y-coordinate doesn't matter, we set it to be some default value.*)


ToFourCoord[el_]:={{el[[1]],1000},{el[[2,1]],1000},{el[[2,2]],1000},{el[[3]],1000}}


(* ::Text:: *)
(*Convert back from the four-coordinates to a list of elements.*)


ToWaveEl[crd_]:={crd[[1,1]],{crd[[2,1]],crd[[3,1]]},crd[[4,1]]}


(* ::Text:: *)
(*Initialize coordinate list:*)


CoordInit[Spectra_,center_,d_]:=Module[{\[Lambda]list},
\[Lambda]list=If[ChoiceDialog["Do you want to reset the wavelength list?"],
\[Lambda]list=Table[{\[Lambda]center,{center-d/2,center+d/2},center-d/4},{i,1,Length[Spectra]}];
Return[ToFourCoord/@\[Lambda]list]
]
];
CoordInit::usage="Taking the list of spectra in the first argument, generate a coordinate list of initial guesses based on the center position (third argument) and the spread in peak positions (third argument) \nThese initial guesses will be returned as a list of guesses in the format {center, {left of range, right of range}, background point}.";


(* ::Text:: *)
(*Write a function to pick guesses for the peaks. Use locatorpane to make it easy to fit data.*)
(*Using dynamic evaluation creates a scoping problem that is solved with With[]. See http://reference.wolfram.com/mathematica/howto/EvaluateExpressionsInsideDynamicOrManipulate.html*)
(*Also, it is a bit tricky to handle passing the desired dynamic variable symbolically, because the function wants to evaluate it right away. So we have to set some properties of the function to tell it not to do that, and then use the unevaluated form in the dynamic evaluation. *)


SetAttributes[PickGuesses,HoldRest]


PickGuesses[Spectra_,CrdList_]:=Table[
LocatorPane[With[{i=i},Dynamic[Unevaluated@CrdList[[i]]]],
ListLinePlot[Spectra[[i]],
PlotRange->{{\[Lambda]center-\[CapitalDelta]\[Lambda],\[Lambda]center+\[CapitalDelta]\[Lambda]},All},ImageSize->Medium,PlotLabel->Import[DataDir][[i]]],Appearance->{Style["o",Red],Style[">",Blue],Style["<",Blue],Style["\[Vee]",Black]}
],
{i,1,Length[Spectra]}]


PickGuesses::usage="This function allows the user to interactively select guesses for parameters that will be used later for various curve fitting routines.\n
The inputs are first the list of spectra and second the coordinate list in the format described by the CoordInit command (see the usage notes there). Note that this coordinate list should be named as a variable and not passed implicitly as the output of the CoordInit function. This is required to handle the dynamic evaluation necessary for interactive user selection of the points.\n
This function will generate a list of zoomed-in plots with markers that the user can drag to the desired positions for peak fitting.";


(* ::Text:: *)
(*Now we have a few commands to zoom into the user-selected region by chopping off the spectra. These commands work on a complete list of spectra, not on one spectum.*)


ChopTop[Spectra_,CrdList_]:=Table[TakeWhile[Spectra[[i]],#[[1]]<(ToWaveEl/@CrdList)[[i,2,2]]&],{i,1,Length[Spectra]}];


ChopBoth[Spectra_,CrdList_]:=Module[{untopped=ChopTop[Spectra,CrdList]},
Table[TakeWhile[Reverse[untopped[[i]]],#[[1]]>(ToWaveEl/@CrdList)[[i,2,1]]&],
{i,1,Length[Spectra]}]
];


(* ::Text:: *)
(*Command to take the background count value at the user selected position. Interpolate. (Don't need module, just loop over i)*)


(* ::Text:: *)
(*Remove the user-selected average counts:*)


UserRemoveAve[Spectra_,CrdList_]:=Module[{cb=ChopBoth[Spectra,CrdList]},
Table[
{cb[[i]]\[Transpose][[1]],cb[[i]]\[Transpose][[2]]-Interpolation[Spectra[[i]],(ToWaveEl/@CrdList)[[i,3]]]}\[Transpose]
,{i,1,Length[cb]}]//Return;
]
UserRemoveAve::usage="Removes the user-selected average position in CrdList from Spectra to aid fitting.";


(* ::Text:: *)
(*Fit a single Lorentzian to each element of spectra.*)


SingleLorentzianFit[Spectra_,CrdList_]:=Module[{zoomed=UserRemoveAve[Spectra,CrdList],wavelist=(ToWaveEl/@CrdList)},
Table[
Print["Fitting "<>ToString[i]<>" out of "<>ToString[Length[Spectra]]];
NonlinearModelFit[zoomed[[i]],{A (1+((x-x0)/\[Gamma])^2)^-1,A>0,x0>wavelist[[i,2,1]],
x0<wavelist[[i,2,2]],
\[Gamma]<10},
{A,\[Gamma],x0},x,
Method->{NMinimize,Method->{"DifferentialEvolution","ScalingFactor"->0.9,"CrossProbability"->0.1,"PostProcess"->{FindMinimum,Method->"QuasiNewton"}}}
],
{i,1,Length[Spectra]}
]
];
SingleLorentzianFit::usage="Fits each spectrum in \"Spectra\" (the first argument) to a Lorentzian, based on the guesses in CrdList. The output will be a list of fitted models.";


(* ::Text:: *)
(*Calculate the Q based on the HWHM. The FWHM is 2\[Gamma] in my parameterization, and the Q is Subscript[\[Omega], 0]/\[CapitalDelta]\[Omega] where \[CapitalDelta]\[Omega] is 2\[Gamma]. I no longer remember why I have these two separate functions.*)


CalcQ[l_,dl_]:=Module[{\[Omega]=0,d\[Omega]=0},
\[Omega]=UnitConvert[Quantity[1,"Speed of Light"]/Quantity[l,"Nanometers"],"Terahertz"];
d\[Omega]=\[Omega]-UnitConvert[Quantity[1,"Speed of Light"]/Quantity[l+2dl,"Nanometers"],"Terahertz"];
Return[Abs[\[Omega]/d\[Omega]]]
];
CalcQ[l_,dl_,ddl_]:=Module[{\[Omega]=0,d\[Omega]=0,dd\[Omega]=0},
\[Omega]=UnitConvert[Quantity[1,"Speed of Light"]/Quantity[l,"Nanometers"],"Terahertz"];
d\[Omega]=\[Omega]-UnitConvert[Quantity[1,"Speed of Light"]/Quantity[l+2dl,"Nanometers"],"Terahertz"];
dd\[Omega]=\[Omega]-UnitConvert[Quantity[1,"Speed of Light"]/Quantity[l+2(dl+ddl),"Nanometers"],"Terahertz"];
Return[{Abs[\[Omega]/d\[Omega]],Abs[Abs[\[Omega]/d\[Omega]]-Abs[\[Omega]/dd\[Omega]]]}]
];
CalcQ::usage="Calculate the Q based on the \!\(\*
StyleBox[\"HWHM\",\nFontWeight->\"Bold\"]\). The FWHM is 2\[Gamma] in my parameterization, and the Q is \!\(\*FormBox[SubscriptBox[\(\[Omega]\), \(0\)],
TraditionalForm]\)/\[CapitalDelta]\[Omega] where \[CapitalDelta]\[Omega] is 2\[Gamma].";


ShowFits[Spectra_,CrdList_,Fits_,Path_]:=
Module[{wavelist=(ToWaveEl/@CrdList),AveList=UserRemoveAve[Spectra,CrdList]},
Table[
Print[
{ToString[DeleteCases[Import[Path],_?(StringMatchQ[#,"*.nb"]||StringMatchQ[#,"*.m"]&)][[i]]]<>
", \!\(\*SubscriptBox[\(\[Lambda]\), \(center\)]\)="<>ToString[Fits[[i]]["BestFitParameters"][[3,2]]]<>"\[PlusMinus]"<>ToString[Fits[[i]]["ParameterErrors"][[2]]]<>
", \!\(\*SubscriptBox[\(\[Gamma]\), \(FWHM\)]\)="<>ToString[Fits[[i]]["BestFitParameters"][[2,2]]]<>"\[PlusMinus]"<>ToString[Fits[[i]]["ParameterErrors"][[3]]]<>
", Q="<>ToString[CalcQ[Fits[[i]]["BestFitParameters"][[3,2]],Fits[[i]]["BestFitParameters"][[2,2]]]]<>"\[PlusMinus]"<>ToString[CalcQ[Fits[[i]]["BestFitParameters"][[3,2]],Fits[[i]]["BestFitParameters"][[2,2]],Fits[[i]]["ParameterErrors"][[3]]][[2]]],
Show[
Plot[Fits[[i]]//Normal,{x,wavelist[[i,2,1]],wavelist[[i,2,2]]},PlotRange->All,PlotStyle->{Red,Thick}],
ListLinePlot[Spectra[[i]],PlotRange->All,PlotStyle->Larger]],
Show[
Plot[Fits[[i]]//Normal,{x,wavelist[[i,2,1]],wavelist[[i,2,2]]},PlotRange->All,PlotStyle->{Red,Thick}],
ListLinePlot[AveList[[i]],PlotRange->All,PlotStyle->Larger]]}
]
,{i,1,Length[Fits]}]
]
ShowFits::usage="Show the fits for Spectra based on the guesses in CrdList. The actual fits must be given as a list of models in Fits. The original directory should be given as a path in the last argument to generate the file names corresponding to the spectra.";


SpectraHelp[]
