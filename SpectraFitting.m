(* ::Package:: *)

(* ::Text:: *)
(*TO DO:*)
(*	Generalize model to fixedpeaksmodel, or have fixedpeaksmodel call model through constraints.*)
(*	Create superfunction to fit variable number of peaks and stop when agreement is sufficiently good.*)
(*	Solve normalization problem definitively*)
(*	More intelligent background subtraction*)
(*	Residuals does not give satisfying answer. Make sure it is not subtracting negative values.*)


(* ::Section:: *)
(*Help Section*)


SpectraHelp[]:=Print["Usage Instructions:\n
1. Get a list of spectra from a directory by using the GetSpecDir[path] function.\n
2. (Optional) Get an individual spectrum for flatfield correction using the GetSpec[path] function. Divide each spectrum by this flat-field using the SpecDivide[spec,ffspec] function.\n
3. Initialize a list of guesses using the CoordInit[Spectra,center,d] function, where center is a very preliminary guess for the center point of a generic spectrum and d is the overall spread to zoom in on for further fitting. You should save this guess as a variable.\n
4. Use the PickGuesses[Spectra,CoordList] command to interactively refine these guesses. CoordList should be the variable you defined in the previous step.\n
5. Run SingleLorentzianFit[Spectra, Coordlist] to fit each function to a single Lorentzian. Save the output to a variable. More options are coming soon.\n
6. Use the ShowFits[Spectra, CoordList, Fits, Path] where Fits is the list of fits and Path is the original directory to display all of the fits and important parameters.\n\n
If you need additional help, all of the functions have usage instructions that can be queried with two question marks and the name of the function, e.g. ??ShowFits ."];


SpectraHelp[]


(* ::Text:: *)
(*Here are a set of tools to fit spectra effectively. For more information, see the readme and example usage files at https://github.com/ruffinevans/SpectraFitting*)


(* ::Section:: *)
(*File I/O and grooming*)


(* ::Text:: *)
(*First, define a fast duplicate removal function that we will apply to all spectra because they should be single valued.*)


QuickDD[list_]:=First/@GatherBy[list,First]
QuickDD::usage="In a list of points, deletes any points where a previous point with the same first value (x value) exists. Necessary for many plotting and fitting routines that expect single-valued functions.";


(* ::Text:: *)
(*Here is a function that will take in imported sheets from an excel file and spit out 2D arrays.*)


ProcessXPSSpectra[Spectra_]:=
Module[{SpecOut=Spectra},
Table[SpecOut[[i]]=SortBy[Transpose[DeleteCases[Transpose[
	Reverse[TakeWhile[Reverse[Spectra[[i]]],NumberQ[#[[1]]]&]]],Except[_?(NumberQ[#[[1]]]&&#[[1]]>0&)]]],First],
{i,1,Length[SpecOut]}];
Return[SpecOut];
]
ProcessXPSSpectra::usage="Takes in a list of imported sheets from excel files and spits out a list of 2D arrays, stripping away headers and fixing the formatting. It is used in the GetSpec function when the \"Excel XPS\" file format is specified.";


(* ::Text:: *)
(*Import Spectrum from File. Assume CSV but allow for other options.*)


GetSpec[path_,format_:"csv",xpsfilter_:"Scan",verbose_:True]:=If[format!="Excel XPS",QuickDD[Import[path,format]],
	Module[{specsout,sheets=Import[path,"Sheets"],keepsheets},
		If[verbose,Print["Removing sheets that do not contain the string \""<>xpsfilter<>"\""]];
		keepsheets=Flatten[Position[sheets,_?(StringMatchQ[ToString[#],___~~xpsfilter~~___]&)]]//Quiet;
		If[verbose,Print["Here is a list of sheets that are being kept. The order of the original Excel file is being preserved."]];
		If[verbose,Print[Part[sheets,keepsheets]]];
		If[verbose,Print[ToString[Length[keepsheets]]<>" spectra in all."]];
		specsout=Table[If[verbose,Print["Importing Sheet "<>ToString[sheets[[i]]]]]; Import[path,{"xlsx","Data",i}],{i,keepsheets}];
		If[verbose,Print["Formatting XPS Spectra"]];
		Return[ProcessXPSSpectra[specsout]];
	]
]
GetSpec[path_,format_:"csv"]:=GetSpec[path,format,"Scan"];
GetSpec[path_]:=GetSpec[path,"csv","Scan"];
GetSpec::usage="Imports a simple spectrum from a text file or a properly formatted set of XPS files from an excel file.
In the first case, the function assumes a standard {energy, amplitude} formatting like a csv file and does no grooming. If the file is organized in essentially the same way but is formatted differently, an optional segcond argument can be included that is passed directly to the Mathematica \"Import[]\" function as a format string. See the usage notes for that function. Sometimes \"Table\" is a useful choice for this argument.
If you want to import an Excel file that has been formatted by the Advantage XPS program, put \"Excel XPS\" in the second argument. In this case, the function will return a list of XPS spectra with the headers removed so that the data can be convenienty manipulated. By default the sheets in the excel file that do not have XPS data will be discarded. If you want to keep only certain sheets, the optional third argument will keep all shets matching a particular string. Put in an empty string \"\" to match all sheets. For example, you can use \"O1s\" to only take oxygen data.";


(* ::Text:: *)
(*Function to return the list of sheets:*)


GetSheets[path_,xpsfilter_:"Scan",verbose_:True]:=
Module[{specsout,sheets=Import[path,"Sheets"],keepsheets},
	If[verbose,Print["Removing sheets that do not contain the string \""<>xpsfilter<>"\""]];
	keepsheets=Flatten[Position[sheets,_?(StringMatchQ[ToString[#],___~~xpsfilter~~___]&)]]//Quiet;
	If[verbose,Print["Here is a list of sheets that are being kept. The order of the original Excel file is being preserved."]];
	If[verbose,Print[ToString[Length[keepsheets]]<>" spectra in all."]];
	Return[Part[sheets,keepsheets]];
]
GetSheets[path_]:=GetSheets[path,"Scan",True];
GetSheets::usage="Imports a simple spectrum from a text file or a properly formatted set of XPS files from an excel file.
In the first case, the function assumes a standard {energy, amplitude} formatting like a csv file and does no grooming. If the file is organized in essentially the same way but is formatted differently, an optional segcond argument can be included that is passed directly to the Mathematica \"Import[]\" function as a format string. See the usage notes for that function. Sometimes \"Table\" is a useful choice for this argument.
If you want to import an Excel file that has been formatted by the Advantage XPS program, put \"Excel XPS\" in the second argument. In this case, the function will return a list of XPS spectra with the headers removed so that the data can be convenienty manipulated. By default the sheets in the excel file that do not have XPS data will be discarded. If you want to keep only certain sheets, the optional third argument will keep all shets matching a particular string. Put in an empty string \"\" to match all sheets. For example, you can use \"O1s\" to only take oxygen data.";


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


(* ::Section::Closed:: *)
(*Simple Spectra Manipulation and Fitting*)


(* ::Text:: *)
(*Convert list of elements to four coordinates for user input. Because the y-coordinate doesn't matter, we set it to be some default value.*)


ToFourCoord[el_,ypos_:1000]:={{el[[1]],ypos},{el[[2,1]],ypos},{el[[2,2]],ypos},{el[[3]],ypos}}


(* ::Text:: *)
(*Convert back from the four-coordinates to a list of elements.*)


ToWaveEl[crd_]:={crd[[1,1]],{crd[[2,1]],crd[[3,1]]},crd[[4,1]]}


(* ::Text:: *)
(*Initialize coordinate list:*)


CoordInit[Spectra_,center_,d_,ypos_:1000]:=Module[{\[Lambda]list},
\[Lambda]list=Table[{center,{center-d/2,center+d/2},center-3d/8},{i,1,Length[Spectra]}];
Return[ToFourCoord[#,ypos]&/@\[Lambda]list]
];
CoordInit::usage="Taking the list of spectra in the first argument, generate a coordinate list of initial guesses based on the center position (second argument) and the spread in peak positions (third argument) \nThese initial guesses will be returned as a list of guesses in the format {center, {left of range, right of range}, background point}.";


(* ::Text:: *)
(*Write a function to pick guesses for the peaks. Use locatorpane to make it easy to fit data.*)
(*Using dynamic evaluation creates a scoping problem that is solved with With[]. See http://reference.wolfram.com/mathematica/howto/EvaluateExpressionsInsideDynamicOrManipulate.html*)
(*Also, it is a bit tricky to handle passing the desired dynamic variable symbolically, because the function wants to evaluate it right away. So we have to set some properties of the function to tell it not to do that, and then use the unevaluated form in the dynamic evaluation. *)


SetAttributes[PickGuesses,HoldRest]


PickGuesses[Spectra_,CrdList_,DataDir_:" "]:=Module[{CrdListTemp=CrdList},
Table[
	LocatorPane[With[{i=i},Dynamic[Unevaluated@CrdList[[i]]]],
		ListLinePlot[Spectra[[i]],
			ImageSize->Medium,If[DirectoryQ[DataDir],PlotLabel->Import[DataDir][[i]],PlotLabel->"Plot "<>ToString[i]],
			PlotRange->{{CrdListTemp[[i]]\[Transpose][[1,2]],CrdListTemp[[i]]\[Transpose][[1,3]]},All}
		]
		,Appearance->{Style["o",Red],Style[">",Blue],Style["<",Blue],Style["\[Vee]",Black]}
	],
{i,1,Length[Spectra]}]
]
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
UserRemoveAve::usage="Removes the user-selected average position in CrdList from Spectra to aid fitting. Spectra MUST be sorted by increasing x values!";


(* ::Text:: *)
(*Fit a single Lorentzian to each element of spectra.*)


SingleLorentzianFit[Spectra_,CrdList_]:=Module[{zoomed=UserRemoveAve[Spectra,CrdList],wavelist=(ToWaveEl/@CrdList)},
	Table[
		Print["Fitting "<>ToString[i]<>" out of "<>ToString[Length[Spectra]]];
		NonlinearModelFit[zoomed[[i]],{A (1+((x-x0)/\[Gamma])^2)^-1,A>0,x0>wavelist[[i,2,1]],
			x0<wavelist[[i,2,2]], \[Gamma]<10},{A,\[Gamma],x0},x,
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
				ListLinePlot[AveList[[i]],PlotRange->All,PlotStyle->Larger]]
			}
		]
	,{i,1,Length[Fits]}]
]
ShowFits::usage="Show the fits for Spectra based on the guesses in CrdList. The actual fits must be given as a list of models in Fits. The original directory should be given as a path in the last argument to generate the file names corresponding to the spectra.";


(* ::Section:: *)
(*Multipeak spectrum fitting*)


(* ::Subsection:: *)
(*More data grooming*)


(* ::Text:: *)
(*Useful function to find x-coordinate corresponding to maximum y-coordinate.*)


MaxPos[data_,filter_:(1*#&)]:=#[[Position[#,Max[#\[Transpose][[2]]]][[1,1]]]][[1]]&[filter[data]];
MaxPos::usage="Gives the x-coordinate corresponding to the maximum y-coordinate, such that data[[MaxPos[data]]] returns the maximum y-value of the list data. The optional filter agument will apply the function filter to the data before finding the maximum position. ";


(* ::Text:: *)
(*Center data around peak value and renormalize by sum:*)


CenterAndNormalize[data_,filter_:(1*#&)]:=SortBy[{data\[Transpose][[1]]-MaxPos[data,filter],data\[Transpose][[2]]/Total[data\[Transpose][[2]]]}\[Transpose],First]
CenterAndNormalize::usage="Takes two-dimensional data and recenters it around the maximum y-value. This is often helpful for numerical routines to fit the data. It also renormalizes the data so that it integrates to one. The optional filter agument will apply the function filter to the data before finding the maximum position.";


(* ::Text:: *)
(*First, define a Gaussian*)


gfn[A_,\[Mu]_,\[Sigma]_,x_]:=A^2/\[Sigma]*Exp[-((x-\[Mu])^2/(2\[Sigma]^2))]
gfn[A_,\[Mu]_,\[Sigma]_,x_,c_]:=A^2*Exp[-((x-\[Mu])^2/(2\[Sigma]^2))]+c
gfn::usage="gfn[A,\[Mu],\[Sigma],x] is a gaussian with prefactor A, mean \[Mu], standard deviation \[Sigma], with independent variable x.\ngfn[A,\[Mu],\[Sigma],x,c] is the same plus a constant c.";


(* ::Text:: *)
(*Define a lorentzian:*)


lfn[A_,\[Mu]_,\[Sigma]_,x_]:=A^2*(1+((x-\[Mu])/\[Sigma])^2)^-1;
lfn[A_,\[Mu]_,\[Sigma]_,x_,c_]:=A^2*(1+((x-\[Mu])/\[Sigma])^2)^-1+c;
lfn::usage="lfn[A,\[Mu],\[Sigma],x] is a Lorentzian with prefactor A, mean \[Mu], standard deviation \[Sigma], with independent variable x.\ngfn[A,\[Mu],\[Sigma],x,c] is the same plus a constant c.";


(* ::Subsubsection:: *)
(*Voigt peak definitions*)


(* ::Text:: *)
(*Also define Voigt function. Using PDF for normal Voigt distribution is too slow (needs to compute complex error function), so better to use approximation. *)


(* ::Text:: *)
(*Here is a reasonably fast Voigt distribution from the Mathematica VoigtDistribution help page.*)


PseudoVoigtDistribution[de_,si_]:=Block[{g=(de^5+si^5+2.69296 si^4 de+2.42843 si^3 de^2+4.47163 si^2 de^3+0.07842 si de^4)^(1/5),eta},
eta=de/g;
eta=eta*(1.36603 - 0.47719 eta+0.11116 eta^2);
MixtureDistribution[{1-eta,eta},{NormalDistribution[0,g],CauchyDistribution[0,g]}]
]


(* ::Text:: *)
(*Slow voigt function, not exact.*)


vfn[A_,\[Mu]_,\[Sigma]_,\[Delta]_,x_]:=A^2*(PDF@PseudoVoigtDistribution[\[Delta],\[Sigma]])[x-\[Mu]]


(* ::Text:: *)
(*Fast voigt function with PDF of MixtureDistribution above precomputed. A little less exact for some reason.*)


vfnfast[A_,\[Mu]_,\[Sigma]_,\[Delta]_,x_]:=A^2 Block[{g=(\[Delta]^5+\[Sigma]^5+2.69296 \[Sigma]^4 \[Delta]+2.42843 \[Sigma]^3 \[Delta]^2+4.47163 \[Sigma]^2 \[Delta]^3+0.07842 \[Sigma] \[Delta]^4)^(1/5),eta},
eta=\[Delta]/g;
eta=eta*(1.36603 - 0.47719 eta+0.11116 eta^2);
eta/(E^((x - \[Mu])^2/(2*g^2))*g*Sqrt[2*Pi]) + (1 - eta)/(g*Pi*(1 + (x - \[Mu])^2/g^2))
]


(* ::Text:: *)
(*Fast CASA XPS version. See http : // www.casaxps.com/help_manual/line_shapes.htm. This is just a product of a gaussian and a lorentzian. Extremely approximate, and normalization is unclear.*)


vfnsimp[A_,\[Mu]_,\[Sigma]_,\[Delta]_,x_]:=A^2*Exp[-4*Log[2]*(1-\[Delta])*(x-\[Mu])^2/\[Sigma]^2]/(1+4\[Delta]*(x-\[Mu])^2/\[Sigma]^2)


(* ::Text:: *)
(*Compare exact, slow, fast, and CASA XPS versions:*)


(*Plot[{vfn[1,2,1,0.5,x],2 vfnfast[1,2,1,0.5,x],(PDF@VoigtDistribution[0.5,1])[x-2],vfnsimp[0.5,2,1,0.5,x]},{x,-10,10},PlotRange->All]*)


(* ::Subsection:: *)
(*Peak fitting functions*)


(* ::Subsubsection:: *)
(*Free peak positions*)


(* ::Text:: *)
(*This function outputs a n-gaussian model for data. It does not do a very good job of fitting.*)


SimpModel[data_,n_]:=Module[{dataconfig,modelfunc,objfunc,fitvar,fitres},
dataconfig={A[#],\[Mu][#],\[Sigma][#]}&/@Range[n];
modelfunc=gfn[##,fitvar]&@@@dataconfig//Total;
objfunc=Total[(data[[All,2]]-(modelfunc/.fitvar->#&)/@data[[All,1]])^2];
FindMinimum[objfunc,Flatten@dataconfig]
]


(* ::Text:: *)
(*Model does a better job of fitting. It has more finely tuned parameters and can often fit *everything* nicely.*)


Model[data_,nOrParamguess_,bounds_:{},func_:"g",sf_:0.95,cp_:0,method_:0]:=
Module[{dataconfig,modelfunc,objfunc,fitvar,fitres,guess,n,cons,tempbounds=bounds},
	If[func=="v",PrintTemporary["Voigt functions selected. Lowering ScalingFactor (to around 0.5) is recommended.\nAlso, please ignore any errors passed by NMinimize."]];
	(* Generate guess *)
	If[ListQ[nOrParamguess],
		n=Length[nOrParamguess];
		guess=nOrParamguess; (* Note that this guess does NOT treat the distinction between weights and prefactors seriously, which as-is could cause problems for wildly different sigmas. *)
		,
		n=nOrParamguess;
		If[bounds!={},Print["Must specify parameter guesses if you want to use bounds/constraints!"];tempbounds={};]
		If[func=="v",
			guess={0#+Mean[data\[Transpose][[2]]],MaxPos[data],0#+Mean[data\[Transpose][[1]]]/4,0#+Mean[data\[Transpose][[1]]]/4}&/@Range[n],
			guess={0#+Mean[data\[Transpose][[2]]],MaxPos[data],0#+Mean[data\[Transpose][[1]]]/4}&/@Range[n] (* Default guess *)
		];
	];
	(* Set up variables. Different variables for Voigt vs. Gaussian/Lorentzian *)
	If[func=="v",
		dataconfig={A[#],\[Mu][#],\[Sigma][#],\[Delta][#]}&/@Range[n];
		If[tempbounds=={},
			cons={A[#]>0,\[Sigma][#]>0,\[Delta][#]>0}&/@Range[n]
		,
			cons={nOrParamguess[[#,1]]*(1+tempbounds[[1]])>A[#]>nOrParamguess[[#,1]]*(1-tempbounds[[1]]),
					nOrParamguess[[#,2]]+tempbounds[[2]]>\[Mu][#]>nOrParamguess[[#,2]]-tempbounds[[2]],
					nOrParamguess[[#,3]]*(1+tempbounds[[3]])>\[Sigma][#]>nOrParamguess[[#,3]]*(1-tempbounds[[3]]),
					nOrParamguess[[#,4]]*(1+tempbounds[[4]])>\[Delta][#]>nOrParamguess[[#,4]]*(1-tempbounds[[4]])}&/@Range[n]
		]
	,
		dataconfig={A[#],\[Mu][#],\[Sigma][#]}&/@Range[n];
		If[tempbounds=={},
			cons={}&/@Range[n] (* For some reason, fit works better without constraints. *)
		,
					cons={nOrParamguess[[#,1]]*(1+tempbounds[[1]])>A[#]>nOrParamguess[[#,1]]*(1-tempbounds[[1]]),
						nOrParamguess[[#,2]]+tempbounds[[2]]>\[Mu][#]>nOrParamguess[[#,2]]-tempbounds[[2]],
						nOrParamguess[[#,3]]*(1+tempbounds[[3]])>\[Sigma][#]>nOrParamguess[[#,3]]*(1-tempbounds[[3]])}&/@Range[n]
		]
	];
	modelfunc=Switch[func,"l",lfn,"v",vfnfast,"g",gfn][##,fitvar]&@@@dataconfig//Total;
	NonlinearModelFit[data,{modelfunc,cons},{Flatten@dataconfig,Flatten@guess}\[Transpose],fitvar,
		 If[method!=0,ToExpression[method],Method->
			{
				NMinimize,
				Method -> {"DifferentialEvolution",
					"ScalingFactor" -> sf, "CrossProbability" -> cp,(*Tuned for good gaussian fitting. In particular, CrossProbability should be low. See e.g. http://mathematica.stackexchange.com/questions/2309/problem-with-nonlinearmodelfit*)
					"PostProcess" -> {FindMinimum, Method -> "QuasiNewton"}
				}
			}
		]
	]
]
Model::usage="Model[data,n] takes in a two-dimensional list data and a desired number of gaussians n and returns a nonlinear model for the data.
This is a difficult thing to do in general, so the function is tuned to fit the sort of data we usually get from the XPS. It works best if CenterAndNormalize has first been applied to the data. Sometimes additional tuning in the form of two optional arguments (the Scaling Factor sf and the Crossing Probability cp) can be helpful. See the complete argument list (type ??Model); many other optional parameters must also be specified in this case.
Instead of a desired number of gaussians, a list of guesses can be passed to the function in the format {{Prefactor 1, Mean 1, \[Sigma] 1}, {Prefactor 2, Mean 2, \[Sigma] 2}, ..., {Prefactor n, Mean n, \[Sigma] n}}. In this case, the function will automatically detect that you're passing it a guess and start fitting around these parameters.
The third optional parameter is a three-argument list of bounds that gives the fractional variation to allow in each of the prefactors, means, and widths. 
The next optional parameter is a flag \"g\", \"l\", or \"v\" to pick gaussian, lorentzian, or voigt peaks to fit with.";


(* ::Text:: *)
(*VoightModel has been removed since it is encompassed in Model.*)


(* ::Subsubsection:: *)
(*Fixed peak positions*)


(* ::Text:: *)
(*This function is similar to model but takes peak positions as given. Note that it is just a special case of the above with perfect constraints on the peak positions. Once I compare the two functions to each other, I can get rid of this function.*)


FixedPeaksModel[data_,offsets_,sf_:0.95,cp_:0]:=Module[{dataconfig,modelfunc,objfunc,fitvar,fitres,guess,n=Length[offsets],funcparams},
	dataconfig={A[#],\[Sigma][#]}&/@Range[n];
	funcparams={A[#],\[Mu]+offsets[[#]],\[Sigma][#]}&/@Range[n];
	guess={0#+Mean[data\[Transpose][[2]]],0#+Mean[data\[Transpose][[1]]]/4}&/@Range[n];
	modelfunc=gfn[##,fitvar]&@@@funcparams//Total;
	NonlinearModelFit[data,modelfunc,{Flatten@{dataconfig,\[Mu]},Flatten@{guess,Mean[data\[Transpose][[1]]]/2}}\[Transpose],fitvar,
		Method -> {NMinimize,
			Method -> {"DifferentialEvolution",
				"ScalingFactor" -> sf, "CrossProbability" -> cp,(*Tuned for good gaussian fitting. In particular, CrossProbability should be low. See e.g. http://mathematica.stackexchange.com/questions/2309/problem-with-nonlinearmodelfit*)
				"PostProcess" -> {FindMinimum, Method -> "QuasiNewton"}
			}
		}
	]
]
FixedPeaksModel::usage=" FixedPeaksModel[data,n] takes in a two-dimensional list data and a list of relative positions for gaussians to fit and returns a nonlinear model for the data.
\n This is a difficult thing to do in general, so the function is tuned to fit the sort of data we usually get from the XPS. It works best if CenterAndNormalize has first been applied to the data.";


FixedPeaksVoigtModel[data_,offsets_]:=Module[{dataconfig,modelfunc,objfunc,fitvar,fitres,guess,n=Length[offsets],funcparams},
	funcparams={A[#],\[Mu]+offsets[[#]],\[Sigma][#],\[Delta][#]}&/@Range[n];	
	dataconfig={A[#],\[Sigma][#],\[Delta][#]}&/@Range[n];
	guess={0#+Mean[data\[Transpose][[2]]],0#+Mean[data\[Transpose][[1]]]/4,0#+Mean[data\[Transpose][[1]]]/4}&/@Range[n];
	modelfunc=vfn[##,fitvar]&@@@funcparams//Total;
	NonlinearModelFit[data,modelfunc,{Flatten@{dataconfig,\[Mu]},Flatten@{guess,Mean[data\[Transpose][[1]]]/2}}\[Transpose],fitvar,
		Method -> {NMinimize,
			Method -> {"DifferentialEvolution",
				"ScalingFactor" -> 0.8, "CrossProbability" -> 0,(*Tuned for good gaussian fitting. In particular, CrossProbability should be low. See e.g. http://mathematica.stackexchange.com/questions/2309/problem-with-nonlinearmodelfit*)
				"PostProcess" -> {FindMinimum, Method -> "QuasiNewton"}
			}
		}
	]
]


(* ::Subsection:: *)
(*Summary generation for multipeak fits*)


(* ::Text:: *)
(*Summary data from a single fit. Mostly interested in relative weights (scale as A/\[Sigma]) and positions*)


FitSummary[data_,nlm_,func_:"g",fixedpeaks_:False,plot_:True]:=Module[{params=nlm["BestFitParameters"],dataconfig,modelfuncs,fitvar,n,datalistraw,datalist,totalweight,maxcenter,bounds={data[[1,1]],Last[data][[1]]}},
	Print["Sum of squares of residuals (as percent of total): "<>ToString[FortranForm[Total[nlm["FitResiduals"]^2]/Total[data\[Transpose][[2]]^2]*100]]];
	n=If[fixedpeaks,
		Last[Most[params]][[1,1]],
		Quiet[Check[Last[params][[1,1]],Print["Error generated in parsing parameters. Did you forget to set the fixedpeaks=True flag? Type ??FitSummary for usage instructions."],Part::partd],Part::partd]
	];
	If[fixedpeaks,
			If[func=="v",
				dataconfig={A[#],\[Sigma][#],\[Delta][#]}&/@Range[n],
				dataconfig={A[#],\[Sigma][#]}&/@Range[n]
			],
			If[func=="v",
				dataconfig={A[#],\[Mu][#],\[Sigma][#],\[Delta][#]}&/@Range[n],
				dataconfig={A[#],\[Mu][#],\[Sigma][#]}&/@Range[n]
			]
	];
	datalistraw=dataconfig/.params;
	If[fixedpeaks,
		Print["Using fixed peaks analysis methods. "<>ToString[Last[params]]];
		If[func!="v", (* Renormalize gaussians and lorentzians: normally, gaussian is A^2/\[Sigma] Exp[...] and integrates \[Proportional]A^2. Here, we have absorbed 1/\[Sigma] into A^2 in defining the gaussian, so A^2 is really A^2/\[Sigma]. So, to get something proportional to the integral (A^2) we need to multiply by \[Sigma] again. The same is true for Lorentzians.*)
			totalweight=Total@Abs[datalistraw[[All,1]]^2*datalistraw[[All,2]]];
			datalist={Abs[datalistraw[[All,1]]^2*datalistraw[[All,2]]/(totalweight)],datalistraw[[All,2]]}\[Transpose];
			Print[TableForm[Flatten[{{{"Weight","\[Sigma]"}},datalist},1]]]
		,
			(* Not sure what the proper normalization is for Voigt functions. Working on this. *)
			totalweight=Total@Abs[datalistraw[[All,1]]^2];
			datalist={Abs[datalistraw[[All,1]]^2/(totalweight)],datalistraw[[All,2]],datalistraw[[All,3]]}\[Transpose];
			Print[TableForm[Flatten[{{{"A","\[Sigma]","\[Delta]"}},datalist},1]]]
		];
		modelfuncs=Switch[func,"l",lfn,"v",vfnfast,"g",gfn][##,fitvar]&@@@(Flatten@{\[Mu],dataconfig});
	,
		If[func!="v",
			totalweight=Total@Abs[datalistraw[[All,1]]^2*datalistraw[[All,3]]];
			datalist={Abs[datalistraw[[All,1]]^2*datalistraw[[All,3]]/(totalweight)],datalistraw[[All,2]],datalistraw[[All,3]]}\[Transpose];
			Print[TableForm[Flatten[{{{"Weight","\[Mu]","\[Sigma]"}},datalist},1]]];
		,
			(* Not sure what the proper normalization is for Voigt functions. Working on this. *)
			totalweight=Total@Abs[datalistraw[[All,1]]^2];
			datalist={Abs[datalistraw[[All,1]]^2/(totalweight)],datalistraw[[All,2]],datalistraw[[All,3]],datalistraw[[All,4]]}\[Transpose];
			Print[TableForm[Flatten[{{{"A","\[Mu]","\[Sigma]","\[Delta]"}},datalist},1]]];
		];
		modelfuncs=Switch[func,"l",lfn,"v",vfnfast,"g",gfn][##,fitvar]&@@@dataconfig;
	];
	If[plot,
		Print@Show@{
			ListPlot[data,PlotRange->All],
			Plot[nlm[fitvar],{fitvar,bounds[[1]],bounds[[2]]},PlotRange->All,PlotStyle->{Pink},ImageSize->Medium],
			Plot[Evaluate@{modelfuncs/.params},{fitvar,bounds[[1]],bounds[[2]]},PlotRange->All,ImageSize->Medium,PlotStyle->({Directive[Dashed,Thick,ColorData["Rainbow"][#]]} & /@Rescale[Range[n]])]};
		Print@Show@ListPlot[{data[[All,1]],nlm["FitResiduals"]}\[Transpose],PlotRange->All,PlotLabel->"Fit Residuals",AspectRatio->0.25,ImageSize->Medium];
	];
	Return[datalist];
]
FitSummary::usage="FitSummary takes in the data from the original fit and the nonlinear model that you fit to this data, and outputs some summary information and plots. Fitsummary returns the table of data it prints out during processing.
Optional arguments are a boolean fixedpeaks which tells the function whether or not the model was generated with the FixedPeaksModel function.
The last optional argument plot tells the function whether or not to generate plots or just tables of statistical data.";


ClearAll@ComparePeaks


ComparePeaks[summs_,sheets_,peakpositions_:Null,models_:Null]:=If[ListQ[peakpositions],
	Module[{means=Last[#["BestFitParameters"]][[2]]&/@models,weights=Reverse[Most[#\[Transpose]]]\[Transpose]&/@summs,shiftpeakpos,peaklist},
		shiftpeakpos=Table[peakpositions+means[[i]],{i,1,Length[means]}];
		weights=Flatten/@weights;
		peaklist=Table[{shiftpeakpos[[i]],weights[[i]]}\[Transpose],{i,1,Length[means]}];
		Print[ListPlot[peaklist,PlotRange->All,PlotMarkers->(StringTake[#,-1]&/@sheets)]];
		Return[peaklist];
	]
,
	Print[ListPlot[Reverse[#\[Transpose][[{1,2}]]]\[Transpose]&/@summs,PlotRange->All,PlotMarkers->(StringTake[#,-1]&/@sheets)]];
	Return[Reverse[#\[Transpose][[{1,2}]]]\[Transpose]&/@summs];
];
ComparePeaks::usage="ComparePeaks shows a plot of locations of the peaks in each spectrum labeled by the last symbol in the second argument. Typically, the second argument is the list of sheets, so the last letter is unique.
In the case of output from the FixedPeaksModel, the peak positions used in the model and the list of models themselves must also be included as arguments (in that order.)";


(* ::Text:: *)
(*Create a function that gives a goodness of fit in terms of the sum of the squares of the residuals, but only if we're fitting at least one peak (so n=0 is out.)*)


modelvalue[data_,n_]/;NumericQ[n]:=If[n>=1,model[data,n][[1]],0]
