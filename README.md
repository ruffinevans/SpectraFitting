SpectraFitting
==============

Mathematica tools for quick and easy fitting and analysis of spectral lines. 

Introduction
------------

The package was initially designed to fit lorentzians to nanocavity transmission spectra. The package also has some powerful tools for fitting XPS spectra including Shirley background subtraction, Voigt multi-peak fitting, and routines to import data directly from the Thermo-Fisher K-alpha XPS excel. All of the techniques are designed to handle large datasets (thousands of spectra) efficiently with a minimum of parameter tweaking.

Usage Notes
-----------
The bulk of the code is in the mathematica package SpectraFitting.m. Open and run the package so that the functions are defined in your instance of mathematica.

To see an example, download the whole project (including the example data) and open example.nb. Make sure that the first line has the correct path to SpectraFitting.m.

The rest of the usage notes can be found in the example notebooks. There are examples for both transmission spectra and for XPS fitting.
