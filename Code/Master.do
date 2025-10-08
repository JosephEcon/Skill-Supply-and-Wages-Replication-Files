
/*==================================================================================================
Project: 	Unpacking Skill Supply and Wages 
Author: 	Joseph Richardson
Purpose:	This is the master code to replicate all of the tables and figures in the paper
----------------------------------------------------------------------------------------------------
Index:		A. Install Programs
			B. Set Path Directories
			C. Set Options for Replication
			D. Execute Programs
	
===================================================================================================*/

clear all

/*===============================================================================================
                                  A. Install Programs 
===============================================================================================*/

foreach program in estout outreg2 gtools winsor2 boottest coefplot ereplace{
	ssc install `program', replace
}
* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)

/*===============================================================================================
                                  B. Set Path Directories
===============================================================================================*/

global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global doclean "$path/code/do/Cleaning"
global dooutput "$path/code/do/Output"
global rawdata "$path/data/raw"
global intermediatedata "$path/data/intermediate"
global cleandata "$path/data/clean"
global interout "$path/Intermediate Output"
global rscriptpath "$path/code/R scripts"
global out "$path/output"

/*===============================================================================================
                                  C. Set Options for Replication
===============================================================================================*/


local cluster=1	// Switch this option to zero if you do not want to run the clustering code
local clean=1	// Switch this option to zero if you do not want to run the data cleaning code
local tables=1 	// Switch this option to zero if you do not want to run the code replicating the ACS tables
local figures=1 // Switch this option to zero if you do not want to run the code replicating the figures
local apptab=1 	// Switch this option to zero if you do not want to run the code replicating the appendix tables 
local appfig=1 	// Switch this option to zero if you do not want to run the code replicating the appendix figures

/*===============================================================================================
                                  D. Execute Programs
===============================================================================================*/

** Clean the data. This step is optional

	if `clean'==1 {
		global master 1
		do "$doclean/clustering.do"
		do "$doclean/cleaning.do"

	}
	
if `cluster'==1 {
		global master 1
		do "$doclean/clustering.do"
		
			*May need to change version number in path, or path itself if using a non-windows operating system*
	rsource using "$rscriptpath/weighted clustering.r", rpath("C:\Program Files\R\R-4.5.1\bin\x64\Rterm.exe") roptions(`"--vanilla"')

		

	}
	
	
** Generate the main tables 

	if `tables'==1 {
		global master 1
		do "$dooutput/Main Regressions.do"			// generates Tables
		do "$dooutput/RBET replication.do"			// generates Katz and Murphy replication tables
		do "$dooutput/Obviously Related IV"  // Generates ORIV
	}

** Generate the main figures

	if `figures'==1 {
		global master 1
		do "$dooutput/Figure 1.do"
		do "$dooutput/Figure 3.do"
		do "$dooutput/Figures 4 and 5.do"

	}
	

** Generate the appendix tables

	if `apptab'==1 {
	    global master 1
		do "$dooutput/Appendix Regressions.do"	// Tables 
		do "$dooutput/Acemoglu Autor Replication.do"	// Tables 
	}

** Generate the appendix figures

	if `appfig' == 1 {
		global master 1
		do "$dooutput/Different clusters coefplots.do"
	}
