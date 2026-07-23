
/*==================================================================================================
Project: 	Unpacking Skill Supply and Wages
Author: 	Joseph Richardson
Purpose:	This is the master code to replicate all of the tables and figures in the paper
----------------------------------------------------------------------------------------------------
Replication (see readme.txt for full details, data access, and exhibit map):
	1. Set $path in Section B to the root of the replication folder
	2. Set $rpath in Section B to the path of your Rterm.exe (R installation)
	3. If running the clustering stage, set mypath to the same value as $path in
	   "weighted clustering.R", "cluster_visualizations.R", "onet_activity_prep.R",
	   and "task clustering.R" (all in Code/R scripts)
	4. Toggle the local switches in Section C; defaults reproduce all tables and
	   figures from the saved analysis datasets (outputs-only mode)
----------------------------------------------------------------------------------------------------
Regression weighting:
	All IV/OLS regressions on the (degfieldd, agegroup, year) panel use
	[pweight=sqrt_weight], where sqrt_weight = sqrt(weight) and weight is the
	2009 cell-size measure (perwt sum × mean hours). Square-root weighting
	addresses heteroskedastic measurement error in cell-level outcomes (whose
	SE scales with 1/sqrt(n_cell)) while spreading leverage more evenly across
	clusters than pure population weighting. Population-weighted and unweighted
	robustness checks are reported in the Weighting Robustness appendix table.
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

local install=1	// Switch this option to zero to skip re-installing Stata packages (first run must use 1)

if `install'==1 {

foreach program in estout outreg2 gtools winsor2 boottest coefplot ereplace rsource{
	ssc install `program', replace
}
* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/") replace

* Install require (needed by reghdfe)
cap ssc install require, replace

* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/") replace

* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2, replace

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/) replace

* ARDL bounds test package (Kripfganz & Schneider)
ssc install ardl, replace

}

/*===============================================================================================
                                  B. Set Path Directories
===============================================================================================*/

global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global doclean "$path/Code/Do/Cleaning"
global dooutput "$path/Code/Do/Output"
global rawdata "$path/Data/Raw"
global intermediatedata "$path/Data/intermediate"
global cleandata "$path/Data/clean"
global interout "$path/Intermediate Output"
global rscriptpath "$path/Code/R scripts"
global out "$path/Output"
global rpath "C:/Program Files/R/R-4.5.1/bin/x64/Rterm.exe" // Path to Rterm.exe -- update version number as needed

/*===============================================================================================
                                  C. Set Options for Replication
===============================================================================================*/


* Default: outputs-only replication from the saved analysis datasets.
* For a FULL rebuild from the raw ACS extract, set cluster=1 and clean=1 as well
* (multi-hour runtime; requires ~7 GB raw data and generates ~59 GB of
* intermediates — see readme.txt for data access and disk requirements).
local cluster=0	// Clustering stage: major-occupation shares + R clustering (Tables B1-B3, Figures 2, B1)
local clean=0	// Data cleaning stage: build analysis panels from the raw ACS extract
local tables=1 	// Main tables (Tables 1-6, A8)
local figures=1 // Main figures (Figures 1, 3, 4, 5)
local apptab=1 	// Appendix tables (Tables A1-A7, A9-A11, B4, C1, C2)
local appfig=1 	// Appendix figures (Figures B2-B5; A1 is produced with the appendix tables)

/*===============================================================================================
                                  D. Execute Programs
===============================================================================================*/

** Clean the data. This step is optional

	if `clean'==1 {
		global master 1
		do "$doclean/clustering.do"
		do "$doclean/cleaning.do"
		do "$doclean/mw_union_series.do"			// National effective min. wage + union density series (Tables A9-A11)

	}
	
if `cluster'==1 {
		global master 1
		do "$doclean/clustering.do"					// Major-by-occupation share matrices (SOC2-SOC4)
		do "$doclean/degree_soc6_shares.do"			// Major-by-SOC6 share matrix (input to task clustering)

		rsource using "$rscriptpath/weighted clustering.R", rpath("$rpath") roptions(`"--vanilla"')		// Cluster files; Tables B1-B3; Figure B1
		rsource using "$rscriptpath/cluster_visualizations.R", rpath("$rpath") roptions(`"--vanilla"')	// Figure 2 (dendrogram)
		rsource using "$rscriptpath/onet_activity_prep.R", rpath("$rpath") roptions(`"--vanilla"')		// O*NET occupation-by-activity matrices
		rsource using "$rscriptpath/task clustering.R", rpath("$rpath") roptions(`"--vanilla"')			// Task-based (IWA) cluster files for Figure B5

	}
	
	
** Generate the main tables 

	if `tables'==1 {
		global master 1
		do "$dooutput/Main Regressions.do"			// Tables 1, 2, 3
		do "$dooutput/RBET replication.do"			// Tables 5, 6; Figures D1-D3
		do "$dooutput/Obviously Related IV.do"		// Tables 4, A2, A3
		do "$dooutput/Francis KM robustness.do"		// Table A8 (+ 2023-cutoff robustness, not in paper)
	}

** Generate the main figures

	if `figures'==1 {
		global master 1
		do "$dooutput/Figure 1.do"					// Figure 1
		do "$dooutput/Figure 3.do"					// Figure 3
		do "$dooutput/Figures 4 and 5.do"			// Figures 4, 5
	}


** Generate the appendix tables

	if `apptab'==1 {
	    global master 1
		do "$dooutput/Appendix Regressions.do"	// Tables A1, A4, A5, B4; Figure A1
		do "$dooutput/Acemoglu Autor Replication.do"	// Tables C1, C2
		do "$dooutput/Weighting robustness.do"	// Table A6
		do "$dooutput/Table A7 AGK extended.do"	// Table A7 (AGK series extended through 2019)
		do "$dooutput/KM controls robustness.do"	// Tables A9-A11 + Figures D4-D5 (min. wage + union density controls)
	}

** Generate the appendix figures

	if `appfig' == 1 {
		global master 1
		do "$dooutput/Appendix B coefplots.do"	// Figures B2, B3, B4
		do "$dooutput/Task clustering coefplots.do"	// Figure B5
	}
