*Setting path and clearing*
local master=$master
if `master'==1{
	clear all
}
else {
clear all
global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global rawdata "$path/data/raw"
global intermediatedata "$path/data/intermediate"
global cleandata "$path/data/clean"
global interout "$path/intermediateoutput"
global out "$path/output"
}

*Controls*
global controls blackshare hispanicshare asianshare femaleshare averageage
global dcontrols d.blackshare d.hispanicshare d.asianshare d.femaleshare d.averageage
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage
global hdfedcontrols hdfedblackshare hdfedhispanicshare hdfedasianshare hdfedfemaleshare hdfedaverageage


*Obviously Related IV errors-in-variables*

use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

*Duplicating the data to stack regressions*
expand 2, generate(replicant)

*Updating clusterage to handle duplicates
replace clusterage=10*clusterage+replicant
xtset clusterage year



*Generating the endogenous variable*
gen lncluster25hoursshareendog=lncluster25hoursshare1 if replicant==0
replace lncluster25hoursshareendog=lncluster25hoursshare2 if replicant==1

*Generating the internal iv*
gen lncluster25hoursshareiv=lncluster25hoursshare2 if replicant==0
replace lncluster25hoursshareiv=lncluster25hoursshare1 if replicant==1

*Generating the external iv*
gen lnnatcluster25hoursshareiv=lnnationalhourscluster25share2 if replicant==0
replace lnnatcluster25hoursshareiv=lnnationalhourscluster25share1 if replicant==1

*Residualising so that jackknife standard errors will work*

rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175#i.replicant i.cluster175#i.agegroup#i.replicant) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage dlnnatcluster25hoursshareiv dlncluster25hoursshareiv dlncluster25hoursshareendog dblackshare dhispanicshare dasianshare dfemaleshare daverageage{
	quietly: reghdfe `x' [pweight=weight], absorb(i.cluster175#i.replicant) resid
	predict hdfe`x', resid
}

*Specifying controls with replicant interactions*
global hdfecontrolsreplicant c.hdfeblackshare#i.replicant c.hdfehispanicshare#i.replicant c.hdfeasianshare#i.replicant c.hdfefemaleshare#i.replicant c.hdfeaverageage#i.replicant
global hdfedcontrolsreplicant c.hdfedblackshare#i.replicant c.hdfedhispanicshare#i.replicant c.hdfedasianshare#i.replicant c.hdfedfemaleshare#i.replicant c.hdfedaverageage#i.replicant



*First regression*
gen lnclustersharetable=hdfelncluster25hoursshareendog
label var lnclustersharetable "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*Second*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo iv_fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'

*FD internal*
replace lnclustersharetable=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrols i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo iv_fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'


esttab fe_model_errors iv_fe_model_errors fd_model_errors iv_fd_model_errors using "$out/errorstable.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. Either instrument is also interacted with the log of the skill cluster's size in 2009, to account for the structure of the measurement error.") /// 
   keep(lnclustersharetable) ///
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///   
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
*LIML*

*First regression*
replace lnclustersharetable=hdfelncluster25hoursshareendog
label var lnclustersharetable "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25) liml

eststo liml_fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*Second*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25) liml

eststo iv_liml_fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'

*FD internal*
replace lnclustersharetable=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25) liml

eststo liml_fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrols i.year#i.replicant [pweight=weight], cluster(cluster25) liml

eststo iv_liml_fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'


esttab liml_fe_model_errors iv_liml_fe_model_errors liml_fd_model_errors iv_liml_fd_model_errors using "$out/errorstableliml.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. Either instrument is also interacted with the log of the skill cluster's size in 2009, to account for the structure of the measurement error.") /// 
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///   
keep(lnclustersharetable) ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
*Just-identified*

*FE internal*
replace lnclustersharetable=hdfelncluster25hoursshareendog
label var lnclustersharetable "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelncluster25hoursshareiv) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo justid_fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*FE External*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnatcluster25hoursshareiv) $hdfecontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo iv_justid_fe_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'

*FD internal*
replace lnclustersharetable=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlncluster25hoursshareiv) $hdfedcontrolsreplicant i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo justid_fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "Internal"

estadd scalar first_stage_f = `e(widstat)'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnatcluster25hoursshareiv) $hdfedcontrols i.year#i.replicant [pweight=weight], cluster(cluster25)

eststo iv_justid_fd_model_errors
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd local instrument "External"

estadd scalar first_stage_f = `e(widstat)'


esttab justid_fe_model_errors iv_justid_fe_model_errors justid_fd_model_errors iv_justid_fd_model_errors using "$out/errorstablejustid.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. ") /// 
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///   
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
keep(lnclustersharetable) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar


