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


use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

xtset clusterage year


*FE regression*
gen lnclustersharetable=lncluster25hoursshare
label var lnclustersharetable "Ln(Cluster share)"

*Partialling out hdfe to fix *
*Levels*
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* lncluster25shareminusown lnnationalclustershare blackshare hispanicshare asianshare femaleshare averageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* blackshare hispanicshare asianshare femaleshare averageage{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster25hoursshare* dlnnationalcluster25share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage{
	quietly: reghdfe `x' [pweight=weight], absorb(i.cluster175) resid
	predict hdfe`x', resid
}

* Fixed Effects Model
jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"



* Instrumental Variables Fixed Effects Model
replace lnclustersharetable=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster25share) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fe_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'
* First Differences Model
xtset clusterage year
replace lnclustersharetable=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage lnclustersharetable $dcontrols [pweight=weight], absorb(i.year i.cluster175) cluster(cluster25)
eststo fd_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"


* Instrumental Variables First Differences Model
replace lnclustersharetable=hdfedlncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnationalcluster25share) $hdfedcontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fd_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'

* Output regression table
esttab fe_model iv_fe_model fd_model iv_fd_model using "$out/mainregression.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue. Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclustersharetable) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

		 
		 
		 *Differences by initial wage*
replace lnclustersharetable=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustersharetable $controls if stdinitialwage25>0 [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model_above_mean
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"


replace lnclustersharetable=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster25share) $hdfecontrols i.year if stdinitialwage25>0 [pweight=weight], cluster(cluster25)
eststo iv_fe_model_above_mean
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'

replace lnclustersharetable=lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustersharetable $controls if stdinitialwage25<=0 [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model_below_mean
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

replace lnclustersharetable=hdfelncluster25hoursshare

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster25share) $controls i.year if stdinitialwage25<=0 [pweight=weight], cluster(cluster25)
eststo iv_fe_model_below_mean
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'

esttab fe_model_below_mean iv_fe_model_below_mean fe_model_above_mean iv_fe_model_above_mean using "$out/heterogeneityregressiontable.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7). Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. Below mean average wage means that the average wage of graduates from that skill cluster was below the mean graduate's wage in 2009. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclustersharetable) ///
   mgroups("Below Mean" "Above Mean", pattern(1 0 1 0)) ///
   mtitles("FE" "IV-FE" "FE" "IV-FE") ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)))		/// if you specify (X & Y) it posts these two statistics to the same cell
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" ///
"Controls" "Time trend"  "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 


*Placebo test?*
capture: gen lnclustertableminusown=lncluster25shareminusown
label var lnclustertableminusown "Ln(Other degree cluster share)"
jackknife, mse cluster(cluster25): reghdfe lnclustertableminusown lnclusterhoursshare $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo counterfactual_model_25
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"


replace lnclustertableminusown=hdfelncluster25shareminusown

jackknife, mse cluster(cluster25) : ivreg2 lnclustertableminusown (lnclusterhoursshare=hdfelnnationalclustershare) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_counterfactual_model_25
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'

esttab counterfactual_model_25 iv_counterfactual_model_25 using "$out/placebotest.rtf", ///
   replace nocons label ///
   addnotes("Notes: Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The instrument is defined as proportion of hours worked by natives from within the same skill cluster, excluding those from the same major, if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclusterhoursshare) ///
      cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)))		/// 
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 mtitles("FE" "IV-FE") ///
		 mgroups("Depvar", pattern(1 0)) ///
		 nodepvar



		 


