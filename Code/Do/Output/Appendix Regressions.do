*Setting path and clearing*
local master=$master
if `master'==1{
	clear
}
else {
clear all
global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global rawdata "$path/data/raw"
global intermediatedata "$path/data/intermediate"
global cleandata "$path/data/clean"
global interout "$path/intermediate output"
global out "$path/output"
}

*Controls*
global controls blackshare hispanicshare asianshare femaleshare averageage
global dcontrols d.blackshare d.hispanicshare d.asianshare d.femaleshare d.averageage
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage
global hdfedcontrols hdfedblackshare hdfedhispanicshare hdfedasianshare hdfedfemaleshare hdfedaverageage


use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

xtset clusterage year

*Partialling out hdfe to fix *
*Levels*
rename lnnationalcluster25*share lnnational25*share
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage* lncluster25*share* lnnational*25*share blackshare hispanicshare asianshare femaleshare averageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* blackshare hispanicshare asianshare femaleshare averageage{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster25hoursshare* dlnnationalcluster25share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}


*FE regression*
gen lnclustersharetable=lncluster25hoursshare
label var lnclustersharetable "Ln(Cluster share)"

*Robustness check restricting to 25-65*
jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustersharetable $controls if agegroup>0&agegroup<9 [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_modelagerestrict
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

* Instrumental Variables Fixed Effects Modelagerestrict
replace lnclustersharetable=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster25share) $hdfecontrols i.year if agegroup>0&agegroup<9 [pweight=weight], cluster(cluster25)
eststo iv_fe_modelagerestrict
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'

* First Differences Modelagerestrict
xtset clusterage year
replace lnclustersharetable=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage lnclustersharetable $dcontrols if agegroup>0&agegroup<9 [pweight=weight], absorb(i.year i.cluster175) cluster(cluster25)
eststo fd_modelagerestrict
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"



* Instrumental Variables First Differences Modelagerestrict
replace lnclustersharetable=hdfedlncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnationalcluster25share) $hdfedcontrols i.year if agegroup>0&agegroup<9 [pweight=weight], cluster(cluster25)
eststo iv_fd_modelagerestrict
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'


* Output regression table
esttab fe_modelagerestrict iv_fe_modelagerestrict fd_modelagerestrict iv_fd_modelagerestrict using "$out/agerestricttable.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue aged for groups aged between 25 and 65. Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclustersharetable) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
		 *Wild cluster bootstrap jackknife inference*
		 
*FE regression*
replace lnclustersharetable=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_modelwild
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

reghdfe lnclusterwage lnclustersharetable $controls i.year c.year#i.cluster175 [pweight=weight], absorb( i.cluster175#i.agegroup) cluster(cluster25)
boottest lnclustersharetable, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = lnclustersharetable //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = lnclustersharetable //same as above

mat list lo 
mat list hi //check we have the correct ones

estadd mat lo = lo : fe_modelwild
estadd mat hi = hi : fe_modelwild


* Instrumental Variables Fixed Effects Modelwild
replace lnclustersharetable=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster25share) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fe_modelwild
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'
boottest lnclustersharetable, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = lnclustersharetable //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = lnclustersharetable //same as above

mat list lo 
mat list hi //check we have the correct ones

estadd mat lo = lo : iv_fe_modelwild
estadd mat hi = hi : iv_fe_modelwild

* First Differences Modelwild
xtset clusterage year
replace lnclustersharetable=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage lnclustersharetable $dcontrols i.year [pweight=weight], absorb( i.cluster175) cluster(cluster25)
eststo fd_modelwild
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

boottest lnclustersharetable, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = lnclustersharetable //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = lnclustersharetable //same as above

mat list lo 
mat list hi //check we have the correct ones

estadd mat lo = lo : fd_modelwild
estadd mat hi = hi : fd_modelwild


* Instrumental Variables First Differences Modelwild
replace lnclustersharetable=hdfedlncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnationalcluster25share) $hdfedcontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fd_modelwild
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'
boottest lnclustersharetable, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = lnclustersharetable //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = lnclustersharetable //same as above

mat list lo 
mat list hi //check we have the correct ones

estadd mat lo = lo : iv_fd_modelwild
estadd mat hi = hi : iv_fd_modelwild



* Output regression table
esttab fe_modelwild iv_fe_modelwild fd_modelwild iv_fd_modelwild using "$out/wildtable.rtf", ///
   replace nocons label ///
   addnotes("Notes: This table is an exact replication of Table 2, with the addition of wild cluster bootstrap-t 95% confidence intervals using jackknife standard errors in braces.") ///
   keep(lnclustersharetable) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)) /// if you specify (X & Y) it posts these two statistics to the same cell     
(lo(f(2) par(`"<"') keep(lnclustersharetable))  & /// 
     hi(f(2) par(""`">"') keep(lnclustersharetable))))  ///  
incelldelimiter(", ") ///puts comma in between the lo and hi
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
		 
		 
*Undergraduate only, no professional degrees, dual major*




*Exclude those with professional degrees*

*  Fixed Effects Model
replace lnclustersharetable=lncluster25noproshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwagenoprof lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model_nopro
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"



* Instrumental Variables Fixed Effects Model
replace lnclustersharetable=hdfelncluster25noproshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwagenoprof (lnclustersharetable=hdfelnnational25noproshare) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fe_model_nopro
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'


**Exclude those with any postgraduate training


* Undergraduate only Fixed Effects Model
replace lnclustersharetable=lncluster25ugshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwageug lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model_ug
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"


* Instrumental Variables Fixed Effects Model
replace lnclustersharetable=hdfelncluster25ugshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwageug (lnclustersharetable=hdfelnnational25ugshare) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fe_model_ug
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'


**Single majors only and double majors assigned 50-50****
* Undergraduate only Fixed Effects Model
replace lnclustersharetable=lncluster25doubleshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwagesingle lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_model_double
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"



* Instrumental Variables Fixed Effects Model
replace lnclustersharetable=hdfelncluster25doubleshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwagesingle (lnclustersharetable=hdfelnnational25doubleshare) $hdfecontrols i.year [pweight=weight], cluster(cluster25)
eststo iv_fe_model_double
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `e(widstat)'



* Output regression table
esttab fe_model_nopro iv_fe_model_nopro fe_model_ug  iv_fe_model_ug fe_model_double iv_fe_model_double using "$out/differentsuppliestable.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7). Columns (1) and (2) exclude those with professional degrees from the wage and supply calculations, columns (3) and (4) exclude those with any postgraduate qualifications, while columns (5) and (6) only include single majors in the wage calculations and allocate double majors' hours of work as 50% to their primary subject's cluster and 50% to their secondary one. Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclustersharetable) ///
   mgroups("No Professional Degrees" "No Postgraduates" "Double Majors", pattern(1 0 1 0 1 0)) ///
   mtitles("FE" "IV-FE" "FE" "IV-FE" "FE" "IV-FE") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

		 
*Clustering using 2009 occupational data only*

use "$cleandata/analysisweighted_average_linkage_jsd_soc409", clear

xtset clusterage year


*FE regression*
gen lnclustersharetable=lncluster101hoursshare
label var lnclustersharetable "Ln(Cluster share)"

*Partialling out hdfe to fix *
*Levels*
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster101hoursshareminusown* lncluster101shareminusown*

foreach x of varlist lnclusterwage* lncluster101hoursshare* lnnationalcluster101share* lncluster101shareminusown lnnationalclustershare blackshare hispanicshare asianshare femaleshare averageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster101hoursshare* lnnationalcluster101share* blackshare hispanicshare asianshare femaleshare averageage{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster101hoursshare* dlnnationalcluster101share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage{
	quietly: reghdfe `x' [pweight=weight], absorb(i.cluster175) resid
	predict hdfe`x', resid
}

* Fixed Effects Model
jackknife, mse cluster(cluster101): reghdfe lnclusterwage lnclustersharetable $controls [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster101)
eststo fe_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"



* Instrumental Variables Fixed Effects Model
replace lnclustersharetable=hdfelncluster101hoursshare
jackknife, mse cluster(cluster101): ivreg2 hdfelnclusterwage (lnclustersharetable=hdfelnnationalcluster101share) $hdfecontrols i.year [pweight=weight], cluster(cluster101)
eststo iv_fe_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'
* First Differences Model
xtset clusterage year
replace lnclustersharetable=d.lncluster101hoursshare
jackknife, mse cluster(cluster101): reghdfe d.lnclusterwage lnclustersharetable $dcontrols [pweight=weight], absorb(i.year i.cluster175) cluster(cluster101)
eststo fd_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"


* Instrumental Variables First Differences Model
replace lnclustersharetable=hdfedlncluster101hoursshare
jackknife, mse cluster(cluster101): ivreg2 hdfedlnclusterwage (lnclustersharetable=hdfedlnnationalcluster101share) $hdfedcontrols i.year [pweight=weight], cluster(cluster101)
eststo iv_fd_model
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "Yes"
estadd local time_trend "Yes"

estadd scalar first_stage_f = `e(widstat)'

* Output regression table
esttab fe_model iv_fe_model fd_model iv_fd_model using "$out/mainregression09clusters.rtf", ///
   replace nocons label ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue. Major-age (five year) combinations, the unit of analysis, are weighted proportionally to the sum of their members. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the proportion of asian, hispanic, black, and female workers used to calculate the major-by-age group alongside their average age. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclustersharetable) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

		 
		 
		 
		 
		 
**********************
**Differences by age**
**********************	

replace lnclustersharetable=lncluster25hoursshare


foreach x in 0 1 2 3 4 5 6 7 8{
	
	

gen lnclustershareage`x'=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustershareage`x' $controls  if agegroup==`x' [pweight=weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)


mat b`x'=_b[lnclustershareage`x']
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1] 
mat ci`x'[2,1]=r(table)[6,1]
drop lnclustershareage`x'
}

matrix b=(b0, b1, b2, b3, b4, b5, b6, b7, b8)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9
matrix ci=(ci0, ci1, ci2, ci3, ci4, ci5, ci6, ci7, ci8)
matrix colnames ci=c1 c2 c3 c4 c5 c6 c7 c8 c9


coefplot mat(b), ci(ci) xline(0)  ytitle("Age Group") xtitle("Elasticity")  title("OLS") ///
ylabel(1 "20–24" 2 "25–29" 3 "30–34" 4 "35–39" 5 "40–44" 6 "45–49" 7 "50–54" 8 "55–59" 9 "60–64")
graph save "$interout/coefplotageols", replace


**Differences by age IV**


foreach x in 0 1 2 3 4 5 6 7 8{
	
	foreach y of varlist lnclusterwage lncluster25hoursshare lnnationalcluster25share $controls{
		reghdfe `y', absorb(c.year#i.clusterage i.clusterage) resid
		predict hdfe`x'`y', resid
	}
	


jackknife, mse cluster(cluster25): ivregress 2sls hdfe`x'lnclusterwage (hdfe`x'lncluster25hoursshare=hdfe`x'lnnationalcluster25share) hdfe`x'asianshare hdfe`x'averageage hdfe`x'blackshare hdfe`x'hispanicshare hdfe`x'femaleshare i.year if agegroup==`x' [pweight=weight], cluster(cluster25)


mat b`x'=_b[hdfe`x'lncluster25hoursshare]
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1] 
mat ci`x'[2,1]=r(table)[6,1]
drop hdfe`x'*
}

matrix b=(b0, b1, b2, b3, b4, b5, b6, b7, b8)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9
matrix ci=(ci0, ci1, ci2, ci3, ci4, ci5, ci6, ci7, ci8)
matrix colnames ci=c1 c2 c3 c4 c5 c6 c7 c8 c9


coefplot mat(b), ci(ci) xline(0)  ytitle("Age Group") xtitle("Elasticity")  title("IV") ///
ylabel(1 "20–24" 2 "25–29" 3 "30–34" 4 "35–39" 5 "40–44" 6 "45–49" 7 "50–54" 8 "55–59" 9 "60–64") 
graph save "$interout/coefplotageiv", replace

graph combine "$interout/coefplotageols" "$interout/coefplotageiv" , rows(2) xcommon
graph export "$out/coefplotage.jpg", as(jpg) replace




