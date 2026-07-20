/*==============================================================================
  Script:   Appendix Regressions.do
  Paper:    Tables A1, A4, A5, B4; Figure A1
  Outputs:  tableB4_clusters_2009.rtf, tableA1_varying_samples.rtf,
            tableA4_wild_bootstrap.rtf, table_age_restricted.rtf
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"


use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

xtset clusterage year

*Partialling out hdfe to fix *
*Levels*
rename lnnationalcluster25*share lnnational25*share
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage* lncluster25*share* lnnational*25*share blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster25hoursshare* dlnnationalcluster25share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage dmigrantshare dpostgradshare dnortheastshare dmidwestshare dsouthshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}


*───────────────────────────────────────────────────────────────────────────────
* Age-Restricted Sample (robustness check, not a numbered exhibit) (25-65)
* Output: $out/table_age_restricted.rtf
*───────────────────────────────────────────────────────────────────────────────

gen ln_cluster_share=lncluster25hoursshare
label var ln_cluster_share "Ln(Cluster share)"

*Robustness check restricting to 25-65*
jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share $controls if agegroup>0&agegroup<9 [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_modelagerestrict)

* Instrumental Variables Fixed Effects Modelagerestrict
replace ln_cluster_share=hdfelncluster25hoursshare
first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) cond(if agegroup>0&agegroup<9) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) $hdfecontrols i.year if agegroup>0&agegroup<9 [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_modelagerestrict)
estadd scalar first_stage_f = `f'

* First Differences Modelagerestrict
xtset clusterage year
replace ln_cluster_share=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage ln_cluster_share $dcontrols if agegroup>0&agegroup<9 [pweight=sqrt_weight], absorb(i.year i.cluster175) cluster(cluster25)
store_regression, name(fd_modelagerestrict)



* Instrumental Variables First Differences Modelagerestrict
replace ln_cluster_share=hdfedlncluster25hoursshare
first_stage_jk, endog(hdfedlncluster25hoursshare) inst(hdfedlnnationalcluster25share) rhs($hdfedcontrols i.year) cond(if agegroup>0&agegroup<9) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnationalcluster25share) $hdfedcontrols i.year if agegroup>0&agegroup<9 [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fd_modelagerestrict)
estadd scalar first_stage_f = `f'


* Output regression table
esttab fe_modelagerestrict iv_fe_modelagerestrict fd_modelagerestrict iv_fd_modelagerestrict using "$out/table_age_restricted.rtf", ///
   replace nocons label ///
   title("Age-Restricted Sample (robustness check, not a numbered exhibit)") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue, restricting to age groups between 25 and 65. Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(ln_cluster_share) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
*───────────────────────────────────────────────────────────────────────────────
* Table A4: Wild Cluster Bootstrap Inference
* Output: $out/tableA4_wild_bootstrap.rtf
*───────────────────────────────────────────────────────────────────────────────

*FE regression*
replace ln_cluster_share=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_modelwild)

reghdfe lnclusterwage ln_cluster_share $controls i.year c.year#i.cluster175 [pweight=sqrt_weight], absorb( i.cluster175#i.agegroup) cluster(cluster25)
boottest ln_cluster_share, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
* TODO: r(CI) can have multiple rows when the bootstrap test stat is non-monotone
* in the null hypothesis value (yielding a disconnected CI). The current code grabs
* only the first row's bounds. With the new (region/migrant/postgrad) covariates,
* this FE specification's bootstrap inverted to a disconnected set:
*   [-0.0008, 0.0069] ∪ [0.0481, 0.1003], with the point estimate (0.076) in the
* second interval. Currently we report only the first interval, so the displayed CI
* doesn't contain the point estimate (a sanity-check failure).
* Manual fix applied to v2.docx Table A8 col (1): cell shows the union explicitly.
* For a permanent fix here: detect rowsof(r(CI))>1, store both intervals as
* (lo, hi, lo2, hi2), and extend the esttab cells() argument to render with " ∪ "
* between them. For connected cases (rows 2-4 below), behavior is unchanged.
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = ln_cluster_share //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = ln_cluster_share //same as above

mat list lo
mat list hi //check we have the correct ones

estadd mat lo = lo : fe_modelwild
estadd mat hi = hi : fe_modelwild


* Instrumental Variables Fixed Effects Modelwild
replace ln_cluster_share=hdfelncluster25hoursshare
first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_modelwild)
estadd scalar first_stage_f = `f'
boottest ln_cluster_share, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = ln_cluster_share //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = ln_cluster_share //same as above

mat list lo
mat list hi //check we have the correct ones

estadd mat lo = lo : iv_fe_modelwild
estadd mat hi = hi : iv_fe_modelwild

* First Differences Modelwild
xtset clusterage year
replace ln_cluster_share=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage ln_cluster_share $dcontrols i.year [pweight=sqrt_weight], absorb( i.cluster175) cluster(cluster25)
store_regression, name(fd_modelwild)

boottest ln_cluster_share, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = ln_cluster_share //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = ln_cluster_share //same as above

mat list lo 
mat list hi //check we have the correct ones

estadd mat lo = lo : fd_modelwild
estadd mat hi = hi : fd_modelwild


* Instrumental Variables First Differences Modelwild
replace ln_cluster_share=hdfedlncluster25hoursshare
first_stage_jk, endog(hdfedlncluster25hoursshare) inst(hdfedlnnationalcluster25share) rhs($hdfedcontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnationalcluster25share) $hdfedcontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fd_modelwild)
estadd scalar first_stage_f = `f'
boottest ln_cluster_share, cluster(cluster25) seed(1234) nograph reps(9999) jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = ln_cluster_share //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = ln_cluster_share //same as above

mat list lo
mat list hi //check we have the correct ones

estadd mat lo = lo : iv_fd_modelwild
estadd mat hi = hi : iv_fd_modelwild



* Output regression table
esttab fe_modelwild iv_fe_modelwild fd_modelwild iv_fd_modelwild using "$out/tableA4_wild_bootstrap.rtf", ///
   replace nocons label ///
   title("Table A4: Wild Cluster Bootstrap Inference") ///
   addnotes("Notes: This table is an exact replication of Table 2, with the addition of wild cluster bootstrap-t 95% confidence intervals using jackknife standard errors in braces.") ///
   keep(ln_cluster_share) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)) /// if you specify (X & Y) it posts these two statistics to the same cell     
(lo(f(2) par(`"<"') keep(ln_cluster_share))  & /// 
     hi(f(2) par(""`">"') keep(ln_cluster_share))))  ///  
incelldelimiter(", ") ///puts comma in between the lo and hi
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 
		 
		 
*───────────────────────────────────────────────────────────────────────────────
* Table A1: Varying Supply Samples (No Professional Degrees / No Postgrads / Double Majors)
* Output: $out/tableA1_varying_samples.rtf
*───────────────────────────────────────────────────────────────────────────────

*Exclude those with professional degrees*

*  Fixed Effects Model
replace ln_cluster_share=lncluster25noproshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwagenoprof ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model_nopro)



* Instrumental Variables Fixed Effects Model
replace ln_cluster_share=hdfelncluster25noproshare
first_stage_jk, endog(hdfelncluster25noproshare) inst(hdfelnnational25noproshare) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwagenoprof (ln_cluster_share=hdfelnnational25noproshare) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model_nopro)
estadd scalar first_stage_f = `f'


**Exclude those with any postgraduate training


* Undergraduate only Fixed Effects Model
replace ln_cluster_share=lncluster25ugshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwageug ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model_ug)


* Instrumental Variables Fixed Effects Model
replace ln_cluster_share=hdfelncluster25ugshare
first_stage_jk, endog(hdfelncluster25ugshare) inst(hdfelnnational25ugshare) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwageug (ln_cluster_share=hdfelnnational25ugshare) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model_ug)
estadd scalar first_stage_f = `f'


**Single majors only and double majors assigned 50-50****
* Undergraduate only Fixed Effects Model
replace ln_cluster_share=lncluster25doubleshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwagesingle ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model_double)



* Instrumental Variables Fixed Effects Model
replace ln_cluster_share=hdfelncluster25doubleshare
first_stage_jk, endog(hdfelncluster25doubleshare) inst(hdfelnnational25doubleshare) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwagesingle (ln_cluster_share=hdfelnnational25doubleshare) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model_double)
estadd scalar first_stage_f = `f'



* Output regression table
esttab fe_model_nopro iv_fe_model_nopro fe_model_ug  iv_fe_model_ug fe_model_double iv_fe_model_double using "$out/tableA1_varying_samples.rtf", ///
   replace nocons label ///
   title("Table A1: Varying Supply Samples") ///
   addnotes("Notes: All regressions estimate equation (7). Columns (1) and (2) exclude those with professional degrees from the wage and supply calculations, columns (3) and (4) exclude those with any postgraduate qualifications, while columns (5) and (6) only include single majors in the wage calculations and allocate double majors' hours of work as 50% to their primary subject's cluster and 50% to their secondary one. Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(ln_cluster_share) ///
   mgroups("No Professional Degrees" "No Postgraduates" "Double Majors", pattern(1 0 1 0 1 0)) ///
   mtitles("FE" "IV-FE" "FE" "IV-FE" "FE" "IV-FE") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

		 
*───────────────────────────────────────────────────────────────────────────────
* Table B4: Clustering Using 2009 Occupational Data Only
* Output: $out/tableB4_clusters_2009.rtf
*───────────────────────────────────────────────────────────────────────────────

use "$cleandata/analysisweighted_average_linkage_jsd_soc409", clear

* 2009 clustering has 171 clusters (not 175) for 176 majors, so cluster171 is not
* 1:1 with degfieldd. Use degfieldd directly for the panel identifier.
egen clusterage = group(degfieldd agegroup)
xtset clusterage year


*FE regression*
gen ln_cluster_share=lncluster101hoursshare
label var ln_cluster_share "Ln(Cluster share)"

*Partialling out hdfe to fix *
*Levels*
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster101hoursshareminusown* lncluster101shareminusown*

foreach x of varlist lnclusterwage* lncluster101hoursshare* lnnationalcluster101share* lncluster101shareminusown lnnationalclustershare blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster171 i.cluster171#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster101hoursshare* lnnationalcluster101share* blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster101hoursshare* dlnnationalcluster101share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage dmigrantshare dpostgradshare dnortheastshare dmidwestshare dsouthshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(i.cluster171) resid
	predict hdfe`x', resid
}

* Fixed Effects Model
jackknife, mse cluster(cluster101): reghdfe lnclusterwage ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster171 i.cluster171#i.agegroup) cluster(cluster101)
store_regression, name(fe_model)



* Instrumental Variables Fixed Effects Model
* Jackknife first-stage F (clustered at cluster101, matching the second stage)
first_stage_jk, endog(hdfelncluster101hoursshare) inst(hdfelnnationalcluster101share) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight]) cl(cluster101)
local f=r(F)
replace ln_cluster_share=hdfelncluster101hoursshare
jackknife, mse cluster(cluster101): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster101share) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster101)
store_regression, name(iv_fe_model)
estadd scalar first_stage_f = `f'

* First Differences Model
xtset clusterage year
replace ln_cluster_share=d.lncluster101hoursshare
jackknife, mse cluster(cluster101): reghdfe d.lnclusterwage ln_cluster_share $dcontrols [pweight=sqrt_weight], absorb(i.year i.cluster171) cluster(cluster101)
store_regression, name(fd_model)


* Instrumental Variables First Differences Model
first_stage_jk, endog(hdfedlncluster101hoursshare) inst(hdfedlnnationalcluster101share) rhs($hdfedcontrols i.year) wt([pweight=sqrt_weight]) cl(cluster101)
local f=r(F)
replace ln_cluster_share=hdfedlncluster101hoursshare
jackknife, mse cluster(cluster101): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnationalcluster101share) $hdfedcontrols i.year [pweight=sqrt_weight], cluster(cluster101)
store_regression, name(iv_fd_model)
estadd scalar first_stage_f = `f'

* Output regression table
esttab fe_model iv_fe_model fd_model iv_fd_model using "$out/tableB4_clusters_2009.rtf", ///
   replace nocons label ///
   title("Table B4: Clustering Using 2009 Occupational Data Only") ///
   addnotes("Notes: This table estimates the same specification as Table 2, but uses the clusters formed in Table B3 rather than Table B1. The difference is that the occupational distribution for clustering was measured using 2009 data only, rather than the full sample.") ///
   keep(ln_cluster_share) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

		 
		 
		 
		 
		 
*───────────────────────────────────────────────────────────────────────────────
* Figure A1: Elasticity by Age Group (OLS and IV coefficient plots)
* Output: $out/coefplotage.jpg
*───────────────────────────────────────────────────────────────────────────────

* Reload main dataset (Table A3 loaded the 2009-clusters dataset)
use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear
xtset clusterage year
rename lnnationalhourscluster*share* lnnationalcluster*share*

gen ln_cluster_share=lncluster25hoursshare


foreach x in 0 1 2 3 4 5 6 7 8{
	
	

gen lnclustershareage`x'=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage lnclustershareage`x' $controls  if agegroup==`x' [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)


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
	


jackknife, mse cluster(cluster25): ivregress 2sls hdfe`x'lnclusterwage (hdfe`x'lncluster25hoursshare=hdfe`x'lnnationalcluster25share) hdfe`x'asianshare hdfe`x'averageage hdfe`x'blackshare hdfe`x'hispanicshare hdfe`x'femaleshare hdfe`x'migrantshare hdfe`x'postgradshare hdfe`x'northeastshare hdfe`x'midwestshare hdfe`x'southshare i.year if agegroup==`x' [pweight=sqrt_weight], cluster(cluster25)


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



*───────────────────────────────────────────────────────────────────────────────
* Table A9: No Demographic Controls (robustness for footnote 16)
* Output: $out/tableA5_no_controls.rtf
*
* Same four specifications as Table 2 but with the demographic controls
* dropped. Footnote 16 of the paper claims "these controls have negligible
* effect on our estimates" and points to this table as evidence.
*───────────────────────────────────────────────────────────────────────────────

* Re-residualise the level + difference variables we'll need below. The
* dataset was reloaded at line 345 (for Figure A1) which dropped the hdfe
* prefix variables created at the top of this file. Re-build them now.
foreach x of varlist lnclusterwage lncluster25hoursshare lnnationalcluster25share {
    capture confirm variable hdfe`x'
    if _rc != 0 {
        quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
        predict hdfe`x', resid
    }
}
* First differences (for IV-FD column)
xtset clusterage year
foreach x of varlist lnclusterwage lncluster25hoursshare lnnationalcluster25share {
    capture drop d`x'
    capture confirm variable hdfed`x'
    if _rc != 0 {
        gen d`x' = d.`x'
        quietly: reghdfe d`x' [pweight=sqrt_weight], absorb(i.cluster175) resid
        predict hdfed`x', resid
    }
}

* Restore the level ln_cluster_share variable
replace ln_cluster_share = lncluster25hoursshare

* Fixed Effects Model (no controls)
jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
eststo fe_nocontrols
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "No"
estadd local time_trend "Yes"

* Instrumental Variables Fixed Effects Model (no controls)
replace ln_cluster_share = hdfelncluster25hoursshare
first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs(i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) i.year [pweight=sqrt_weight], cluster(cluster25)
eststo iv_fe_nocontrols
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "No"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `f'

* First Differences Model (no controls)
xtset clusterage year
replace ln_cluster_share = d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage ln_cluster_share [pweight=sqrt_weight], absorb(i.year i.cluster175) cluster(cluster25)
eststo fd_nocontrols
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "No"
estadd local time_trend "Yes"

* Instrumental Variables First Differences Model (no controls)
replace ln_cluster_share = hdfedlncluster25hoursshare
first_stage_jk, endog(hdfedlncluster25hoursshare) inst(hdfedlnnationalcluster25share) rhs(i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnationalcluster25share) i.year [pweight=sqrt_weight], cluster(cluster25)
eststo iv_fd_nocontrols
estadd local fixed_effects "Yes"
estadd local year_effects "Yes"
estadd local controls "No"
estadd local time_trend "Yes"
estadd scalar first_stage_f = `f'

* Output regression table
esttab fe_nocontrols iv_fe_nocontrols fd_nocontrols iv_fd_nocontrols using "$out/tableA5_no_controls.rtf", ///
   replace nocons label ///
   title("Table A5: Elasticity of Graduate Wages to Supply of Graduates with Similar Majors, No Demographic Controls") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue, dropping the demographic controls used in Table 2. Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(ln_cluster_share) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations") fmt(1 0 0 0 0 %8.0fc)) ///
         collabels(none) ///
         nodepvar

