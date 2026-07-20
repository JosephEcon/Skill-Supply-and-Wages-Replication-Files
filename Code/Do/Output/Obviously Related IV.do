/*==============================================================================
  Script:   Obviously Related IV.do
  Paper:    Tables 4, A2, A3
  Outputs:  table4_oriv.rtf, tableA2_oriv_liml.rtf, tableA3_oriv_just_identified.rtf
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"


*Obviously Related IV errors-in-variables*

use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

*Duplicating the data to stack regressions*
expand 2, generate(halfsample)
* Stack two copies of the data to implement ORIV (Gillen et al, 2019):
* halfsample=0: uses sample 1's supply as endogenous var, sample 2's as instrument
* halfsample=1: uses sample 2's supply as endogenous var, sample 1's as instrument

*Updating clusterage to handle duplicates
replace clusterage=10*clusterage+halfsample
xtset clusterage year



*Generating the endogenous variable*
gen lncluster25hoursshareendog=lncluster25hoursshare1 if halfsample==0
replace lncluster25hoursshareendog=lncluster25hoursshare2 if halfsample==1

*Generating the internal iv*
gen lncluster25hoursshareiv=lncluster25hoursshare2 if halfsample==0
replace lncluster25hoursshareiv=lncluster25hoursshare1 if halfsample==1

*Generating the external iv*
gen lnnatcluster25hoursshareiv=lnnationalhourscluster25share2 if halfsample==0
replace lnnatcluster25hoursshareiv=lnnationalhourscluster25share1 if halfsample==1

*Residualising so that jackknife standard errors will work*

rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster175#i.halfsample i.cluster175#i.agegroup#i.halfsample) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage dlnnatcluster25hoursshareiv dlncluster25hoursshareiv dlncluster25hoursshareendog dblackshare dhispanicshare dasianshare dfemaleshare daverageage dmigrantshare dpostgradshare dnortheastshare dmidwestshare dsouthshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(i.cluster175#i.halfsample) resid
	predict hdfe`x', resid
}

*Specifying controls with halfsample interactions*
global hdfecontrolshalfsample c.hdfeblackshare#i.halfsample c.hdfehispanicshare#i.halfsample c.hdfeasianshare#i.halfsample c.hdfefemaleshare#i.halfsample c.hdfeaverageage#i.halfsample c.hdfemigrantshare#i.halfsample c.hdfepostgradshare#i.halfsample c.hdfenortheastshare#i.halfsample c.hdfemidwestshare#i.halfsample c.hdfesouthshare#i.halfsample
global hdfedcontrolshalfsample c.hdfedblackshare#i.halfsample c.hdfedhispanicshare#i.halfsample c.hdfedasianshare#i.halfsample c.hdfedfemaleshare#i.halfsample c.hdfedaverageage#i.halfsample c.hdfedmigrantshare#i.halfsample c.hdfedpostgradshare#i.halfsample c.hdfednortheastshare#i.halfsample c.hdfedmidwestshare#i.halfsample c.hdfedsouthshare#i.halfsample



*───────────────────────────────────────────────────────────────────────────────
* Jackknife first-stage F-statistics
* Over-identified specs (Tables 4 and A2, which share first stages): joint Wald
* test of both excluded instruments under the leave-one-cluster-out jackknife
* VCE. Just-identified specs (Table A3): Wald test of the single instrument.
*───────────────────────────────────────────────────────────────────────────────

* FE internal (instrument + size interaction)
quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share
local fjk_fe_int = r(F)

* FE external
quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share
local fjk_fe_ext = r(F)

* FD internal
quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share
local fjk_fd_int = r(F)

* FD external
quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share
local fjk_fd_ext = r(F)

* Just-identified first stages for Table A3 (no size interaction)
quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelncluster25hoursshareiv $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfelncluster25hoursshareiv
local fjk_ji_fe_int = r(F)
quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelnnatcluster25hoursshareiv $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfelnnatcluster25hoursshareiv
local fjk_ji_fe_ext = r(F)
quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlncluster25hoursshareiv $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfedlncluster25hoursshareiv
local fjk_ji_fd_int = r(F)
quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlnnatcluster25hoursshareiv $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)
quietly test hdfedlnnatcluster25hoursshareiv
local fjk_ji_fd_ext = r(F)

*───────────────────────────────────────────────────────────────────────────────
* Table 4: ORIV 2SLS Estimates
* Output: $out/table4_oriv.rtf
*───────────────────────────────────────────────────────────────────────────────

gen ln_cluster_share=hdfelncluster25hoursshareendog
label var ln_cluster_share "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(fe_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_fe_int'

*Second*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(iv_fe_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_fe_ext'

*FD internal*
replace ln_cluster_share=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(fd_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_fd_int'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(iv_fd_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_fd_ext'


esttab fe_model_errors iv_fe_model_errors fd_model_errors iv_fd_model_errors using "$out/table4_oriv.rtf", ///
   replace nocons label ///
   title("Table 4: ORIV 2SLS Estimates") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. Either instrument is also interacted with the log of the skill cluster's size in 2009, to account for the structure of the measurement error.") ///
   keep(ln_cluster_share) ///
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///
nomtitles ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

*───────────────────────────────────────────────────────────────────────────────
* Table A2: ORIV LIML Estimates
* Output: $out/tableA2_oriv_liml.rtf
*───────────────────────────────────────────────────────────────────────────────

replace ln_cluster_share=hdfelncluster25hoursshareendog
label var ln_cluster_share "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25) liml

store_regression, name(liml_fe_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_fe_int'

*Second*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25) liml

store_regression, name(iv_liml_fe_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_fe_ext'

*FD internal*
replace ln_cluster_share=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25) liml

store_regression, name(liml_fd_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_fd_int'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25) liml

store_regression, name(iv_liml_fd_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_fd_ext'


esttab liml_fe_model_errors iv_liml_fe_model_errors liml_fd_model_errors iv_liml_fd_model_errors using "$out/tableA2_oriv_liml.rtf", ///
   replace nocons label ///
   title("Table A2: ORIV LIML Estimates") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. Either instrument is also interacted with the log of the skill cluster's size in 2009, to account for the structure of the measurement error.") ///
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///
nomtitles ///
keep(ln_cluster_share) ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

*───────────────────────────────────────────────────────────────────────────────
* Table A3: ORIV Just-Identified Estimates
* Output: $out/tableA3_oriv_just_identified.rtf
*───────────────────────────────────────────────────────────────────────────────

*FE internal*
replace ln_cluster_share=hdfelncluster25hoursshareendog
label var ln_cluster_share "Ln(Cluster share)"

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelncluster25hoursshareiv) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(justid_fe_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_ji_fe_int'

*FE External*
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnatcluster25hoursshareiv) $hdfecontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(iv_justid_fe_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_ji_fe_ext'

*FD internal*
replace ln_cluster_share=hdfedlncluster25hoursshareendog

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlncluster25hoursshareiv) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(justid_fd_model_errors) instrument("Internal")
estadd scalar first_stage_f = `fjk_ji_fd_int'

*FD external*

jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnatcluster25hoursshareiv) $hdfedcontrolshalfsample i.year#i.halfsample [pweight=sqrt_weight], cluster(cluster25)

store_regression, name(iv_justid_fd_model_errors) instrument("External")
estadd scalar first_stage_f = `fjk_ji_fd_ext'


esttab justid_fe_model_errors iv_justid_fe_model_errors justid_fd_model_errors iv_justid_fd_model_errors using "$out/tableA3_oriv_just_identified.rtf", ///
   replace nocons label ///
   title("Table A3: ORIV Just-Identified Estimates") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue using the obviously related instrumental variables approach from Gillen et al (2019). Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The external instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009 from the other half of the sample. The internal instrument is the independent variable calculated in the other half of the sample. Estimates are not reported for column (3) because the specification is underidentified: its jackknife first-stage F-statistic of 0.5 shows that, without the size interaction, the internal instrument has no explanatory power in first differences.") ///
mgroups("ORIV-FE" "ORIV-FD", pattern(1 0 1 0 )) ///
nomtitles ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
keep(ln_cluster_share) ///
   stats(first_stage_f instrument fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Instruments" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar


