/*==============================================================================
  Script:   Main Regressions.do
  Paper:    Tables 1, 2, 3
  Outputs:  table1_placebo_test.rtf, table2_main_regression.rtf,
            table3_heterogeneity.rtf
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"


use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

xtset clusterage year


*───────────────────────────────────────────────────────────────────────────────
* Residualise variables by partialling out HDFE
* ivreg2 cannot handle absorb(), so we manually residualise all variables
* against major-by-year-trend and major-by-age-group fixed effects
* The 'd' prefix variables are first-differences, 'hdfe' prefix are residuals
*───────────────────────────────────────────────────────────────────────────────

gen ln_cluster_share=lncluster25hoursshare
label var ln_cluster_share "Ln(Cluster share)"

*Partialling out hdfe to fix *
*Levels*
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* lncluster25shareminusown lnnationalclustershare blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}

*Differences*
foreach x of varlist lnclusterwage* lncluster25hoursshare* lnnationalcluster25share* blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare{
	gen d`x'=d.`x'
}

foreach x of varlist dlnclusterwage* dlncluster25hoursshare* dlnnationalcluster25share* dblackshare dhispanicshare dasianshare dfemaleshare daverageage dmigrantshare dpostgradshare dnortheastshare dmidwestshare dsouthshare{
	quietly: reghdfe `x' [pweight=sqrt_weight], absorb(i.cluster175) resid
	predict hdfe`x', resid
}

*───────────────────────────────────────────────────────────────────────────────
* Table 2: Elasticity of Graduate Wages to Supply of Graduates with Similar Majors
* Output: $out/table2_main_regression.rtf
*───────────────────────────────────────────────────────────────────────────────

* ln_cluster_share is reused across specifications via `replace` to carry the
* variable label "Ln(Cluster share)" for consistent esttab display

* Fixed Effects Model
jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model)



* Instrumental Variables Fixed Effects Model
* First stage with jackknife inference (coefficient panel + jackknife F)
first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local b=r(b)
local se=r(se)
local f=r(F)
local lo=r(lo)
local hi=r(hi)
replace ln_cluster_share=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model)
add_first_stage_panel, b(`b') se(`se') f(`f') lo(`lo') hi(`hi')

* First Differences Model
xtset clusterage year
replace ln_cluster_share=d.lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe d.lnclusterwage ln_cluster_share $dcontrols [pweight=sqrt_weight], absorb(i.year i.cluster175) cluster(cluster25)
store_regression, name(fd_model)


* Instrumental Variables First Differences Model
first_stage_jk, endog(hdfedlncluster25hoursshare) inst(hdfedlnnationalcluster25share) rhs($hdfedcontrols i.year) wt([pweight=sqrt_weight])
local b=r(b)
local se=r(se)
local f=r(F)
local lo=r(lo)
local hi=r(hi)
replace ln_cluster_share=hdfedlncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (ln_cluster_share=hdfedlnnationalcluster25share) $hdfedcontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fd_model)
add_first_stage_panel, b(`b') se(`se') f(`f') lo(`lo') hi(`hi')

* Output regression table. In the manuscript the first-stage row label is
* rendered as the equation (8) expression ln(PredictedClusterHours_ct /
* PredictedTotalHours_t); rtf output carries the plain-text equivalent.
esttab fe_model iv_fe_model fd_model iv_fd_model using "$out/table2_main_regression.rtf", ///
   replace nocons label ///
   title("Table 2: Elasticity of Graduate Wages to Supply of Graduates with Similar Majors") ///
   addnotes("Notes: All regressions estimate equation (7) or a first-differenced analogue. Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(ln_cluster_share) ///
   mtitles("FE" "IV-FE" "FD" "IV-FD") ///
   cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2))) ///
   stats(first_stage_b first_stage_se first_stage_ci first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("First stage: Ln(Predicted cluster supply)" "" "" "F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(3 0 0 1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar



*───────────────────────────────────────────────────────────────────────────────
* Table 3: Elasticity by Average Wage of Skill Cluster
* Output: $out/table3_heterogeneity.rtf
* Split sample into above/below mean initial (2009) cluster wages
*───────────────────────────────────────────────────────────────────────────────

replace ln_cluster_share=lncluster25hoursshare

jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share $controls if stdinitialwage25>0 [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model_above_mean)


first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) cond(if stdinitialwage25>0) wt([pweight=sqrt_weight])
local b=r(b)
local se=r(se)
local f=r(F)
local lo=r(lo)
local hi=r(hi)
replace ln_cluster_share=hdfelncluster25hoursshare
jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) $hdfecontrols i.year if stdinitialwage25>0 [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model_above_mean)
add_first_stage_panel, b(`b') se(`se') f(`f') lo(`lo') hi(`hi')

replace ln_cluster_share=lncluster25hoursshare
jackknife, mse cluster(cluster25): reghdfe lnclusterwage ln_cluster_share $controls if stdinitialwage25<=0 [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(fe_model_below_mean)

first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) cond(if stdinitialwage25<=0) wt([pweight=sqrt_weight])
local b=r(b)
local se=r(se)
local f=r(F)
local lo=r(lo)
local hi=r(hi)
replace ln_cluster_share=hdfelncluster25hoursshare

jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (ln_cluster_share=hdfelnnationalcluster25share) $controls i.year if stdinitialwage25<=0 [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_fe_model_below_mean)
add_first_stage_panel, b(`b') se(`se') f(`f') lo(`lo') hi(`hi')

esttab fe_model_below_mean iv_fe_model_below_mean fe_model_above_mean iv_fe_model_above_mean using "$out/table3_heterogeneity.rtf", ///
   replace nocons label ///
   title("Table 3: Elasticity of Graduate Wages to Supply of Graduates with Similar Majors by Average Wage of Skill Cluster") ///
   addnotes("Notes: All regressions estimate equation (7). Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. Below mean average wage means that the average wage of graduates from that skill cluster was below the mean graduate's wage in 2009. The instrument is defined as proportion of hours worked by natives from within the same skill cluster if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(ln_cluster_share) ///
   mgroups("Below Mean" "Above Mean", pattern(1 0 1 0)) ///
   mtitles("FE" "IV-FE" "FE" "IV-FE") ///
cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)))		/// if you specify (X & Y) it posts these two statistics to the same cell
   stats(first_stage_b first_stage_se first_stage_ci first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("First stage: Ln(Predicted cluster supply)" "" "" "F-statistic" "Major-by-age fixed effects" "Year fixed effects" ///
"Controls" "Time trend"  "Observations")  fmt(3 0 0 1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar
		 


*───────────────────────────────────────────────────────────────────────────────
* Table 1: Placebo Test
* Output: $out/table1_placebo_test.rtf
* DV: supply of hours from OTHER majors in the same cluster (excluding own major)
* Tests whether own-major supply predicts other-major supply within the cluster
*───────────────────────────────────────────────────────────────────────────────

capture: gen ln_other_cluster_supply=lncluster25shareminusown
label var ln_other_cluster_supply "Ln(Other degree cluster share)"
label var lnclusterhoursshare "Ln(Cluster supply of hours)"
jackknife, mse cluster(cluster25): reghdfe ln_other_cluster_supply lnclusterhoursshare $controls [pweight=sqrt_weight], absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
store_regression, name(counterfactual_model_25)


replace ln_other_cluster_supply=hdfelncluster25shareminusown

* Jackknife first-stage F for the placebo IV
first_stage_jk, endog(lnclusterhoursshare) inst(hdfelnnationalclustershare) rhs($hdfecontrols i.year) wt([pweight=sqrt_weight])
local f=r(F)
jackknife, mse cluster(cluster25) : ivreg2 ln_other_cluster_supply (lnclusterhoursshare=hdfelnnationalclustershare) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster25)
store_regression, name(iv_counterfactual_model_25)
estadd scalar first_stage_f = `f'

esttab counterfactual_model_25 iv_counterfactual_model_25 using "$out/table1_placebo_test.rtf", ///
   replace nocons label ///
   title("Table 1: OLS Regression of Supply of Hours Worked from Other Majors within the Same Skill Cluster on the Supply of Hours Worked from the Same Major") ///
   addnotes("Notes: Major-age (five year) combinations, the unit of analysis, are weighted by the square root of the cell-size measure. Jackknife standard errors clustered at the skill cluster level are in parentheses and the resulting 95% confidence intervals are in square brackets. Controls cover the share of graduates by major-by-age cell who are female, Black, Hispanic, Asian, migrants, pursued postgraduate study, alongside their average age and the proportions from each US census region. The instrument is defined as proportion of hours worked by natives from within the same skill cluster, excluding those from the same major, if everyone worked the hours typical of their age-major-sex combination in the base period of 2009.") ///
   keep(lnclusterhoursshare) ///
      cells(b(fmt(3)) se(par fmt(3)) ci(par([ , ]) fmt(2)))		///
   stats(first_stage_f fixed_effects year_effects controls time_trend N, ///
         labels("F-statistic" "Major-by-age fixed effects" "Year fixed effects" "Controls" "Time trend" "Observations")  fmt(1 0 0 0 0 %8.0fc)) ///
		 collabels(none) ///
		 mtitles("FE" "IV-FE") ///
		 nodepvar
