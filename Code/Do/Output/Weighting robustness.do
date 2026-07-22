/*==============================================================================
  Script:   Weighting robustness.do
  Paper:    Table A6 (Robustness to Regression Weighting)
  Output:   $out/table_weighting_robustness.rtf

  Re-estimates the paper's IV specifications (Table 2 IV-FE / IV-FD) and ORIV
  specifications (Table 4: internal/external instrument, FE and FD) under three
  regression weights:
      (i)   sqrt(weight)   - the main specification
      (ii)  weight (full)  - population weighting
      (iii) unweighted
  For each of the 18 spec x weighting rows, reports the structural coefficient,
  its jackknife SE (leave-one-cluster-out, clustered at cluster25), and the
  jackknife first-stage F (just-identified IV: Wald test of the instrument;
  over-identified ORIV: joint Wald test of instrument and size interaction).
  The sqrt(weight) rows replicate the corresponding Table 2 / Table 4 columns.
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

tempfile results
postfile grid str16 spec str16 weighting double(b se fjk) using `results'

* ============================================================================
* Part 1: standard IV rows (Table 2 IV-FE / IV-FD machinery per weight)
* ============================================================================
foreach scheme in sqrt full unwt {
    if "`scheme'"=="sqrt" {
        local wtc "[pweight=sqrt_weight]"
        local wlab "sqrt(weight)"
    }
    else if "`scheme'"=="full" {
        local wtc "[pweight=weight]"
        local wlab "weight (full)"
    }
    else {
        local wtc ""
        local wlab "unweighted"
    }
    display _n "==================== IV rows: `wlab' ===================="

    use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear
    rename lnnationalhourscluster*share* lnnationalcluster*share*
    xtset clusterage year

    * residualise under this weighting (FE absorbs, as in Main Regressions.do)
    foreach x of varlist lnclusterwage lncluster25hoursshare lnnationalcluster25share $controls {
        quietly reghdfe `x' `wtc', absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
        predict hdfe`x', resid
    }
    * first differences, residualised (FD absorbs)
    foreach x of varlist lnclusterwage lncluster25hoursshare lnnationalcluster25share $controls {
        gen d`x'=d.`x'
        quietly reghdfe d`x' `wtc', absorb(i.cluster175) resid
        predict hdfed`x', resid
    }

    * FE (OLS, no IV; first-stage F not applicable)
    quietly jackknife, mse cluster(cluster25): reghdfe lnclusterwage lncluster25hoursshare $controls `wtc', absorb(i.year c.year#i.cluster175 i.cluster175#i.agegroup) cluster(cluster25)
    post grid ("FE") ("`wlab'") (_b[lncluster25hoursshare]) (_se[lncluster25hoursshare]) (.)

    * IV-FE
    first_stage_jk, endog(hdfelncluster25hoursshare) inst(hdfelnnationalcluster25share) rhs($hdfecontrols i.year) wt(`wtc')
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (hdfelncluster25hoursshare=hdfelnnationalcluster25share) $hdfecontrols i.year `wtc', cluster(cluster25)
    post grid ("IV-FE") ("`wlab'") (_b[hdfelncluster25hoursshare]) (_se[hdfelncluster25hoursshare]) (`f')

    * FD (OLS, no IV) — uses the d* variables generated above
    quietly jackknife, mse cluster(cluster25): reghdfe dlnclusterwage dlncluster25hoursshare $dcontrols `wtc', absorb(i.year i.cluster175) cluster(cluster25)
    post grid ("FD") ("`wlab'") (_b[dlncluster25hoursshare]) (_se[dlncluster25hoursshare]) (.)

    * IV-FD
    first_stage_jk, endog(hdfedlncluster25hoursshare) inst(hdfedlnnationalcluster25share) rhs($hdfedcontrols i.year) wt(`wtc')
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (hdfedlncluster25hoursshare=hdfedlnnationalcluster25share) $hdfedcontrols i.year `wtc', cluster(cluster25)
    post grid ("IV-FD") ("`wlab'") (_b[hdfedlncluster25hoursshare]) (_se[hdfedlncluster25hoursshare]) (`f')
}

* ============================================================================
* Part 2: ORIV rows (Table 4 machinery per weight)
* ============================================================================
foreach scheme in sqrt full unwt {
    if "`scheme'"=="sqrt" {
        local wtc "[pweight=sqrt_weight]"
        local wlab "sqrt(weight)"
    }
    else if "`scheme'"=="full" {
        local wtc "[pweight=weight]"
        local wlab "weight (full)"
    }
    else {
        local wtc ""
        local wlab "unweighted"
    }
    display _n "==================== ORIV rows: `wlab' ===================="

    * --- build the stacked ORIV panel (as in Obviously Related IV.do) ---
    use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear
    expand 2, generate(halfsample)
    replace clusterage=10*clusterage+halfsample
    xtset clusterage year

    gen lncluster25hoursshareendog=lncluster25hoursshare1 if halfsample==0
    replace lncluster25hoursshareendog=lncluster25hoursshare2 if halfsample==1
    gen lncluster25hoursshareiv=lncluster25hoursshare2 if halfsample==0
    replace lncluster25hoursshareiv=lncluster25hoursshare1 if halfsample==1
    gen lnnatcluster25hoursshareiv=lnnationalhourscluster25share2 if halfsample==0
    replace lnnatcluster25hoursshareiv=lnnationalhourscluster25share1 if halfsample==1

    rename lnnationalhourscluster*share* lnnationalcluster*share*
    rename lncluster25hoursshareminusown* lncluster25shareminusown*

    foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls {
        quietly reghdfe `x' `wtc', absorb(c.year#i.cluster175#i.halfsample i.cluster175#i.agegroup#i.halfsample) resid
        predict hdfe`x', resid
    }
    foreach x of varlist lnclusterwage *iv lncluster25hoursshareendog $controls {
        gen d`x'=d.`x'
    }
    foreach x of varlist dlnclusterwage dlnnatcluster25hoursshareiv dlncluster25hoursshareiv dlncluster25hoursshareendog dblackshare dhispanicshare dasianshare dfemaleshare daverageage dmigrantshare dpostgradshare dnortheastshare dmidwestshare dsouthshare {
        quietly reghdfe `x' `wtc', absorb(i.cluster175#i.halfsample) resid
        predict hdfe`x', resid
    }

    global hdfecontrolshalfsample c.hdfeblackshare#i.halfsample c.hdfehispanicshare#i.halfsample c.hdfeasianshare#i.halfsample c.hdfefemaleshare#i.halfsample c.hdfeaverageage#i.halfsample c.hdfemigrantshare#i.halfsample c.hdfepostgradshare#i.halfsample c.hdfenortheastshare#i.halfsample c.hdfemidwestshare#i.halfsample c.hdfesouthshare#i.halfsample
    global hdfedcontrolshalfsample c.hdfedblackshare#i.halfsample c.hdfedhispanicshare#i.halfsample c.hdfedasianshare#i.halfsample c.hdfedfemaleshare#i.halfsample c.hdfedaverageage#i.halfsample c.hdfedmigrantshare#i.halfsample c.hdfedpostgradshare#i.halfsample c.hdfednortheastshare#i.halfsample c.hdfedmidwestshare#i.halfsample c.hdfedsouthshare#i.halfsample

    * --- ORIV-FE internal ---
    quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share $hdfecontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    quietly test hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (hdfelncluster25hoursshareendog=hdfelncluster25hoursshareiv c.hdfelncluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    post grid ("ORIV-FE (int)") ("`wlab'") (_b[hdfelncluster25hoursshareendog]) (_se[hdfelncluster25hoursshareendog]) (`f')

    * --- ORIV-FE external ---
    quietly jackknife, mse cluster(cluster25): reg hdfelncluster25hoursshareendog hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share $hdfecontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    quietly test hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfelnclusterwage (hdfelncluster25hoursshareendog=hdfelnnatcluster25hoursshareiv c.hdfelnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfecontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    post grid ("ORIV-FE (ext)") ("`wlab'") (_b[hdfelncluster25hoursshareendog]) (_se[hdfelncluster25hoursshareendog]) (`f')

    * --- ORIV-FD internal ---
    quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share $hdfedcontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    quietly test hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (hdfedlncluster25hoursshareendog=hdfedlncluster25hoursshareiv c.hdfedlncluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    post grid ("ORIV-FD (int)") ("`wlab'") (_b[hdfedlncluster25hoursshareendog]) (_se[hdfedlncluster25hoursshareendog]) (`f')

    * --- ORIV-FD external ---
    quietly jackknife, mse cluster(cluster25): reg hdfedlncluster25hoursshareendog hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share $hdfedcontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    quietly test hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share
    local f=r(F)
    quietly jackknife, mse cluster(cluster25): ivreg2 hdfedlnclusterwage (hdfedlncluster25hoursshareendog=hdfedlnnatcluster25hoursshareiv c.hdfedlnnatcluster25hoursshareiv#c.lninitialcluster25share) $hdfedcontrolshalfsample i.year#i.halfsample `wtc', cluster(cluster25)
    post grid ("ORIV-FD (ext)") ("`wlab'") (_b[hdfedlncluster25hoursshareendog]) (_se[hdfedlncluster25hoursshareendog]) (`f')
}

postclose grid

* ============================================================================
* Write the RTF table in the published row order and formats
* (coefficient 3dp, SE 3dp, first-stage F 1dp)
* ============================================================================
use `results', clear
gen worder = cond(weighting=="sqrt(weight)",1,cond(weighting=="weight (full)",2,3))
gen sorder = cond(spec=="FE",1,cond(spec=="IV-FE",2, ///
             cond(spec=="FD",3,cond(spec=="IV-FD",4, ///
             cond(spec=="ORIV-FE (int)",5,cond(spec=="ORIV-FE (ext)",6, ///
             cond(spec=="ORIV-FD (int)",7,8)))))))
gen block = (sorder>=5)
sort block worder sorder
list spec weighting b se fjk, noobs sepby(block worder)

tempname rtf
file open `rtf' using "$out/table_weighting_robustness.rtf", write replace
file write `rtf' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}\plain\fs24" _n
file write `rtf' "{\pard\keepn\qc {\b Table A6:} Robustness to Regression Weighting\par}" _n
file write `rtf' "\trowd\trgaph108\trleft0\cellx2200\cellx4400\cellx6000\cellx7600\cellx9200" _n
file write `rtf' "\pard\intbl\ql {\b Spec}\cell \pard\intbl\ql {\b Weighting}\cell \pard\intbl\qc {\b Coefficient}\cell \pard\intbl\qc {\b SE}\cell \pard\intbl\qc {\b First-stage F}\cell \row" _n
forvalues i = 1/`=_N' {
    file write `rtf' "\trowd\trgaph108\trleft0\cellx2200\cellx4400\cellx6000\cellx7600\cellx9200" _n
    file write `rtf' "\pard\intbl\ql `=spec[`i']'\cell \pard\intbl\ql `=weighting[`i']'\cell "
    file write `rtf' "\pard\intbl\qc " (string(b[`i'], "%9.3f")) "\cell "
    file write `rtf' "\pard\intbl\qc " (string(se[`i'], "%9.3f")) "\cell "
    * F is blank for the OLS (FE/FD) rows, where no instrument is used
    if fjk[`i'] < . {
        file write `rtf' "\pard\intbl\qc " (string(fjk[`i'], "%9.1f")) "\cell \row" _n
    }
    else {
        file write `rtf' "\pard\intbl\qc \cell \row" _n
    }
}
file write `rtf' "{\pard\qj {\i Notes:} Re-estimates the Table 2 FE, IV-FE, FD, and IV-FD specifications under three regression sets of weights - square root of estimated population size (main spec), proportional to estimated population size, and no weights (treats each major-age cell equally). " _n
file write `rtf' "{\u946?} is the structural elasticity (second-stage coefficient) and SE is its jackknife standard error, clustered at the skill-cluster level. " _n
file write `rtf' "The first-stage F is left blank for the FE and FD rows, which use no instrument. " _n
file write `rtf' "The lower rows add the obviously related instrumental variables (ORIV) estimates from Table 4 - internal- and external-instrument variants of ORIV-FE and ORIV-FD - under the same set of weights.\par}" _n
file write `rtf' "}" _n
file close `rtf'

display _n "===== DONE: table_weighting_robustness.rtf (18 rows) ====="
