/*==============================================================================
  Script:   setup_globals.do
  Purpose:  Shared setup for all output scripts. Sets path directories,
            defines control variable globals, and provides helper programs.
  Usage:    Each output script calls this after setting $path:
              local master = $master
              if `master' != 1 {
                  global path "C:/Users/..."
              }
              do "$path/Code/Do/setup_globals.do"
==============================================================================*/

* Clear workspace
clear all

* Path directories
global rawdata "$path/Data/Raw"
global intermediatedata "$path/Data/intermediate"
global cleandata "$path/Data/clean"
global interout "$path/Intermediate Output"
global out "$path/Output"

* Control variable globals
* Note: westshare omitted to avoid perfect collinearity (region shares sum to 1 within cell).
global controls blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare
global dcontrols d.blackshare d.hispanicshare d.asianshare d.femaleshare d.averageage d.migrantshare d.postgradshare d.northeastshare d.midwestshare d.southshare
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage hdfemigrantshare hdfepostgradshare hdfenortheastshare hdfemidwestshare hdfesouthshare
global hdfedcontrols hdfedblackshare hdfedhispanicshare hdfedasianshare hdfedfemaleshare hdfedaverageage hdfedmigrantshare hdfedpostgradshare hdfednortheastshare hdfedmidwestshare hdfedsouthshare

*───────────────────────────────────────────────────────────────────────────────
* Helper Programs
*───────────────────────────────────────────────────────────────────────────────

* store_regression: Store estimation results with standard indicator variables
*   name()       - required, the name for eststo
*   fstat        - optional flag, adds first-stage F-statistic from ivreg2
*   instrument() - optional string, adds instrument label (e.g., "Internal")
capture program drop store_regression
program define store_regression
    syntax, name(string) [fstat instrument(string)]
    eststo `name'
    estadd local fixed_effects "Yes"
    estadd local year_effects "Yes"
    estadd local controls "Yes"
    estadd local time_trend "Yes"
    if "`fstat'" != "" {
        estadd scalar first_stage_f = `e(widstat)'
    }
    if "`instrument'" != "" {
        estadd local instrument "`instrument'"
    }
end

* first_stage_jk: run a just-identified first-stage regression with jackknife
* inference (leave-one-cluster-out, clustered at cluster25). Returns:
*   r(b)  first-stage coefficient on the excluded instrument (full sample)
*   r(se) jackknife standard error
*   r(F)  jackknife first-stage F = (b/se_jk)^2 (Wald test of the instrument)
*   r(lo), r(hi) 95% confidence bounds b +/- 1.96*se_jk
* Usage: first_stage_jk, endog(y) inst(z) rhs($hdfecontrols i.year)
*        [cond(if ...)] [wt([pweight=sqrt_weight])] [cl(clustervar)]
capture program drop first_stage_jk
program define first_stage_jk, rclass
    syntax, endog(varname) inst(varname) [rhs(string) cond(string) wt(string) cl(varname)]
    if "`cl'" == "" local cl cluster25
    quietly reg `endog' `inst' `rhs' `cond' `wt', cluster(`cl')
    local b = _b[`inst']
    quietly jackknife, mse cluster(`cl'): reg `endog' `inst' `rhs' `cond' `wt', cluster(`cl')
    local se = _se[`inst']
    return scalar b  = `b'
    return scalar se = `se'
    return scalar F  = (`b'/`se')^2
    return scalar lo = `b' - 1.96*`se'
    return scalar hi = `b' + 1.96*`se'
end

* add_first_stage_panel: post first-stage results onto the current eststo
* estimates as the table's first-stage panel rows + jackknife F. Capture
* first_stage_jk's r() into locals BEFORE the second stage runs (which clears
* r()), then call this after store_regression:
*   first_stage_jk, endog(...) inst(...) rhs(...)
*   local b=r(b) ... etc ...
*   jackknife ...: ivreg2 ...
*   store_regression, name(...)
*   add_first_stage_panel, b(`b') se(`se') f(`f') lo(`lo') hi(`hi')
capture program drop add_first_stage_panel
program define add_first_stage_panel
    syntax, b(real) se(real) f(real) lo(real) hi(real)
    estadd scalar first_stage_b = `b'
    estadd local first_stage_se = "(" + string(`se',"%9.3f") + ")"
    estadd local first_stage_ci = "[" + string(`lo',"%9.2f") + "," + string(`hi',"%9.2f") + "]"
    estadd scalar first_stage_f = `f'
end
