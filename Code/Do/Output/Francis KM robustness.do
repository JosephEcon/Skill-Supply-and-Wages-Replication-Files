/*==============================================================================
  Script:   Francis KM robustness.do
  Paper:    Table A8 (+ 2023-cutoff robustness, not in paper)
            The paper's Table A7/A8 notes describe the data simply as "the
            Autor et al (2020) series extended through to 2019"; the underlying
            series here are Joseph Francis's independent CPS reconstruction
            (Francis 2026, Econ Journal Watch), built by the self-contained
            sub-pipeline in Code/R scripts/Francis_replication and validated
            against the AGK baseline in Section 0 below.
  Purpose:  Rerun KM time-series specs on Francis data with 2019 and 2023 cutoffs
  Outputs:  francis_diagnostics_{2019,2023}.rtf
            tableA8_francis_{2019,2023}_levels.rtf, _fd.rtf, _fd2.rtf
            irfcombine_francis_{2019,2023}.png
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"


*───────────────────────────────────────────────────────────────────────────────
* Section 0: Verify Francis 6318 data against AGK baseline
*───────────────────────────────────────────────────────────────────────────────

* Load Francis 6318 (to 2017) for comparison
import delimited using "$rawdata/Francis/km-cg-rsup-6318.csv", clear
rename clphsg_all wprem2_francis
rename eu_lnclg relsup_francis
keep year wprem2_francis relsup_francis
tempfile francis_6318
save `francis_6318'

* Load AGK data (same procedure as RBET replication.do)
use "$rawdata/colhs1405", clear
merge 1:1 year using "$rawdata/km-cg-rsup-6317"
replace wprem2 = clphsg_all + .0037398 if year>=2006
replace relsup = eu_lnclg + .0152795 if year>=2006
keep year wprem2 relsup

* Merge and compare
merge 1:1 year using `francis_6318'
keep if year >= 1963 & year <= 2017
gen diff_wprem = wprem2 - wprem2_francis
gen diff_relsup = relsup - relsup_francis

di "=== FRANCIS vs AGK VALIDATION ==="
di "Max abs difference in wprem2:"
summ diff_wprem, detail
di "Max abs difference in relsup:"
summ diff_relsup, detail

* Run baseline regression on both and compare
gen time_v = year - 1963
reg wprem2 relsup time_v if year>=1963
scalar agk_beta = _b[relsup]
reg wprem2_francis relsup_francis time_v if year>=1963
scalar francis_beta = _b[relsup_francis]
di "AGK relsup coeff: " agk_beta
di "Francis relsup coeff: " francis_beta
di "Coefficient difference: " agk_beta - francis_beta


*───────────────────────────────────────────────────────────────────────────────
* Main Analysis: Loop over cutoffs
*───────────────────────────────────────────────────────────────────────────────

* MacKinnon (2010) response-surface helper. Defined BEFORE the cutoff loop:
* Stata cannot execute a program definition inside a foreach block (the `end`
* statement aborts the replayed loop body with a Break, r(1)).
capture program drop mk_rs
program define mk_rs, rclass
    args T bi b1 b2 b3
    return scalar cv = `bi' + `b1'/`T' + `b2'/(`T'^2) + `b3'/(`T'^3)
end

foreach cutoff in 2019 2023 {

di _n "================================================================="
di "FRANCIS KM ROBUSTNESS: DATA THROUGH `cutoff'"
di "=================================================================" _n

* Load Francis data (extended through 2023)
import delimited using "$rawdata/Francis/km-cg-rsup-6323.csv", clear
rename clphsg_all wprem2
rename eu_lnclg relsup
keep if year <= `cutoff'

* Generate time trend variables (time = year - 1963 for Francis data)
gen time = year - 1963
gen t92 = max(year-1992,0)
gen tsq = time*time/10
gen t3 = tsq*time/10
gen t4 = t3*time/10
gen t5 = t4*time/10

* Variable labels
label var wprem2 "Ln(Relative wage)"
label var relsup "Ln(Relative supply)"

tsset year


*───────────────────────────────────────────────────────────────────────────────
* Table 5 equivalent: Unit Root and Cointegration Tests
* Output: $out/francis_diagnostics_`cutoff'.rtf
*───────────────────────────────────────────────────────────────────────────────

* Panel A: Unit root tests (ADF with trend)
dfuller wprem2 if year>=1963, trend
scalar ur_wage_full_stat = r(Zt)
scalar ur_wage_full_p = r(p)

dfuller wprem2 if year>=1963&year<1993, trend
scalar ur_wage_pre_stat = r(Zt)
scalar ur_wage_pre_p = r(p)

dfuller wprem2 if year>=1993, trend
scalar ur_wage_post_stat = r(Zt)
scalar ur_wage_post_p = r(p)

dfuller relsup if year>=1963, trend
scalar ur_sup_full_stat = r(Zt)
scalar ur_sup_full_p = r(p)

dfuller relsup if year>=1963&year<1993, trend
scalar ur_sup_pre_stat = r(Zt)
scalar ur_sup_pre_p = r(p)

dfuller relsup if year>=1993, trend
scalar ur_sup_post_stat = r(Zt)
scalar ur_sup_post_p = r(p)

* Panel B: Cointegration tests (Engle-Granger)
* All cointegrating regressions restricted to year >= 1963 so the
* cointegrating-regression sample size matches the ADF-on-residuals sample
* size.
reg wprem2 relsup time if year>=1963
predict u1, resid
dfuller u1 if year>=1963
scalar coint_full_stat = r(Zt)
scalar coint_full_p = r(p)

reg wprem2 relsup time if year>=1963&year<1993
predict u2, resid
dfuller u2 if year>=1963&year<1993
scalar coint_pre_stat = r(Zt)
scalar coint_pre_p = r(p)

reg wprem2 relsup time if year>=1993
predict u3, resid
dfuller u3 if year>=1993
scalar coint_post_stat = r(Zt)
scalar coint_post_p = r(p)

*───────────────────────────────────────────────────────────────────────────────
* MacKinnon (2010) Table 3 response-surface CVs, computed for the current
* cutoff's sample sizes (Panel A: N=1 ct; Panel B: N=2 ct).
*───────────────────────────────────────────────────────────────────────────────

local T_full = `cutoff' - 1963 + 1
local T_pre  = 1992    - 1963 + 1
local T_post = `cutoff' - 1993 + 1

* (mk_rs is defined above the cutoff loop)

* Panel A (N=1 ct), 5%
foreach s in full pre post {
    mk_rs `T_`s'' -3.41049 -4.3904 -9.036 -45.374
    scalar fr_mka_5_`s' = r(cv)
}
* Panel B (N=2 ct), 5%
foreach s in full pre post {
    mk_rs `T_`s'' -3.78057 -9.5106 -12.074 0
    scalar fr_mk_`s'_5 = r(cv)
}
* Panel B (N=2 ct), 10%
foreach s in full pre post {
    mk_rs `T_`s'' -3.49631 -7.0815 -7.538 21.892
    scalar fr_mk_`s'_10 = r(cv)
}

display _n "MacKinnon (2010) Table 3 critical values for Francis `cutoff':"
display "  Panel A 5%:  full=" %6.3f fr_mka_5_full " pre=" %6.3f fr_mka_5_pre " post=" %6.3f fr_mka_5_post
display "  Panel B 5%:  full=" %6.3f fr_mk_full_5  " pre=" %6.3f fr_mk_pre_5  " post=" %6.3f fr_mk_post_5
display "  Panel B 10%: full=" %6.3f fr_mk_full_10 " pre=" %6.3f fr_mk_pre_10 " post=" %6.3f fr_mk_post_10


* Write combined RTF table (same format as RBET Table 5)
tempname fh
file open `fh' using "$out/francis_diagnostics_`cutoff'.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table 5 (Francis `cutoff'): Unit Root and Cointegration Tests\par}" _n

local c1 2508
local c2 4164
local c3 5820
local c4 7476

* Panel A header
file write `fh' "{\pard\ql\b Panel A: Unit Root Tests (ADF with Trend)\par}" _n
file write `fh' "{" _n

* Column headers
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {ADF Statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {P-value}\cell\row}" _n

* Relative Wage subheader
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {\i Ln(Relative wage)}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

* Wage rows
foreach sample in full pre post {
	if "`sample'" == "full" local label "Full Sample (1963-`cutoff')"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local stat : display %6.3f ur_wage_`sample'_stat
	local crit : display %6.3f fr_mka_5_`sample'
	local pval : display %6.3f ur_wage_`sample'_p
	file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {  `label'}\cell"
	file write `fh' " \pard\intbl\qc {`stat'}\cell"
	file write `fh' " \pard\intbl\qc {`crit'}\cell"
	file write `fh' " \pard\intbl\qc {`pval'}\cell\row}" _n
}

* Relative Supply subheader
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {\i Ln(Relative supply)}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

* Supply rows
foreach sample in full pre post {
	if "`sample'" == "full" local label "Full Sample (1963-`cutoff')"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local stat : display %6.3f ur_sup_`sample'_stat
	local crit : display %6.3f fr_mka_5_`sample'
	local pval : display %6.3f ur_sup_`sample'_p
	local bordopt ""
	if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
	file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {  `label'}\cell"
	file write `fh' " \pard\intbl\qc {`stat'}\cell"
	file write `fh' " \pard\intbl\qc {`crit'}\cell"
	file write `fh' " \pard\intbl\qc {`pval'}\cell\row}" _n
}

* Panel B header
file write `fh' "{\pard\ql\b Panel B: Cointegration Tests (Engle-Granger)\par}" _n

* NOTE: Critical values are computed inline from MacKinnon (2010, QED WP 1227,
*       Table 3) response-surface coefficients for each sample's observation
*       count, so they self-adjust across the cutoff loop. The earlier code
*       hard-coded standard ADF unit-root critical values here
*       (-2.935/-2.978/-3.000), which were incorrect for the Engle-Granger
*       cointegration test on residuals.
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {ADF Statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (10%)}\cell\row}" _n

foreach sample in full pre post {
	if "`sample'" == "full" local label "Full Sample (1963-`cutoff')"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local cv5  : display %6.3f fr_mk_`sample'_5
	local cv10 : display %6.3f fr_mk_`sample'_10
	local stat : display %6.3f coint_`sample'_stat
	local bordopt ""
	if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
	file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {`label'}\cell"
	file write `fh' " \pard\intbl\qc {`stat'}\cell"
	file write `fh' " \pard\intbl\qc {`cv5'}\cell"
	file write `fh' " \pard\intbl\qc {`cv10'}\cell\row}" _n
}

file write `fh' "{\pard\ql\fs20 Notes: Francis (2026) independently-constructed CPS data through `cutoff'. Panel A reports ADF unit root tests with a linear trend. Panel B reports Engle-Granger cointegration tests. Critical values for both panels are computed inline from MacKinnon (2010, QED Working Paper 1227, Table 3) response-surface coefficients at each sample's observation count.\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'

capture drop u1 u2 u3


*───────────────────────────────────────────────────────────────────────────────
* Table 6 equivalent: KM Levels and FD Regressions
*───────────────────────────────────────────────────────────────────────────────

* ─── Levels regressions ───
reg wprem2 relsup time if year>=1963
outreg2 using "$out/tableA8_francis_`cutoff'_levels", replace ///
	addtext(Sample, 1963-`cutoff', Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time t92 if year>=1963
outreg2 using "$out/tableA8_francis_`cutoff'_levels", append ///
	addtext(Sample, 1963-`cutoff', Time trend, Linear spline) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time tsq if year>=1963
outreg2 using "$out/tableA8_francis_`cutoff'_levels", append ///
	addtext(Sample, 1963-`cutoff', Time trend, Quadratic) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1963&year<1993
outreg2 using "$out/tableA8_francis_`cutoff'_levels", append ///
	addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1993
outreg2 using "$out/tableA8_francis_`cutoff'_levels", append ///
	addtext(Sample, 1993-`cutoff', Time trend, Linear) nocons keep(relsup) word dec(3) label


* ─── First differenced regressions ───
gen dwprem2 = d.wprem2
gen drelsup = d.relsup
label var dwprem2 "Ln(Relative wage)"
label var drelsup "Ln(Relative supply)"

reg dwprem2 drelsup d.time if year>=1963, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd", replace ///
	addtext(Sample, 1963-`cutoff', Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.t92 if year>=1963, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd", append ///
	addtext(Sample, 1963-`cutoff', Time trend, Linear spline) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq if year>=1963, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd", append ///
	addtext(Sample, 1963-`cutoff', Time trend, Quadratic) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1963&year<1993, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd", append ///
	addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1993, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd", append ///
	addtext(Sample, 1993-`cutoff', Time trend, Linear) nocons keep(drelsup) word dec(3) label


* ─── Two-year differenced regressions ───
preserve

gen time2=floor(time/2)
gen time2no=time-2*time2
drop if time2no!=0

tsset time2
replace dwprem2=d.wprem2
replace drelsup=d.relsup

reg dwprem2 drelsup d.time if year>=1963, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd2", replace ///
	addtext(Sample, 1963-`cutoff', Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq if year>=1963, r nocons
outreg2 using "$out/tableA8_francis_`cutoff'_fd2", append ///
	addtext(Sample, 1963-`cutoff', Time trend, Quadratic) nocons keep(drelsup) word dec(3) label

restore


*───────────────────────────────────────────────────────────────────────────────
* LP IRFs in First Differences
*───────────────────────────────────────────────────────────────────────────────

tsset year
gen ldrelsup = L.d.relsup

* Generate forward difference variables
foreach x of numlist 0/7 {
	gen f`x'dwprem2 = F`x'.wprem2 - L.wprem2
}

* ─── HC3 ───
tempfile irf_diff_hc3
postfile hc3_post int(horizon) double(b se df_r) using `irf_diff_hc3', replace

foreach x of numlist 0/7 {
	quietly regress f`x'dwprem2 drelsup d.time, vce(hc3) nocons
	post hc3_post (`x') (_b[drelsup]) (_se[drelsup]) (e(df_r))
}
postclose hc3_post

preserve
use `irf_diff_hc3', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se
twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("HC3") ///
	yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph save "$interout/differencesirf_francis_`cutoff'", replace
restore

* ─── Newey-West ───
tempfile irf_diff_newey
postfile newey_post int(horizon) double(b se df_r) using `irf_diff_newey', replace

foreach x of numlist 0/7 {
	quietly newey f`x'dwprem2 drelsup d.time, nocons lag(3)
	post newey_post (`x') (_b[drelsup]) (_se[drelsup]) (e(df_r))
}
postclose newey_post

preserve
use `irf_diff_newey', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se
twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("Newey-West") ///
	yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph save "$interout/differencesirfnewey_francis_`cutoff'", replace
restore

* ─── HC3 with Lags ───
tempfile irf_diff_lag
postfile lag_post int(horizon) double(b se df_r) using `irf_diff_lag', replace

foreach x of numlist 0/7 {
	quietly regress f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, nocons vce(hc3)
	post lag_post (`x') (_b[drelsup]) (_se[drelsup]) (e(df_r))
}
postclose lag_post

preserve
use `irf_diff_lag', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se
twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("HC3 with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph save "$interout/differencesirflag_francis_`cutoff'", replace
restore

* ─── Newey-West with Lags ───
tempfile irf_diff_neweylag
postfile neweylag_post int(horizon) double(b se df_r) using `irf_diff_neweylag', replace

foreach x of numlist 0/7 {
	quietly newey f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, nocons lag(3)
	post neweylag_post (`x') (_b[drelsup]) (_se[drelsup]) (e(df_r))
}
postclose neweylag_post

preserve
use `irf_diff_neweylag', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se
twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("Newey-West with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph save "$interout/differencesirflagnewey_francis_`cutoff'", replace
restore

* Combine all four LP plots
graph combine "$interout/differencesirf_francis_`cutoff'" ///
	"$interout/differencesirfnewey_francis_`cutoff'" ///
	"$interout/differencesirflag_francis_`cutoff'" ///
	"$interout/differencesirflagnewey_francis_`cutoff'", ///
	ycommon rows(2) title("LP IRFs (Francis `cutoff')")
graph export "$out/irfcombine_francis_`cutoff'.png", as(png) replace

* Clean up forward difference variables
capture drop f*dwprem2 ldrelsup dwprem2 drelsup

} // end cutoff loop
