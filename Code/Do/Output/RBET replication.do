/*==============================================================================
  Script:   RBET replication.do
  Paper:    Tables 5, 6, 7, 8, 9; Figures C1, C2, C3, C4
  Outputs:  table5_unit_root_cointegration.rtf,
            table6_katz_murphy_levels.rtf, table6_katz_murphy_fd.rtf,
            table6_katz_murphy_fd2.rtf, table7_ardl_bounds.rtf,
            table8_ecm.rtf, table9_cointegration_tests.rtf,
            irfcombine.png, differencesirfquadratic.png,
            irf_levels_augmented.png
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

*Loading in data*
use "$rawdata/colhs1405", clear

*File-path adjusted AGK cleaning code code*
list year wprem2 relsup
merge year using "$rawdata/km-cg-rsup-6317"
tab _merge
sort year
list

replace wprem2 = clphsg_all + .0037398 if year>=2006
*relsup - eu_lnclg = .0152795 in 2005
replace relsup = eu_lnclg + .0152795 if year>=2006

list

gen time = year - 1914
gen t49 = max(year-1949,0)
gen t59 = max(year-1959,0)
gen t92 = max(year-1992,0)
gen tsq = time*time/10
gen t3 = tsq*time/10
gen t4 = t3*time/10
gen t5 = t4*time/10
gen d49 = year==1949

*Setting the time*
tsset time

*College Wage Premium, 1914-2017, wprem2, young males for 1914-39 college premium change
*Col. (1)
reg wprem2 relsup time t49 t92
*Col. (2)
reg wprem2 relsup time t59 t92
predict pred2
*Col. (3)
reg wprem2 relsup time t59 t92 d49
predict pred3
*Col. (4)
*Shift in Relative Supply Coeff After 1949
gen relsup49 = relsup
replace relsup49 = 0 if year<=1949
reg wprem2 relsup relsup49 time t59 t92 d49



*───────────────────────────────────────────────────────────────────────────────
* Table 5: Unit Root and Cointegration Tests (combined)
* Output: $out/table5_unit_root_cointegration.rtf
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
* All cointegrating regressions are restricted to year >= 1963 so the
* cointegrating-regression sample size matches the ADF-on-residuals sample
* size. This ensures the MacKinnon (2010) response-surface T is unambiguous:
*   Full     1963-2017: T = 55
*   Pre-1993 1963-1992: T = 30
*   Post-1993 1993-2017: T = 25
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
* Compute ARDL bounds (PSS 2001) and Shin (1994) LM tests for each sample so
* the resulting scalars can be written into Panels C and D of Table 5.
* Linear trend is included throughout, matching Panels A and B.
*───────────────────────────────────────────────────────────────────────────────

tsset time

foreach spec in 1 2 3 {
    if `spec' == 1 {
        local sample_cond "year>=1963"
    }
    if `spec' == 2 {
        local sample_cond "year>=1963 & year<1993"
    }
    if `spec' == 3 {
        local sample_cond "year>=1993"
    }

    * ── ARDL bounds: AIC search over p∈{1,2}, q∈{1,2} with linear trend ──
    local best_aic = 1e20
    local best_p 1
    local best_q 1
    forvalues pp = 1/2 {
        forvalues qq = 1/2 {
            capture quietly ardl wprem2 relsup if `sample_cond', ///
                lags(`pp' `qq') ec exog(time)
            if _rc == 0 {
                local this_aic = -2*e(ll) + 2*e(rank)
                if `this_aic' < `best_aic' {
                    local best_aic = `this_aic'
                    local best_p `pp'
                    local best_q `qq'
                }
            }
        }
    }
    quietly ardl wprem2 relsup if `sample_cond', lags(`best_p' `best_q') ec exog(time)
    scalar t5_ardl_p`spec'    = `best_p'
    scalar t5_ardl_q`spec'    = `best_q'
    quietly estat ectest
    scalar t5_ardl_F`spec'    = r(F_pss)
    scalar t5_ardl_t`spec'    = r(t_pss)
    matrix t5_ardl_cv`spec'   = r(cvmat)
    scalar t5_ardl_F_I0_`spec' = t5_ardl_cv`spec'[1,3]
    scalar t5_ardl_F_I1_`spec' = t5_ardl_cv`spec'[1,4]
    scalar t5_ardl_t_I0_`spec' = t5_ardl_cv`spec'[2,3]
    scalar t5_ardl_t_I1_`spec' = t5_ardl_cv`spec'[2,4]

    * ── Shin (1994) LM test on DOLS residuals (1 lead, 1 lag of D.relsup) ──
    capture drop u_shin S_shin Ssq_shin
    quietly reg wprem2 relsup time F.d.relsup L.d.relsup if `sample_cond'
    predict double u_shin if e(sample), resid
    quietly count if !missing(u_shin)
    local T = r(N)
    quietly gen double S_shin = sum(u_shin) if !missing(u_shin)
    local bw = floor(4*(`T'/100)^(2/9))
    if `bw' < 1 local bw 1
    local omega = 0
    forvalues j = 0/`bw' {
        quietly gen double _ulag = L`j'.u_shin
        quietly gen double _uprod = u_shin * _ulag
        quietly summ _uprod
        local gamma_j = r(sum) / `T'
        local weight = 1 - `j'/(`bw' + 1)
        if `j' == 0 local omega = `gamma_j'
        else        local omega = `omega' + 2 * `weight' * `gamma_j'
        drop _ulag _uprod
    }
    quietly gen double Ssq_shin = S_shin^2
    quietly summ Ssq_shin
    scalar t5_shin_LM`spec' = r(sum) / (`T'^2 * `omega')
    scalar t5_shin_T`spec'  = `T'
    drop u_shin S_shin Ssq_shin
}

* Shin (1994) Table 1 critical values for the C_tau (detrended) case, m=1
* regressor. The original paper (Econometric Theory 10:91-115, Cambridge) is
* paywalled; the secondary citation Erlat (MEEA Vol. 5, Table 5 notes)
* attributes the pair (0.121, 0.184) to "Shin (1994) Table 1" at the (5%, 1%)
* significance levels. Should be cross-checked against the primary source.
scalar t5_shin_cv_5  = 0.121
scalar t5_shin_cv_1  = 0.184


*───────────────────────────────────────────────────────────────────────────────
* MacKinnon (2010) finite-sample critical values via Table 3 response surface
*   CV(T) = beta_inf + beta_1/T + beta_2/T^2 + beta_3/T^3
* Source: MacKinnon, J.G. (2010). "Critical Values for Cointegration Tests."
*   QED Working Paper No. 1227, Queen's University, Table 3.
*
* Panel A: ADF unit-root with trend (N=1 ct).
* Panel B: Engle-Granger with trend (N=2 ct).
*
* Body Table 5 sample sizes (in years of level observations):
*   Full = 55 (1963-2017), Pre = 30 (1963-1992), Post = 25 (1993-2017).
*───────────────────────────────────────────────────────────────────────────────

local T_full = 55
local T_pre  = 30
local T_post = 25

capture program drop mk_rs
program define mk_rs, rclass
    args T bi b1 b2 b3
    return scalar cv = `bi' + `b1'/`T' + `b2'/(`T'^2) + `b3'/(`T'^3)
end

* Panel A 5% CVs (N=1 ct: bi=-3.41049, b1=-4.3904, b2=-9.036, b3=-45.374)
foreach s in full pre post {
    mk_rs `T_`s'' -3.41049 -4.3904 -9.036 -45.374
    scalar t5_mka_5_`s' = r(cv)
}
* Panel B 5% CVs (N=2 ct: bi=-3.78057, b1=-9.5106, b2=-12.074, b3=0)
foreach s in full pre post {
    mk_rs `T_`s'' -3.78057 -9.5106 -12.074 0
    scalar t5_mk_`s'_5 = r(cv)
}
* Panel B 10% CVs (N=2 ct: bi=-3.49631, b1=-7.0815, b2=-7.538, b3=21.892)
foreach s in full pre post {
    mk_rs `T_`s'' -3.49631 -7.0815 -7.538 21.892
    scalar t5_mk_`s'_10 = r(cv)
}

display _n "MacKinnon (2010) Table 3 critical values for body Table 5:"
display "  Panel A 5%:  full=" %6.3f t5_mka_5_full " pre=" %6.3f t5_mka_5_pre " post=" %6.3f t5_mka_5_post
display "  Panel B 5%:  full=" %6.3f t5_mk_full_5  " pre=" %6.3f t5_mk_pre_5  " post=" %6.3f t5_mk_post_5
display "  Panel B 10%: full=" %6.3f t5_mk_full_10 " pre=" %6.3f t5_mk_pre_10 " post=" %6.3f t5_mk_post_10


* Write combined RTF table
tempname fh
file open `fh' using "$out/table5_unit_root_cointegration.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table 5: Unit Root and Cointegration Tests\par}" _n

* Define column widths
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
	if "`sample'" == "full" local label "Full Sample"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local stat : display %6.3f ur_wage_`sample'_stat
	local crit : display %6.3f t5_mka_5_`sample'
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
	if "`sample'" == "full" local label "Full Sample"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local stat : display %6.3f ur_sup_`sample'_stat
	local crit : display %6.3f t5_mka_5_`sample'
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

* Column headers for Panel B
* NOTE: Critical values are MacKinnon (2010) Case IV (constant + trend) k=2,
*       finite-sample tabulated values. The earlier code used standard ADF
*       unit-root critical values here (-2.935/-2.978/-3.000), which were
*       incorrect for the Engle-Granger cointegration test on residuals.
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {ADF Statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (10%)}\cell\row}" _n

* Cointegration rows
foreach sample in full pre post {
	if "`sample'" == "full" local label "Full Sample"
	if "`sample'" == "pre"  local label "Pre-1993"
	if "`sample'" == "post" local label "Post-1993"
	local stat : display %6.3f coint_`sample'_stat
	local cv5  : display %6.3f t5_mk_`sample'_5
	local cv10 : display %6.3f t5_mk_`sample'_10
	local bordopt ""
	if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
	file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {`label'}\cell"
	file write `fh' " \pard\intbl\qc {`stat'}\cell"
	file write `fh' " \pard\intbl\qc {`cv5'}\cell"
	file write `fh' " \pard\intbl\qc {`cv10'}\cell\row}" _n
}

* Panel C header (ARDL bounds test)
file write `fh' "{\pard\ql\b Panel C: ARDL Bounds Test (Pesaran-Shin-Smith 2001)\par}" _n

* Column headers for Panel C
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {F-statistic}\cell"
file write `fh' " \pard\intbl\qc {t-statistic}\cell"
file write `fh' " \pard\intbl\qc {5% Bounds [I(0), I(1)]}\cell\row}" _n

* ARDL bounds rows
foreach sample in full pre post {
	if "`sample'" == "full" {
		local label "Full Sample"
		local s 1
	}
	if "`sample'" == "pre" {
		local label "Pre-1993"
		local s 2
	}
	if "`sample'" == "post" {
		local label "Post-1993"
		local s 3
	}
	local fstat : display %6.3f t5_ardl_F`s'
	local tstat : display %6.3f t5_ardl_t`s'
	local fcv : display "F: [" %4.2f t5_ardl_F_I0_`s' ", " %4.2f t5_ardl_F_I1_`s' "]; t: [" %5.2f t5_ardl_t_I0_`s' ", " %5.2f t5_ardl_t_I1_`s' "]"
	local bordopt ""
	if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
	file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {`label'}\cell"
	file write `fh' " \pard\intbl\qc {`fstat'}\cell"
	file write `fh' " \pard\intbl\qc {`tstat'}\cell"
	file write `fh' " \pard\intbl\qc {`fcv'}\cell\row}" _n
}

* Panel D header (Shin LM test)
file write `fh' "{\pard\ql\b Panel D: Shin (1994) LM Test (H0: cointegration)\par}" _n

* Column headers for Panel D
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {LM-statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (1%)}\cell\row}" _n

* Shin LM rows
local cv5 : display %5.3f t5_shin_cv_5
local cv1 : display %5.3f t5_shin_cv_1
foreach sample in full pre post {
	if "`sample'" == "full" {
		local label "Full Sample"
		local s 1
	}
	if "`sample'" == "pre" {
		local label "Pre-1993"
		local s 2
	}
	if "`sample'" == "post" {
		local label "Post-1993"
		local s 3
	}
	local lm : display %6.3f t5_shin_LM`s'
	local bordopt ""
	if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
	file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
	file write `fh' "\pard\intbl\ql {`label'}\cell"
	file write `fh' " \pard\intbl\qc {`lm'}\cell"
	file write `fh' " \pard\intbl\qc {`cv5'}\cell"
	file write `fh' " \pard\intbl\qc {`cv1'}\cell\row}" _n
}

* Notes
file write `fh' "{\pard\ql\fs20 Notes: Panel A reports augmented Dickey-Fuller unit root tests with a linear trend for the log college wage premium and log relative supply of college graduates. Panel B reports Engle-Granger cointegration tests (ADF on residuals from a cointegrating regression of the wage premium on relative supply and a linear trend). Critical values for Panels A and B are computed inline from MacKinnon (2010, QED Working Paper 1227, Table 3) response-surface coefficients evaluated at each sample's observation count. Panel C reports the Pesaran, Shin, and Smith (2001) ARDL bounds F- and t-tests for a levels relationship; the ARDL lag order is selected by AIC over p,q in {1,2}, with finite-sample I(0) and I(1) critical bounds at the 5\% level from Kripfganz and Schneider (2020). Panel D reports the Shin (1994) residual-based LM test for the null of cointegration; critical values are from Shin (1994), Table 1 (m=1 regressor, detrended case).\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'

*Variable labels
label var wprem2 "Ln(Relative wage)"
label var relsup "Ln(Relative supply)"


*───────────────────────────────────────────────────────────────────────────────
* Table 6: Katz-Murphy Regressions (Levels, First Differences, Two-Year Differences)
* Outputs: $out/table6_katz_murphy_levels, $out/table6_katz_murphy_fd, $out/table6_katz_murphy_fd2
*───────────────────────────────────────────────────────────────────────────────

*AGK Original
reg wprem2 relsup time if year>=1963
outreg2 using "$out/table6_katz_murphy_levels", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time t92 if year>=1963
outreg2 using "$out/table6_katz_murphy_levels", append addtext(Sample, 1963-2017, Time trend, Linear spline) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time tsq if year>=1963
outreg2 using "$out/table6_katz_murphy_levels", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1963&year<1993
outreg2 using "$out/table6_katz_murphy_levels", append addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1993
outreg2 using "$out/table6_katz_murphy_levels", append addtext(Sample, 1993-2017, Time trend, Linear) nocons keep(relsup) word dec(3) label

*First differenced regressions*
gen dwprem2=d.wprem2
gen drelsup=d.relsup
label var dwprem2 "Ln(Relative wage)"
label var drelsup "Ln(Relative supply)"


reg dwprem2 drelsup d.time if year>=1963, r nocons
outreg2 using "$out/table6_katz_murphy_fd", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.t92 if year>=1963, r nocons
outreg2 using "$out/table6_katz_murphy_fd", append addtext(Sample, 1963-2017, Time trend, Linear spline) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq if year>=1963, r nocons
outreg2 using "$out/table6_katz_murphy_fd", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1963&year<1993, r nocons
outreg2 using "$out/table6_katz_murphy_fd", append addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1993, r nocons
outreg2 using "$out/table6_katz_murphy_fd", append addtext(Sample, 1993-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label


*First differenced regressions - two year gap*
preserve

gen time2=floor(time/2)
gen time2no=time-2*time2
drop if time2no!=0

tsset time2
replace dwprem2=d.wprem2
replace drelsup=d.relsup

reg dwprem2 drelsup d.time if year>=1963, r nocons
outreg2 using "$out/table6_katz_murphy_fd2", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq if year>=1963, r nocons
outreg2 using "$out/table6_katz_murphy_fd2", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(drelsup) word dec(3) label


restore


*───────────────────────────────────────────────────────────────────────────────
* Table 7: ARDL Bounds Test for Cointegration (PSS 2001)
* Output: $out/table7_ardl_bounds.rtf
*───────────────────────────────────────────────────────────────────────────────

* Requires: ssc install ardl, replace
capture which ardl
if _rc != 0 {
	ssc install ardl, replace
}

* Generate post-1992 indicator (if not already present)
capture drop post1992
gen post1992 = (year >= 1993) if !missing(year)

* ─── Col 1: Bivariate (no trend, no break) ───
ardl wprem2 relsup if year>=1963, maxlags(2 2) aic ec
matrix ardl1_lags = e(lags)
scalar ardl1_aic_p = ardl1_lags[1,1]
scalar ardl1_aic_q = ardl1_lags[1,2]
* Extract long-run coefficient on relsup from LR equation
scalar ardl1_lr_relsup = _b[LR:relsup]
scalar ardl1_lr_relsup_se = _se[LR:relsup]
* Extract error correction (speed of adjustment) coefficient
scalar ardl1_ec = _b[ADJ:L.wprem2]
estat ectest
scalar ardl1_F = r(F_pss)
scalar ardl1_F_p = .

* ─── Col 2: KM spec (linear trend) ───
ardl wprem2 relsup if year>=1963, maxlags(2 2) aic exog(time) ec
matrix ardl2_lags = e(lags)
scalar ardl2_aic_p = ardl2_lags[1,1]
scalar ardl2_aic_q = ardl2_lags[1,2]
scalar ardl2_lr_relsup = _b[LR:relsup]
scalar ardl2_lr_relsup_se = _se[LR:relsup]
scalar ardl2_ec = _b[ADJ:L.wprem2]
estat ectest
scalar ardl2_F = r(F_pss)
scalar ardl2_F_p = .

* ─── Col 3: AKK/GK spec (trend + post-1992 break) ───
ardl wprem2 relsup if year>=1963, maxlags(2 2) aic exog(time post1992) ec
matrix ardl3_lags = e(lags)
scalar ardl3_aic_p = ardl3_lags[1,1]
scalar ardl3_aic_q = ardl3_lags[1,2]
scalar ardl3_lr_relsup = _b[LR:relsup]
scalar ardl3_lr_relsup_se = _se[LR:relsup]
scalar ardl3_ec = _b[ADJ:L.wprem2]
estat ectest
scalar ardl3_F = r(F_pss)
scalar ardl3_F_p = .

* ─── Write RTF Table ───
tempname fh
file open `fh' using "$out/table7_ardl_bounds.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table 7: ARDL Bounds Test for Cointegration (PSS 2001)\par}" _n

local c1 3000
local c2 4800
local c3 6600
local c4 8400

* Header row
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(1)}\cell"
file write `fh' " \pard\intbl\qc {(2)}\cell"
file write `fh' " \pard\intbl\qc {(3)}\cell\row}" _n

* Panel A: Bounds test
file write `fh' "{\pard\ql\b Panel A: Bounds F-Test\par}" _n

local f1 : display %6.3f ardl1_F
local f2 : display %6.3f ardl2_F
local f3 : display %6.3f ardl3_F
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {F-statistic}\cell"
file write `fh' " \pard\intbl\qc {`f1'}\cell"
file write `fh' " \pard\intbl\qc {`f2'}\cell"
file write `fh' " \pard\intbl\qc {`f3'}\cell\row}" _n

* ARDL order row
local o1 : display "(" ardl1_aic_p "," ardl1_aic_q ")"
local o2 : display "(" ardl2_aic_p "," ardl2_aic_q ")"
local o3 : display "(" ardl3_aic_p "," ardl3_aic_q ")"
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {ARDL order (p,q)}\cell"
file write `fh' " \pard\intbl\qc {`o1'}\cell"
file write `fh' " \pard\intbl\qc {`o2'}\cell"
file write `fh' " \pard\intbl\qc {`o3'}\cell\row}" _n

* Critical values
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {\i PSS critical values [I(0)/I(1)]:}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {  10%: [4.14, 4.92]}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {  5%: [5.11, 5.99]}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\clbrdrb\brdrw10\brdrs\cellx`c1'\clbrdrb\brdrw10\brdrs\cellx`c2'\clbrdrb\brdrw10\brdrs\cellx`c3'\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {  1%: [7.38, 8.41]}\cell"
file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n

* Panel B: Long-run coefficients
file write `fh' "{\pard\ql\b Panel B: Long-Run Coefficients\par}" _n

local lr1 : display %6.3f ardl1_lr_relsup
local lr2 : display %6.3f ardl2_lr_relsup
local lr3 : display %6.3f ardl3_lr_relsup
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Ln(Relative supply)}\cell"
file write `fh' " \pard\intbl\qc {`lr1'}\cell"
file write `fh' " \pard\intbl\qc {`lr2'}\cell"
file write `fh' " \pard\intbl\qc {`lr3'}\cell\row}" _n

local se1 : display %6.3f ardl1_lr_relsup_se
local se2 : display %6.3f ardl2_lr_relsup_se
local se3 : display %6.3f ardl3_lr_relsup_se
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(`se1')}\cell"
file write `fh' " \pard\intbl\qc {(`se2')}\cell"
file write `fh' " \pard\intbl\qc {(`se3')}\cell\row}" _n

* Panel C: Error correction coefficient
file write `fh' "{\pard\ql\b Panel C: Error Correction Estimate\par}" _n

local ec1 : display %6.3f ardl1_ec
local ec2 : display %6.3f ardl2_ec
local ec3 : display %6.3f ardl3_ec
file write `fh' "{\trowd\trgaph108\trleft-108\clbrdrb\brdrw10\brdrs\cellx`c1'\clbrdrb\brdrw10\brdrs\cellx`c2'\clbrdrb\brdrw10\brdrs\cellx`c3'\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {Speed of adjustment}\cell"
file write `fh' " \pard\intbl\qc {`ec1'}\cell"
file write `fh' " \pard\intbl\qc {`ec2'}\cell"
file write `fh' " \pard\intbl\qc {`ec3'}\cell\row}" _n

* Specification labels
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Time trend}\cell"
file write `fh' " \pard\intbl\qc {No}\cell"
file write `fh' " \pard\intbl\qc {Yes}\cell"
file write `fh' " \pard\intbl\qc {Yes}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Structural break}\cell"
file write `fh' " \pard\intbl\qc {No}\cell"
file write `fh' " \pard\intbl\qc {No}\cell"
file write `fh' " \pard\intbl\qc {Post-1992}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Sample}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell\row}" _n

* Notes
file write `fh' "{\pard\ql\fs20 Notes: Pesaran, Shin and Smith (2001) bounds test for cointegration. ARDL lag order selected by AIC with maximum 2 lags. Finite-sample critical values for Case III (unrestricted intercept, no trend) with k=1 from Kripfganz and Schneider (2020), evaluated at n=53. F-statistics above the I(1) bound reject the null of no levels relationship.\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'

capture drop post1992


*───────────────────────────────────────────────────────────────────────────────
* Table 8: ARDL Bounds Test and Conditional ECM (alternative trend specifications)
* Output: $out/table8_ecm.rtf
*
* Complements Table 7 by allowing richer deterministic trends in the
* cointegrating regression. Lag order selected by AIC (max 2,2). Bounds test
* and ECM coefficients reported jointly.
*───────────────────────────────────────────────────────────────────────────────

capture which ardl
if _rc != 0 {
    ssc install ardl, replace
}

tsset time

* AIC lag selection over p ∈ {1,2}, q ∈ {1,2}. Constraining q≥1 is required
* because q=0 collapses the contemporaneous and long-run effects and is
* outside the standard PSS bounds-test framework. The PSS finite-sample
* critical values reported by estat ectest depend on the chosen lag order
* (through the number of short-run coefficients) and are pulled from
* r(cvmat) per spec rather than hard-coded.

foreach spec in 1 2 3 {
    if `spec' == 1 local exogvars "time"
    if `spec' == 2 local exogvars "time t92"
    if `spec' == 3 local exogvars "time tsq"

    * Manual AIC search with p≥1, q≥1
    local best_aic = 1e20
    local best_p 1
    local best_q 1
    forvalues pp = 1/2 {
        forvalues qq = 1/2 {
            capture quietly ardl wprem2 relsup if year>=1963, ///
                lags(`pp' `qq') ec exog(`exogvars')
            if _rc == 0 {
                local this_aic = -2*e(ll) + 2*e(rank)
                if `this_aic' < `best_aic' {
                    local best_aic = `this_aic'
                    local best_p `pp'
                    local best_q `qq'
                }
            }
        }
    }

    * Re-estimate at the AIC-selected order
    ardl wprem2 relsup if year>=1963, lags(`best_p' `best_q') ec exog(`exogvars')
    scalar ecm`spec'_p      = `best_p'
    scalar ecm`spec'_q      = `best_q'
    scalar ecm`spec'_adj    = _b[ADJ:L.wprem2]
    scalar ecm`spec'_adjse  = _se[ADJ:L.wprem2]
    scalar ecm`spec'_sr     = _b[SR:D.relsup]
    scalar ecm`spec'_srse   = _se[SR:D.relsup]

    estat ectest
    scalar ecm`spec'_F      = r(F_pss)
    scalar ecm`spec'_t      = r(t_pss)
    matrix ecm`spec'_cv     = r(cvmat)
    * cvmat columns: I(0)_10, I(1)_10, I(0)_5, I(1)_5, I(0)_1, I(1)_1, p_I0, p_I1
    * cvmat rows:    1=F, 2=t
    scalar ecm`spec'_F_I0_5 = ecm`spec'_cv[1,3]
    scalar ecm`spec'_F_I1_5 = ecm`spec'_cv[1,4]
    scalar ecm`spec'_t_I0_5 = ecm`spec'_cv[2,3]
    scalar ecm`spec'_t_I1_5 = ecm`spec'_cv[2,4]
}

* ─── Write RTF Table ───
tempname fh
file open `fh' using "$out/table8_ecm.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table 8: ARDL Bounds Test and Conditional ECM (alternative trend specifications)\par}" _n

local c1 3000
local c2 4800
local c3 6600
local c4 8400

* Header row
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(1)}\cell"
file write `fh' " \pard\intbl\qc {(2)}\cell"
file write `fh' " \pard\intbl\qc {(3)}\cell\row}" _n

* Panel A: Bounds test
file write `fh' "{\pard\ql\b Panel A: Bounds F-Test\par}" _n

local f1 : display %6.3f ecm1_F
local f2 : display %6.3f ecm2_F
local f3 : display %6.3f ecm3_F
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {F-statistic}\cell"
file write `fh' " \pard\intbl\qc {`f1'}\cell"
file write `fh' " \pard\intbl\qc {`f2'}\cell"
file write `fh' " \pard\intbl\qc {`f3'}\cell\row}" _n

local t1 : display %6.3f ecm1_t
local t2 : display %6.3f ecm2_t
local t3 : display %6.3f ecm3_t
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {t-statistic on EC term}\cell"
file write `fh' " \pard\intbl\qc {`t1'}\cell"
file write `fh' " \pard\intbl\qc {`t2'}\cell"
file write `fh' " \pard\intbl\qc {`t3'}\cell\row}" _n

local o1 : display "(" ecm1_p "," ecm1_q ")"
local o2 : display "(" ecm2_p "," ecm2_q ")"
local o3 : display "(" ecm3_p "," ecm3_q ")"
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {ARDL order (p,q), AIC}\cell"
file write `fh' " \pard\intbl\qc {`o1'}\cell"
file write `fh' " \pard\intbl\qc {`o2'}\cell"
file write `fh' " \pard\intbl\qc {`o3'}\cell\row}" _n

* Per-spec K&S 2020 critical values at 5% (depend on chosen ARDL order)
local fcv1 : display "[" %4.2f ecm1_F_I0_5 ", " %4.2f ecm1_F_I1_5 "]"
local fcv2 : display "[" %4.2f ecm2_F_I0_5 ", " %4.2f ecm2_F_I1_5 "]"
local fcv3 : display "[" %4.2f ecm3_F_I0_5 ", " %4.2f ecm3_F_I1_5 "]"
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {\i F bounds, 5% [I(0), I(1)]}\cell"
file write `fh' " \pard\intbl\qc {`fcv1'}\cell"
file write `fh' " \pard\intbl\qc {`fcv2'}\cell"
file write `fh' " \pard\intbl\qc {`fcv3'}\cell\row}" _n

local tcv1 : display "[" %5.2f ecm1_t_I0_5 ", " %5.2f ecm1_t_I1_5 "]"
local tcv2 : display "[" %5.2f ecm2_t_I0_5 ", " %5.2f ecm2_t_I1_5 "]"
local tcv3 : display "[" %5.2f ecm3_t_I0_5 ", " %5.2f ecm3_t_I1_5 "]"
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {\i t bounds, 5% [I(0), I(1)]}\cell"
file write `fh' " \pard\intbl\qc {`tcv1'}\cell"
file write `fh' " \pard\intbl\qc {`tcv2'}\cell"
file write `fh' " \pard\intbl\qc {`tcv3'}\cell\row}" _n

* Panel B: ECM coefficients
file write `fh' "{\pard\ql\b Panel B: Conditional ECM Coefficients\par}" _n

local adj1 : display %6.3f ecm1_adj
local adj2 : display %6.3f ecm2_adj
local adj3 : display %6.3f ecm3_adj
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Speed of adjustment}\cell"
file write `fh' " \pard\intbl\qc {`adj1'}\cell"
file write `fh' " \pard\intbl\qc {`adj2'}\cell"
file write `fh' " \pard\intbl\qc {`adj3'}\cell\row}" _n

local adjse1 : display %6.3f ecm1_adjse
local adjse2 : display %6.3f ecm2_adjse
local adjse3 : display %6.3f ecm3_adjse
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(`adjse1')}\cell"
file write `fh' " \pard\intbl\qc {(`adjse2')}\cell"
file write `fh' " \pard\intbl\qc {(`adjse3')}\cell\row}" _n

local sr1 : display %6.3f ecm1_sr
local sr2 : display %6.3f ecm2_sr
local sr3 : display %6.3f ecm3_sr
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Short-run elasticity}\cell"
file write `fh' " \pard\intbl\qc {`sr1'}\cell"
file write `fh' " \pard\intbl\qc {`sr2'}\cell"
file write `fh' " \pard\intbl\qc {`sr3'}\cell\row}" _n

local srse1 : display %6.3f ecm1_srse
local srse2 : display %6.3f ecm2_srse
local srse3 : display %6.3f ecm3_srse
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(`srse1')}\cell"
file write `fh' " \pard\intbl\qc {(`srse2')}\cell"
file write `fh' " \pard\intbl\qc {(`srse3')}\cell\row}" _n

* Specification details
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Trend}\cell"
file write `fh' " \pard\intbl\qc {Linear}\cell"
file write `fh' " \pard\intbl\qc {Linear spline}\cell"
file write `fh' " \pard\intbl\qc {Quadratic}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Structural break}\cell"
file write `fh' " \pard\intbl\qc {No}\cell"
file write `fh' " \pard\intbl\qc {Post-1992}\cell"
file write `fh' " \pard\intbl\qc {No}\cell\row}" _n

file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Sample}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell"
file write `fh' " \pard\intbl\qc {1963-2017}\cell\row}" _n

* Notes
file write `fh' "{\pard\ql\fs20 Notes: Pesaran, Shin, and Smith (2001) bounds test for cointegration with conditional ECM coefficients. ARDL lag order selected by AIC over p, q in {1, 2}. The constraint q >= 1 is required for the bounds-test framework: q = 0 collapses the contemporaneous and long-run effects of relative supply into a single coefficient. The trend specifications mirror the levels regressions in Table 6: Spec 1 includes a linear trend, Spec 2 adds a slope kink at 1992 (t92 = max(year-1992, 0)), and Spec 3 replaces the kink with a quadratic trend. Critical values are finite-sample bounds from Kripfganz and Schneider (2020), Case III (unrestricted intercept, no trend; deterministic trend variables treated as exogenous regressors), evaluated separately for each spec at k = 1 and the appropriate sample size and number of short-run coefficients. F-statistics above the I(1) bound and t-statistics more negative than the I(1) bound jointly reject the null of no levels relationship. Sample: 1963-2017.\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'


*───────────────────────────────────────────────────────────────────────────────
* Table 9: Cointegration Tests across Sub-Samples (ARDL, Shin, Johansen)
* Output: $out/table9_cointegration_tests.rtf
*
* Each column uses a linear trend within the indicated sample, so Shin's
* exact published critical values (Table 1, m=1, detrended) apply throughout.
* This replaces the spline/quadratic full-sample approximation: instead of
* approximating an unknown deterministic-trend distribution, the structural
* break around 1992 is handled directly by sub-sample analysis (matching the
* sub-sample structure of Table 5).
*
* Three tests with different null hypotheses:
*   - ARDL bounds (PSS 2001):  H0 = no levels relationship
*   - Shin (1994):             H0 = cointegration exists
*   - Johansen (vecrank):      H0 = at most r cointegrating vectors
*───────────────────────────────────────────────────────────────────────────────

tsset time

foreach spec in 1 2 3 {
    if `spec' == 1 {
        local sample_cond "year>=1963"
    }
    if `spec' == 2 {
        local sample_cond "year>=1963 & year<1993"
    }
    if `spec' == 3 {
        local sample_cond "year>=1993"
    }

    * ─── ARDL bounds: AIC over p∈{1,2}, q∈{1,2} ───
    local best_aic = 1e20
    local best_p 1
    local best_q 1
    forvalues pp = 1/2 {
        forvalues qq = 1/2 {
            capture quietly ardl wprem2 relsup if `sample_cond', ///
                lags(`pp' `qq') ec exog(time)
            if _rc == 0 {
                local this_aic = -2*e(ll) + 2*e(rank)
                if `this_aic' < `best_aic' {
                    local best_aic = `this_aic'
                    local best_p `pp'
                    local best_q `qq'
                }
            }
        }
    }
    quietly ardl wprem2 relsup if `sample_cond', lags(`best_p' `best_q') ec exog(time)
    scalar cot`spec'_p   = `best_p'
    scalar cot`spec'_q   = `best_q'
    quietly estat ectest
    scalar cot`spec'_F   = r(F_pss)
    scalar cot`spec'_t   = r(t_pss)
    matrix cot`spec'_cv  = r(cvmat)
    scalar cot`spec'_F_I0_5 = cot`spec'_cv[1,3]
    scalar cot`spec'_F_I1_5 = cot`spec'_cv[1,4]

    * ─── Shin (1994) LM test on DOLS residuals (1 lead, 1 lag of D.relsup) ───
    capture drop u_shin S_shin Ssq_shin
    quietly reg wprem2 relsup time F.d.relsup L.d.relsup if `sample_cond'
    predict double u_shin if e(sample), resid
    quietly count if !missing(u_shin)
    local T = r(N)
    quietly gen double S_shin = sum(u_shin) if !missing(u_shin)
    local bw = floor(4*(`T'/100)^(2/9))
    if `bw' < 1 local bw 1
    local omega = 0
    forvalues j = 0/`bw' {
        quietly gen double _ulag = L`j'.u_shin
        quietly gen double _uprod = u_shin * _ulag
        quietly summ _uprod
        local gamma_j = r(sum) / `T'
        local weight = 1 - `j'/(`bw' + 1)
        if `j' == 0 local omega = `gamma_j'
        else        local omega = `omega' + 2 * `weight' * `gamma_j'
        drop _ulag _uprod
    }
    quietly gen double Ssq_shin = S_shin^2
    quietly summ Ssq_shin
    scalar cot`spec'_LM = r(sum) / (`T'^2 * `omega')
    scalar cot`spec'_T  = `T'
    scalar cot`spec'_bw = `bw'
    drop u_shin S_shin Ssq_shin

    * ─── Johansen test (vecrank, restricted linear trend, 2 lags) ───
    capture quietly vecrank wprem2 relsup if `sample_cond', trend(rtrend) lags(2)
    if _rc == 0 {
        matrix _tracemat = e(trace)
        matrix _maxmat   = e(max)
        scalar cot`spec'_tr = _tracemat[1,1]
        scalar cot`spec'_mx = _maxmat[1,1]
    }
    else {
        scalar cot`spec'_tr = .
        scalar cot`spec'_mx = .
    }
}

* Shin (1994) Table 1, m=1 regressor, "Detrended" (C_tau) case
scalar shin_cv_10 = 0.097
scalar shin_cv_5  = 0.121
scalar shin_cv_1  = 0.184

* MacKinnon-Haug-Michelis (1999) 5% critical values for k-r=2, restricted trend
scalar joh_cv_tr_5 = 25.32
scalar joh_cv_mx_5 = 18.96

* ─── Write RTF Table ───
tempname fh
file open `fh' using "$out/table9_cointegration_tests.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table 9: Cointegration Tests across Sub-Samples\par}" _n

local c1 3000
local c2 4800
local c3 6600
local c4 8400

* Header row
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {(1) Full 1963-2017}\cell"
file write `fh' " \pard\intbl\qc {(2) Pre-1993}\cell"
file write `fh' " \pard\intbl\qc {(3) Post-1992}\cell\row}" _n

* T row
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Sample size (T)}\cell"
file write `fh' " \pard\intbl\qc {`=cot1_T'}\cell"
file write `fh' " \pard\intbl\qc {`=cot2_T'}\cell"
file write `fh' " \pard\intbl\qc {`=cot3_T'}\cell\row}" _n

* Panel A: ARDL Bounds Test
file write `fh' "{\pard\ql\b Panel A: ARDL Bounds Test (H0: no levels relationship)\par}" _n

local f1 : display %6.3f cot1_F
local f2 : display %6.3f cot2_F
local f3 : display %6.3f cot3_F
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {F-statistic}\cell"
file write `fh' " \pard\intbl\qc {`f1'}\cell"
file write `fh' " \pard\intbl\qc {`f2'}\cell"
file write `fh' " \pard\intbl\qc {`f3'}\cell\row}" _n

local t1 : display %6.3f cot1_t
local t2 : display %6.3f cot2_t
local t3 : display %6.3f cot3_t
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {t-statistic on EC term}\cell"
file write `fh' " \pard\intbl\qc {`t1'}\cell"
file write `fh' " \pard\intbl\qc {`t2'}\cell"
file write `fh' " \pard\intbl\qc {`t3'}\cell\row}" _n

local fcv1 : display "[" %4.2f cot1_F_I0_5 ", " %4.2f cot1_F_I1_5 "]"
local fcv2 : display "[" %4.2f cot2_F_I0_5 ", " %4.2f cot2_F_I1_5 "]"
local fcv3 : display "[" %4.2f cot3_F_I0_5 ", " %4.2f cot3_F_I1_5 "]"
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {F bounds, 5% [I(0), I(1)]}\cell"
file write `fh' " \pard\intbl\qc {`fcv1'}\cell"
file write `fh' " \pard\intbl\qc {`fcv2'}\cell"
file write `fh' " \pard\intbl\qc {`fcv3'}\cell\row}" _n

* Panel B: Shin (1994) LM test (exact CVs apply for all columns)
file write `fh' "{\pard\ql\b Panel B: Shin (1994) LM Test (H0: cointegration)\par}" _n

local lm1 : display %6.3f cot1_LM
local lm2 : display %6.3f cot2_LM
local lm3 : display %6.3f cot3_LM
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {LM statistic}\cell"
file write `fh' " \pard\intbl\qc {`lm1'}\cell"
file write `fh' " \pard\intbl\qc {`lm2'}\cell"
file write `fh' " \pard\intbl\qc {`lm3'}\cell\row}" _n

local cv10 : display %6.3f shin_cv_10
local cv5  : display %6.3f shin_cv_5
local cv1  : display %6.3f shin_cv_1
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {10% critical value}\cell"
file write `fh' " \pard\intbl\qc {`cv10'}\cell"
file write `fh' " \pard\intbl\qc {`cv10'}\cell"
file write `fh' " \pard\intbl\qc {`cv10'}\cell\row}" _n
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {5% critical value}\cell"
file write `fh' " \pard\intbl\qc {`cv5'}\cell"
file write `fh' " \pard\intbl\qc {`cv5'}\cell"
file write `fh' " \pard\intbl\qc {`cv5'}\cell\row}" _n
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {1% critical value}\cell"
file write `fh' " \pard\intbl\qc {`cv1'}\cell"
file write `fh' " \pard\intbl\qc {`cv1'}\cell"
file write `fh' " \pard\intbl\qc {`cv1'}\cell\row}" _n

* Panel C: Johansen Trace Test
file write `fh' "{\pard\ql\b Panel C: Johansen Trace Test (H0: r=0)\par}" _n

local tr1 = cond(missing(cot1_tr), "\u8212", string(cot1_tr, "%6.3f"))
local tr2 = cond(missing(cot2_tr), "\u8212", string(cot2_tr, "%6.3f"))
local tr3 = cond(missing(cot3_tr), "\u8212", string(cot3_tr, "%6.3f"))
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Trace statistic}\cell"
file write `fh' " \pard\intbl\qc {`tr1'}\cell"
file write `fh' " \pard\intbl\qc {`tr2'}\cell"
file write `fh' " \pard\intbl\qc {`tr3'}\cell\row}" _n

local trcv : display %6.2f joh_cv_tr_5
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {5% critical value}\cell"
file write `fh' " \pard\intbl\qc {`trcv'}\cell"
file write `fh' " \pard\intbl\qc {`trcv'}\cell"
file write `fh' " \pard\intbl\qc {`trcv'}\cell\row}" _n

* Panel D: Johansen Max-Eigenvalue Test
file write `fh' "{\pard\ql\b Panel D: Johansen Max-Eigenvalue Test (H0: r=0)\par}" _n

local mx1 = cond(missing(cot1_mx), "\u8212", string(cot1_mx, "%6.3f"))
local mx2 = cond(missing(cot2_mx), "\u8212", string(cot2_mx, "%6.3f"))
local mx3 = cond(missing(cot3_mx), "\u8212", string(cot3_mx, "%6.3f"))
file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
file write `fh' "\pard\intbl\ql {Max-eigenvalue statistic}\cell"
file write `fh' " \pard\intbl\qc {`mx1'}\cell"
file write `fh' " \pard\intbl\qc {`mx2'}\cell"
file write `fh' " \pard\intbl\qc {`mx3'}\cell\row}" _n

local mxcv : display %6.2f joh_cv_mx_5
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {5% critical value}\cell"
file write `fh' " \pard\intbl\qc {`mxcv'}\cell"
file write `fh' " \pard\intbl\qc {`mxcv'}\cell"
file write `fh' " \pard\intbl\qc {`mxcv'}\cell\row}" _n

* Notes
file write `fh' "{\pard\ql\fs20 Notes: Three tests of cointegration between the log college wage premium and log relative supply. Each column estimates the cointegrating regression with an intercept and a linear time trend (no spline or quadratic), so the three columns differ only in sample. Splitting the sample at 1992 directly handles the structural break that motivated the spline and quadratic specifications elsewhere, while keeping the deterministic structure within each sub-sample exactly the case for which Shin's published critical values apply. Panel A reports the ARDL bounds test (Pesaran, Shin, and Smith 2001), with lag order selected by AIC over p, q in {1, 2}; finite-sample F bounds are from Kripfganz and Schneider (2020) and depend on each sub-sample's n and number of short-run coefficients. Panel B reports the Shin (1994) LM test on residuals from a dynamic OLS regression with one lead and one lag of D.relsup; the long-run variance uses a Bartlett kernel with bandwidth floor(4(T/100)^(2/9)). Critical values are exact published values from Shin (1994, Table 1, m = 1, detrended case); they apply to all three columns. Panels C and D report the Johansen trace and maximum-eigenvalue tests with two lags and a restricted linear trend; 5% critical values are MacKinnon, Haug, and Michelis (1999) for k - r = 2. Sub-sample sizes (T = 30 pre-break, T = 25 post-break) are small relative to the asymptotic theory, so finite-sample size distortion is a concern, particularly for Johansen.\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'


*───────────────────────────────────────────────────────────────────────────────
* Figure C4: Lag-Augmented Local Projections in Levels (Toda-Yamamoto)
* Output: $out/irf_levels_augmented.png
*───────────────────────────────────────────────────────────────────────────────

* Toda-Yamamoto approach: include d_max=1 extra lags (for I(1) data)
* so that Wald tests on beta_h have standard chi-squared asymptotics.

tsset time

****************************************************
* A. HC3 standard errors
****************************************************

tempfile irf_la_hc3
postfile la_hc3_post ///
	int horizon ///
	double b ///
	double se ///
	double df_r ///
	using `irf_la_hc3', replace

foreach x of numlist 0/7 {
	gen f`x'wprem2_la = F`x'.wprem2

	quietly regress f`x'wprem2_la relsup L.wprem2 L.relsup time if year>=1963, ///
		vce(hc3)

	post la_hc3_post (`x') (_b[relsup]) (_se[relsup]) (e(df_r))
	drop f`x'wprem2_la
}

postclose la_hc3_post

preserve
use `irf_la_hc3', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) ///
	xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("HC3") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
	legend(off)
graph save "$interout/levels_la_hc3", replace
graph export "$out/levels_la_hc3.jpeg", as(jpg) replace
restore


****************************************************
* B. Newey-West HAC standard errors
****************************************************

tempfile irf_la_newey
postfile la_newey_post ///
	int horizon ///
	double b ///
	double se ///
	double df_r ///
	using `irf_la_newey', replace

foreach x of numlist 0/7 {
	gen f`x'wprem2_la = F`x'.wprem2

	quietly newey f`x'wprem2_la relsup L.wprem2 L.relsup time if year>=1963, ///
		lag(3)

	post la_newey_post (`x') (_b[relsup]) (_se[relsup]) (e(df_r))
	drop f`x'wprem2_la
}

postclose la_newey_post

preserve
use `irf_la_newey', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) ///
	xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("Newey-West") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
	legend(off)
graph save "$interout/levels_la_newey", replace
graph export "$out/levels_la_newey.jpeg", as(jpg) replace
restore


****************************************************
* C. HC3 with extra lags
****************************************************

tempfile irf_la_hc3lag
postfile la_hc3lag_post ///
	int horizon ///
	double b ///
	double se ///
	double df_r ///
	using `irf_la_hc3lag', replace

gen l2wprem2 = L2.wprem2
gen l2relsup = L2.relsup

foreach x of numlist 0/7 {
	gen f`x'wprem2_la = F`x'.wprem2

	quietly regress f`x'wprem2_la relsup L.wprem2 L.relsup ///
		l2wprem2 l2relsup time if year>=1963, ///
		vce(hc3)

	post la_hc3lag_post (`x') (_b[relsup]) (_se[relsup]) (e(df_r))
	drop f`x'wprem2_la
}

postclose la_hc3lag_post

preserve
use `irf_la_hc3lag', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) ///
	xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("HC3 with Extra Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
	legend(off)
graph save "$interout/levels_la_hc3lag", replace
graph export "$out/levels_la_hc3lag.jpeg", as(jpg) replace
restore


****************************************************
* D. Newey-West with extra lags
****************************************************

tempfile irf_la_neweylag
postfile la_neweylag_post ///
	int horizon ///
	double b ///
	double se ///
	double df_r ///
	using `irf_la_neweylag', replace

foreach x of numlist 0/7 {
	gen f`x'wprem2_la = F`x'.wprem2

	quietly newey f`x'wprem2_la relsup L.wprem2 L.relsup ///
		l2wprem2 l2relsup time if year>=1963, ///
		lag(3)

	post la_neweylag_post (`x') (_b[relsup]) (_se[relsup]) (e(df_r))
	drop f`x'wprem2_la
}

postclose la_neweylag_post

preserve
use `irf_la_neweylag', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
	(rcap ub lb horizon, lwidth(medium)) ///
	(line b horizon, lwidth(medium) lcolor(black)), ///
	xlabel(0(1)7) ///
	xtitle("Horizon (years)") ///
	ytitle("Response of College Wage Premium") ///
	title("Newey-West with Extra Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
	legend(off)
graph save "$interout/levels_la_neweylag", replace
graph export "$out/levels_la_neweylag.jpeg", as(jpg) replace
restore

capture drop l2wprem2 l2relsup

* Combined figure
graph combine "$interout/levels_la_hc3" "$interout/levels_la_newey" ///
	"$interout/levels_la_hc3lag" "$interout/levels_la_neweylag", ///
	ycommon rows(2) title("Lag-Augmented Local Projections in Levels")
graph export "$out/irf_levels_augmented.png", as(png) replace



drop _merge


*───────────────────────────────────────────────────────────────────────────────
* Figures C1, C2, C3: Local Projection Impulse Response Functions
* Outputs: $out/irfcombine.png, $out/differencesirfquadratic.png
*───────────────────────────────────────────────────────────────────────────────

*Local Projections specifying time trend*
gen tcube=time^3
gen tquart=time^4


****************************************************
*  IRF for Local Projections in First Differences
****************************************************


*Basic*
* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time
gen ldrelsup   = l.d.relsup
gen dlwprem2   = L.wprem2 - L2.wprem2

foreach x of numlist 0/7 {
    tsset time
    
    *— construct the dependent‐"difference" at horizon x —*
    gen f`x'dwprem2 = F`x'.wprem2 - L.wprem2
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup d.time, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("HC3") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph save "$interout/differencesirf", replace

	graph export "$out/differencesirf.jpeg", as(jpg) replace
restore


***********
*Newey*****
***********

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_newey ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    
    *— run regression without constant
    quietly newey f`x'dwprem2 drelsup d.time, ///
        nocons lag(3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_newey (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_newey

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Newey") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
			graph save "$interout/differencesirfnewey", replace

	graph export "$out/differencesirfnewey.jpeg", as(jpg) replace
restore


*****************
****Lag**********
*****************

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_lag ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, ///
        nocons vce(hc3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_lag (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_lag

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("HC3 with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
		graph save "$interout/differencesirflag", replace

	graph export "$out/differencesirflag.jpeg", as(jpg) replace
restore

************************************
********Lag and Newey***************
************************************

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_newey_lag ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    
    *— run regression without constant
    quietly newey f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, ///
        nocons lag(3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_newey_lag (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_newey_lag

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Newey with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph save "$interout/differencesirflagnewey", replace
	graph export "$out/differencesirflagnewey.jpeg", as(jpg) replace
restore


*********************************
***Quadratic*********************
*********************************

tempfile irf_diff_quadratic
postfile diff_post_quadratic ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff_quadratic', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup d.time d.tsq, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_quadratic (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_quadratic

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff_quadratic', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph export "$out/differencesirfquadratic.png", as(png) replace
restore


*Relsup impulse response*


tempfile irf_relsup
postfile diff_post_relsup ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_relsup', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
	gen f`x'relsup=F`x'.relsup-L.relsup

    *— run regression without constant
    quietly regress f`x'relsup drelsup d.time, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_relsup (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_relsup

* 3. Load the results, compute CI, and plot
preserve
use `irf_relsup', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of Relative Supply") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph export "$out/irfrelsup.png", as(png) replace
restore



*Combining graphs*
graph combine "$interout/differencesirf" "$interout/differencesirfnewey"  "$interout/differencesirflag" "$interout/differencesirflagnewey", ycommon rows(2)
graph export "$out/irfcombine.png", as(png) replace

* Clean up temporary horizon variables
capture drop f*dwprem2 f*relsup


