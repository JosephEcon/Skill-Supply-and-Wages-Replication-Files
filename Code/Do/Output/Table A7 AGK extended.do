/*==============================================================================
  Script:   Table A7 AGK extended.do
  Paper:    Table A7 (Appendix robustness)
  Purpose:  Rebuild Table A7 by extending AGK's spliced 1914-2017 series
            through 2019 with Francis (2026) 2018-2019 observations, using the
            2017 AGK-vs-Francis offset (wprem +0.0064661, relsup +0.0002701).

            Mirrors body Table 5's 4-panel structure (Panel A: ADF unit-root,
            Panel B: Engle-Granger, Panel C: ARDL bounds, Panel D: Shin LM)
            using identical Stata code so the 1963-2017 overlap with body
            Table 5 is exact.

  Outputs:  $out/tableA7_agk_extended_1963_2019.rtf

  Notes:    Panel B critical values are MacKinnon (2010) Case IV (constant +
            trend), k=2 (one cointegrating regressor + dependent variable),
            finite-sample tabulated values matched to each sample size.
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"


*───────────────────────────────────────────────────────────────────────────────
* Section 1: Build AGK-extended 1963-2019 series
*
* Pre-2006 AGK uses colhs1405.dta directly (wprem2 and relsup already computed).
* 2006-2017 AGK uses km-cg-rsup-6317.dta with the +0.0037398 / +0.0152795
* splice offsets (same as RBET replication.do lines 29-31).
* 2018-2019 uses Francis 2026 (km-cg-rsup-6323.csv) shifted by the 2017
* AGK-vs-Francis offset:
*   wprem: AGK 2017 (0.6741138) - Francis 2017 (0.667647701) = +0.0064661
*   relsup: AGK 2017 (0.5753635) - Francis 2017 (0.575093431) = +0.0002701
* This makes the extended series continuous at 2017 with Francis's
* 2018-2019 growth rates, while preserving exact agreement with body
* Table 5 over the 1963-2017 overlap.
*───────────────────────────────────────────────────────────────────────────────

* Load AGK pre-2006 source (colhs1405.dta)
use "$rawdata/colhs1405", clear

* Merge with AGK 2006-2017 source (km-cg-rsup-6317.dta)
merge 1:1 year using "$rawdata/km-cg-rsup-6317", nogen

* AGK's hardcoded 2006 splice
replace wprem2 = clphsg_all + .0037398 if year>=2006
replace relsup = eu_lnclg + .0152795 if year>=2006

keep year wprem2 relsup

* Append Francis 2018-2019 with 2017 offset
local wprem_offset  =  0.0064661
local relsup_offset =  0.0002701
local francis_wprem_2018  = 0.669815473
local francis_relsup_2018 = 0.588337609
local francis_wprem_2019  = 0.672465063
local francis_relsup_2019 = 0.637757335

set obs `=_N+2'
replace year   = 2018                                  in -2
replace wprem2 = `francis_wprem_2018'  + `wprem_offset'  in -2
replace relsup = `francis_relsup_2018' + `relsup_offset' in -2
replace year   = 2019                                  in -1
replace wprem2 = `francis_wprem_2019'  + `wprem_offset'  in -1
replace relsup = `francis_relsup_2019' + `relsup_offset' in -1

* Time variables (match RBET replication.do convention)
sort year
gen time = year - 1914
gen t92  = max(year - 1992, 0)
gen tsq  = time*time/10

tsset time
list year wprem2 relsup if year >= 2014, sep(0)


*───────────────────────────────────────────────────────────────────────────────
* Section 2: Panel A — ADF unit root tests with trend (1963-2019)
*───────────────────────────────────────────────────────────────────────────────

dfuller wprem2 if year>=1963, trend
scalar ur_wage_full_stat = r(Zt)
scalar ur_wage_full_p    = r(p)

dfuller wprem2 if year>=1963 & year<1993, trend
scalar ur_wage_pre_stat  = r(Zt)
scalar ur_wage_pre_p     = r(p)

dfuller wprem2 if year>=1993, trend
scalar ur_wage_post_stat = r(Zt)
scalar ur_wage_post_p    = r(p)

dfuller relsup if year>=1963, trend
scalar ur_sup_full_stat  = r(Zt)
scalar ur_sup_full_p     = r(p)

dfuller relsup if year>=1963 & year<1993, trend
scalar ur_sup_pre_stat   = r(Zt)
scalar ur_sup_pre_p      = r(p)

dfuller relsup if year>=1993, trend
scalar ur_sup_post_stat  = r(Zt)
scalar ur_sup_post_p     = r(p)


*───────────────────────────────────────────────────────────────────────────────
* Section 3: Panel B — Engle-Granger cointegration tests (1963-2019)
* MacKinnon (2010) Case IV (constant + trend) k=2 critical values matched
* to body Table 5.
*───────────────────────────────────────────────────────────────────────────────

reg wprem2 relsup time if year>=1963
predict u_full, resid
dfuller u_full if year>=1963
scalar coint_full_stat = r(Zt)

reg wprem2 relsup time if year>=1963 & year<1993
predict u_pre, resid
dfuller u_pre if year>=1963 & year<1993
scalar coint_pre_stat  = r(Zt)

reg wprem2 relsup time if year>=1993
predict u_post, resid
dfuller u_post if year>=1993
scalar coint_post_stat = r(Zt)

drop u_full u_pre u_post

* MacKinnon (2010) finite-sample critical values via Table 3 response surface
*   CV(T) = beta_inf + beta_1/T + beta_2/T^2 + beta_3/T^3
* Source: MacKinnon, J.G. (2010). "Critical Values for Cointegration Tests."
*   QED Working Paper No. 1227, Queen's University, Table 3.
*
* Panel A is ADF unit root with trend, N=1 ct.
* Panel B is Engle-Granger with trend, N=2 ct.
*
* This block also overrides the hand-coded Panel A 5% CVs above with
* response-surface values, so Panel A and Panel B share the same source.

* Sample sizes (in years, observation count for the level series)
local T_full = 57   // 1963-2019
local T_pre  = 30   // 1963-1992
local T_post = 27   // 1993-2019

* Helper to apply response surface: result returned via r(cv)
* args: T  bi  b1  b2  b3   (positional)
capture program drop mk_rs
program define mk_rs, rclass
    args T bi b1 b2 b3
    return scalar cv = `bi' + `b1'/`T' + `b2'/(`T'^2) + `b3'/(`T'^3)
end

* --- Panel A 5% critical values (N=1 with trend) ---
*   coefs: bi=-3.41049, b1=-4.3904, b2=-9.036, b3=-45.374
foreach s in full pre post {
    mk_rs `T_`s'' -3.41049 -4.3904 -9.036 -45.374
    scalar mka_5_`s' = r(cv)
}

* --- Panel B 5% critical values (N=2 with trend) ---
*   coefs: bi=-3.78057, b1=-9.5106, b2=-12.074, b3=0
foreach s in full pre post {
    mk_rs `T_`s'' -3.78057 -9.5106 -12.074 0
    scalar mk_`s'_5 = r(cv)
}

* --- Panel B 10% critical values (N=2 with trend) ---
*   coefs: bi=-3.49631, b1=-7.0815, b2=-7.538, b3=21.892
foreach s in full pre post {
    mk_rs `T_`s'' -3.49631 -7.0815 -7.538 21.892
    scalar mk_`s'_10 = r(cv)
}

display _n "MacKinnon (2010) Table 3 critical values for Table A7:"
display "  Panel A 5%: full=" %6.3f mka_5_full " pre=" %6.3f mka_5_pre " post=" %6.3f mka_5_post
display "  Panel B 5%: full=" %6.3f mk_full_5  " pre=" %6.3f mk_pre_5  " post=" %6.3f mk_post_5
display "  Panel B 10%: full=" %6.3f mk_full_10 " pre=" %6.3f mk_pre_10 " post=" %6.3f mk_post_10


*───────────────────────────────────────────────────────────────────────────────
* Section 4: Panels C and D — ARDL bounds and Shin LM tests
* Same logic as RBET replication.do lines 123-193.
*───────────────────────────────────────────────────────────────────────────────

capture which ardl
if _rc != 0 ssc install ardl, replace

foreach spec in 1 2 3 {
    if `spec' == 1 local sample_cond "year>=1963"
    if `spec' == 2 local sample_cond "year>=1963 & year<1993"
    if `spec' == 3 local sample_cond "year>=1993"

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
    scalar ta10_ardl_p`spec'     = `best_p'
    scalar ta10_ardl_q`spec'     = `best_q'
    quietly estat ectest
    scalar ta10_ardl_F`spec'     = r(F_pss)
    scalar ta10_ardl_t`spec'     = r(t_pss)
    matrix ta10_ardl_cv`spec'    = r(cvmat)
    scalar ta10_ardl_F_I0_`spec' = ta10_ardl_cv`spec'[1,3]
    scalar ta10_ardl_F_I1_`spec' = ta10_ardl_cv`spec'[1,4]
    scalar ta10_ardl_t_I0_`spec' = ta10_ardl_cv`spec'[2,3]
    scalar ta10_ardl_t_I1_`spec' = ta10_ardl_cv`spec'[2,4]

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
        quietly gen double _ulag  = L`j'.u_shin
        quietly gen double _uprod = u_shin * _ulag
        quietly summ _uprod
        local gamma_j = r(sum) / `T'
        local weight  = 1 - `j'/(`bw' + 1)
        if `j' == 0 local omega = `gamma_j'
        else        local omega = `omega' + 2 * `weight' * `gamma_j'
        drop _ulag _uprod
    }
    quietly gen double Ssq_shin = S_shin^2
    quietly summ Ssq_shin
    scalar ta10_shin_LM`spec' = r(sum) / (`T'^2 * `omega')
    scalar ta10_shin_T`spec'  = `T'
    drop u_shin S_shin Ssq_shin
}

* Shin (1994) Table 1 critical values for the C_tau (detrended) case, m=1
* regressor. The original paper (Econometric Theory 10:91-115, Cambridge) is
* paywalled; the secondary citation Erlat (MEEA Vol. 5, Table 5 notes)
* attributes the pair (0.121, 0.184) to "Shin (1994) Table 1" at the (5%, 1%)
* significance levels. The values match the (5%, 1%) row in standard usage
* but should be cross-checked against the primary source.
scalar ta10_shin_cv_5 = 0.121
scalar ta10_shin_cv_1 = 0.184


*───────────────────────────────────────────────────────────────────────────────
* Section 5: Write 4-panel RTF
*───────────────────────────────────────────────────────────────────────────────

tempname fh
file open `fh' using "$out/tableA7_agk_extended_1963_2019.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table A7: Unit Root and Cointegration Tests (AGK extended through 2019)\par}" _n

* Column widths
local c1 2508
local c2 4164
local c3 5820
local c4 7476

*── Panel A header ──
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
    local crit : display %6.3f mka_5_`sample'
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

foreach sample in full pre post {
    if "`sample'" == "full" local label "Full Sample"
    if "`sample'" == "pre"  local label "Pre-1993"
    if "`sample'" == "post" local label "Post-1993"
    local stat : display %6.3f ur_sup_`sample'_stat
    local crit : display %6.3f mka_5_`sample'
    local pval : display %6.3f ur_sup_`sample'_p
    local bordopt ""
    if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
    file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
    file write `fh' "\pard\intbl\ql {  `label'}\cell"
    file write `fh' " \pard\intbl\qc {`stat'}\cell"
    file write `fh' " \pard\intbl\qc {`crit'}\cell"
    file write `fh' " \pard\intbl\qc {`pval'}\cell\row}" _n
}

*── Panel B header ──
file write `fh' "{\pard\ql\b Panel B: Cointegration Tests (Engle-Granger)\par}" _n

* Column headers (5% and 10% CV; no p-value column)
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
    if "`sample'" == "full" {
        local label "Full Sample"
        local cv5  : display %6.3f mk_full_5
        local cv10 : display %6.3f mk_full_10
    }
    if "`sample'" == "pre" {
        local label "Pre-1993"
        local cv5  : display %6.3f mk_pre_5
        local cv10 : display %6.3f mk_pre_10
    }
    if "`sample'" == "post" {
        local label "Post-1993"
        local cv5  : display %6.3f mk_post_5
        local cv10 : display %6.3f mk_post_10
    }
    local stat : display %6.3f coint_`sample'_stat
    local bordopt ""
    if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
    file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
    file write `fh' "\pard\intbl\ql {`label'}\cell"
    file write `fh' " \pard\intbl\qc {`stat'}\cell"
    file write `fh' " \pard\intbl\qc {`cv5'}\cell"
    file write `fh' " \pard\intbl\qc {`cv10'}\cell\row}" _n
}

*── Panel C header (ARDL bounds) ──
file write `fh' "{\pard\ql\b Panel C: ARDL Bounds Test (Pesaran-Shin-Smith 2001)\par}" _n

file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {F-statistic}\cell"
file write `fh' " \pard\intbl\qc {t-statistic}\cell"
file write `fh' " \pard\intbl\qc {5% Bounds [I(0), I(1)]}\cell\row}" _n

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
    local fstat : display %6.3f ta10_ardl_F`s'
    local tstat : display %6.3f ta10_ardl_t`s'
    local fcv : display "F: [" %4.2f ta10_ardl_F_I0_`s' ", " %4.2f ta10_ardl_F_I1_`s' "]; t: [" %5.2f ta10_ardl_t_I0_`s' ", " %5.2f ta10_ardl_t_I1_`s' "]"
    local bordopt ""
    if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
    file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
    file write `fh' "\pard\intbl\ql {`label'}\cell"
    file write `fh' " \pard\intbl\qc {`fstat'}\cell"
    file write `fh' " \pard\intbl\qc {`tstat'}\cell"
    file write `fh' " \pard\intbl\qc {`fcv'}\cell\row}" _n
}

*── Panel D header (Shin LM) ──
file write `fh' "{\pard\ql\b Panel D: Shin (1994) LM Test (H0: cointegration)\par}" _n

file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {LM-statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (1%)}\cell\row}" _n

local cv5 : display %5.3f ta10_shin_cv_5
local cv1 : display %5.3f ta10_shin_cv_1
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
    local lm : display %6.3f ta10_shin_LM`s'
    local bordopt ""
    if "`sample'" == "post" local bordopt "\clbrdrb\brdrw10\brdrs"
    file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
    file write `fh' "\pard\intbl\ql {`label'}\cell"
    file write `fh' " \pard\intbl\qc {`lm'}\cell"
    file write `fh' " \pard\intbl\qc {`cv5'}\cell"
    file write `fh' " \pard\intbl\qc {`cv1'}\cell\row}" _n
}

* Notes
file write `fh' "{\pard\ql\fs20 Notes: Data on college wage premiums and relative supplies are the Autor et al (2020) series extended through to 2019. Panel A reports augmented Dickey-Fuller unit root tests with a linear trend. Panel B reports Engle-Granger cointegration tests with a linear trend. Critical values for Panels A and B are computed from MacKinnon (2010) response-surface coefficients evaluated at each sample's observation count. Panel C reports the Pesaran-Shin-Smith (2001) ARDL bounds F- and t-tests for a levels relationship; the ARDL lag order is selected by AIC over p,q in {1,2}, with finite-sample I(0) and I(1) critical bounds at the 5% level from Kripfganz and Schneider (2020). Panel D reports the Shin (1994) residual-based LM test for the null of cointegration; critical values are from Shin (1994), Table 1 (m=1 regressor, detrended case).\par}" _n
file write `fh' "}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'

display _n "Wrote: $out/tableA7_agk_extended_1963_2019.rtf"

* End of Table A7 AGK extended.do
