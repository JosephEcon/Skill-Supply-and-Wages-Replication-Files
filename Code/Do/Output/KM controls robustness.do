/*==============================================================================
  Script:   KM controls robustness.do
  Paper:    Tables A9, A10, A11 (Appendix robustness)
  Purpose:  Rerun the Katz-Murphy/AGK time-series specifications adding
            labour-market institution controls: the log real effective minimum
            wage (following Vogel, "The Race Between Education, Technology,
            and the Minimum Wage") and union density (following Farber,
            Herbst, Kuziemko & Naidu 2021, QJE). Every column includes BOTH
            controls; the five columns mirror the trend specifications and
            samples of body Table 6 / Table A8: (1) linear, (2) linear spline,
            (3) quadratic, (4) pre-1993 linear, (5) post-1993 linear.
  Samples:  Table A9:  AGK spliced series, 1963-2017 (as body Table 6)
            Table A10: extended series used in Table A8, 1963-2019
            Table A11: Engle-Granger cointegration tests with the controls,
                       Table 5 sample periods on the AGK series
            (Francis 2023 cutoff run to the log only, not in paper: the union
            density series ends in 2019, truncating those columns.)
  Inputs:   $intermediatedata/mw_union_national.dta (built by
            Code/Do/Cleaning/mw_union_series.do; auto-built here if missing)
  Outputs:  $out/tableA9_km_controls_agk2017_levels.rtf,  _fd.rtf
            $out/tableA10_km_controls_francis2019_levels.rtf, _fd.rtf
            $out/tableA11_km_controls_diagnostics.rtf
            $out/irfrelsupinstitutions.png (Figure D4)
            $out/irfinstitutions.png (Figure D5)
==============================================================================*/

* Shared setup: paths, globals, helper programs
* (string comparison so the guard also works in a fresh standalone session)
if "$master" ~= "1" {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

* Build the minimum wage / union density series if not present
capture confirm file "$intermediatedata/mw_union_national.dta"
if _rc {
    do "$path/Code/Do/Cleaning/mw_union_series.do"
}

*───────────────────────────────────────────────────────────────────────────────
* Shared regression block: five columns mirroring Table 6 / Table A8, each
* including both institutional controls, in levels and first differences:
*   (1) 1963-cutoff, linear trend      (2) 1963-cutoff, linear spline (t92)
*   (3) 1963-cutoff, quadratic trend   (4) 1963-1992, linear trend
*   (5) 1993-cutoff, linear trend
* Expects: wprem2 relsup time t92 tsq lnmw uniondens, tsset by the caller.
*───────────────────────────────────────────────────────────────────────────────

capture program drop km_controls_tables
program define km_controls_tables
    syntax, outstub(string) cutoff(string)

    * ─── Levels regressions ───
    reg wprem2 relsup time lnmw uniondens if year>=1963
    outreg2 using "$out/`outstub'_levels", replace ///
        addtext(Sample, 1963-`cutoff', Time trend, Linear) nocons keep(relsup lnmw uniondens) word dec(3) label

    reg wprem2 relsup time t92 lnmw uniondens if year>=1963
    outreg2 using "$out/`outstub'_levels", append ///
        addtext(Sample, 1963-`cutoff', Time trend, Linear spline) nocons keep(relsup lnmw uniondens) word dec(3) label

    reg wprem2 relsup time tsq lnmw uniondens if year>=1963
    outreg2 using "$out/`outstub'_levels", append ///
        addtext(Sample, 1963-`cutoff', Time trend, Quadratic) nocons keep(relsup lnmw uniondens) word dec(3) label

    reg wprem2 relsup time lnmw uniondens if year>=1963&year<1993
    outreg2 using "$out/`outstub'_levels", append ///
        addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(relsup lnmw uniondens) word dec(3) label

    reg wprem2 relsup time lnmw uniondens if year>=1993
    outreg2 using "$out/`outstub'_levels", append ///
        addtext(Sample, 1993-`cutoff', Time trend, Linear) nocons keep(relsup lnmw uniondens) word dec(3) label

    * ─── First differenced regressions ───
    capture drop dwprem2 drelsup dlnmw duniondens
    gen dwprem2 = d.wprem2
    gen drelsup = d.relsup
    gen dlnmw = d.lnmw
    gen duniondens = d.uniondens
    label var dwprem2 "Ln(Relative wage)"
    label var drelsup "Ln(Relative supply)"
    label var dlnmw "Ln(Real minimum wage)"
    label var duniondens "Union density"

    reg dwprem2 drelsup d.time dlnmw duniondens if year>=1963, r nocons
    outreg2 using "$out/`outstub'_fd", replace ///
        addtext(Sample, 1963-`cutoff', Time trend, Linear) nocons keep(drelsup dlnmw duniondens) word dec(3) label

    reg dwprem2 drelsup d.time d.t92 dlnmw duniondens if year>=1963, r nocons
    outreg2 using "$out/`outstub'_fd", append ///
        addtext(Sample, 1963-`cutoff', Time trend, Linear spline) nocons keep(drelsup dlnmw duniondens) word dec(3) label

    reg dwprem2 drelsup d.time d.tsq dlnmw duniondens if year>=1963, r nocons
    outreg2 using "$out/`outstub'_fd", append ///
        addtext(Sample, 1963-`cutoff', Time trend, Quadratic) nocons keep(drelsup dlnmw duniondens) word dec(3) label

    reg dwprem2 drelsup d.time dlnmw duniondens if year>=1963&year<1993, r nocons
    outreg2 using "$out/`outstub'_fd", append ///
        addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(drelsup dlnmw duniondens) word dec(3) label

    reg dwprem2 drelsup d.time dlnmw duniondens if year>=1993, r nocons
    outreg2 using "$out/`outstub'_fd", append ///
        addtext(Sample, 1993-`cutoff', Time trend, Linear) nocons keep(drelsup dlnmw duniondens) word dec(3) label
end


* Engle-Granger residual ADF statistics for the Table A11 diagnostics, using
* whatever sample is currently in memory. Cointegrating regressions include a
* linear trend and are restricted to the given sample condition so the
* regression T matches the residual-ADF T (same convention as Table 5 Panel
* B). Stores scalars eg_`pfx'_{base,mw,un,both}.
capture program drop eg_tests
program define eg_tests
    syntax, pfx(string) cond(string)
    capture drop u_base u_mw u_un u_both
    reg wprem2 relsup time if `cond'
    predict u_base, resid
    dfuller u_base if `cond'
    scalar eg_`pfx'_base = r(Zt)
    reg wprem2 relsup time lnmw if `cond'
    predict u_mw, resid
    dfuller u_mw if `cond'
    scalar eg_`pfx'_mw = r(Zt)
    reg wprem2 relsup time uniondens if `cond'
    predict u_un, resid
    dfuller u_un if `cond'
    scalar eg_`pfx'_un = r(Zt)
    reg wprem2 relsup time lnmw uniondens if `cond'
    predict u_both, resid
    dfuller u_both if `cond'
    scalar eg_`pfx'_both = r(Zt)
    drop u_base u_mw u_un u_both
end


*───────────────────────────────────────────────────────────────────────────────
* Table A9: AGK spliced series, 1963-2017 (sample build as RBET replication.do)
*───────────────────────────────────────────────────────────────────────────────

use "$rawdata/colhs1405", clear
merge 1:1 year using "$rawdata/km-cg-rsup-6317", nogen
replace wprem2 = clphsg_all + .0037398 if year>=2006
replace relsup = eu_lnclg + .0152795 if year>=2006
keep year wprem2 relsup

merge 1:1 year using "$intermediatedata/mw_union_national.dta", keep(1 3) nogen

gen time = year - 1914
gen t92 = max(year-1992,0)
gen tsq = time*time/10
sort year
tsset time

label var wprem2 "Ln(Relative wage)"
label var relsup "Ln(Relative supply)"
label var lnmw "Ln(Real minimum wage)"
label var uniondens "Union density"

km_controls_tables, outstub(tableA9_km_controls_agk2017) cutoff(2017)

* Engle-Granger tests for Table A11: the three sample periods of body Table 5
* Panel B (full 1963-2017, pre-1993, post-1993) on the AGK series.
eg_tests, pfx(full) cond(year>=1963)
eg_tests, pfx(pre)  cond(year>=1963 & year<1993)
eg_tests, pfx(post) cond(year>=1993)


*───────────────────────────────────────────────────────────────────────────────
* Figures D4 and D5: Local projections with the institutional controls.
* Same design as Appendix D's main single-panel specification (first
* differences, linear time trend, no lags, HC3 standard errors), with the
* relative supply impulse and both institutional impulses entering jointly.
* Figure D4 plots the relative supply impulse controlling for the
* institutions; Figure D5 plots the two institutional impulses. Uses the AGK
* 1963-2017 sample and the d-variables generated by km_controls_tables above.
* Outputs: $out/irfrelsupinstitutions.png, $out/irfinstitutions.png
*───────────────────────────────────────────────────────────────────────────────

tempfile irf_inst
postfile inst_post int horizon ///
    double bsup double sesup ///
    double bmw double semw double bun double seun double df_r ///
    using `irf_inst', replace

foreach x of numlist 0/7 {
    gen f`x'dwprem2 = F`x'.wprem2 - L.wprem2
    quietly regress f`x'dwprem2 drelsup dlnmw duniondens d.time, vce(hc3) nocons
    post inst_post (`x') (_b[drelsup]) (_se[drelsup]) ///
        (_b[dlnmw]) (_se[dlnmw]) ///
        (_b[duniondens]) (_se[duniondens]) (e(df_r))
    drop f`x'dwprem2
}
postclose inst_post

preserve
use `irf_inst', clear
sort horizon
gen tcrit = invttail(df_r, 0.025)
gen ubsup = bsup + tcrit*sesup
gen lbsup = bsup - tcrit*sesup
gen ubmw = bmw + tcrit*semw
gen lbmw = bmw - tcrit*semw
gen ubun = bun + tcrit*seun
gen lbun = bun - tcrit*seun

* Figure D4: relative supply impulse controlling for the institutions
twoway ///
    (rcap ubsup lbsup horizon, lwidth(medium)) ///
    (line bsup horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    yline(0, lpattern(dash) lcolor(gs10)) legend(off)
graph export "$out/irfrelsupinstitutions.png", as(png) replace

* Figure D5: institutional impulses
twoway ///
    (rcap ubmw lbmw horizon, lwidth(medium)) ///
    (line bmw horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Minimum Wage Impulse") ///
    yline(0, lpattern(dash) lcolor(gs10)) legend(off) ///
    name(g_inst_mw, replace) nodraw

twoway ///
    (rcap ubun lbun horizon, lwidth(medium)) ///
    (line bun horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Union Density Impulse") ///
    yline(0, lpattern(dash) lcolor(gs10)) legend(off) ///
    name(g_inst_un, replace) nodraw

graph combine g_inst_mw g_inst_un, rows(1)
graph export "$out/irfinstitutions.png", as(png) replace
restore


*───────────────────────────────────────────────────────────────────────────────
* Table A10 (2019) and log-only 2023 run: extended series used in Table A8
* (sample build as Francis KM robustness.do)
*───────────────────────────────────────────────────────────────────────────────

foreach cutoff in 2019 2023 {

    import delimited using "$rawdata/Francis/km-cg-rsup-6323.csv", clear
    rename clphsg_all wprem2
    rename eu_lnclg relsup
    keep if year <= `cutoff'

    merge 1:1 year using "$intermediatedata/mw_union_national.dta", keep(1 3) nogen

    gen time = year - 1963
    gen t92 = max(year-1992,0)
    gen tsq = time*time/10
    sort year
    tsset year

    label var wprem2 "Ln(Relative wage)"
    label var relsup "Ln(Relative supply)"
    label var lnmw "Ln(Real minimum wage)"
    label var uniondens "Union density"

    if `cutoff' == 2019 {
        km_controls_tables, outstub(tableA10_km_controls_francis2019) cutoff(2019)
    }
    else {
        * Not in paper: union density ends in 2019, so the union columns run
        * 1963-2019 while the minimum wage columns run 1963-2023. Log only.
        di _n "=== FRANCIS 2023 CUTOFF (log only, not in paper) ===" _n
        reg wprem2 relsup time lnmw uniondens if year>=1963
        reg wprem2 relsup time t92 lnmw uniondens if year>=1963
        reg wprem2 relsup time tsq lnmw uniondens if year>=1963
        reg wprem2 relsup time lnmw uniondens if year>=1963&year<1993
        reg wprem2 relsup time lnmw uniondens if year>=1993
        capture drop dwprem2 drelsup dlnmw duniondens
        gen dwprem2 = d.wprem2
        gen drelsup = d.relsup
        gen dlnmw = d.lnmw
        gen duniondens = d.uniondens
        reg dwprem2 drelsup d.time dlnmw duniondens if year>=1963, r nocons
        reg dwprem2 drelsup d.time d.t92 dlnmw duniondens if year>=1963, r nocons
        reg dwprem2 drelsup d.time d.tsq dlnmw duniondens if year>=1963, r nocons
        reg dwprem2 drelsup d.time dlnmw duniondens if year>=1963&year<1993, r nocons
        reg dwprem2 drelsup d.time dlnmw duniondens if year>=1993, r nocons
    }
}


*───────────────────────────────────────────────────────────────────────────────
* Table A11: Engle-Granger cointegration tests adding the institutional
* controls to the cointegrating regression, over the three sample periods of
* body Table 5 Panel B (scalars stored by eg_tests above): full 1963-2017
* (T=55), pre-1993 1963-1992 (T=30), post-1993 1993-2017 (T=25).
* Critical values: MacKinnon (2010, QED WP 1227) Table 3 response surface,
* linear-trend case (tau_ct), evaluated at each sample's T with N=2 (baseline),
* N=3 (relsup + one control), or N=4 (relsup + both controls). N counts the
* I(1) variables in the cointegrating relation incl. the dependent variable;
* the deterministic trend is handled by the ct variant.
*───────────────────────────────────────────────────────────────────────────────

capture program drop mk_rs
program define mk_rs, rclass
    args T bi b1 b2 b3
    return scalar cv = `bi' + `b1'/`T' + `b2'/(`T'^2) + `b3'/(`T'^3)
end

* Engle-Granger CVs at the Table 5 sample sizes
foreach T in 55 30 25 {
    mk_rs `T' -3.78057 -9.5106 -12.074 0
    scalar cv2_5_`T' = r(cv)            // N=2 5% (baseline)
    mk_rs `T' -3.49631 -7.0815 -7.538 21.892
    scalar cv2_10_`T' = r(cv)           // N=2 10%
    mk_rs `T' -4.11890 -11.8922 -19.031 77.332
    scalar cv3_5_`T' = r(cv)            // N=3 5%
    mk_rs `T' -3.83511 -9.0723 -8.504 35.403
    scalar cv3_10_`T' = r(cv)           // N=3 10%
    mk_rs `T' -4.42871 -14.5876 -18.228 39.647
    scalar cv4_5_`T' = r(cv)            // N=4 5%
    mk_rs `T' -4.14633 -11.2500 -9.873 54.109
    scalar cv4_10_`T' = r(cv)           // N=4 10%
}

display _n "=== TABLE A11 DIAGNOSTICS SUMMARY ==="
display "Engle-Granger (stat | 5% CV | 10% CV):"
foreach pfx in full pre post {
    if "`pfx'" == "full" local T 55
    if "`pfx'" == "pre"  local T 30
    if "`pfx'" == "post" local T 25
    display "  `pfx' base: " %6.3f eg_`pfx'_base " | " %6.3f cv2_5_`T' " | " %6.3f cv2_10_`T'
    display "  `pfx' mw:   " %6.3f eg_`pfx'_mw   " | " %6.3f cv3_5_`T' " | " %6.3f cv3_10_`T'
    display "  `pfx' un:   " %6.3f eg_`pfx'_un   " | " %6.3f cv3_5_`T' " | " %6.3f cv3_10_`T'
    display "  `pfx' both: " %6.3f eg_`pfx'_both " | " %6.3f cv4_5_`T' " | " %6.3f cv4_10_`T'
}

* Write the RTF table (same hand-written style as Table 5: top rule, header
* rule, bottom rule only)
tempname fh
file open `fh' using "$out/tableA11_km_controls_diagnostics.rtf", write replace
file write `fh' "{\rtf1\ansi\deff0 {\fonttbl\f0\fnil Times New Roman;}" _n
file write `fh' "{\info {\author .}{\company .}{\title .}}" _n
file write `fh' "\deflang1033\plain\fs24" _n
file write `fh' "{\pard\keepn\ql Table A11: Engle-Granger Cointegration Tests with Institutional Controls\par}" _n

local c1 2508
local c2 4164
local c3 5820
local c4 7476

* Header row: rule above and below
file write `fh' "{\trowd\trgaph108\trleft-108"
file write `fh' "\clbrdrt\brdrw10\brdrs\clbrdrb\brdrw10\brdrs\cellx`c1'"
file write `fh' "\clbrdrt\brdrw10\brdrs\clbrdrb\brdrw10\brdrs\cellx`c2'"
file write `fh' "\clbrdrt\brdrw10\brdrs\clbrdrb\brdrw10\brdrs\cellx`c3'"
file write `fh' "\clbrdrt\brdrw10\brdrs\clbrdrb\brdrw10\brdrs\cellx`c4'"
file write `fh' "\pard\intbl\ql {}\cell"
file write `fh' " \pard\intbl\qc {ADF Statistic}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (5%)}\cell"
file write `fh' " \pard\intbl\qc {Critical Value (10%)}\cell\row}" _n

foreach pfx in full pre post {
    if "`pfx'" == "full" {
        local T 55
        local sublabel "Full Sample, 1963-2017"
    }
    if "`pfx'" == "pre" {
        local T 30
        local sublabel "Pre-1993, 1963-1992"
    }
    if "`pfx'" == "post" {
        local T 25
        local sublabel "Post-1993, 1993-2017"
    }
    file write `fh' "{\trowd\trgaph108\trleft-108\cellx`c1'\cellx`c2'\cellx`c3'\cellx`c4'"
    file write `fh' "\pard\intbl\ql {\i `sublabel'}\cell"
    file write `fh' " \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell \pard\intbl\qc {}\cell\row}" _n
    foreach spec in base mw un both {
        if "`spec'" == "base" {
            local label "Baseline (no controls)"
            local k 2
        }
        if "`spec'" == "mw" {
            local label "+ Minimum wage"
            local k 3
        }
        if "`spec'" == "un" {
            local label "+ Union density"
            local k 3
        }
        if "`spec'" == "both" {
            local label "+ Both"
            local k 4
        }
        local stat : display %6.3f eg_`pfx'_`spec'
        local cv5 : display %6.3f cv`k'_5_`T'
        local cv10 : display %6.3f cv`k'_10_`T'
        local bordopt ""
        if "`pfx'" == "post" & "`spec'" == "both" local bordopt "\clbrdrb\brdrw10\brdrs"
        file write `fh' "{\trowd\trgaph108\trleft-108`bordopt'\cellx`c1'`bordopt'\cellx`c2'`bordopt'\cellx`c3'`bordopt'\cellx`c4'"
        file write `fh' "\pard\intbl\ql {  `label'}\cell"
        file write `fh' " \pard\intbl\qc {`stat'}\cell"
        file write `fh' " \pard\intbl\qc {`cv5'}\cell"
        file write `fh' " \pard\intbl\qc {`cv10'}\cell\row}" _n
    }
}

file write `fh' "{\pard\ql\fs20 Notes: Engle-Granger cointegration tests on the Autor et al (2020) data over the Table 5 sample periods: an ADF test (no deterministics) on the residuals of the cointegrating regression of the log relative wage on log relative supply, a linear trend, and the indicated institutional controls described in Table A9. Critical values are computed from the MacKinnon (2010, QED Working Paper 1227, Table 3) linear-trend response surface at each sample's observation count, with N equal to the number of I(1) variables in the cointegrating relation (N=2 baseline, N=3 with one control, N=4 with both).\par}" _n
file write `fh' "{\pard \par}" _n
file write `fh' "}" _n
file close `fh'
