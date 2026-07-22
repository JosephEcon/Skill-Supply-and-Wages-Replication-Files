/*==============================================================================
  Script:   mw_union_series.do
  Purpose:  Build the national effective real minimum wage series (1963-2023)
            following Vogel ("The Race Between Education, Technology, and the
            Minimum Wage"): state applied minimum wages weighted by fixed
            average state population shares, deflated by the GDP deflator.
            1963-2016 uses Vogel's replication series directly; 2017-2023
            extends it with FRED state minimum wages. Merges in the
            Farber-Herbst-Kuziemko-Naidu (2021, QJE) union density series.
  Inputs:   $rawdata/MinWage/state_year_combo.dta        Vogel: nominal state
                applied MW (max of state and federal), 1963-2016
            $rawdata/MinWage/state_pop_weights.dta       Fixed avg ASEC state
                population shares (Vogel's Ext_CM_national.do L332-345,
                derived from his cps_00095.dta; see readme.txt)
            $rawdata/MinWage/vogel_mw_series_1963_2016.dta  Vogel's published
                national real MW series (validation target)
            $rawdata/MinWage/cps-to-census.dta           State abbrev-FIPS map
            $rawdata/MinWage/FRED/STTMINWG??.csv         FRED state minimum
                wages (fresh pulls; see readme.txt for download date)
            $rawdata/MinWage/FRED/GDPDEF.csv             FRED GDP deflator
            $rawdata/Farber2021/union_density_farber2021.dta  Union density
                (avg of Gallup and BLS/CPS household series), 1937-2019
  Output:   $intermediatedata/mw_union_national.dta  (year, lnmw, uniondens)
==============================================================================*/

* Shared setup: paths, globals, helper programs
* (string comparison so the guard also works in a fresh standalone session)
if "$master" ~= "1" {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

*───────────────────────────────────────────────────────────────────────────────
* 1. Annual GDP deflator (mean of quarterly GDPDEF), one vintage for all years
*    Rebuilding the whole 1963-2023 series with a single deflator vintage means
*    any base-year difference from Vogel's vintage is a pure additive constant
*    in logs and cannot affect regression coefficients.
*───────────────────────────────────────────────────────────────────────────────

import delimited "$rawdata/MinWage/FRED/GDPDEF.csv", varnames(1) case(lower) clear
capture rename observation_date date
gen year = real(substr(date,1,4))
bys year: egen gdp = mean(gdpdef)
keep year gdp
duplicates drop
tempfile gdpdef_year
save `gdpdef_year'

*───────────────────────────────────────────────────────────────────────────────
* 2. State applied minimum wages 2017-2023 from FRED
*    Applied MW = max(state statutory MW, federal MW). The federal minimum has
*    been $7.25 since July 2009. AL, LA, MS, SC, and TN have no state minimum
*    wage (no FRED series), so the federal minimum binds there.
*───────────────────────────────────────────────────────────────────────────────

tempfile fredpanel
local first = 1
foreach s in AK AR AZ CA CO CT DC DE FL GA HI IA ID IL IN KS KY MA MD ME MI MN ///
    MO MT NC ND NE NH NJ NM NV NY OH OK OR PA RI SD TX UT VA VT WA WI WV WY {
    import delimited "$rawdata/MinWage/FRED/STTMINWG`s'.csv", varnames(1) case(lower) clear
    capture rename observation_date date
    gen year = real(substr(date,1,4))
    rename sttminwg* state_min
    keep if year >= 2017 & year <= 2023
    keep year state_min
    gen abbrev = "`s'"
    if `first' == 0 append using `fredpanel'
    local first = 0
    save `fredpanel', replace
}

* Add the five states with no state minimum wage
foreach s in AL LA MS SC TN {
    forval y = 2017/2023 {
        set obs `=_N+1'
        replace year = `y' if year == .
        replace abbrev = "`s'" if abbrev == ""
    }
}
bys abbrev: gen count = _N
assert count == 7    // 51 "states" x 7 years (2017-2023)
drop count

* Applied MW = max(state, federal); federal = $7.25 throughout 2017-2023
gen mw = max(state_min, 7.25)
assert mw ~= . & mw >= 7.25

* State abbreviations to census FIPS codes
merge m:1 abbrev using "$rawdata/MinWage/cps-to-census.dta", assert(3) nogen
rename state_cen statefips
keep year statefips mw

*───────────────────────────────────────────────────────────────────────────────
* 3. Combine with Vogel's 1963-2016 nominal panel and aggregate to a national
*    series using his fixed average population shares
*───────────────────────────────────────────────────────────────────────────────

append using "$rawdata/MinWage/state_year_combo.dta"
bys statefips: gen count = _N
assert count == 61   // 2023-1963+1
drop count

merge m:1 statefips using "$rawdata/MinWage/state_pop_weights.dta", assert(3) nogen
gen double mw_wtd = mw * weight_sy
bys year: egen double mw_nat = sum(mw_wtd)
bys year: keep if _n == 1
keep year mw_nat

* Deflate (index units: 100 * nominal / deflator, as in Vogel)
merge 1:1 year using `gdpdef_year', assert(2 3) keep(3) nogen
gen double mw_real = 100 * mw_nat / gdp
gen double lnmw = ln(mw_real)
label variable lnmw "Ln(Real effective minimum wage)"

*───────────────────────────────────────────────────────────────────────────────
* 4. Validation against Vogel's published series (1963-2016)
*    His series uses an older GDPDEF vintage/base year, so compare demeaned
*    logs: max abs deviation must be < 0.01 log points.
*───────────────────────────────────────────────────────────────────────────────

merge 1:1 year using "$rawdata/MinWage/vogel_mw_series_1963_2016.dta", ///
    assert(1 3) nogen
gen double ln_vogel = ln(mw)
quietly sum lnmw if ln_vogel ~= .
gen double dm_ours = lnmw - r(mean) if ln_vogel ~= .
quietly sum ln_vogel
gen double dm_vogel = ln_vogel - r(mean)
gen double valdiff = abs(dm_ours - dm_vogel)
quietly sum valdiff
display "Max abs demeaned-log deviation vs Vogel (1963-2016): " r(max)
assert r(max) < 0.01
drop mw ln_vogel dm_ours dm_vogel valdiff

* Seam check: no discontinuity at the 2016 -> 2017 splice
quietly sum lnmw if year == 2016
local l16 = r(mean)
quietly sum lnmw if year == 2017
assert abs(r(mean) - `l16') < 0.10

*───────────────────────────────────────────────────────────────────────────────
* 5. Merge the Farber et al. (2021) union density series and save
*    Series ends in 2019: uniondens is missing for 2020-2023, which truncates
*    the union-control columns on the longest samples.
*───────────────────────────────────────────────────────────────────────────────

merge 1:1 year using "$rawdata/Farber2021/union_density_farber2021.dta", ///
    keep(1 3) nogen
assert uniondens ~= . if year >= 1963 & year <= 2019
label variable uniondens "Union density"

keep year mw_nat mw_real lnmw uniondens
sort year
assert year[1] == 1963 & year[_N] == 2023 & _N == 61
save "$intermediatedata/mw_union_national.dta", replace
