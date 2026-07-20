/*==============================================================================
  Script:   degree_soc6_shares.do
  Purpose:  Build a major x 6-digit-SOC occupation-share matrix at FULL
            occupation granularity (no SOC truncation), for the task-based
            (O*NET work-activity) clustering alternative. This is the matrix S
            in  M_task = S %*% A, where A is the O*NET occupation x activity
            "requires" matrix built in onet_activity_prep.R.

            Mirrors Code/Do/Cleaning/clustering.do (same sample, same filters,
            same neuroscience fix, same totalcellsize weight) but keeps occsoc
            at its native 6-character granularity and saves LONG form so the R
            side can align SOC keys with O*NET cleanly.

  Outputs:  $intermediatedata/degree_soc6_shares.dta   (degfieldd soc6 share)
            $intermediatedata/degree_totalcellsize.dta  (degfieldd totalcellsize)
  Paper:    Input to "Code/R scripts/task clustering.R"
==============================================================================*/

*Setting path and clearing*
if "$master"=="" global master 0
local master=$master
if `master'==1{
	clear
}
else {
clear
global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global rawdata "$path/Data/Raw"
global intermediatedata "$path/Data/intermediate"
global cleandata "$path/Data/clean"
global interout "$path/Intermediate Output"
global out "$path/Output"
}

*Loading main ACS dataset (full 2009-2019 sample, matching the SOC4 main spec)*
use "$rawdata/acssimple.dta", clear

drop if degfieldd==.|degfieldd==0
drop if occ==0|occ==9920

keep occsoc degfieldd perwt year

*Fixing neuroscience to one entry (matches clustering.do)*
replace degfieldd=3611 if degfieldd==4003

*------------------------------------------------------------------------------
* DIAGNOSTICS: verify occsoc format before using it as the O*NET merge key.
* We need to know (a) it is a string, (b) its character length (expect 6),
* (c) whether IPUMS "X" placeholder broad codes are present (e.g. 1110XX), and
* (d) whether any punctuation (-/.) is present.
*------------------------------------------------------------------------------
display _newline "==================== OCCSOC FORMAT DIAGNOSTICS ===================="
describe occsoc
capture confirm string variable occsoc
if _rc==0 {
	display "occsoc IS a string variable"
	gen int _occlen = length(occsoc)
	display "--- distribution of occsoc string length ---"
	tabulate _occlen
	gen byte _hasX = (strpos(upper(occsoc), "X") > 0)
	gen byte _hasdash = (strpos(occsoc, "-") > 0)
	gen byte _hasdot  = (strpos(occsoc, ".") > 0)
	quietly count
	local N = r(N)
	quietly count if _hasX==1
	display "rows with an X placeholder in occsoc: " r(N) " of " `N'
	quietly count if _hasdash==1
	display "rows with a '-' in occsoc:            " r(N) " of " `N'
	quietly count if _hasdot==1
	display "rows with a '.' in occsoc:            " r(N) " of " `N'
	display "--- 20 example occsoc values ---"
	preserve
		contract occsoc
		list occsoc in 1/20, clean noobs
	restore
	drop _occlen _hasX _hasdash _hasdot
}
else {
	display "WARNING: occsoc is NUMERIC, not string. Converting to a zero-padded 6-char string."
	tostring occsoc, replace
}
display "===================================================================" _newline

*Full-granularity occupation key*
gen soc6 = occsoc
label var soc6 "Native 6-char IPUMS occsoc (O*NET merge key)"
drop occsoc

*------------------------------------------------------------------------------
* Weight: totalcellsize = total perwt per major (same as clustering.do).
* Saved separately for use as hclust 'members' weight on the R side.
*------------------------------------------------------------------------------
bysort degfieldd: egen double totalcellsize = total(perwt)

preserve
	keep degfieldd totalcellsize
	duplicates drop degfieldd, force
	label var totalcellsize "Total perwt across all occupations for the major"
	save "$intermediatedata/degree_totalcellsize.dta", replace
	display "Saved degree_totalcellsize.dta: " _N " majors"
restore

*------------------------------------------------------------------------------
* Major x occupation shares (LONG form).
*   share = perwt(major,occ) / sum_occ perwt(major,occ)  = proportion of the
*   major's graduate-weight employed in 6-digit SOC occupation soc6.
*------------------------------------------------------------------------------
collapse (sum) perwt, by(degfieldd soc6)
bysort degfieldd: egen double degreeweight = total(perwt)
gen double share = perwt/degreeweight
drop perwt degreeweight

label var share "Share of major's graduate-weight in this 6-digit SOC occupation"

* Sanity: shares should sum to ~1 within each major
preserve
	collapse (sum) share, by(degfieldd)
	summarize share
	display "Share sums within major: min=" r(min) " max=" r(max) " (expect ~1)"
restore

compress
save "$intermediatedata/degree_soc6_shares.dta", replace
display "Saved degree_soc6_shares.dta: " _N " major-occupation rows"
