/*==============================================================================
  Script:   clustering.do
  Purpose:  Compute occupation distributions by major for use in hierarchical
            clustering. For each SOC level (2, 3, 4-digit) and optionally
            2009-only data, produces a major-by-occupation share matrix
            saved as degreeoccupationsoc{digit}.dta.
  Paper:    Input to R clustering script, which produces Table A1, A2
==============================================================================*/

*Setting path and clearing*
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

* Loop over SOC code granularity levels:
* 2/3/4 = 2/3/4-digit SOC using full 2009-2019 sample
* 209/309/409 = same but using 2009 data only (for robustness Table A2)
foreach digit in 2 3 4 209 309 409{

global digit `digit'


*Loading main ACS dataset*

use "$rawdata/acssimple.dta", clear

drop if degfieldd==.|degfieldd==0
drop if occ==0|occ==9920



*Limiting to 2009 for relevant groups*
if regexm("$digit", "09")==1{
	keep if year==2009
}


keep occsoc degfieldd perwt year

*Fixing neuroscience to one entry*
replace degfieldd=3611 if degfieldd==4003



**

gen soc`digit'=substr(occsoc, 1, `digit')
label var soc`digit' "$digit digit soc code"

drop occsoc


*Weights*
gegen totalcellsize=sum(perwt), by(degfieldd)

* Save totalcellsize for later merge
preserve
keep degfieldd totalcellsize
duplicates drop degfieldd, force
tempfile cellsizes
save `cellsizes', replace
restore

* Compute weighted occupation shares using collapse+reshape
collapse (sum) perwt, by(degfieldd soc`digit')
gegen degreeweight=sum(perwt), by(degfieldd)
gen meanocc=perwt/degreeweight
drop perwt degreeweight

reshape wide meanocc, i(degfieldd) j(soc`digit') string

* Fill missing shares with 0 (degree has no workers in that occupation)
foreach v of varlist meanocc* {
	replace `v'=0 if missing(`v')
}

* Merge back totalcellsize
merge 1:1 degfieldd using `cellsizes', nogenerate

* Standardize occupation shares
foreach v of varlist meanocc*{
	egen z_`v'=std(`v')
}


save "$intermediatedata/degreeoccupationsoc$digit", replace
}
