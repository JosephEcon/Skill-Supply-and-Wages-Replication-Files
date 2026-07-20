/*==============================================================================
  Script:   Figures 4 and 5.do
  Paper:    Figure 4: Graduate Share in Occupations
            Figure 5: Same Skill Cluster Share in Occupations
  Outputs:  $out/kdensitycollege.png, $out/kdensitycluster25.png,
            $out/cdfgraduate.png, $out/cdfcluster25.png
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"
*Graphing non-college or cluster share of occupation*

use "$rawdata/acssimple.dta", clear

keep if year>=2009
drop if occ==0|occ==9920|uhrswork==0

gen college=(degfieldd>0)


bys occ: gegen meancollegeshareocc=mean(college) [aweight=perwt]

label var meancollegeshareocc "College Educated Share in Occupation"

kdensity meancollegeshareocc if college==1, note("") title("")
graph export "$out/kdensitycollege.png", as(png) replace
kdensity meancollegeshareocc if college==1, note("") xtitle("Proportion of College Educated Workers in Same Occupation who are Fellow Graduates") title("Distribution of Graduate Share in Same Occupation")
graph export "$out/kdensitycollegepresentation.png", as(png) replace

merge m:1 degfieldd using "$intermediatedata/weighted_average_linkage_jsd_soc4"
drop _merge


foreach x in 25 175{
levelsof cluster`x', local(skill`x')
gen meanclustershareocc`x'=.
foreach y of local skill`x'{
	gen group`y'=(cluster`x'==`y')
	gegen meanclustershareownocc`x'=mean(group`y') [aweight=perwt], by(occ)
	replace meanclustershareocc`x'=meanclustershareownocc`x' if cluster`x'==`y'
    drop meanclustershareownocc`x' group`y'
}
}

label var meanclustershareocc25 "Share in Same Skill Cluster within Occupation"

save "$cleandata/clusteroccshare.dta", replace
use "$cleandata/clusteroccshare.dta", clear


kdensity meanclustershareocc25 if college==1, note("") title("")
graph export "$out/kdensitycluster25.png", as(png) replace

kdensity meanclustershareocc25 if college==1, note("") title("Distribution of Same Skill Cluster Share in Same Occupation") xtitle("Proportion of Workers in Same Occupation from the Same Skill Cluster")
graph export "$out/kdensitycluster25presentation.png", as(png) replace


cumul meancollegeshareocc if college==1, generate(meancollegeshareocccumul)
line meancollegeshareocccumul meancollegeshareocc if college==1, sort
graph export "$out/cdfgraduate.png", as(png) replace


cumul meanclustershareocc25, generate(meanclustershareocc25cumul)
line meanclustershareocc25cumul meanclustershareocc25, sort

graph export "$out/cdfcluster25.png", as(png) replace


