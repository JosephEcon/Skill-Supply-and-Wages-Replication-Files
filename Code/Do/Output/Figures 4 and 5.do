*Setting path and clearing*
local master=$master
if `master'==1{
	clear
}
else {
clear
global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global rawdata "$path/data/raw"
global intermediatedata "$path/data/intermediate"
global cleandata "$path/data/clean"
global interout "$path/intermediateoutput"
global out "$path/output"
}
*Graphing non-college or cluster share of occupation*

use "$rawdata/acssimple.dta", clear

keep if year>=2009
drop if occ==0|occ==9920|uhrswork==0

gen college=(degfieldd>0)


bys occ: gegen meancollegeshareocc=mean(college) [aweight=perwt]

label var meancollegeshareocc "College Educated Share in Occupation"

kdensity meancollegeshareocc if college==1, note("") title("")
graph export "$out/kdensitycollege.jpg", as(jpg) replace
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
graph export "$out/kdensitycluster25.jpg", as(jpg) replace


cumul meancollegeshareocc if college==1, generate(meancollegeshareocccumul)
line meancollegeshareocccumul meancollegeshareocc if college==1, sort
graph export "$out/cdfgraduate.jpg", as(jpg) replace


cumul meanclustershareocc25, generate(meanclustershareocc25cumul)
line meanclustershareocc25cumul meanclustershareocc25, sort

graph export "$out/cdfcluster25.jpg", as(jpg) replace


