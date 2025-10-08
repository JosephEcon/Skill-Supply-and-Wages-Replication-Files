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

*Loop for different soc code levels*
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

save "$intermediatedata/acsdegoccsoc$digit", replace


*Splitting up dataset for each major and saving*
levelsof soc`digit', local(occupation)
levelsof degfieldd, local(degree)
cd "$intermediatedata"

foreach x of local degree{
	use acsdegoccsoc`digit', clear
	keep if degfieldd==`x'
	foreach y of local occupation{
		global targetocc `y'
		gen occ`y'=(soc`digit'=="$targetocc")
		gegen meanocc`y'=mean(occ`y') [aweight=perwt]
		drop occ`y'
	}
	keep if _n==1
	drop soc`digit' perwt
	save acsdeg`x'soc`digit', replace
	}

clear
foreach x of local degree{
	append using acsdeg`x'soc`digit'
}

foreach v of varlist meanocc*{
	egen z_`v'=std(`v')
}


save "$intermediatedata/degreeoccupationsoc$digit", replace
}

*Dimension reduction for four and three digit SOC codes. Otherwise, don't. Also use different linkage methods. Different numbers of principal components. And proportions vs z-scores*
foreach digit in 2 3 4{
foreach m in z_m m{
foreach linkage in averagelinkage wardslinkage{
foreach no in 5 10{
use "$intermediatedata/degreeoccupationsoc$digit", clear
gen no=`no'
gen digit=`digit'
if (digit==3|digit==4){
pca `m'eanocc*
if no==5{
	predict pc1 pc2 pc3 pc4 pc5
}
else{
	predict pc1 pc2 pc3 pc4 pc5 pc6 pc7 pc8 pc9 pc10
}
cluster `linkage' pc*
}
else{
	cluster `linkage' `m'eanocc*
}


cluster stop, groups (2/50)
cluster gen cluster20=groups(20)
cluster gen cluster10=groups(10)
cluster gen cluster5=groups(5)
cluster gen cluster30=groups(30)
cluster gen cluster40=groups(40)
cluster gen cluster50=groups(50)
cluster gen cluster60=groups(60)
cluster gen cluster70=groups(70)
cluster gen cluster80=groups(80)
cluster gen cluster90=groups(90)
cluster gen cluster100=groups(100)
cluster gen cluster176=groups(176)


drop meanocc* z_meanocc* _clus* no

global name `linkage'pc`no'`m'soc`digit'

save "$intermediatedata/$name", replace
}
}
}
}