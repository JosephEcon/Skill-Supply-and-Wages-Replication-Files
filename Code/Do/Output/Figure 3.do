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
global interout "$path/intermediate output"
global out "$path/output"
}



*Controls*
global controls blackshare hispanicshare asianshare femaleshare averageage
global dcontrols d.blackshare d.hispanicshare d.asianshare d.femaleshare d.averageage
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage
global hdfedcontrols hdfedblackshare hdfedhispanicshare hdfedasianshare hdfedfemaleshare hdfedaverageage


use "$cleandata/analysisweighted_average_linkage_jsd_soc4", clear

xtset clusterage year

*Partialling out hdfe to fix *
*Levels*
rename lnnationalcluster25*share lnnational25*share
rename lnnationalhourscluster*share* lnnationalcluster*share*
rename lncluster25hoursshareminusown* lncluster25shareminusown*

drop *double* *nopro* 

foreach x of varlist lnclusterwage* lncluster*share lnnational*share blackshare hispanicshare asianshare femaleshare averageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
}


*Coefplot OLS*
foreach x in 20 30 40 50 60 70 80 90 100 175{
label var lncluster`x'hoursshare `x'
jackknife, mse cluster(cluster`x'): reghdfe lnclusterwage lncluster`x'hoursshare $controls  [pweight=weight], absorb(i.cluster175#i.agegroup i.year c.year#i.cluster175) cluster(cluster`x')
mat b`x'=_b[lncluster`x'hoursshare] //subsets the r(CI) matrx to only have the lower CI
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1]  // lower ci
mat ci`x'[2,1]=r(table)[6,1]  // upper ci

}

matrix b=(b20, b30, b40, b50, b60, b70, b80, b90, b100, b175)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
matrix clusterci=(ci20, ci30, ci40, ci50, ci60, ci70, ci80, ci90, ci100, ci175)
matrix colnames clusterci=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("OLS") xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  

graph save "$interout/varyingclusterscoefplotols", replace

*Coefplot IV*
foreach x in 20 30 40 50 60 70 80 90 100 175{
label var hdfelncluster`x'hoursshare `x'
jackknife, mse cluster(cluster`x'): ivregress 2sls hdfelnclusterwage (hdfelncluster`x'hoursshare=hdfelnnationalcluster`x'share) $hdfecontrols i.year  [pweight=weight], cluster(cluster`x')
mat b`x'=_b[hdfelncluster`x'hoursshare] //subsets the r(CI) matrx to only have the lower CI
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1]  // lower ci
mat ci`x'[2,1]=r(table)[6,1]  // upper ci
}
mat list b30

matrix b=(b20, b30, b40, b50, b60, b70, b80, b90, b100, b175)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
matrix clusterci=(ci20, ci30, ci40, ci50, ci60, ci70, ci80, ci90, ci100, ci175)
matrix colnames clusterci=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("IV") xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  

graph save "$interout/varyingclusterscoefplotiv", replace


*Combining graphs*
graph combine "$interout/varyingclusterscoefplotols" "$interout/varyingclusterscoefplotiv", cols(2) ycommon
graph export "$out/figure 3.jpg", replace
