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
global controls blackshare hispanicshare asianshare femaleshare
global dcontrols d.blackshare d.hispanicshare d.asianshare d.femaleshare
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage


*Coefplot for 3-digit and 4-digit soc codes*

foreach soc in _soc4 _soc3 _soc2{
foreach linkage in average_linkage_ ward_linkage_{
foreach distance in manhattan jsd{

	global sample `linkage'`distance'`soc'
	global distance `distance'
*Loading relevant dataset*

if regexm("$sample", "ward")==0|regexm("$sample", "jsd")==1{
	
use "$cleandata/analysisweighted_$sample", clear

xtset clusterage year

		 
*Varying number of clusters figure*
foreach x in 20 30 40 50 60 70 80 90 100 175{
label var lncluster`x'hoursshare `x'
jackknife, mse cluster(cluster`x'): reghdfe lnclusterwage lncluster`x'hoursshare $controls  [pweight=weight], absorb(i.cluster175#i.agegroup i.year c.year#i.cluster175) cluster(cluster`x')
mat b`x'=_b[lncluster`x'hoursshare]
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1] 
mat ci`x'[2,1]=r(table)[6,1]
}
mat list b30

matrix b=(b20, b30, b40, b50, b60, b70, b80, b90, b100, b175)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
matrix clusterci=(ci20, ci30, ci40, ci50, ci60, ci70, ci80, ci90, ci100, ci175)
matrix colnames clusterci=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

*Coefplot*


if regexm("$sample", "_soc2")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC2") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}
if regexm("$sample", "_soc3")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC3") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}
if regexm("$sample", "_soc4")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC4") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}

graph save "$interout/coefplot$sample", replace


}

}
}
}




*IV

*Coefplot IV*
foreach soc in _soc4 _soc3 _soc2{
foreach linkage in average_linkage_ ward_linkage_{
foreach distance in manhattan jsd{
	

	global sample `linkage'`distance'`soc'
	global distance `distance'

	if regexm("$sample", "ward")==0|regexm("$sample", "jsd")==1{
	
	*Loading relevant dataset*
use "$cleandata/analysisweighted_$sample", clear

*Demeaning*
xtset clusterage year

*Partialling out hdfe to fix *
*Levels*
drop *double* *nopro*
rename lnnationalhourscluster*share* lnnationalcluster*share*

foreach x of varlist lnclusterwage lncluster*share lnnational*share blackshare hispanicshare asianshare femaleshare averageage{
	quietly: reghdfe `x' [pweight=weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
	predict hdfe`x', resid
	
	
}	




xtset clusterage year
foreach x in 20 30 40 50 60 70 80 90 100 175{
label var lncluster`x'hoursshare `x'
jackknife, mse cluster(cluster`x'): ivregress 2sls hdfelnclusterwage (hdfelncluster`x'hoursshare=hdfelnnationalcluster`x'share) $hdfecontrols i.year [pweight=weight],  cluster(cluster`x')
mat b`x'=_b[hdfelncluster`x'hoursshare]
mat ci`x'=[0\0]
mat ci`x'[1,1]=r(table)[5,1] 
mat ci`x'[2,1]=r(table)[6,1]
}
mat list b30

matrix b=(b20, b30, b40, b50, b60, b70, b80, b90, b100, b175)
matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
matrix clusterci=(ci20, ci30, ci40, ci50, ci60, ci70, ci80, ci90, ci100, ci175)
matrix colnames clusterci=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

*Coefplot with right titles*
if regexm("$sample", "_soc2")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC2") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}
if regexm("$sample", "_soc3")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC3") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}
if regexm("$sample", "_soc4")==1{
coefplot mat(b), ci(clusterci) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("SOC4") ///
xlabel(1 "20" 2 "30" 3 "40" 4 "50" 5 "60" 6 "70" 7 "80" 8 "90" 9 "100" 10 "175")  
}

graph save "$interout/ivplot$sample", replace


}
}
}
}

*Average linkage jsd*

graph combine "$interout/coefplotaverage_linkage_jsd_soc2" "$interout/coefplotaverage_linkage_jsd_soc3" "$interout/coefplotaverage_linkage_jsd_soc4", ycommon rows(1) title("OLS")
graph save "$interout/coefplot_average_jsd", replace

graph combine "$interout/ivplotaverage_linkage_jsd_soc2" "$interout/ivplotaverage_linkage_jsd_soc3" "$interout/ivplotaverage_linkage_jsd_soc4", ycommon rows(1) title("IV")
graph save "$interout/ivplot_average_jsd", replace

graph combine "$interout/coefplot_average_jsd" "$interout/ivplot_average_jsd", ycommon rows(2)
graph export "$out/coefplots_average_jsd_combine.jpg", as(jpg) replace


*Ward linkage jsd*

graph combine "$interout/coefplotward_linkage_jsd_soc2" "$interout/coefplotward_linkage_jsd_soc3" "$interout/coefplotward_linkage_jsd_soc4", ycommon rows(1) title("OLS")
graph save "$interout/coefplot_ward_jsd", replace

graph combine "$interout/ivplotward_linkage_jsd_soc2" "$interout/ivplotward_linkage_jsd_soc3" "$interout/ivplotward_linkage_jsd_soc4", ycommon rows(1) title("IV")
graph save "$interout/ivplot_ward_jsd", replace

graph combine "$interout/coefplot_ward_jsd" "$interout/ivplot_ward_jsd", ycommon rows(2)
graph export "$out/coefplots_ward_jsd_combine.jpg", as(jpg) replace

*Average linkage manhattan*

graph combine "$interout/coefplotaverage_linkage_manhattan_soc2" "$interout/coefplotaverage_linkage_manhattan_soc3" "$interout/coefplotaverage_linkage_manhattan_soc4", ycommon rows(1) title("OLS")
graph save "$interout/coefplot_average_manhattan", replace

graph combine "$interout/ivplotaverage_linkage_manhattan_soc2" "$interout/ivplotaverage_linkage_manhattan_soc3" "$interout/ivplotaverage_linkage_manhattan_soc4", ycommon rows(1) title("IV")
graph save "$interout/ivplot_average_manhattan", replace

graph combine "$interout/coefplot_average_manhattan" "$interout/ivplot_average_manhattan", ycommon rows(2)
graph export "$out/coefplots_average_manhattan_combine.jpg", as(jpg) replace
