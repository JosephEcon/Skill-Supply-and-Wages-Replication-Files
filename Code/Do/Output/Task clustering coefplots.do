/*==============================================================================
  Script:   Task clustering coefplots.do
  Paper:    Figure B5 (elasticity coefplots using O*NET IWA task clustering)
  Output:   $out/coefplots_task_iwa_manhattan.png

  Clustering-robustness coefficient plots for the O*NET IWA task clustering
  (Manhattan distance), Average and Ward linkage, OLS and IV, jackknife CIs,
  sqrt_weight. Layout: rows = {OLS, IV}, cols = {Average, Ward}.
  Inputs: Data/intermediate/task_weighted_{average,ward}_linkage_manhattan_iwa.dta
          (from "task clustering.R") and nationalclustersprep.dta (from cleaning.do).
  RESUMABLE: a panel whose .gph already exists is skipped (delete stale ones
  before the first run).
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

set maxvar 10000

* Intentional local override: the published Figure B5 OLS spec omits averageage
global controls blackshare hispanicshare asianshare femaleshare migrantshare postgradshare northeastshare midwestshare southshare
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage hdfemigrantshare hdfepostgradshare hdfenortheastshare hdfemidwestshare hdfesouthshare

local XLAB xlabel(1 "20" 2 "25" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90" 10 "100" 11 "175")

foreach s in avg ward {
    if "`s'"=="avg" {
        global SAMP task_weighted_average_linkage_manhattan_iwa
        local LBL "Average linkage"
    }
    else {
        global SAMP task_weighted_ward_linkage_manhattan_iwa
        local LBL "Ward linkage"
    }

    capture confirm file "$interout/task_`s'_ols.gph"
    local haveols = (_rc==0)
    capture confirm file "$interout/task_`s'_iv.gph"
    local haveiv = (_rc==0)
    if `haveols' & `haveiv' {
        display "[cache] `s' panels already present -- skipping"
        continue
    }

    *---- build panel ----
    use "$intermediatedata/$SAMP", clear
    capture destring degfieldd, replace
    tempfile cl
    save `cl', replace
    use "$intermediatedata/nationalclustersprep", clear
    merge m:1 degfieldd using `cl'
    keep if _merge==3
    drop _merge
    foreach x in 20 25 30 40 50 60 70 80 90 100 175 {
        capture: gegen cluster`x'hoursshare=sum(clusterhoursshare), by(cluster`x' agegroup year)
        capture: gen lncluster`x'hoursshare=ln(cluster`x'hoursshare)
        capture: gegen nationalhourscluster`x'share=sum(nationalhoursclustershare), by(cluster`x' agegroup year)
        capture: gen lnnationalhourscluster`x'share=ln(nationalhourscluster`x'share)
    }
    capture: gen clusterage=cluster175*10+agegroup
    tempfile built
    save `built', replace

    if !`haveols' {
        use `built', clear
        xtset clusterage year
        foreach x in 20 25 30 40 50 60 70 80 90 100 175 {
            label var lncluster`x'hoursshare "`x'"
            quietly jackknife, mse cluster(cluster`x'): reghdfe lnclusterwage lncluster`x'hoursshare $controls [pweight=sqrt_weight], absorb(i.cluster175#i.agegroup i.year c.year#i.cluster175) cluster(cluster`x')
            mat b`x'=_b[lncluster`x'hoursshare]
            mat ci`x'=[0\0]
            mat ci`x'[1,1]=r(table)[5,1]
            mat ci`x'[2,1]=r(table)[6,1]
        }
        matrix b=(b20,b25,b30,b40,b50,b60,b70,b80,b90,b100,b175)
        matrix colnames b=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        matrix cc=(ci20,ci25,ci30,ci40,ci50,ci60,ci70,ci80,ci90,ci100,ci175)
        matrix colnames cc=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        coefplot mat(b), ci(cc) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("`LBL'") `XLAB'
        graph save "$interout/task_`s'_ols", replace
    }

    if !`haveiv' {
        use `built', clear
        xtset clusterage year
        rename lnnationalhourscluster*share* lnnationalcluster*share*
        foreach v of varlist lnclusterwage lncluster*share lnnational*share blackshare hispanicshare asianshare femaleshare averageage migrantshare postgradshare northeastshare midwestshare southshare {
            quietly: reghdfe `v' [pweight=sqrt_weight], absorb(c.year#i.cluster175 i.cluster175#i.agegroup) resid
            predict hdfe`v', resid
        }
        xtset clusterage year
        foreach x in 20 25 30 40 50 60 70 80 90 100 175 {
            label var lncluster`x'hoursshare "`x'"
            quietly jackknife, mse cluster(cluster`x'): ivregress 2sls hdfelnclusterwage (hdfelncluster`x'hoursshare=hdfelnnationalcluster`x'share) $hdfecontrols i.year [pweight=sqrt_weight], cluster(cluster`x')
            mat b`x'=_b[hdfelncluster`x'hoursshare]
            mat ci`x'=[0\0]
            mat ci`x'[1,1]=r(table)[5,1]
            mat ci`x'[2,1]=r(table)[6,1]
        }
        matrix biv=(b20,b25,b30,b40,b50,b60,b70,b80,b90,b100,b175)
        matrix colnames biv=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        matrix civ=(ci20,ci25,ci30,ci40,ci50,ci60,ci70,ci80,ci90,ci100,ci175)
        matrix colnames civ=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        coefplot mat(biv), ci(civ) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("`LBL'") `XLAB'
        graph save "$interout/task_`s'_iv", replace
    }
}

*---- combine: OLS row (Average, Ward) + IV row (Average, Ward) ----
graph combine "$interout/task_avg_ols" "$interout/task_ward_ols", ycommon rows(1) title("OLS")
graph save "$interout/task_ols_row", replace
graph combine "$interout/task_avg_iv" "$interout/task_ward_iv", ycommon rows(1) title("IV")
graph save "$interout/task_iv_row", replace
graph combine "$interout/task_ols_row" "$interout/task_iv_row", ycommon rows(2)
graph export "$out/coefplots_task_iwa_manhattan.png", as(png) replace
display _n "===== DONE: coefplots_task_iwa_manhattan.png (avg + ward, consistent titles) ====="
