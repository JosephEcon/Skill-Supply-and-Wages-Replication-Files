/*==============================================================================
  coefplot_core.do  —  build ONE combined OLS/IV elasticity-vs-k figure.
  Parameterised by globals set by the caller:
     $LINK  e.g. average_linkage_   $DIST  jsd|manhattan   $LAB  average_jsd
  Produces $out/coefplots_${LAB}_combine.png  (same filename as the monolith).
  k grid includes 25.  Mirrors "Different clusters coefplots.do" exactly.

  RESUMABLE: each of the 6 panels is cached as a .gph; a panel whose .gph
  already exists is skipped. So a kill/suspend costs at most one panel — re-run
  the same driver and it picks up where it stopped. (Stale .gph must be purged
  before the first run; the driver assumes any existing panel .gph is valid.)
==============================================================================*/
* control overrides (same asymmetry as the original figure script)
global controls blackshare hispanicshare asianshare femaleshare migrantshare postgradshare northeastshare midwestshare southshare
global hdfecontrols hdfeblackshare hdfehispanicshare hdfeasianshare hdfefemaleshare hdfeaverageage hdfemigrantshare hdfepostgradshare hdfenortheastshare hdfemidwestshare hdfesouthshare

*------------------------------- OLS -------------------------------*
foreach soc in _soc2 _soc3 _soc4 {
    global sample "${LINK}${DIST}`soc'"
    capture confirm file "$interout/coefplot$sample.gph"
    if _rc==0 {
        display "[cache] OLS $sample already done -- skipping"
    }
    else {
        use "$cleandata/analysisweighted_$sample", clear
        xtset clusterage year
        foreach x in 20 25 30 40 50 60 70 80 90 100 175 {
            label var lncluster`x'hoursshare "`x'"
            quietly jackknife, mse cluster(cluster`x'): reghdfe lnclusterwage lncluster`x'hoursshare $controls [pweight=sqrt_weight], absorb(i.cluster175#i.agegroup i.year c.year#i.cluster175) cluster(cluster`x')
            mat b`x'=_b[lncluster`x'hoursshare]
            mat ci`x'=[0\0]
            mat ci`x'[1,1]=r(table)[5,1]
            mat ci`x'[2,1]=r(table)[6,1]
        }
        matrix bb=(b20,b25,b30,b40,b50,b60,b70,b80,b90,b100,b175)
        matrix colnames bb=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        matrix cc=(ci20,ci25,ci30,ci40,ci50,ci60,ci70,ci80,ci90,ci100,ci175)
        matrix colnames cc=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        local ttl = upper(substr("`soc'",2,.))
        coefplot mat(bb), ci(cc) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("`ttl'") ///
            xlabel(1 "20" 2 "25" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90" 10 "100" 11 "175")
        graph save "$interout/coefplot$sample", replace
    }
}

*------------------------------- IV -------------------------------*
foreach soc in _soc2 _soc3 _soc4 {
    global sample "${LINK}${DIST}`soc'"
    capture confirm file "$interout/ivplot$sample.gph"
    if _rc==0 {
        display "[cache] IV $sample already done -- skipping"
    }
    else {
        use "$cleandata/analysisweighted_$sample", clear
        xtset clusterage year
        capture drop *double* *nopro*
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
        matrix bb=(b20,b25,b30,b40,b50,b60,b70,b80,b90,b100,b175)
        matrix colnames bb=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        matrix cc=(ci20,ci25,ci30,ci40,ci50,ci60,ci70,ci80,ci90,ci100,ci175)
        matrix colnames cc=c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
        local ttl = upper(substr("`soc'",2,.))
        coefplot mat(bb), ci(cc) yline(0) vertical xtitle("Number of clusters") ytitle("Elasticity") title("`ttl'") ///
            xlabel(1 "20" 2 "25" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90" 10 "100" 11 "175")
        graph save "$interout/ivplot$sample", replace
    }
}

*------------------------------- combine -------------------------------*
graph combine "$interout/coefplot${LINK}${DIST}_soc2" "$interout/coefplot${LINK}${DIST}_soc3" "$interout/coefplot${LINK}${DIST}_soc4", ycommon rows(1) title("OLS")
graph save "$interout/coefplot_$LAB", replace
graph combine "$interout/ivplot${LINK}${DIST}_soc2" "$interout/ivplot${LINK}${DIST}_soc3" "$interout/ivplot${LINK}${DIST}_soc4", ycommon rows(1) title("IV")
graph save "$interout/ivplot_$LAB", replace
graph combine "$interout/coefplot_$LAB" "$interout/ivplot_$LAB", ycommon rows(2)
graph export "$out/coefplots_${LAB}_combine.png", as(png) replace
display _n "===== DONE: coefplots_${LAB}_combine.png ====="
