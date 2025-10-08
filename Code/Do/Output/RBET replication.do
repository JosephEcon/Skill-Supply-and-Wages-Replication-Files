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

*Loading in data*
use "$rawdata/colhs1405", clear

*File-path adjusted AGK cleaning code code*
set more 1
set mem 50m
use "$rawdata/colhs1405"
d
list year wprem2 relsup
merge year using "$rawdata/km-cg-rsup-6317"
tab _merge
sort year
list

replace wprem2 = clphsg_all + .0037398 if year>=2006
*relsup - eu_lnclg = .0152795 in 2005
replace relsup = eu_lnclg + .0152795 if year>=2006

list

gen time = year - 1914
gen t49 = max(year-1949,0)
gen t59 = max(year-1959,0)
gen t92 = max(year-1992,0)
gen tsq = time*time/10
gen t3 = tsq*time/10
gen t4 = t3*time/10
gen t5 = t4*time/10
gen d49 = year==1949

*Setting the time*
tsset time

*College Wage Premium, 1914-2017, wprem2, young males for 1914-39 college premium change
*Col. (1)
reg wprem2 relsup time t49 t92
*Col. (2)
reg wprem2 relsup time t59 t92
predict pred2
*Col. (3)
reg wprem2 relsup time t59 t92 d49
predict pred3
*Col. (4)
*Shift in Relative Supply Coeff After 1949
gen relsup49 = relsup
replace relsup49 = 0 if year<=1949
reg wprem2 relsup relsup49 time t59 t92 d49



*Testing for a unit root*
dfuller wprem2 if year>=1963, trend
dfuller wprem2 if year>=1963&year<1993, trend 
dfuller wprem2 if year>=1993, trend 

dfuller relsup if year>=1963, trend
dfuller relsup if year>=1963&year<1993, trend 
dfuller relsup if year>=1993, trend 



*Testing for cointegration*
reg wprem2 relsup time
predict u1, resid
dfuller u1 if year>=1963

reg wprem2 relsup time if year>=1949&year<1993
predict u2, resid
dfuller u2 if year>=1963&year<1993

reg wprem2 relsup time if year>=1993
predict u3, resid
dfuller u3 if year>=1993


*Variable labels
label var wprem2 "Ln(Relative wage)"
label var relsup "Ln(Relative supply)"


*AGK Original
reg wprem2 relsup time if year>=1963
outreg2 using "$out/agkoriginal", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time t92 if year>=1963
outreg2 using "$out/agkoriginal", append addtext(Sample, 1963-2017, Time trend, Linear spline) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time tsq if year>=1963
outreg2 using "$out/agkoriginal", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1963&year<1993
outreg2 using "$out/agkoriginal", append addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(relsup) word dec(3) label

reg wprem2 relsup time if year>=1993
outreg2 using "$out/agkoriginal", append addtext(Sample, 1993-2017, Time trend, Linear) nocons keep(relsup) word dec(3) label

*First differenced regressions*
gen dwprem2=d.wprem2
gen drelsup=d.relsup
label var dwprem2 "Ln(Relative wage)"
label var drelsup "Ln(Relative supply)"


reg dwprem2 drelsup d.time, r nocons
outreg2 using "$out/agkdifferenced", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.t92, r nocons
outreg2 using "$out/agkdifferenced", append addtext(Sample, 1963-2017, Time trend, Linear spline) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq, r nocons
outreg2 using "$out/agkdifferenced", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1949&year<1993, r nocons
outreg2 using "$out/agkdifferenced", append addtext(Sample, 1963-1992, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time if year>=1993, r nocons
outreg2 using "$out/agkdifferenced", append addtext(Sample, 1993-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label


*First differenced regressions - two year gap*
preserve

gen time2=floor(time/2)
gen time2no=time-2*time2
drop if time2no!=0

tsset time2
replace dwprem2=d.wprem2
replace drelsup=d.relsup

reg dwprem2 drelsup d.time, r nocons
outreg2 using "$out/agkdifferenced2", replace addtext(Sample, 1963-2017, Time trend, Linear) nocons keep(drelsup) word dec(3) label

reg dwprem2 drelsup d.time d.tsq, r nocons
outreg2 using "$out/agkdifferenced2", append addtext(Sample, 1963-2017, Time trend, Quadratic) nocons keep(drelsup) word dec(3) label


restore





drop _merge
merge 1:1 year using "$intermediatedata/mayunrate"


*Local Projections specifying time trend*
gen tcube=time^3
gen tquart=time^4


****************************************************
*  IRF for Local Projections in First Differences
****************************************************


*Basic*
* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time
gen ldrelsup   = l.d.relsup
gen dlwprem2   = L.wprem2 - L2.wprem2

foreach x of numlist 0/7 {
    tsset time
    
    *— construct the dependent‐"difference" at horizon x —*
    gen f`x'dwprem2 = F`x'.wprem2 - L.wprem2
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup d.time, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("HC3") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph save "$interout/differencesirf", replace

	graph export "$out/differencesirf.jpeg", as(jpg) replace
restore


***********
*Newey*****
***********

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_newey ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    
    *— run regression without constant
    quietly newey f`x'dwprem2 drelsup d.time, ///
        nocons lag(3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_newey (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_newey

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Newey") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
			graph save "$interout/differencesirfnewey", replace

	graph export "$out/differencesirfnewey.jpeg", as(jpg) replace
restore


*****************
****Lag**********
*****************

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_lag ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, ///
        nocons vce(hc3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_lag (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_lag

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("HC3 with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
		graph save "$interout/differencesirflag", replace

	graph export "$out/differencesirflag.jpeg", as(jpg) replace
restore

************************************
********Lag and Newey***************
************************************

* 1. Create a temporary postfile for the "difference" regressions

tempfile irf_diff
postfile diff_post_newey_lag ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    
    *— run regression without constant
    quietly newey f`x'dwprem2 drelsup ldrelsup L.dwprem2 d.time, ///
        nocons lag(3)
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_newey_lag (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_newey_lag

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
    title("Newey with Lags") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph save "$interout/differencesirflagnewey", replace
	graph export "$out/differencesirflagnewey.jpeg", as(jpg) replace
restore


*********************************
***Quadratic*********************
*********************************

tempfile irf_diff_quadratic
postfile diff_post_quadratic ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_diff_quadratic', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
    *— run regression without constant
    quietly regress f`x'dwprem2 drelsup d.time d.tsq, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_quadratic (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_quadratic

* 3. Load the results, compute CI, and plot
preserve
use `irf_diff_quadratic', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of College Wage Premium") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph export "$out/differencesirfquadratic.jpeg", as(jpg) replace
restore


*Relsup impulse response*


tempfile irf_relsup
postfile diff_post_relsup ///
    int horizon ///
    double b ///
    double se ///
    double df_r ///
    using `irf_relsup', replace

* 2. Ensure the "delta" variables exist before looping
tsset time

foreach x of numlist 0/7 {
    tsset time
    
	gen f`x'relsup=F`x'.relsup-L.relsup

    *— run regression without constant
    quietly regress f`x'relsup drelsup d.time, ///
        vce(hc3) nocons
    
    *— grab coefficient on drelsup, its se, and df
    scalar bcoef  = _b[drelsup]
    scalar bse    = _se[drelsup]
    scalar bdf    = e(df_r)
    
    post diff_post_relsup (`x') (bcoef) (bse) (bdf)
    
}

postclose diff_post_relsup

* 3. Load the results, compute CI, and plot
preserve
use `irf_relsup', clear
sort horizon

gen tcrit = invttail(df_r, 0.025)
gen ub = b + tcrit * se
gen lb = b - tcrit * se

twoway ///
    (rcap ub lb horizon, lwidth(medium)) ///
    (line b horizon, lwidth(medium) lcolor(black)), ///
    xlabel(0(1)7) ///
      xtitle("Horizon (years)") ///
    ytitle("Response of Relative Supply") ///
	yline(0, lpattern(dash) lcolor(gs10)) ///
    legend(off)
	graph export "$out/irfrelsup.jpeg", as(jpg) replace
restore



*Combining graphs*
graph combine "$interout/differencesirf" "$interout/differencesirfnewey"  "$interout/differencesirflag" "$interout/differencesirflagnewey", ycommon rows(2)
graph export "$out/irfcombine.jpeg", as(jpg) replace






*Local projections in levels
foreach x in 0 1 2 3 4 5 6 7{
	tsset time
reg F`x'.wprem2 relsup L.relsup L.wprem2 time tsq tcube tquart, vce(hc3)
}

*Local projections in differences
gen ldrelsup=l.d.relsup
gen dlwprem2=L.wprem2-L2.wprem2

foreach x in 0 1 2 3 4 5 6 7{
	tsset time
	gen f`x'dwprem2=F`x'.wprem2-L.wprem2
reg f`x'dwprem2 drelsup ldrelsup dlwprem2 d.time, vce(hc3) nocons
}


