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


*Merging wage and supply series*
use "$rawdata/effunits-exp-byexp-6308", clear
merge 1:1 expcat year using "$rawdata/clghsgwg-march-regseries-exp"
drop _merge

*Subtracting aggregate supply from own supply
replace euexp_lnclg=euexp_lnclg-eu_lnclg

*Time trends*
gen t=year-1963
gen t2=t^2/100

*Labelling variables
label var euexp_lnclg "Own minus aggregate supply"
label var eu_lnclg "Aggregate supply"
label var t "Time"
label var t2 "Time^2/100"


*Table 9 of Acemoglu and Autor (2011)*
reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2 if expcat<=4, absorb(i.expcat)
outreg2 using "$out/exactreplication", dec(3) nocons label word replace ctitle(All) addnote("Homoscedastic standard errors in parentheses. Panel A contains experience group fixed effects and Panel B has no constant.") noaster

reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2 if expcat==1, absorb(i.expcat)
outreg2 using "$out/exactreplication", dec(3) nocons label word append ctitle(0-9) noaster

reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2 if expcat==2, absorb(i.expcat)
outreg2 using "$out/exactreplication", dec(3) nocons label word append ctitle(10-19) noaster

reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2 if expcat==3, absorb(i.expcat)
outreg2 using "$out/exactreplication", dec(3) nocons label word append ctitle(20-29) noaster

reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2 if expcat==4, absorb(i.expcat)
outreg2 using "$out/exactreplication", dec(3) nocons label word append ctitle(30-39) noaster





*Table 9 first-differenced*
xtset expcat year

reg d.clphsg_exp d.eu_lnclg d.euexp_lnclg d.t d.t2 if expcat<=4, nocons 
outreg2 using "$out/differencedreplication", dec(3) nocons label word replace ctitle(All) noaster

reg d.clphsg_exp d.eu_lnclg d.euexp_lnclg d.t d.t2  if expcat==1, nocons 
outreg2 using "$out/differencedreplication", dec(3) nocons label word append ctitle(0-9) noaster

reg  d.clphsg_exp d.eu_lnclg d.euexp_lnclg d.t d.t2  if expcat==2, nocons 
outreg2 using "$out/differencedreplication", dec(3) nocons label word append ctitle(10-19) noaster

reg  d.clphsg_exp d.eu_lnclg d.euexp_lnclg d.t d.t2 if expcat==3, nocons 
outreg2 using "$out/differencedreplication", dec(3) nocons label word append ctitle(20-29) noaster

reg  d.clphsg_exp d.eu_lnclg d.euexp_lnclg d.t d.t2  if expcat==4, nocons 
outreg2 using "$out/differencedreplication", dec(3) nocons label word append ctitle(30-39) noaster

*Table 9 wild bootstrapped standard errors*
set seed 46596787

reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2, absorb(i.expcat)
eststo wildbootstrap
estadd local fixed_effects "Yes"


boottest euexp_lnclg, cluster(expcat) weight(webb) reps(4999) nograph jackknife
mat lo = r(CI)[1,1] //subsets the r(CI) matrx to only have the lower CI
mat coln lo = euexp_lnclg //needs to match the variable we want it to show up underneath
mat hi = r(CI)[1,2] //upper CI
mat coln hi = euexp_lnclg //same as above

estadd mat lo = lo : wildbootstrap
estadd mat hi = hi : wildbootstrap

jackknife, mse cluster(expcat): reghdfe clphsg_exp eu_lnclg euexp_lnclg t t2, absorb(i.expcat)

mat jacklo=r(table)[5, 2] 
mat coln jacklo = euexp_lnclg
mat jackhi=r(table)[6, 2] 
mat coln jackhi = euexp_lnclg

estadd mat jacklo=jacklo : wildbootstrap
estadd mat jackhi=jackhi : wildbootstrap


esttab wildbootstrap using "$out/wildbootstrap.rtf", ///
   replace nocons label ///
   addnotes("Notes: Homoscedastic standard errors in parentheses. The jackknifed wild bootstrap 95% confidence intervals for age-group relative supply, clustered at the potential experience group level, from 4999 replications using Webb weights is in square brackets. A cluster jackknife 95% confidence intervals is in braces.") ///
   cells(b(fmt(3)) se(par fmt(3)) 	/// if you specify (X & Y) it posts these two statistics to the same cell
		(lo(f(2) par(`"["') keep(euexp_lnclg)) & /// par option gives the left [ bracket but no right one
		hi(f(2) par("" `"]"') keep(euexp_lnclg))) ///
		(jacklo(f(2) par(`"<"') keep(euexp_lnclg)) & /// par option gives the left [ bracket but no right one
		jackhi(f(2) par("" `">"') keep(euexp_lnclg)))) /// par puts nothing on the left, but a ] on the right) 
		incelldelimiter(", ") ///puts comma in between the lo and hi
   stats(fixed_effects r2 N, ///
         labels("Experience fixed effects" "R-squared" "Observations")  fmt(0 3 %8.0fc)) ///
		 collabels(none) ///
		 nodepvar

*Local projections*
xtset expcat year
reghdfe d.(F3.clphsg_exp) d.euexp_lnclg L.(d.euexp_lnclg) L.(d.clphsg_exp), absorb(year) cluster(expcat)
