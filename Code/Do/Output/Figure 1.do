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


*Loading data
use "$rawdata/acssimple.dta"
drop if degfieldd==.
keep if year==2019

*Hours worked variable*
gen weeksworked=wkswork1 

sum wkswork1 if wkswork2==6 

replace weeksworked=`r(mean)' if wkswork2==6&weeksworked==. 

sum wkswork1 if wkswork2==5 

replace weeksworked=`r(mean)' if wkswork2==5&weeksworked==. 

sum wkswork1 if wkswork2==4 

replace weeksworked=`r(mean)' if wkswork2==4&weeksworked==. 

sum wkswork1 if wkswork2==3 

replace weeksworked=`r(mean)' if wkswork2==3&weeksworked==. 

sum wkswork1 if wkswork2==2 

replace weeksworked=`r(mean)' if wkswork2==2&weeksworked==. 

sum wkswork1 if wkswork2==1 

replace weeksworked=`r(mean)' if wkswork2==1&weeksworked==. 

replace weeksworked=0 if wkswork2==0&weeksworked==. 



gen annualhours=uhrswork*weeksworked 
drop if annualhours<1560

*Subject markers*
keep if degfieldd==2300|degfieldd==2414|degfieldd==3301|degfieldd==3600|degfieldd==3700|degfieldd==5003|degfieldd==5200|degfieldd==5501|degfieldd==5506|degfieldd==5507|degfieldd==6000|degfieldd==6107|degfieldd==6200|degfieldd==6402|educd==63

*Graphing income by major*

graph hbar incwage, over(degfieldd, sort(1) relabel(1 "High School Graduate" 2 "General Education" 3 "Mechanical Engineering" 4 "English Language and Literature" 5 "Biology" 6 "Mathematics" 7 "Chemistry" 8 "Psychology" 9 "Economics" 10 "Political Science" 11 "Sociology" 12 "Fine Arts" 13 "Nursing" 14 "Business" 15 "History")) b1title("Average Labour Income (USD)") ytitle("")  saving("$out/degearnings", replace) 

graph export "$out/degearnings.jpg", as(jpg) replace