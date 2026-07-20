
*Setting path and clearing*
local master=$master
if `master'==1{
	clear
}
else {
clear
global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages" // Set the path to the main replication folder
global rawdata "$path/Data/Raw"
global intermediatedata "$path/Data/intermediate"
global cleandata "$path/Data/clean"
global interout "$path/Intermediate Output"
global out "$path/Output"
}
	
	
*Loading main ACS dataset*
use "$rawdata/acsbase.dta", clear
keep year statefip bpl educ educd sex age race raced hispan hispand school degfield degfieldd degfield2 degfield2d occ occsoc empstat empstatd wkswork2 wkswork1 uhrswork incwage perwt region ind indnaics


* Split sample randomly into two halves for ORIV (obviously related IV) estimation
* Each half is used independently to construct instruments, reducing measurement error bias
splitsample, gen(sampleid, replace) rseed(894766)

*These are dropped after splitting the sample because I in
drop statefip raced hispand school degfield degfield2 ind indnaics

* degfieldd = IPUMS "degree field, detailed" (4-digit code identifying undergraduate major)
drop if degfieldd==.

*Education variables variable*
gen college=(degfieldd>0)
gen postgrad=(educ==11)
gen professional=(educd==115)
gen doublemajor=(degfield2d!=0)

*Fixing neuroscience to be one variable*
replace degfieldd=3611 if degfieldd==4003

*Identifying nationals*
gen national=(bpl<=56)
drop bpl empstat educ educd 

*Annual hours of work* 
 

 

gen weeksworked=wkswork1
forvalues i = 6(-1)1 {
	sum wkswork1 if wkswork2==`i'
	replace weeksworked=`r(mean)' if wkswork2==`i' & weeksworked==.
}
replace weeksworked=0 if wkswork2==0 & weeksworked==. 

 

gen annualhours=uhrswork*weeksworked 


*Splitting up by age groups*
* Age groups follow 5-year bins matching the paper's analysis
* Note: age 25 falls in group 1 (25-29), not group 0 (under 25)
gen agegroup = cond(age < 25, 0, cond(age < 30, 1, cond(age < 35, 2, ///
	cond(age < 40, 3, cond(age < 45, 4, cond(age < 50, 5, ///
	cond(age < 55, 6, cond(age < 60, 7, cond(age < 65, 8, 9)))))))))


label define agelabels 0 "20-24" 1 "25-29" 2 "30-34" 3 "35-39" 4 "40-44" 5 "45-49" 6 "50-54" 7 "55-59" 8 "60-64" 9 "65+"
label values agegroup agelabels

*Calculating wages*
* Hourly wage = annual income / annual hours; trimmed at 1st and 99th percentiles within age-college-year cells

gen wage=incwage/annualhours
replace annualhours=0 if incwage==0
replace wage=. if wage==0
winsor2 wage, cuts(1 99) trim by(agegroup college year) replace



drop weeksworked wkswork1 wkswork2 uhrswork incwage

compress


*Calculating values that depend on non-graduates and don't use cluster definitions*
* agemeanhours: mean annual hours by age-major-sex in 2009, used to construct the hours-weighted
* supply instrument (what supply would be if everyone worked their 2009-typical hours)
gegen agemeanhours=mean(annualhours) if year==2009 [aweight=perwt], by(age degfieldd sex year)
ereplace agemeanhours=min(agemeanhours), by(age degfieldd sex)
gegen agemeanhours1=mean(annualhours) if year==2009&sampleid==1 [aweight=perwt], by(age degfieldd sex year sampleid)
ereplace agemeanhours1=min(agemeanhours1), by(age degfieldd sex)
gegen agemeanhours2=mean(annualhours) if year==2009&sampleid==2 [aweight=perwt], by(age degfieldd sex year sampleid)
ereplace agemeanhours2=min(agemeanhours2), by(age degfieldd sex)



gegen nationalclusteragemeanhours=mean(agemeanhours) if national==1 [aweight=perwt], by(national degfieldd year)
gegen nationalclusteragemeanhours1=mean(agemeanhours1) if national==1 [aweight=perwt], by(national degfieldd year)
gegen nationalclusteragemeanhours2=mean(agemeanhours2) if national==1 [aweight=perwt], by(national degfieldd year)
ereplace nationalclusteragemeanhours=min(nationalclusteragemeanhours), by(degfieldd year)
ereplace nationalclusteragemeanhours1=min(nationalclusteragemeanhours1), by(degfieldd year)
ereplace nationalclusteragemeanhours2=min(nationalclusteragemeanhours2), by(degfieldd year)




gegen nationalagemeanhours=mean(agemeanhours) if national==1 [aweight=perwt], by(national year)
ereplace nationalagemeanhours=min(nationalagemeanhours)
gegen nationalagemeanhours1=mean(agemeanhours1) if national==1 [aweight=perwt], by(national year)
ereplace nationalagemeanhours1=min(nationalagemeanhours1)
gegen nationalagemeanhours2=mean(agemeanhours2) if national==1 [aweight=perwt], by(national year)
ereplace nationalagemeanhours2=min(nationalagemeanhours2)





* hoursweight: person weight * annual hours = total hours contributed by this individual
* agemeanhoursweight: person weight * predicted hours from 2009 base period
gen hoursweight=annualhours*perwt
gen agemeanhoursweight=agemeanhours*perwt

gen totalhours=sum(hoursweight)

gegen totalweight=sum(perwt), by(year)
gegen totalhoursweight=sum(hoursweight), by(year)

gegen totalweightsample=sum(perwt), by(sampleid year)
gegen totalhoursweightsample=sum(hoursweight), by(sampleid year)

gegen totalnationalweightsample=sum(perwt), by(national year sampleid)

gegen totalagemeanhoursweight=sum(agemeanhoursweight) if national==1, by(national year)
gegen totalagemeanhoursweightsample=sum(agemeanhoursweight) if national==1, by(national sampleid year)


ereplace totalagemeanhoursweight=min(totalagemeanhoursweight), by(year)
ereplace totalagemeanhoursweightsample=min(totalagemeanhoursweightsample), by(sampleid year)

gegen totalnationalweight=sum(perwt), by(national year)


**Occupation graduate share**
gegen collegeoccshare=mean(college) [aweight=hoursweight] if year==2009, by(occsoc year)
gegen majoroccshare=mean(collegeoccshare) [aweight=hoursweight], by(degfieldd)
ereplace majoroccshare=max(majoroccshare), by(degfieldd)

drop collegeoccshare


*Dropping non-graduates*

keep if college==1


*Wages by cluster*


gegen meanclusterhours=mean(annualhours) [aweight=perwt], by(degfieldd year)
gegen meanclusterhours1=mean(annualhours) if sampleid==1 [aweight=perwt], by(degfieldd year sampleid)
gegen meanclusterhours2=mean(annualhours) if sampleid==2 [aweight=perwt], by(degfieldd year sampleid)
ereplace meanclusterhours1=min(meanclusterhours1), by(degfieldd year)
ereplace meanclusterhours2=min(meanclusterhours2), by(degfieldd year)

* Cell-level mean hours used as the denominator for demographic *hoursshare variables only.
* The supply-path denominator (meanclusterhours above) stays at (degfieldd, year).
gegen meanclusterhours_age=mean(annualhours) [aweight=perwt], by(degfieldd agegroup year)


gegen clusterwage=mean(wage) [aweight=hoursweight], by(degfieldd agegroup year) 
ereplace clusterwage=min(clusterwage), by(degfieldd agegroup year) 
gen lnclusterwage=ln(clusterwage)

*Wages for undergrads only or excluding professional qualifications, or double majors*
gegen clusterwageug=mean(wage) if postgrad==0 [aweight=hoursweight], by(degfieldd postgrad agegroup year) 
ereplace clusterwageug=min(clusterwageug), by(degfieldd agegroup year) 
gen lnclusterwageug=ln(clusterwageug)

gegen clusterwagenoprof=mean(wage) if professional==0 [aweight=hoursweight], by(degfieldd professional agegroup year) 
ereplace clusterwagenoprof=min(clusterwagenoprof), by(degfieldd agegroup year) 
gen lnclusterwagenoprof=ln(clusterwagenoprof)

gegen clusterwagesingle=mean(wage) if doublemajor==0 [aweight=hoursweight], by(degfieldd doublemajor agegroup year)
ereplace clusterwagesingle=min(clusterwagesingle), by(degfieldd agegroup year) 
gen lnclusterwagesingle=ln(clusterwagesingle)



drop wage clusterwagenoprof clusterwagesingle clusterwageug

*Cluster shares and weights*


compress

*Calculating share of population with each degree*
gegen clusterweight=sum(perwt), by(degfieldd year)

gen clustershare=clusterweight/totalweight


*Calculating the cell size weights*
* Regression weight: cell size in 2009 (person-weights) * mean hours for that age-major cell
* This weights major-age cells proportionally to their size
gegen weight=sum(perwt) if year==2009, by(year agegroup degfieldd)
ereplace weight=max(weight), by(agegroup degfieldd)
gegen agegroupmeanhours=mean(agemeanhours) if year==2009, by(year agegroup degfieldd)
ereplace agegroupmeanhours=max(agemeanhours), by( agegroup degfieldd)
replace weight=weight*agegroupmeanhours

* Square-root regression weight (main spec for IV regressions).
* Pure population weighting (weight itself) gives one large cluster outsize
* leverage in jackknife inference and amplifies correlated measurement error
* between the IV and endogenous variable. Square-root weighting handles the
* heteroskedasticity in cell-level outcomes (SE scales with 1/sqrt(n_cell))
* while spreading leverage more evenly across clusters. See appendix table
* on weighting robustness for comparison with weight / unweighted.
gen sqrt_weight=sqrt(weight)
label var sqrt_weight "Square-root regression weight (main spec)"

drop agegroupmeanhours
*Same but for the split samples*
gegen clusterweightsample=sum(perwt), by(degfieldd sampleid year)
gen clustershare1=clusterweightsample/totalweightsample if sampleid==1
gen clustershare2=clusterweightsample/totalweightsample if sampleid==2
ereplace clustershare1=min(clustershare1) , by(degfieldd year)
ereplace clustershare2=min(clustershare2) , by(degfieldd year)


label var clustershare1 "Cluster share sample 1"
label var clustershare2 "Cluster share sample 2"



*Excluding those with further professional qualifications
gegen clusternoproweight=sum(hoursweight) if professional==0, by(degfieldd year professional)
gegen clusternoproweightsample=sum(hoursweight) if professional==0, by(degfieldd sampleid year professional)


ereplace clusternoproweight=min(clusternoproweight), by(degfieldd year)
ereplace clusternoproweightsample=min(clusternoproweightsample), by(degfieldd sampleid year)


gen clusternoproshare=clusternoproweight/totalhoursweight
gen clusternoproshare1=clusternoproweightsample/totalhoursweightsample if sampleid==1
gen clusternoproshare2=clusternoproweightsample/totalhoursweightsample if sampleid==2
ereplace clusternoproshare1=min(clusternoproshare1) , by(degfieldd year)
ereplace clusternoproshare2=min(clusternoproshare2) , by(degfieldd year)


*Excluding all postgrads
gegen clusterugweight=sum(hoursweight) if postgrad==0, by(degfieldd year postgrad)
gegen clusterugweightsample=sum(hoursweight) if postgrad==0, by(degfieldd sampleid year postgrad)

ereplace clusterugweight=min(clusterugweight), by(degfieldd year)
ereplace clusterugweightsample=min(clusterugweightsample), by(degfieldd sampleid year)



gen clusterugshare=clusterugweight/totalhoursweight
gen clusterugshare1=clusterugweightsample/totalhoursweightsample if sampleid==1
gen clusterugshare2=clusterugweightsample/totalhoursweightsample if sampleid==2
ereplace clusterugshare1=min(clusterugshare1) , by(degfieldd year)
ereplace clusterugshare2=min(clusterugshare2) , by(degfieldd year)

*50-50 split for double majors*
replace perwt=perwt/2 if doublemajor==1
gegen clusterweightdouble=sum(hoursweight), by(degfieldd year)
gegen clusterweightdoublesample=sum(hoursweight), by(degfieldd sampleid year)


*Getting the second subject counts*
preserve
replace hoursweight=round(hoursweight)
contract degfield2d [fweight=hoursweight], freq(minorweight)
rename degfield2d degfieldd
tempfile minors
save `minors', replace
restore

preserve
keep if sampleid==1
replace hoursweight=round(hoursweight)
contract degfield2d [fweight=hoursweight], freq(minorweight1)
rename degfield2d degfieldd
tempfile minors1
save `minors1', replace
restore

preserve
replace hoursweight=round(hoursweight)
keep if sampleid==2
contract degfield2d [fweight=hoursweight], freq(minorweight2)
rename degfield2d degfieldd
tempfile minors2
save `minors2', replace
restore


merge m:1 degfieldd using `minors'
drop _merge
merge m:1 degfieldd using `minors1'
drop _merge
merge m:1 degfieldd using `minors2'
drop _merge


replace perwt=perwt*2 if doublemajor==1
replace clusterweightdouble=clusterweightdouble+minorweight
gen clusterweightdouble1=clusterweightdoublesample+minorweight1 if sampleid==1
gen clusterweightdouble2=clusterweightdoublesample+minorweight2 if sampleid==2



*Cluster shares for the other measurements*
gen clusterdoubleshare=clusterweightdouble/totalhoursweight
gen clusterdoubleshare1=clusterweightdouble1/totalhoursweightsample if sampleid==1
gen clusterdoubleshare2=clusterweightdouble2/totalhoursweightsample if sampleid==2

ereplace clusterdoubleshare1=min(clusterdoubleshare1), by(degfieldd year)
ereplace clusterdoubleshare2=min(clusterdoubleshare2), by(degfieldd year)



drop cluster*weight clusterweightdouble totalweight minor*


gegen meanhours=mean(annualhours) [aweight=perwt], by(year)
gegen meanhours1=mean(annualhours) if sampleid==1 [aweight=perwt], by(year sampleid)
gegen meanhours2=mean(annualhours) if sampleid==2 [aweight=perwt], by(year sampleid)
ereplace meanhours1=min(meanhours1), by(year)
ereplace meanhours2=min(meanhours2), by(year)


rename meanclusterhours* meanclusterhourse*
rename meanhours* meanhourse*

foreach x in e e1 e2{
gen lnclustershar`x'=ln(clustershar`x')
gen clusterhoursshar`x'=clustershar`x'*meanclusterhours`x'/meanhours`x'
gen lnclusterhoursshar`x'=ln(clusterhoursshar`x')

gen lnclusterugshar`x'=ln(clusterugshar`x')
gen lnclusternoproshar`x'=ln(clusternoproshar`x')
gen lnclusterdoubleshar`x'=ln(clusterdoubleshar`x')

}

rename meanclusterhourse* meanclusterhours*
drop meanhourse*


compress


gegen nationalclusterweight=sum(perwt), by(national degfieldd year)
gen nationalclustershare=nationalclusterweight/totalnationalweight if national==1
ereplace nationalclustershare=min(nationalclustershare), by(degfieldd year)

*Sample split
gegen nationalclusterweightsample=sum(perwt), by(national degfieldd year sampleid)
gen nationalclustershare1=nationalclusterweightsample/totalnationalweightsample if national==1&sampleid==1
gen nationalclustershare2=nationalclusterweightsample/totalnationalweightsample if national==1&sampleid==2
ereplace nationalclustershare1=min(nationalclustershare1), by(degfieldd year)
ereplace nationalclustershare2=min(nationalclustershare2), by(degfieldd year)

drop totalnationalweightsample nationalclusterweightsample nationalclusterweight totalnationalweight


compress

*Weighting by age mean hours*

gen nationalhoursclustershare=nationalclustershare*nationalclusteragemeanhours/nationalagemeanhours
gen lnnationalhoursclustershare=ln(nationalhoursclustershare)
gen nationalhoursclustershare1=nationalclustershare1*nationalclusteragemeanhours1/nationalagemeanhours1
gen lnnationalhoursclustershare1=ln(nationalhoursclustershare1)
gen nationalhoursclustershare2=nationalclustershare2*nationalclusteragemeanhours2/nationalagemeanhours2
gen lnnationalhoursclustershare2=ln(nationalhoursclustershare2)



drop nationalclusteragemeanhours* nationalagemeanhours* 



*Excluding those with further professional qualifications
gegen nationalclusternoproweight=sum(agemeanhoursweight) if professional==0&national==1, by(degfieldd year professional national)
gegen nationalclusternoproweightsample=sum(agemeanhoursweight) if professional==0&national==1, by(degfieldd sampleid year professional national)


ereplace nationalclusternoproweight=min(nationalclusternoproweight), by(degfieldd year)
ereplace nationalclusternoproweightsample=min(nationalclusternoproweightsample), by(degfieldd sampleid year)


gen nationalclusternoproshare=nationalclusternoproweight/totalagemeanhoursweight
gen nationalclusternoproshare1=nationalclusternoproweightsample/totalagemeanhoursweightsample if sampleid==1
gen nationalclusternoproshare2=nationalclusternoproweightsample/totalagemeanhoursweightsample if sampleid==2
ereplace nationalclusternoproshare1=min(nationalclusternoproshare1) , by(degfieldd year)
ereplace nationalclusternoproshare2=min(nationalclusternoproshare2) , by(degfieldd year)


*Excluding all postgrads
gegen nationalclusterugweight=sum(agemeanhoursweight) if postgrad==0&national==1, by(degfieldd year postgrad national)
gegen nationalclusterugweightsample=sum(agemeanhoursweight) if postgrad==0&national==1, by(degfieldd sampleid year postgrad national)

ereplace nationalclusterugweight=min(nationalclusterugweight), by(degfieldd year)
ereplace nationalclusterugweightsample=min(nationalclusterugweightsample), by(degfieldd sampleid year)



gen nationalclusterugshare=nationalclusterugweight/totalagemeanhoursweight
gen nationalclusterugshare1=nationalclusterugweightsample/totalagemeanhoursweightsample if sampleid==1
gen nationalclusterugshare2=nationalclusterugweightsample/totalagemeanhoursweightsample if sampleid==2
ereplace nationalclusterugshare1=min(nationalclusterugshare1) , by(degfieldd year)
ereplace nationalclusterugshare2=min(nationalclusterugshare2) , by(degfieldd year)

*50-50 split for double majors*
replace perwt=perwt/2 if doublemajor==1
gegen nationalclusterweightdouble=sum(agemeanhoursweight), by(degfieldd year)
gegen ntlclusterweightdoublesample=sum(agemeanhoursweight), by(degfieldd sampleid year)


*Getting the second subject counts*
replace agemeanhoursweight=round(agemeanhoursweight)
preserve
contract degfield2d [fweight=agemeanhoursweight], freq(minorweight)
rename degfield2d degfieldd
tempfile minors
save `minors', replace
restore

preserve
keep if sampleid==1
contract degfield2d [fweight=agemeanhoursweight], freq(minorweight1)
rename degfield2d degfieldd
tempfile minors1
save `minors1', replace
restore

preserve
keep if sampleid==2
contract degfield2d [fweight=agemeanhoursweight], freq(minorweight2)
rename degfield2d degfieldd
tempfile minors2
save `minors2', replace
restore


merge m:1 degfieldd using `minors'
drop _merge
merge m:1 degfieldd using `minors1'
drop _merge
merge m:1 degfieldd using `minors2'
drop _merge


replace perwt=perwt*2 if doublemajor==1
replace nationalclusterweightdouble=nationalclusterweightdouble+minorweight
gen nationalclusterweightdouble1=ntlclusterweightdoublesample+minorweight1 if sampleid==1
gen nationalclusterweightdouble2=ntlclusterweightdoublesample+minorweight2 if sampleid==2



*Nationalcluster shares for the other measurements*
gen nationalclusterdoubleshare=nationalclusterweightdouble/totalagemeanhoursweight
gen nationalclusterdoubleshare1=nationalclusterweightdouble1/totalagemeanhoursweightsample if sampleid==1
gen nationalclusterdoubleshare2=nationalclusterweightdouble2/totalagemeanhoursweightsample if sampleid==2

ereplace nationalclusterdoubleshare1=min(nationalclusterdoubleshare1), by(degfieldd year)
ereplace nationalclusterdoubleshare2=min(nationalclusterdoubleshare2), by(degfieldd year)

drop *agemeanhours* *clusterweight* totalagemeanhoursweight  minor*





*Gender controls*
gen female=(sex==2)
gegen femaleshare=mean(female) [aweight=perwt] , by(degfieldd agegroup year)
gen femalehours=female*annualhours
gegen meanfemalehours=mean(femalehours) [aweight=perwt] , by(degfieldd agegroup year)
gen femalehoursshare=meanfemalehours/meanclusterhours_age

drop female femalehours meanfemalehours sex


*Average age*
gegen averageage=mean(age) [aweight=hoursweight] , by(degfieldd agegroup year)
ereplace averageage=min(averageage), by(degfieldd agegroup year)



*Race controls*
gen black=(race==2)
gegen blackshare=mean(black) [aweight=perwt] , by(degfieldd agegroup year)
gen blackhours=black*annualhours
gegen meanblackhours=mean(blackhours) [aweight=perwt] , by(degfieldd agegroup year)
gen blackhoursshare=meanblackhours/meanclusterhours_age

drop black blackhours meanblackhours

gen asian=(race==4|race==5|race==6)
gegen asianshare=mean(asian) [aweight=perwt] , by(degfieldd agegroup year)
gen asianhours=asian*annualhours
gegen meanasianhours=mean(asianhours) [aweight=perwt] , by(degfieldd agegroup year)
gen asianhoursshare=meanasianhours/meanclusterhours_age

drop asian asianhours meanasianhours race


gen hispanic=(hispan>0)
gegen hispanicshare=mean(hispanic) [aweight=perwt] , by(degfieldd agegroup year)
gen hispanichours=hispanic*annualhours
gegen meanhispanichours=mean(hispanichours) [aweight=perwt] , by(degfieldd agegroup year)
gen hispanichoursshare=meanhispanichours/meanclusterhours_age

drop hispan hispanic hispanichours meanhispanichours


*Migrant share* (born outside the US; complement of `national`)
gen migrant=(national==0)
gegen migrantshare=mean(migrant) [aweight=perwt] , by(degfieldd agegroup year)
gen migranthours=migrant*annualhours
gegen meanmigranthours=mean(migranthours) [aweight=perwt] , by(degfieldd agegroup year)
gen migranthoursshare=meanmigranthours/meanclusterhours_age

drop migrant migranthours meanmigranthours


*Postgraduate share* (postgrad already exists from line 35 and is used downstream, so do NOT drop it)
gegen postgradshare=mean(postgrad) [aweight=perwt] , by(degfieldd agegroup year)
gen postgradhours=postgrad*annualhours
gegen meanpostgradhours=mean(postgradhours) [aweight=perwt] , by(degfieldd agegroup year)
gen postgradhoursshare=meanpostgradhours/meanclusterhours_age

drop postgradhours meanpostgradhours


*Census region shares*
* IPUMS region codes: 11-12 = Northeast, 21-22 = Midwest, 31-33 = South, 41-42 = West
gen northeast=inrange(region,11,12)
gen midwest=inrange(region,21,22)
gen south=inrange(region,31,33)
gen west=inrange(region,41,42)

foreach r in northeast midwest south west {
    gegen `r'share=mean(`r') [aweight=perwt] , by(degfieldd agegroup year)
    gen `r'hours=`r'*annualhours
    gegen mean`r'hours=mean(`r'hours) [aweight=perwt] , by(degfieldd agegroup year)
    gen `r'hoursshare=mean`r'hours/meanclusterhours_age
    drop `r' `r'hours mean`r'hours
}

drop region



* Collapse to one observation per major-agegroup-year (the unit of analysis)
* Keep the first observation from each cell (all variables are already at cell level after gegen)
drop age sampleid annualhours perwt
drop if degfieldd==.|degfieldd==0
gen n=_n
egen rowmin=min(n), by(degfieldd agegroup year)
drop if n!=rowmin
drop rowmin n


*Classifying stem degrees*
gen stem=0
replace stem=1 if ///
	inlist(degfieldd, 1103, 1104, 1105, 1106, 1301, 1302) | ///       /* Agriculture & Environment */
	inlist(degfieldd, 2001, 2100, 2101, 2102, 2105, 2106, 2107) | /// /* Communications & CS */
	inlist(degfieldd, 2400, 2401, 2402, 2403, 2404, 2405, 2406) | /// /* Engineering */
	inlist(degfieldd, 2407, 2408, 2409, 2410, 2411, 2412, 2413) | /// /* Engineering cont. */
	inlist(degfieldd, 2414, 2415, 2416, 2417, 2418, 2419, 2499) | /// /* Engineering cont. */
	inlist(degfieldd, 2500, 2501, 2502, 2503, 2504, 2599) | ///       /* Engineering Tech */
	inlist(degfieldd, 3600, 3601, 3602, 3603, 3604, 3605, 3606) | /// /* Biology */
	inlist(degfieldd, 3607, 3608, 3609, 3611, 3699, 3801) | ///       /* Biology & Math */
	inlist(degfieldd, 4002, 4003, 4005, 4006) | ///                    /* Physical Sciences */
	inlist(degfieldd, 5000, 5001, 5002, 5003, 5004, 5005, 5006) | /// /* Social Sciences (quant) */
	inlist(degfieldd, 5007, 5008, 5098, 5102) | ///                    /* Social Sciences cont. */
	inlist(degfieldd, 5901, 6106, 6108, 6202, 6212)                   /* Other STEM */





save "$intermediatedata/nationalclustersprep", replace

* Aggregate major-level variables up to cluster-level variables for each clustering specification
* Each clustering file (e.g., weighted_average_linkage_jsd_soc4) assigns majors to cluster IDs
foreach sample in weighted_average_linkage_jsd_soc4 weighted_average_linkage_jsd_soc2 weighted_average_linkage_jsd_soc3 weighted_average_linkage_manhattan_soc2 weighted_average_linkage_manhattan_soc3 weighted_average_linkage_manhattan_soc4 weighted_ward_linkage_jsd_soc4 weighted_ward_linkage_jsd_soc3 weighted_ward_linkage_jsd_soc2 weighted_average_linkage_jsd_soc409 weighted_average_linkage_jsd_soc209 weighted_average_linkage_jsd_soc309{

global sample `sample'	

*Destringing degfieldd labels*
use "$intermediatedata/$sample", clear

destring degfieldd, replace

save "$intermediatedata/$sample", replace

*Loading data

use "$intermediatedata/nationalclustersprep", clear


*Merging in degree cluster data for first and second major*

merge m:1 degfieldd using "$intermediatedata/$sample"
drop _merge


save "$intermediatedata/data$sample", replace


use "$intermediatedata/data$sample", clear


* For each cluster level k (20, 25, ..., 175) and sample suffix (e=full, e1=half1, e2=half2):
* - cluster{k}hoursshare: sum of major hours shares within each cluster (total supply by cluster)
* - nationalhourscluster{k}share: same but nationals-only (the IV)
* - cluster{k}hoursshareminusown: cluster supply excluding own major (placebo test variable)
* - Also compute shares excluding professional degrees (nopro), postgrads (ug), and double majors (double)
foreach y in e e1 e2{
foreach x in 20 25 30 40 50 60 70 80 90 100 101 171 175{
	capture: gegen cluster`x'hoursshar`y'=sum(clusterhoursshar`y'), by(cluster`x' agegroup year)
	capture: gen lncluster`x'hoursshar`y'=ln(cluster`x'hoursshar`y')
	capture: gegen nationalhourscluster`x'shar`y'=sum(nationalhoursclustershar`y'), by(cluster`x' agegroup year)
    capture: gen lnnationalhourscluster`x'shar`y'=ln(nationalhourscluster`x'shar`y')
	*Leave one out cluster shares
	capture: gen cluster`x'hoursshareminusown`y'=cluster`x'hoursshare-clusterhoursshar`y'
	capture: gen lncluster`x'hoursshareminusown`y'=ln(cluster`x'hoursshareminusown`y')
    capture: gen nationalcluster`x'shar`y'minusown=nationalhourscluster`x'shar`y'-nationalhoursclustershar`y'
    capture: gen lnntlcluster`x'shar`y'minusown=ln(nationalcluster`x'shar`y'minusown)
	
	*Cluster share with different supply measures*
				capture: gegen cluster`x'doubleshar`y'=sum(clusterdoubleshar`y'), by(cluster`x' agegroup year)
    capture: gen lncluster`x'doubleshar`y'=ln(cluster`x'doubleshar`y')

	
				capture: gegen cluster`x'ugshar`y'=sum(clusterugshar`y'), by(cluster`x' agegroup year)
    capture: gen lncluster`x'ugshar`y'=ln(cluster`x'ugshar`y')

			capture: gegen cluster`x'noproshar`y'=sum(clusternoproshar`y'), by(cluster`x' agegroup year)
    capture: gen lncluster`x'noproshar`y'=ln(cluster`x'noproshar`y')

	

	*National with different supply measures
			capture: gegen nationalcluster`x'doubleshar`y'=sum(nationalclusterdoubleshar`y'), by(cluster`x' agegroup year)
    capture: gen lnnationalcluster`x'doubleshar`y'=ln(nationalcluster`x'doubleshar`y')

	
				capture: gegen nationalcluster`x'ugshar`y'=sum(nationalclusterugshar`y'), by(cluster`x' agegroup year)
    capture: gen lnnationalcluster`x'ugshar`y'=ln(nationalcluster`x'ugshar`y')

			capture: gegen nationalcluster`x'noproshar`y'=sum(nationalclusternoproshar`y'), by(cluster`x' agegroup year)
    capture: gen lnnationalcluster`x'noproshar`y'=ln(nationalcluster`x'noproshar`y')


}
}
* Strip the 'e' suffix from full-sample variables (e1/e2 kept for split-sample ORIV)
rename *hoursshareminusowne* *hoursshareminusown*
* Panel identifier: encodes both cluster assignment and age group into a single numeric ID
capture: gen clusterage=cluster175*10+agegroup
capture: label var clusterage "Unique identifier"



*Initial wages*
* Initial wages: mean cluster wage in 2009, standardised. Used to split clusters into
* above/below mean wage for heterogeneity analysis (Table 3)
foreach x in 25 30 175{
capture: gegen initialwage`x'=mean(clusterwage) [aweight=weight] if year==2009, by(cluster`x' year)
capture: ereplace initialwage`x'=min(initialwage`x'), by(cluster`x')

capture: sum initialwage`x' [aweight=weight]
capture: gen stdinitialwage`x'=(initialwage`x'-`r(mean)')/`r(sd)'
}

* Initial cluster sizes (2009): used in ORIV measurement error models to interact with
* instruments, accounting for the structure of sampling noise (larger clusters have less noise)
foreach x in 25 30 175{
capture: gegen initialcluster`x'share=min(cluster`x'hoursshare) if year==2009, by(cluster`x' year)
capture: ereplace initialcluster`x'share=min(initialcluster`x'share), by(cluster`x')
*This comes from the standard deviation of a binomial distribution divided by its mean*
capture: gen lninitialcluster`x'share=ln(initialcluster`x'share)
}

*Saving data*sample
cd "$cleandata"
save analysis`sample' , replace
}
