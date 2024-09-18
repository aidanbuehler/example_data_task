

****************************
*   Do File for Task 1     * 
****************************

***************
* Question 1  * 
***************

// Setup 
drop _all
clear all
set more off
set scheme s1mono
set linesize 79

// Open a log file 
capture log close
log using Logfile_Task1, replace 

// Set workign directory 
cd "GSS_stata"

// Read the data
set maxvar 120000
use "GSS7218_R1.dta", clear

// Make all variables lowercase 
rename *, lower



***************
* Question 2  *
***************

order vote*, first

// Because the data are cross-sectional, we cannot use obs. on the same individual two years in the future. 
// This in turn means that we have to drop all obs that don't occur on the desired years. 
keep if year < 2000 
tab year 

*Keep only years two years after election years
gen flag = 0 
foreach i of numlist 72 76 80 84 88 92 96 {
	local v = 19`i' + 2
	di "`v'"
	replace flag = 1 if year == `v'
}
tab year if flag
keep if flag 

// Create the presidential voting variable 
gen pres_vote = 0

levelsof year 
foreach i in `r(levels)'{
	local v = `i' - 1902 
	di "`v'"
	replace pres_vote = 1 if year == `i' & vote`v' == 1 
	replace pres_vote = . if year == `i' & (  missing(vote`v') | vote`v' >= 4  ) 
	*** NOTE: noneligible people are counted as not voting. Nonresponses and missing are coded as unknown. 
}

label var pres_vote "Respondent voted in most recent presidential election"


*******************
*   Question 3    * 
*******************
//  I looked up the missing codes 
// https://sda.berkeley.edu/sdaweb/docs/gss21/DOC/NewGSSMDcodes.pdf

// Keep obs that correspond to whether the person voted in the 1972, etc. election
// This means we need to drop obs where it is unclear whether the person voted. 
drop if missing(pres_vote)


******************
*   Question 4   * 
******************

order pres*, first

gen repub_v_dem = 0
label var repub_v_dem "Respondent voted for republican candidate"

// Add values
levelsof year 
foreach i in `r(levels)'{
	local v = `i' - 1902 
	di "`v'"
	replace repub_v_dem = 1 if year == `i' & pres`v' == 2 
	replace repub_v_dem = . if year == `i' & (  missing(pres`v') | pres`v' > 2  ) 
	*** Unknown values and third party votes are coded as missing, as specified 
}


// Create a label 
label define repub_v_dem 1 "Voted Republican" 0 "Voted Democrat"
label values repub_v_dem repub_v_dem
tab repub_v_dem, miss


*******************
*   Question 5    * 
*******************

// Create male variable 
gen male = sex == 1
label define male 1 "Male" 0 "Female"
label values male male
tab male
label var male "Gender"

*** NOTE: the sexnow variable refers to gender (i.e. what the person identifies as)
*** which would seem to be more what the question is referring to. However it has no values 
tab sexnow, miss
*** I assume this is because the distiction bewteen sex and gender was typically 
*** not made in surveys until the 2000s at the very least. 
*** As such, I use biological sex, which is the closest proxy the data seem to afford. 



****************
*  Question 6  * 
****************
tab relig, miss

gen religion = relig
recode religion 1=1 2=2 4 12=3  3 5/11 13 = 4
*** NOTE: I think it's reasonable to assume that if someone doesn't know their religion, they don't have one 
*** I also put the people who did not respond into the "Other category". This is because the question 
*** Only listed four factor levels; in my own research however, I typically code missing values as their own factor level. 
*** (This would be feasible; 106 values is enough to support a parameter in a regression)
label define religion 1 "Protestant" 2 "Catholic" 3 "No religion" 4 "Other"
label values religion religion
tab religion, miss
label var religion "Religion"
// It seems the missing values did not recode properly. I fix this 
replace religion = 3 if religion == .d
replace religion = 4 if religion == .n
tab religion, miss


***************
*  Question 7 * 
***************

* Generate a new string variable for age categories
generate age_cat = ""

* Assign categories based on age ranges
replace age_cat = "18-29" if age >= 18 & age <= 29
replace age_cat = "30-49" if age >= 30 & age <= 49
replace age_cat = "50-64" if age >= 50 & age <= 64
replace age_cat = "65+" if age >= 65
replace age_cat = "Missing" if age == .d | age == .n
*** I create a factor level for missing to avoid dropping values in regresison analysis. 
encode age_cat, gen(age_cat_factor)
drop age_cat
ren age_cat_factor age_cat

tab age_cat, miss
label var age_cat "Age (categorical)"

****************
*  Question 8  * 
****************

tab educ, miss

gen less_high_school = educ < 12
replace less_high_school = . if missing(educ)  // the instructions say not to include missing values 
label var less_high_school "Respondent has <12 years of schooling"



******************************
***   Data Analysis        ***
*******************************



******************************
*   Step 1: Summary Table    * 
******************************

keep pres_vote repub_v_dem male religion age_cat less_high_school year wtssal

// Make varnames more compact 
label var year "Year"
label var pres_vote "Voted in last pres. election"
label var repub_v_dem "Voted for Rep. candidate"

save Estimation.dta, replace 

// Create dummies for the table 
ds religion age_cat male 
local vars `r(varlist)'
foreach var of local vars{
	local varname "`var'"
	tab `var', gen(`var'_)
	replace `var' = -99 
	order `var'_*, after(`var')
	ds `var'_*
	foreach i of varlist `r(varlist)'{
		local oglab: var label `i'
		local oglab: subinstr local oglab "`varname'==" "", all
		label var `i' `"`oglab'"'
	}
}


// Export summary statistics 
preserve 

*** Create sample size variable 
drop wtssal 
gen N = _N 
label var N "Observations" 
drop year
outreg2 using SummaryTable, word replace dec(3) label sum(log) eqkeep(mean ssd min max)

restore 



use Estimation.dta, clear 


***************************
*   Regresion Analysis    * 
***************************

reg pres_vote i.religion [pweight= wtssal], vce(robust)
outreg2 using R1, word replace dec(3) label 

reg pres_vote i.religion i.age [pweight= wtssal], vce(robust)
outreg2 using R1, word append dec(3) label 

reg pres_vote i.religion i.age i.male [pweight= wtssal], vce(robust)
outreg2 using R1, word append dec(3) label 

reg pres_vote i.religion i.age i.male i.less_high_school [pweight= wtssal], vce(robust)
outreg2 using R1, word append dec(3) label 
//
// replace year = year-2
reg pres_vote i.religion i.age i.male i.less_high_school i.year [pweight= wtssal], vce(robust)
outreg2 using R1, word append dec(3) label 



// Voted Republican vs voted democrat
gen rep = repub_v_dem == 1
drop if pres_vote == 0

reg rep i.religion [pweight= wtssal], vce(robust)
outreg2 using R2, word replace dec(3) label 

reg rep i.religion i.age [pweight= wtssal], vce(robust)
outreg2 using R2, word append dec(3) label 

reg rep i.religion i.age i.male [pweight= wtssal], vce(robust)
outreg2 using R2, word append dec(3) label 

reg rep i.religion i.age i.male i.less_high_school [pweight= wtssal], vce(robust)
outreg2 using R2, word append dec(3) label 

// replace year = year-2
reg rep i.religion i.age i.male i.less_high_school i.year [pweight= wtssal], vce(robust)
outreg2 using R2, word append dec(3) label 



/// Diff in Diff 
gen post = year > 1979


reg rep i.less_high_school i.post less_high_school#post i.religion i.age i.male i.year [pweight= wtssal], vce(robust)
outreg2 using R3, word replace dec(3) label 


drop if year == 1972



reg rep i.less_high_school i.post less_high_school#post less_high_school#post#i.year i.religion i.age i.male i.year [pweight= wtssal], vce(robust)
outreg2 using R3, word append dec(3) label 



log close
translate Logfile_Task1.smcl Logfile_Task1.pdf, replace











