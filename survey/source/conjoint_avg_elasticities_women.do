* Choice-based method of conjoint analysis
* Rank-ordered logistic regression model by maximum likelihood
* Without outside option
* All covariates as continuous
* This script is only for female respondents

* Set graph theme
set scheme plottigblind

* Load data
use data/conjoint_tables_long, clear
tostring tab, replace force

merge m:1 uuid using data/survey_data.dta, keepusing(Q2 Q4)
assert _m == 3
drop _m

merge m:1 uuid cj tab using data/conjoint_reflection_long.dta
assert _m == 3
drop _m

clonevar enroll_bin = enroll
recode enroll_bin (2 = 0) (3 = 0)
clonevar earnmore_bin = earnmore
recode earnmore_bin (2 = 0) (3 = 0)
 
* Calculate reference earnings based on survey instructions
gen earn_ref_amount = .
replace earn_ref_amount = Q2/Q4 if earn_ref == 1 // "Week"
replace earn_ref_amount = (52/12)*Q2/Q4 if earn_ref == 2 // "Month"
replace earn_ref_amount = (52)*Q2/Q4 if earn_ref == 3 // "Year"
replace earn_ref_amount = round(earn_ref_amount, 10) if earn_ref == 1
replace earn_ref_amount = round(earn_ref_amount, 100) if earn_ref == 2
replace earn_ref_amount = round(earn_ref_amount, 1000) if earn_ref == 3

* Post-training earnings as proportion/percentage of reference 
gen earn_posttrain_prop = round(earn_posttrain_int / earn_ref_amount, 0.05)

* "counterfactual" earnings as proportion/percentage of reference 
gen earn_notrain_prop = round(earn_notrain_int / earn_ref_amount, 0.05)

rename selected topchoice

keep uuid cj tab prof rank topchoice enroll enroll_bin earnmore earnmore_bin ///
		cost duration traveltime completionrate ///
		earn_posttrain_int earn_posttrain_prop  ///
		earn_notrain_int earn_notrain_prop ///
		earn_increase unemployment Q2 Q4 earn_ref earn_ref_amount
		
order uuid cj tab prof rank topchoice enroll enroll_bin earnmore earnmore_bin ///
		cost duration traveltime completionrate ///
		earn_posttrain_int earn_posttrain_prop ///
		earn_notrain_int earn_notrain_prop ///
		earn_increase unemployment Q2 Q4 earn_ref earn_ref_amount

reshape wide rank topchoice ///
		cost duration traveltime completionrate ///
		earn_posttrain_int earn_posttrain_prop ///
		earn_notrain_int earn_notrain_prop ///
		earn_increase unemployment, i(uuid cj tab) j(prof) string


* Set reference levels for outside option
gen rankc0 = .
replace rankc0 = 0 if enroll == 1
replace rankc0 = 1 if enroll == 3
gen topchoicec0 = .
gen costc0 = 1
gen durationc0 = 0
label define duration 0 "0 weeks", modify
gen traveltimec0 = 0
label define traveltime 0 "0 minutes", modify

gen completionratec0 = 7

gen earn_posttrain_intc0 = earn_ref_amount if cj == 3 | cj == 4 | cj == 7
gen earn_posttrain_propc0 = 1 if cj == 3 | cj == 4 | cj == 7
gen earn_notrain_intc0 = earn_ref_amount if cj == 4
gen earn_notrain_propc0 = 1 if cj == 4
gen earn_increasec0 = 2 if cj == 5


* Set unemployment rate for outside option. currently using 10%
gen unemploymentc0 = 3

reshape long

la def completionrate 7 "100%", modify

order enroll enroll_bin earnmore earnmore_bin, after(topchoice)

* Create numeric variables of all attributes
decode cost, gen(cost_str)
gen cost_int = subinstr(subinstr(cost_str, "$", "", .), ",", "", .)
destring cost_int, replace

decode duration, gen(duration_str)
gen duration_int = subinstr(duration_str, " weeks", "", .)
destring duration_int, replace

decode traveltime, gen(traveltime_str)
gen traveltime_int = subinstr(traveltime_str, " minutes", "", .)
destring traveltime_int, replace

decode completionrate, gen(completionrate_str)
gen completionrate_int = subinstr(completionrate_str, "%", "", .)
destring completionrate_int, replace

rename earn_posttrain_prop earn_posttrain_prop_int
tostring earn_posttrain_prop_int, gen(earn_posttrain_prop_str) force
encode earn_posttrain_prop_str, gen(earn_posttrain_prop)

rename earn_notrain_prop earn_notrain_prop_int
tostring earn_notrain_prop_int, gen(earn_notrain_prop_str) force
encode earn_notrain_prop_str, gen(earn_notrain_prop)

decode earn_increase, gen(earn_increase_str)
gen earn_increase_int = subinstr(subinstr(earn_increase_str, "% less", "", .), "% more", "", .)
destring earn_increase_int, replace

decode unemployment, gen(unemployment_str)
gen unemployment_int = subinstr(unemployment_str, "%", "", .)
destring unemployment_int, replace

drop *_str

gen training = (prof != "c0")

#delimit ;
merge m:1 uuid using data/survey_data.dta, 
	keepusing(uuid S1 dAge region4
	S4 S5* S6 S7 S8 S9 S10 S11
	S14c1 S14ac2 S15 S16 S17 Q3);

#delimit cr
assert _m == 3
drop _m

tab cj if prof != "c0", m

* Conjoint 6 was dropped after piloting, recode 7 as 6
recode cj (7 = 6)

* Race indicator
gen race = 4
replace race = 1 if S4 == 1 & S5r1 == 1 // White non-hispanic
replace race = 2 if S4 == 2 // hispanic
replace race = 3 if S4 == 1 & S5r2 == 1 // Black non-hispanic
la def race 1 "White, non-hispanic" 2 "Hispanic" 3 "Black, non-hispanic" 4 "Other"
la val race race

* Unemployment
gen unemp = (S8 >= 3 & S8 <=5)

* Education
gen educ = .
replace educ = 1 if S6 <= 3
replace educ = 2 if S6 > 3 & S6 <= 5
replace educ = 3 if S6 > 5 & S6 <= 7
la def educ 1 "HS or less" 2 "Some college or Assoc." 3 "Bachelors or more"
la val educ educ

* Industry simplified
gen industry = 0
replace industry = 1 if S16 == 19
replace industry = 2 if S16 == 12
replace industry = 3 if S16 == 11
replace industry = 4 if S16 == 4
replace industry = 5 if S16 == 13
replace industry = 6 if S16 == 16
replace industry = 7 if S16 == 10
replace industry = 8 if S16 == 3
la def industry 0 "Other" 1 "Retail" 2 "Health care" 3 "Education" ///
		4 "Manufacturing" 5 "Hospitality" 6 "Self-employed" ///
		7 "Prof. Services" 8 "Construction"
la val industry industry

drop if prof == "c0" // drop the outside option
encode prof, gen(prfl) // create a factor variable for the profile order
encode uuid, gen(id) // create factor variable for conjoint model respondent identifier

* Trim sample to post-training and no-training levels that are within the expected bounds
keep if mi(earn_posttrain_prop_int) | (earn_posttrain_prop_int > .94 & earn_posttrain_prop_int < 1.31)
keep if mi(earn_notrain_prop_int) | (earn_notrain_prop_int > .94 & earn_notrain_prop_int < 1.31)

*************************************
* Local macros
*************************************
* Set the outcome variable
local outvar = "rank"
* Set covariates for conjoint sets
local cov1 = "cost_int duration_int traveltime_int"
local cov2 = "cost_int duration_int traveltime_int completionrate_int"
local cov3 = "cost_int duration_int traveltime_int completionrate_int earn_posttrain_prop_int"
local cov4 = "cost_int duration_int traveltime_int earn_posttrain_prop_int earn_notrain_prop_int"
local cov5 = "cost_int duration_int traveltime_int earn_increase_int"
local cov6 = "cost_int duration_int traveltime_int earn_posttrain_prop_int unemployment_int"

* Get levels of conjoint attributes
levelsof cost_int, local(costlevs)
levelsof duration_int, local(durationlevs)
levelsof traveltime_int, local(travellevs)
levelsof completionrate_int, local(completionlevs)
levelsof earn_posttrain_prop_int, local(posttrainlevs)
levelsof earn_notrain_prop_int, local(notrainlevs)
levelsof earn_increase_int, local(earninclevs)
levelsof unemployment_int, local(unemplevs)

destring tab, replace

* only women
keep if S1 == 1

*************************************
* Conjoint 1
*************************************
preserve
keep if cj == 1

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov1' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov1') post
estimates store avg_elast1
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar prof2 = `prof2'
estadd scalar prof3 = `prof3'
estadd scalar ymean = r(mean)

restore

*************************************
* Conjoint 2
*************************************
preserve
keep if cj == 2

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov2' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov2') post
estimates store avg_elast2
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar ymean = r(mean)

restore


*************************************
* Conjoint 3
*************************************
preserve
keep if cj == 3

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov3' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov3') post
estimates store avg_elast3
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar ymean = r(mean)

restore

*************************************
* Conjoint 4
*************************************
preserve
keep if cj == 4

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov4' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov4') post
estimates store avg_elast4
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar ymean = r(mean)

restore


*************************************
* Conjoint 5
*************************************
preserve
keep if cj == 5

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov5' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov5') post
estimates store avg_elast5
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar ymean = r(mean)

restore


*************************************
* Conjoint 6
*************************************
preserve
keep if cj == 6

quietly cmset id tab prof

* Choice model
quietly cmrologit `outvar' `cov6' i.prfl, reverse vce(cluster id) ties(efron)
local Nclust = e(N_clust)
local prof2 = _b[2.prfl]
local prof3 = _b[3.prfl]

**** Average elasticities
quietly margins, dyex(`cov6') post
estimates store avg_elast6
summarize enroll_bin if rank == 1
estadd scalar clust = `Nclust'
estadd scalar ymean = r(mean)

restore

esttab avg_elast1 avg_elast2 avg_elast3 avg_elast4 avg_elast5 avg_elast6 ///
	using output/avg_elasticities_women.tex, se ///
	varlabels(cost_int "Cost ($)" duration_int "Duration (Weeks)" ///
	traveltime_int "Travel Time (Minutes)") ///
	stats(N clust prof2 prof3 ymean,  ///
	labels("Choice Options" "Respondents" "Profile 2 F.E." "Profile 3 F.E." "Enroll?")) ///
	mtitles("Conjoint 1" "Conjoint 2" "Conjoint 3" "Conjoint 4" "Conjoint 5" "Conjoint 6") ///
	title(Average ranking elasticity of conjoint attributes\label{avg_elast}]) /// 
	addnotes("Partial elasticities estimated controlling for profile order in conjoint experiment.") ///
	replace booktabs

