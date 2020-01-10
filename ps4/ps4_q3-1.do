*----------------------------------------------------------------------------*
* Title: Problem Set 4 Question 3
* This script tries to answer one question: Are People in the US more likely to 
* drink water on a weekday than a weekend day?

* Author: Xiaolin Cao
* UM ID: 62251943
*----------------------------------------------------------------------------*

* Script Setup:---------------------------------------------------------------
version 16.0

* (a) Import and merge--------------------------------------------------------

// Import and save demographic data
fdause C:\Users\xiaolc\Downloads\DEMO_D.XPT, clear
quietly compress
gsort +seqn
save DEMO_D.dta, replace
 
// Import and save day1 data
fdause C:\Users\xiaolc\Downloads\DR1TOT_D.XPT, clear
quietly compress
gsort +seqn
generate day = 1
merge 1:1 seqn using C:\Users\xiaolc\Documents\DEMO_D.dta
save DR1TOT_D.dta, replace

// Import and save day2 data
fdause C:\Users\xiaolc\Downloads\DR2TOT_D.XPT, clear
quietly compress
gsort +seqn
generate day = 2
merge 1:1 seqn using C:\Users\xiaolc\Documents\DEMO_D.dta
append using C:\Users\xiaolc\Documents\DR1TOT_D.dta, generate(survey)
save DRTOT_D.dta, replace

// reduce to matched data
keep if _merge == 3
save DRTOT_DEMO_merge.dta, replace

// reduce to variables of interest
keep seqn day dr1day dr2day dr1_320z dr2_320z riagendr ridageyr indfmpir ridexmon

// clean up the variable names and generate needed variables
rename seqn id
rename riagendr gender
rename ridageyr age
rename indfmpir pir
rename ridexmon winter

// generate weekday indicator
// 1: weekday, 0: weekend
generate weekday = 1
replace weekday = 0 if (dr1day == 1) | (dr1day == 7) | (dr2day == 1) | (dr2day == 7)

// generate whether drink water yesterday indicator
// 1: drink water yesterday, 0: did not drink water yesterday
generate preday = 1 
replace preday = 0 if (dr1_320z == 0 | dr1_320z == .) & (dr2_320z == 0 | dr2_320z == .)

* (b) further cleaning up the data---------------------------------------------

// drop missing values
	//drop if day_in_week == .
	//drop if dr1_320z == .
	//drop if gender == .
	//drop if age == .
	//drop if pir == .
	//drop if season == .
// 0: no missing values, 1: missing values
generate missing = 0
replace missing = 1 if (id == . | (dr1day == . & dr2day == .) | (dr1_320z == . &dr2_320z == .) | gender == . | age == . | pir == . | winter == .)
drop if missing == 1

// center age and pir
summarize age, detail
return list
generate cage = round(age - r(mean), 1)

summarize pir, detail
return list
generate cpir = round(pir - r(mean), 0.01)

// mean of age: 27.89
// mean of pir: 2.36

// change the unit of centered age to decades
replace cage = cage / 10

* (c)--------------------------------------------------------------------------
preserve
// use only day 1 interviews
keep if day == 1

// outcome variable: yda (0: didn't drink; 1: drank)
// logistic regression against yda
logit preday i.weekday i.winter cage c.cage#c.cage i.gender cpir, or

// put the coefficient and standard error in excel
putexcel set ps4_q3_c.xls, replace

putexcel A1="preday"
putexcel A2="1.weekday"
putexcel A3="2.winter"
putexcel A4="cage"
putexcel A5="cage^2"
putexcel A6="2.gender"
putexcel A7="cpir"

putexcel B1="Odds Ratio"
putexcel B2=1.130136 
putexcel B3=1.109808 
putexcel B4=1.158033 
putexcel B5=0.9737323 
putexcel B6=1.33266
putexcel B7=1.093849

putexcel C1="lwr"
putexcel C2=1.027747
putexcel C3=1.009919 
putexcel C4=1.128201 
putexcel C5=.9648531 
putexcel C6=1.213307 
putexcel C7=1.060388

putexcel D1="upr"
putexcel D2=1.242726
putexcel D3=1.219577
putexcel D4=1.188654
putexcel D5=.9826932
putexcel D6=1.463753
putexcel D7=1.128366

// compute the average marginal effect for each of the variables
margins, dydx(i.weekday i.winter cage i.gender cpir)

putexcel E1="marginal effect"
putexcel E2=.024121 
putexcel E3=.0204689 
putexcel E4=.0316543 
putexcel E6=.0564991 
putexcel E7=.0176006 

putexcel F1="lwr_marginal_effect"
putexcel F2=.0053218 
putexcel F3=.0019269
putexcel F4=.0256843 
putexcel F6=.0380799 
putexcel F7=.0115375 

putexcel G1="upr_marginal_effect"
putexcel G2=.0429203
putexcel G3=.0390109
putexcel G4=.0376244
putexcel G6=.0749184
putexcel G7=.0236637

putexcel save

restore

* (d)--------------------------------------------------------------------------
// use both interview data from day1 and day2 to fit the logistic regression
meglm preday i.weekday i.winter cage c.cage#c.cage i.gender cpir || id:, family(bernoulli) link(logit) or

//estimates store weekday 
//estimates store winter 
//estimates store cage 
//estimates store gender 
//estimates store cpir 
//estimates store id

// put values into excel
putexcel set ps4_q3_d.xls, replace

putexcel A1="preday"
putexcel A2="1.weekday"
putexcel A3="2.winter"
putexcel A4="cage"
putexcel A5="cage^2"
putexcel A6="2.gender"
putexcel A7="cpir"

putexcel B1="Odds Ratio"
putexcel B2=1.239979 
putexcel B3=1.077818
putexcel B4=1.362175 
putexcel B5=.9550781 
putexcel B6=1.757915
putexcel B7=1.179018

putexcel C1="lwr"
putexcel C2=1.10902
putexcel C3=.9369008
putexcel C4=1.308046
putexcel C5=.9420468 
putexcel C6=1.527092 
putexcel C7=1.125907 

putexcel D1="upr"
putexcel D2=1.386404
putexcel D3=1.23993
putexcel D4=1.418544
putexcel D5=.9682896
putexcel D6=2.023627
putexcel D7=1.234633

// compute the average marginal effect for each variable
margins, dydx(i.weekday i.winter cage i.gender cpir)

putexcel E1="marginal effect"
putexcel E2=.0232385 
putexcel E3=.0080304 
putexcel E4=.0359009
putexcel E6=.0605507
putexcel E7=.0176414 

putexcel F1="lwr_marginal_effect"
putexcel F2=.0111058 
putexcel F3=-.0069865
putexcel F4=.0309386
putexcel F6=.0455947  
putexcel F7=.0127545 

putexcel G1="upr_marginal_effect"
putexcel G2=.0353713
putexcel G3=.0230472
putexcel G4=.0408632
putexcel G6=.0755066
putexcel G7=.0225282

putexcel save

//estimates restore weekday 
//estimates restore winter 
//estimates restore cage 
//estimates restore gender 
//estimates restore cpir 
//estimates restore id
















