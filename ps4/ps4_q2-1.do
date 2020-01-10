*------------------------------------------------------------------------------*
* STATS 506 Problem Set 4 Question 2
* This script computes which census division has the largest disparity
* between urban and rural areas in terms of proportions of homes with internet 
* access.

* Author: Xiaolin Cao (UM ID: 62251943)
*------------------------------------------------------------------------------*

* Script Setup: ----------------------------------------------------------------
version 16.0
pwd	

* Data Prep: -----------------------------------------------------------------
import delimited recs2015_public_v4.csv, clear

// reduce variable size
keep doeid division uatyp10 internet nweight brrwt*

// combine urban area and urban cluster 
replace uatyp10 = "U" if uatyp10 == "C"
generate uatype = uatyp10

// make replicate weights from wide to long
reshape long brrwt, i(doeid) j(replicate)

// calculate weighted means
generate nwt = internet * nweight
generate rwt = internet * brrwt
collapse (sum) nwt rwt (sum) nweight brrwt, by(replicate uatype division)
generate nwt_prop = nwt / nweight
generate rwt_prop = rwt / brrwt

// keep useful variables
keep division replicate uatype rwt_prop nwt_prop

// reshape the data from long to wide using different urban types
reshape wide nwt_prop rwt_prop, i(replicate division) j(uatype) string

// calculate the difference between different urban types
generate diff = nwt_propU - nwt_propR
generate diff_rep = rwt_propU - rwt_propR
generate var = (diff - diff_rep) ^2 / (96 * 0.25)

// calculate variance and standard deviation
collapse (sum) var (mean) diff, by(division)
generate sd = sqrt(var)

// calculate CIs
generate lwr = diff - 1.96 * sd
generate upr = diff + 1.96 * sd


// keep relavent variables
keep division diff lwr upr

// export the data to excel
export delimited ps4_q2.csv, replace
