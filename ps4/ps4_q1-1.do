*------------------------------------------------------------------------------*
* STATS 506 Problem Set 4 Question 1
* This script repeat question 2 part d from problem set 2.
* The dataset it uses is the output of question 2 part c.

* Author: Xiaolin Cao (UM ID: 62251943)
*------------------------------------------------------------------------------*

* Script Setup: ----------------------------------------------------------------
version 16.0	
pwd

* Data Prep: -------------------------------------------------------------------
import delimited df_q1.csv, clear

// compute differences by Condition for each curvature measure
// log transform each response variable
generate tot_dist2 = log(tot_dist)
generate max_abs_dev2 = log(max_abs_dev)
generate avg_abs_dev2 = log(avg_abs_dev)
generate auc2 = log(max(1, auc))

// encode condition
encode condition, generate(condition1)

// Compute differences by Condition for each curvature measure
mixed tot_dist2 i.condition1 || _all:R.subject || exemplar:
matrix model1 = r(table)

// maximum absolute deviation
mixed max_abs_dev2 i.condition1 || _all:R.subject || exemplar:
matrix model2 = r(table)

// average absolute deviation
mixed avg_abs_dev2 i.condition1 || _all:R.subject || exemplar:
matrix model3 = r(table)

// area under the curve
mixed auc2 i.condition1 || _all:R.subject || exemplar:
matrix model4 = r(table)

// export the coefficients and CIs
mata
model1 = st_matrix("model1")
model2 = st_matrix("model2")
model3 = st_matrix("model3")
model4 = st_matrix("model4")
model1 = round(model1[(1, 5, 6), 2], 0.001)
model2 = round(model2[(1, 5, 6), 2], 0.001)
model3 = round(model3[(1, 5, 6), 2], 0.001)
model4 = round(model4[(1, 5, 6), 2], 0.001)
model1 = model1'
model2 = model2'
model3 = model3'
model4 = model4'
st_matrix("log_model1", model1)
st_matrix("log_model2", model2)
st_matrix("log_model3", model3)
st_matrix("log_model4", model4)
end

putexcel set ps4_q1.csv, replace
putexcel A2 = "tot_dist"
putexcel A3 = "max_abs_dev"
putexcel A4 = "avg_abs_dev"
putexcel A5 = "auc"
putexcel B1 = "est_log"
putexcel C1 = "lwr"
putexcel D1 = "upr"
putexcel B2 = matrix(log_model1)
putexcel B3 = matrix(log_model2)
putexcel B4 = matrix(log_model3)
putexcel B5 = matrix(log_model4)

putexcel save


