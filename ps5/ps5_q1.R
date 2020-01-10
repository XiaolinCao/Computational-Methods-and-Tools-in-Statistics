#==============================================================================
# Title: Problem Set 5 Question 1
# Author: Xiaolin Cao
# UM ID: 62251943
# The script uses RECS 2015 data to answer which census division has the largest
# disparity between urban and rural areas in terms of the proportion of homes
# with internet access.
#==============================================================================

# library setup
library(dplyr)
library(tidyverse)
library(data.table)

# Question 1
file1 = '/Users/caoxiaolin/Downloads/STATS506/recs2015_public_v4.csv'
recs = fread(file1)
file2 = '/Users/caoxiaolin/Downloads/STATS506/ps5/recs_weights.csv'
replicate = fread(file2)

## select relevant variables
recs_core = recs[, .(DOEID, DIVISION, INTERNET, NWEIGHT, UATYP10 = ifelse(UATYP10 == "C", "U", UATYP10))]
recs_core = recs_core[replicate, on = "DOEID"]

# reshape the data to put replicate weight in long format
recs_core = melt(recs_core, id.vars = c("DOEID", "DIVISION", "INTERNET", "NWEIGHT", "UATYP10"))

# calculate point weighted mean
recs_point = recs_core[, `:=`(ninternet = INTERNET * NWEIGHT)] %>%
                    .[, lapply(.SD, sum), by = .(DIVISION, UATYP10), .SDcols = c("ninternet", "NWEIGHT")] %>%
                    .[, .(DIVISION, UATYP10, p_int = ninternet / NWEIGHT)]

# reshape wide for point difference
recs_point = dcast(recs_point, DIVISION ~ UATYP10, value.var = "p_int")
recs_point = recs_point[, .(DIVISION, p_diff = U - R)]

# calculate replicate weighted mean
recs_replicate = recs_core[, `:=`(rep_internet = INTERNET * value)] %>%
                         .[, lapply(.SD, sum), by = .(DIVISION, UATYP10, variable),
                           .SDcols = c("rep_internet", "value")] %>%
                         .[, .(DIVISION, UATYP10, variable, rep_p_int = rep_internet / value)]

# reshape to wide to calculate the replicate differences
recs_replicate = dcast(recs_replicate, DIVISION + variable ~ UATYP10, value.var = "rep_p_int")
recs_replicate = recs_replicate[, .(DIVISION, variable, rep_diff = U - R)]

# merge the point difference and replicate difference
recs_diff = recs_point[recs_replicate, on = "DIVISION"]
recs_diff_avg = recs_diff[, lapply(.SD, mean), by = .(DIVISION), .SDcols = "p_diff"]
recs_diff_sd = recs_diff[, `:=`(var = (p_diff - rep_diff)^2 / (96 * 0.25))] %>%
                    .[, lapply(.SD, sum), by = .(DIVISION), .SDcols = "var"] %>%
                    .[, lapply(.SD, sqrt), by = .(DIVISION), .SDcols = "var"]
recs_diff = recs_diff_avg[recs_diff_sd, on = "DIVISION"] %>%
                        .[, `:=`(lwr = p_diff - 1.96 * var, upr = p_diff + 1.96 * var)] %>%
                        .[, `:=`(sd = var)] %>%
                        .[, -c("var")]

# change the number_based division to name_based version
decode_division = function(x) {
  # input: a numeric number denoting division
  # output: the corresponding division
  
  stopifnot(is.numeric(x))
  switch(x,
         "New England", "Middle Atlantic", "East North Central", "West North Central",
         "South Atlantic", "East South Central", "West South Central", "Mountain North",
         "Mountain South", "Pacific")
}

decode_all_division = function(x) {
  # input: a vector of interger-based division
  # output: a vector of name-based division
  sapply(x, decode_division)
}

recs_diff[, `:=`(DIVISION = decode_all_division(DIVISION))]

knitr::kable(recs_diff, caption = "Internet Disparity Between Urban and Rural 
             Areas for Each Division")

# DIVISION Mountain South have the largest disparity 
# between urban and rural internet usage.
