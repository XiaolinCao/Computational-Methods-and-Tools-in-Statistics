# 80:  -------------------------------------------------------------------------
# Libraries: -------------------------------------------------------------------

library(mousetrap)
library(stringr)
library(dplyr)
library(tidyverse)
# a.
source("/Users/caoxiaolin/Downloads/STATS506/hw2/ps2_q2_funcs.R", local = TRUE)

# b.
data = mousetrap::KH2017_raw
head(data)
# xpos_dat = data["xpos_get_response"] # a list with length 1
# xpos_str = xpos_dat[[1]] #length = 1140
# xpos_str = str_replace_all(xpos_str, "\\[", "")
# xpos_str = str_replace_all(xpos_str, "\\]", "")
# xpos_str = str_split(xpos_str, ",")
# xpos_str = lapply(xpos_str, function(x) as.numeric(x))
# ypos_dat = data["ypos_get_response"] # a list with length 1
# typeof(ypos_dat)
# length(ypos_dat)
# ypos_str = ypos_dat[[1]] #length = 1140
# typeof(ypos_str) # integer
# length(ypos_str) # length = 1140

change_to_numeric = function(list) {
  list = list[[1]] #length = 1140
  list = str_replace_all(list, "\\[", "")
  list = str_replace_all(list, "\\]", "")
  list = str_split(list, ",")
  list = lapply(list, function(x) as.numeric(x))
  return(list)
}
xpos_str = change_to_numeric(data["xpos_get_response"])
ypos_str = change_to_numeric(data["ypos_get_response"])
timepos_str = change_to_numeric(data["timestamps_get_response"])

data_tib = as_tibble(data)
data_tib_new = data_tib %>% 
  select(subject_nr, count_trial, Condition, Exemplar, correct) %>%
  mutate(xpos_get_response = xpos_str, ypos_get_response = ypos_str,
         timestamps_get_response = timepos_str) %>%
  filter(correct == 1)

# c.
data_tib_sm = data_tib_new %>% 
                select(subject_nr, count_trial, Condition, Exemplar, correct,
                       xpos_get_response, ypos_get_response, 
                       timestamps_get_response) %>%
                group_by(subject_nr, count_trial) %>%
                do(as_tibble(
                    describe_curvature(
                      normalize_trajectory(
                        cbind(.$xpos_get_response[[1]], .$ypos_get_response[[1]],
                           .$timestamps_get_response[[1]]))
                      )
                    )
                  )

# d.
# fit model for total_dist
dist_fit = lmer(log(total_dist) ~ Condition + (1 | Exemplar) + (1 | subject_nr), data = data_tib_sm)
summary(dist_fit)

# fit model for max_devi
max_devi_fit = lmer(log(max_devi) ~ Condition + (1 | Exemplar) + (1 | subject_nr), data = data_tib_sm)
summary(max_devi_fit)

# fit model for average_devi
avg_devi_fit = lmer(log(average_devi) ~ Condition + (1 | Exemplar) + (1 | subject_nr), data = data_tib_sm)
summary(avg_devi_fit)

# fit model for AUC
auc_fit = lmer(log(AUC) ~ Condition + (1 | Exemplar) + (1 | subject_nr), data = data_tib_sm)
summary(auc_fit)

beta = c(-0.161, -0.510, -0.651, -0.390)
beta_mat = as.matrix(beta, byrow=TRUE)
rownames(beta_mat) = c("total_devi", "max_devi", "average_devi", "AUC")
colnames(beta_mat) = "Coefficient"
beta_mat


