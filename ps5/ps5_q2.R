#==============================================================================
# Title: Problem Set 5 Question 2
# Author: Xiaolin Cao
# UM ID: 62251943
# The script uses data.table to clean and analyze the DNA methylation 
#==============================================================================

# library setup
library(data.table)
library(dplyr)
library(parallel)
library(future)

# a.

# <GSE138311_series_matrix.txt head -n 100
# grep -n -E "\!series_matrix" GSE138311_series_matrix.txt
# 68:!series_matrix_table_begin
# 786090:!series_matrix_table_end

# Answer: There are 68 lines of header information.


# b.
series = fread('/Users/caoxiaolin/Downloads/STATS506/ps5/GSE138311_series_matrix.txt', skip = 68)

# filter the data with ID_REF starting with "ch" and discard all NA rows
series = series[grep("ch", ID_REF)]
series = series[, -c("GSM4105199"), with=FALSE]

# reshape the data to long format so that each row is a single probe-sample pair
series_long = melt(series, id.vars = "ID_REF",
                   variable.name = "sample",
                   value.name = "value")


# c. 
# add a column sample_group to denote Crohn's disease and non-Crohn's disease
non_Crohn_sample = c("GSM4105194", "GSM4105195", "GSM4105196", "GSM4105197",
                     "GSM4105198")
series_long = series_long[, sample_group := "Crohn"] 
series_long = series_long[sample == 'GSM4105194' | sample == 'GSM4105195' |
                          sample == 'GSM4105196' | sample == 'GSM4105197' |
                          sample == 'GSM4105198', sample_group := "non_Crohn"]
series_long_ref = series_long

# d.
# create a new data.table by computing a t-statistic comparing the difference in means
# between groups for each unique probe
t_series = series_long[order(ID_REF), 
                       c(lapply(.SD, mean),
                         lapply(.SD, sd)), by=.(ID_REF, sample_group), .SDcols = "value"]
new_names = c(key(t_series), 
              paste("ID_REF"),
              paste("sample_group"),
              paste("mean"),
              paste("sd"))
setnames(t_series, new_names)
t_series_wide = dcast(t_series, ID_REF ~ sample_group, value.var = c("mean", "sd"))
t_series_wide = t_series_wide[, `:=`(diff_mean = mean_Crohn - mean_non_Crohn)] %>%
                            .[, `:=`(sp = sqrt( (6 * sd_Crohn^2 + 4 * sd_non_Crohn^2) / 10 ))] %>%
                            .[, `:=`(t_stat = diff_mean / (sp * sqrt(1/7 + 1/5)))]


# e.
# add a column probe_group by reference, assigning probes to groups 
# using the first 5 digits of the probe ID

# reference
t_series_wide2 = t_series_wide

# get the first 5 digits of the probe ID
subname = substr(t_series_wide2[, .(ID_REF)][[1]], 1, 5)
t_series_wide2 = t_series_wide2[, `:=`(probe_group = subname)]


# f.
# get the proportion of probes that are significant in each group

# t critical value for two-tailed t-test with alpha = 0.05 and df = 10
t_critical = qt(0.975, df = 10, lower.tail = TRUE)
t_sig_prop = t_series_wide2[, .(sig_prop = sum(abs(t_stat) >= t_critical) / .N), by=probe_group]
t_df = as.data.frame(t_sig_prop)
figure = ggplot(t_df, aes(x = probe_group, y = sig_prop)) +
  geom_point()
plot(figure)

# Answer: By looking at the plot, probe group = ch.12, ch.14 and ch.8 stands out as potentially
#         over-represented.



# g.
permutation_test = function(dt, type, flag=TRUE) {
  # input: a data.table
  # output: a data.table with t statistics,
  #         if permute flag is true, then permute the data.table
  #         and compute the three statistics (T_abs, T_upr, T_down)
  stopifnot(is.data.table(dt));
  
  # permute if flag == TRUE
  if (flag == TRUE) {
      dt_perm = dt[, sample_group:= sample_group[sample(1:nrow(.SD), replace = FALSE)], by = ID_REF]
      dt = dt_perm
  }
  
  # compute t-statistics
  t_stats = dt[, c(lapply(.SD, mean), lapply(.SD, sd)), by=.(ID_REF, sample_group), .SDcols="value"]
  new_names = c(key(t_stats),
                paste("ID_REF"),
                paste("sample_group"),
                paste("mean"),
                paste("sd"))
  setnames(t_stats, new_names)
  t_stats_wide = dcast(t_stats, ID_REF ~ sample_group, value.var = c("mean", "sd"))
  t_stats_wide = t_stats_wide[, `:=`(diff_mean = mean_Crohn - mean_non_Crohn)] %>%
    .[, `:=`(sp = sqrt( (6 * sd_Crohn^2 + 4 * sd_non_Crohn^2) / 10 ))] %>%
    .[, `:=`(t_val = diff_mean / sp * sqrt(1/7 + 1/5))]
  
  # get the first 5 digits of the probe ID
  subname = substr(t_stats_wide[, .(ID_REF)][[1]], 1, 5)
  t_stats_wide = t_stats_wide[, `:=`(probe_group = subname)]
  
      
      ## i. two-tailed
      t_crit_abs = qt(0.975, df = 10, lower.tail = TRUE)
      ## ii. greater
      t_crit_upr = qt(0.95, df = 10, lower.tail = TRUE)
      ## iii. lesser
      t_crit_lwr = qt(0.05, df = 10, lower.tail = TRUE)
      
      if (type == "two_tailed") {
        t_stats_wide = t_stats_wide[, .(T_abs = sum(abs(t_val) * 1 * (abs(t_val) > t_crit_abs)) / .N), 
                                    by = probe_group]  
      }
      if (type == "greater") {
        t_stats_wide = t_stats_wide[, .(T_up = sum(t_val * 1 * (t_val > t_crit_upr)) / .N), 
                                    by = probe_group]
      }
      if (type == "lesser") {
        t_stats_wide = t_stats_wide[, .(T_down = sum(t_val * 1 * (t_val < t_crit_lwr)) / .N),
                                    by = probe_group]
      }
      # t_stats_wide = t_stats_wide[, .(T_abs = sum(abs(t_val) * 1 * (abs(t_val) > t_crit_abs)) / .N,
      #                          T_up = sum(t_val * 1 * (t_val > t_crit_upr)) / .N,
      #                          T_down = sum(t_val * 1 * (t_val < t_crit_lwr)) / .N),
      #                           by=probe_group]
  
      return(t_stats_wide)
}




# h. 

## compute T_abs score for each probe group on the original data
series_original = copy(series_long)
T_abs_original = permutation_test(series_original, type = "two_tailed", flag = FALSE)
T_abs_original

## permute 1000 times to calculate permuted T_abs
nperm = 1e3
start = proc.time()
test = 0
for (i in 1:nperm) {
  T_abs = permutation_test(series_original, type = "two_tailed", flag = TRUE)
  test = test + 1 * (abs(T_abs_original[, .(T_abs)]) < abs(T_abs[, .(T_abs)]))
}
end = proc.time() - start
end
res_abs = T_abs_original[, .(probe_group, 
                             p_value = (test + 1) / (nperm + 1))]
res_abs

## i. use mclapply to permute T_up
series_original = copy(series_long)
T_up_original = permutation_test(series_original, type = "greater", flag = FALSE)
nperm = 1e3
start = proc.time()
test= 0
T_up = mclapply(1:nperm, permutation_test, dt=series_original, type="greater")
for (i in 1:nperm) {
  T_up_i = T_up[[i]]
  test = test + 1 * (abs(T_up_original[, .(T_up)]) < abs(T_up_i[, .(T_up)]))
}
end = proc.time() - start
end
res_up = T_up_original[, .(probe_group,
                           p_value = (test + 1) / (nperm + 1))]
res_up

## ii. use futures to permute T_down
series_original = copy(series_long)
T_down_original = permutation_test(series_original, type = "lesser", flag = FALSE)
start = proc.time()
plan(multisession)
nperm = 1e3
T_down %<-% {
  lapply(1:nperm, permutation_test, dt = series_original, type = "lesser")  
}
end = proc.time() - start
end
test = 0
for (i in 1:nperm) {
  T_down_i = T_down[[i]]
  test = test + 1 * (abs(T_down_original[, .(T_down)]) < abs(T_down_i[, .(T_down)]))
}
res_down = T_down_original[, .(probe_group,
                               p_value = (test + 1) / (nperm + 1))]
res_down













