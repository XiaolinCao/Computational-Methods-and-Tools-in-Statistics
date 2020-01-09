# 80:  -------------------------------------------------------------------------
# Libraries: -------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

# Question 1
file = '/Users/caoxiaolin/Downloads/STATS506/recs2015_public_v4.csv'
recs_tib = readr::read_delim(file, delim=',')  

# replicate weights: for computing the standard errors
weights_long = 
  recs_tib %>%
  select(DOEID, BRRWT1:BRRWT96) %>%
  pivot_longer(cols = BRRWT1:BRRWT96,
               names_to = "replicate",
               values_to = "weight")

## a.
## What is the national average home temperature at night in winter,
## among homes that use space heating?

## point estimate
temp_night = 
  recs_tib %>%
  filter(HEATHOME == 1, TEMPNITE > 0) %>%
  select(DOEID, TEMPNITE, NWEIGHT) %>%
  summarise(temp_night = sum(TEMPNITE * NWEIGHT / sum(NWEIGHT)))

## replicate estimates
temp_night_rep = 
  recs_tib %>%
  filter(HEATHOME == 1, TEMPNITE > 0) %>%
  select(DOEID, TEMPNITE) %>%
  left_join(weights_long, by = "DOEID") %>%
  group_by(replicate) %>%
  summarise(temp_night = sum(TEMPNITE * weight) / sum(weight))

## !! means to refer temp_night in the global environment
temp_night = 
  temp_night_rep %>%
  mutate( estimate = !!temp_night$temp_night ) %>% 
  summarize( pe = estimate[1], 
             se = 2 * sqrt( mean( {temp_night - estimate}^2 ) ) ) %>%
  mutate( lwr = pe - qnorm(.975) * se, upr = pe + qnorm(.975) * se )

## string with an answer
str_temp_night = with(temp_night, sprintf("%4.1f (95%% CI: %4.1f-%4.1f)", 
                                          pe, lwr, upr))

## b.
# file with replicate weights
# Create a table showing the proportion of homes 
# using each level of ???main space heating fuel??? 
# within each unique combination of census division 
# and census (2010) urban type among homes that use space heating.
file2 = "/Users/caoxiaolin/Downloads/STATS506/hw2/recs_weights.csv"
weights = readr::read_delim(file2, delim = ",")

# the weighted_proportion point estimate
fuel_type_prop = recs_tib %>%
  filter(HEATHOME == 1, FUELHEAT > 0) %>%
  transmute(DOEID, division=DIVISION, urban=UATYP10, 
            fuelheat=FUELHEAT, weight=NWEIGHT) %>%
  group_by(division, urban, fuelheat) %>%
  summarise(homes = sum(weight), nr = n()) %>%
  mutate(N = sum(homes), pct = 100 * homes / N)
  # group_by(division, urban) %>%
  # mutate(pct = 100 * homes / sum(homes))

# key values for each observation
fuel_type = recs_tib %>%
  filter(HEATHOME == 1, FUELHEAT > 0) %>%
  transmute(DOEID, division=DIVISION, urban=UATYP10, 
            fuelheat=FUELHEAT, weight=NWEIGHT) %>%
  replace_na(list(weight=0)) %>%
  group_by(division, urban, fuelheat)

# convert weights to long format
weights_long = weights %>%
  gather(key="repl", value="w", BRRWT1:BRRWT96)

# join fuel_type to weights
fuel_type_rep =
  weights_long %>%
  left_join(fuel_type %>% mutate(DOEID=as.integer(DOEID)), by="DOEID")

# replicate weighted_proportion point estimate
fuel_type_prop_repl =
  fuel_type_rep %>%
  group_by(division, urban, fuelheat, repl) %>%
  summarise(home_r=sum(w)) %>%
  group_by(division, urban, repl) %>%
  mutate(pct_r = 100 * home_r / sum(home_r)) 

# join with point estimate
fuel_type_prop_repl =
  fuel_type_prop_repl %>%
  ungroup() %>%
  left_join(fuel_type_prop, by=c("division", "urban", "fuelheat"))

# compute standard errors:
fuel_type_prop = 
  fuel_type_prop_repl %>%
  group_by(division, urban, fuelheat) %>%
  summarise(pct = pct[1],
            std_err = sqrt(sum((pct_r - pct)^2) / (96 * (1 - 0.5)^2))) %>%
  mutate(lwr = pct - qnorm(0.975) * std_err,
         upr = pct + qnorm(0.975) * std_err)
# Table: ----------------------------------------------------------------------
cap = "**Proportion of Homes Using Each Level of Fuel Type Within Each Unique
Combination of Census Division and Census Urban Type"
fuel_type_prop %>%
  knitr::kable(caption = cap, align = 'c')

## c.
# plot = recs_tib %>%
#         group_by(DIVISION, UATYP10) %>%
#         filter(TEMPNITE > 0, TEMPHOME > 0, TEMPGONE > 0) %>%
#         summarise(mean_tempnite = mean(TEMPNITE), mean_tempday = mean(TEMPHOME), 
#                   mean_tempgone = mean(TEMPGONE))
# 
# par(mfrow=c(1,3))
# plot %>%
#   gather(-mean_tempnite, -mean_tempday, -mean_tempgone, -UATYP10,
#          key = "DIVISION",
#          value = "value") %>%
#   ggplot(aes(x = factor(value), y = mean_tempnite, shape = factor(UATYP10))) +
#   geom_point() +
#   facet_wrap(~ DIVISION, scales = "free") +
#   theme_bw()
# 
# plot %>%
#   gather(-mean_tempnite, -mean_tempday, -mean_tempgone, -UATYP10,
#          key = "DIVISION",
#          value = "value") %>%
#   ggplot(aes(x = factor(value), y = mean_tempday, shape = factor(UATYP10))) +
#   geom_point() +
#   facet_wrap(~ DIVISION, scales = "free") +
#   theme_bw()
# 
# plot %>%
#   gather(-mean_tempnite, -mean_tempday, -mean_tempgone, -UATYP10,
#          key = "DIVISION",
#          value = "value") %>%
#   ggplot(aes(x = factor(value), y = mean_tempgone, shape = factor(UATYP10))) +
#   geom_point() +
#   geom_errorbar(ymin = plot$mean_tempgone + sd(recs_tib$TEMPGONE), 
#                 ymax = plot$mean_tempgone + sd(recs_tib$TEMPGONE),
#                 position = "dodge") +
#   facet_wrap(~ DIVISION, scales = "free") +
#   theme_bw()
# the weighted temperature point estimate for night, day and gone
plot_temp = recs_tib %>%
  filter(TEMPNITE > 0, TEMPHOME > 0, TEMPGONE > 0) %>%
  transmute(DOEID, division=DIVISION, urban=UATYP10, weight=NWEIGHT,
            tempnite=TEMPNITE, tempday=TEMPHOME, tempgone=TEMPGONE) %>%
  group_by(division, urban) %>%
  summarise(homes=sum(weight), night=mean(tempnite), 
            day=mean(tempday), gone=mean(tempgone)) %>%
  group_by(division) %>%
  mutate(night_wei1 = night * homes / sum(homes),
         day_wei1 = day * homes / sum(homes),
         gone_wei1 = gone * homes / sum(homes))

# key values for each observation
plot = recs_tib %>%
  filter(TEMPNITE > 0, TEMPHOME > 0, TEMPGONE > 0) %>%
  transmute(DOEID, division=DIVISION, urban=UATYP10, weight=NWEIGHT,
            tempnite=TEMPNITE, tempday=TEMPHOME, tempgone=TEMPGONE) %>%
  group_by(division, urban)

# convert weights to long
weights_long = weights %>%
  gather(key="repl", value="w", BRRWT1:BRRWT96)

# join plot to weights
plot_repl = 
  weights_long %>%
  left_join(plot %>% mutate(DOEID = as.integer(DOEID)), by="DOEID")

# replicate weighted temperature for night, day and gone
plot_temp_repl = 
  plot_repl %>%
  group_by(division, urban, repl) %>%
  summarise(home_r=sum(w), night=mean(tempnite), 
            day=mean(tempday), gone=mean(tempgone)) %>%
  group_by(division, repl) %>%
  mutate(night_weir = night * home_r / sum(home_r),
         day_weir = day * home_r / sum(home_r),
         gone_weir = gone * home_r / sum(home_r))

# join point estimate with replicate
plot_temp_repl = 
  plot_temp_repl %>%
  ungroup() %>%
  left_join(plot_temp, by=c("division", "urban"))

# compute standard error
plot_temp = 
  plot_temp_repl %>%
  group_by(division, urban) %>%
  summarise(night_point = night_wei1[1],
            std_err_ni = sqrt(sum((night_weir - night_point)^2) / (96 * (1 - 0.5)^2)),
            day_point = day_wei1[1],
            std_err_day = sqrt(sum((day_weir - day_point)^2) / (96 * (1 - 0.5)^2)),
            gone_point = gone_wei1[1],
            std_err_gone = sqrt(sum((gone_weir - gone_point)^2) / (96 * (1 - 0.5)^2))) %>%
  mutate(night_lwr = night_point - qnorm(0.975) * std_err_ni,
         night_upr = night_point + qnorm(0.975) * std_err_ni,
         day_lwr = day_point - qnorm(0.975) * std_err_day,
         day_upr = day_point + qnorm(0.975) * std_err_day,
         gone_lwr = gone_point - qnorm(0.975) * std_err_gone,
         gone_upr = gone_point + qnorm(0.975) * std_err_gone) %>%
  select(-std_err_ni, -std_err_day, -std_err_gone)

# plot night temperature by division in terms of urban type
p_night = 
  plot_temp %>%
  ungroup() %>%
  filter(!is.na(urban)) %>%
  select(night_point, urban, night_lwr, night_upr, division) %>%
  gather(-night_point, -urban, -night_lwr, -night_upr,
         key = "division",
         value = "value") %>%
  arrange(desc(night_point)) %>%
  # mutate(division = factor(division)) %>%
  ggplot(aes(x = factor(value), y = night_point, shape = factor(urban))) +
  geom_point() +
  geom_errorbar(aes(ymin = night_lwr, ymax = night_upr), col="navy") +
  facet_wrap(~division, scales="free_x") +
  theme_bw()

# plot day temperature by division in terms of urban type
p_day = 
  plot_temp %>%
  ungroup() %>%
  filter(!is.na(urban)) %>%
  select(day_point, day_lwr, day_upr, urban, division) %>%
  gather(-day_point, -urban, -day_lwr, -day_upr,
         key = "division",
         value = "value") %>%
  arrange(desc(day_point)) %>%
  ggplot(aes(x = factor(value), y = day_point, shape = factor(urban))) +
  geom_point() +
  geom_errorbar(aes(ymin = day_lwr, ymax = day_upr), col="navy") +
  facet_wrap(~division, scales="free_x") +
  theme_bw()

# plot day temperature when there is no one home 
# by division in terms of urban type
p_gone = 
  plot_temp %>%
  ungroup() %>%
  filter(!is.na(urban)) %>%
  select(gone_point, gone_lwr, gone_upr, urban, division) %>%
  gather(-gone_point, -urban, -gone_lwr, -gone_upr,
         key = "division",
         value = "value") %>%
  arrange(desc(gone_point)) %>%
  ggplot(aes(x = factor(value), y = gone_point, shape = factor(urban))) +
  geom_point() +
  geom_errorbar(aes(ymin = gone_lwr, ymax = gone_upr), col="navy") +
  facet_wrap(~division, scales="free_x") +
  theme_bw()

p_night
p_day
p_gone

## d.
# the median of the differences between tempnite and temphome
diff_median = recs_tib %>%
  filter(HEATHOME == 1, TEMPHOME > 0, TEMPNITE > 0, EQUIPMUSE > 0) %>%
  transmute(DOEID, equipmuse=EQUIPMUSE,
            weight=NWEIGHT, diff=TEMPHOME - TEMPNITE) %>%
  group_by(equipmuse) %>%
  summarise(homes = sum(weight),
            diff_median = median(diff)) %>%
  mutate(diff_median_wei = diff_median * homes / sum(homes))

# key values for each observation
diff = recs_tib %>%
  filter(HEATHOME == 1, TEMPHOME > 0, TEMPNITE > 0, EQUIPMUSE > 0) %>%
  transmute(DOEID, equipmuse=EQUIPMUSE,
            weight=NWEIGHT, diff=TEMPHOME - TEMPNITE) %>%
  group_by(equipmuse)

# convert weights to long
weights_long = weights %>%
  gather(key="repl", value="w", BRRWT1:BRRWT96)

# join plot to weights
diff_repl = 
  weights_long %>%
  left_join(diff %>% mutate(DOEID = as.integer(DOEID)), by="DOEID")

# replicate median of difference
diff_median_repl = 
  diff_repl %>%
  group_by(equipmuse, repl) %>%
  summarise(home_r = sum(w),
            diff_median = median(diff)) %>%
  mutate(diff_median_r = diff_median * home_r / sum(home_r))

# join point median with replicate
diff_median_repl = 
  diff_median_repl %>%
  ungroup() %>%
  filter(!is.na(equipmuse)) %>%
  left_join(diff_median, by=c("equipmuse"))

# compute standard error
diff_median = 
  diff_median_repl %>%
  group_by(equipmuse) %>%
  summarise(diff_pct = diff_median_wei[1],
            std_err = sqrt(sum((diff_median_r - diff_pct)^2) / (96 * (1-0.5)^2))) %>%
  mutate(lwr = diff_pct - qnorm(0.975) * std_err,
         upr = diff_pct + qnorm(0.975) * std_err)

diff_median %>%
  knitr::kable(align = "c")



