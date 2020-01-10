#==============================================================================
# Title: Problem Set 3 Question 1
# Author: Xiaolin Cao
# UM ID: 62251943
#==============================================================================

# Question 1

## a.
### write a function that takes two numeric vector x, y and returns a CI with
### jackknife estimate for the standard error
jacknife_ci = function(x, y) {
  # input: numeric vector x and y
  # output: a CI with the jackknife estimate for the standard error
  
  # the center: theta_hat
  theta_hat = mean(x) / mean(y)
  
  # leave one out for vector x
  x_mat = matrix(x, nrow = length(x), ncol = length(x), byrow = TRUE)
  diag(x_mat) = 0
  # compute the corresponding theta_hat
  theta_x = rowMeans(x_mat) / mean(y)
  
  # leave one out for vector y
  y_mat = matrix(y, nrow = length(y), ncol = length(y), byrow = TRUE)
  diag(y_mat) = 0
  # compute the corresponding theta_hat
  theta_y = mean(x) / rowMeans(y_mat)
  
  # combine the theta's
  thetas = c(theta_x, theta_y)
  thetas_bar = mean(thetas)
  
  # compute the sigma_2_jackknife
  x_len = length(x)
  y_len = length(y)
  n = x_len + y_len
  sigma_2_jackknife = (n - 1) / n * sum({thetas - thetas_bar}^2)
  # compute the sigma_jackknife
  sigma_jackknife = sqrt(sigma_2_jackknife)
  
  lwb = theta_hat - qnorm(0.975) * sigma_jackknife
  upb = theta_hat + qnorm(0.975) * sigma_jackknife
  
  return(c(lwb, upb))
}

#==============================================================================
# test for function jacknife_ci above
nx = 50
x = rgamma(nx, shape=3, rate=1)
ny = 70
y = rnorm(ny, 0, 1)
theta_hat = mean(x) / mean(y)
n_boot_x = 1000
n_boot_y = 2000
x_mat = sample(x, nx * n_boot_x, replace = TRUE)
y_mat = sample(y, ny * n_boot_y, replace = TRUE)
dim(x_mat) = c(nx, n_boot_x)
dim(y_mat) = c(ny, n_boot_y)
x_bars = colMeans(x_mat)
y_bars = colMeans(y_mat)
thetas = x_bars / y_bars

x = c(1:6)
y = c(7:10)
 
x_mat = matrix(x, nrow = length(x), ncol = length(x), byrow = TRUE)
diag(x_mat) = 0
theta_x = rowMeans(x_mat) / mean(y)

y_mat = matrix(y, nrow = length(y), ncol = length(y), byrow = TRUE)
diag(y_mat) = 0
theta_y = mean(x) / rowMeans(y_mat)

thetas = c(theta_x, theta_y)
thetas_bar = mean(thetas)
x_len = length(x)
y_len = length(y)
n = x_len + y_len
sigma_2_jackknife = (n - 1) / n * sum({thetas - thetas_bar}^2)
sigma = sqrt(sigma_2_jackknife)

theta_hat = mean(x) / mean(y)
lwb = theta_hat - qnorm(0.975) * sigma
upb = theta_hat + qnorm(0.975) * sigma
ci = c(lwb, upb)

jacknife_ci(x, y)
#==============================================================================

## b. 
### return three different confidence intervals based on three methods
generate_cis = function(x, y, B = 1e3) {
  # input: numeric vector x and y, and B denoted as the number of bootstrap samples
  # output: three different cis
  
  # generate bootstrap samples for x and y
  x_len = length(x)
  y_len = length(y)
  x_mat = sample(x, x_len * B, replace = TRUE)
  y_mat = sample(y, y_len * B, replace = TRUE)
  dim(x_mat) = c(x_len, B)
  dim(y_mat) = c(y_len, B)
  x_bars = colMeans(x_mat)
  y_bars = colMeans(y_mat)
  theta_boots = x_bars / y_bars
  
  # the percentile method
  percentile_ci = unname(quantile(theta_boots, c(0.025, 0.975)))

  # the basic bootstrap
  theta_hat = mean(x) / mean(y)
  basic_lwb = 2 * theta_hat - percentile_ci[2]
  basic_upb = 2 * theta_hat - percentile_ci[1]
  basic_ci = c(basic_lwb, basic_upb)
  
  # the normal approximation with bootstrap standard error
  standard_lwb = theta_hat - qnorm(0.975) * sd(theta_boots)
  standard_upb = theta_hat + qnorm(0.975) * sd(theta_boots)
  standard_ci = c(standard_lwb, standard_upb)
  
  cis = list("percentile_ci" = percentile_ci, "basic_ci" = basic_ci, 
             "standard_ci" = standard_ci)
  return(cis)
}

# c.
### consider the dataset "ToothGrowth" in R and compute the cis using the above method
tg = as_tibble(ToothGrowth)
jackknife_tg = tg %>%
  group_by(dose) %>%
  do(as_tibble(
      jacknife_ci((tg %>% filter(supp == 'OJ'))$len, (tg %>%filter(supp == 'VC'))$len)
    )
  )

three_cis_tg = tg %>%
  group_by(dose) %>%
  do(as_tibble(
      generate_cis((tg %>% filter(supp == 'OJ'))$len, (tg %>% filter(supp == 'VC'))$len)
    )
  )

# to produce a nicely formatted table for jackknife CI of the ToothGrowth dataset
cn_jackknife = c("dose", "CI(lower, upper)")
cap1 = 
  '**Table 1.** *Jackknife CI for Different Dosage*'
knitr::kable(jackknife_tg, digits = 3, col.names = cn_jackknife, caption = cap1)

# to produce a nicely formatted table for 3 bootstrap CI of the ToothGrowth dataset
cn_boot = c("dose", "percentile_ci", "basic_ci", "standard_ci")
cap2 = 
  '**Table 2.** *Three Bootstrap CIs for Different Dosage*'
knitr::kable(three_cis_tg, digits = 3, col.names = cn_boot, caption = cap2)
