#==============================================================================
# Title: Problem Set 3 Question 2
# Author: Xiaolin Cao
# UM ID: 62251943
#==============================================================================

# Question 2

## a.
jackknife_ci_matrix = function(x, y) {
  # input: matrices x and y representing the monte carlo replicates from q1
  # output: the cis based on the jackknife standard error estimate
  
  # each row should represent one monte carlo replicate
  # each row is a dataset
  
  # repeat each rows for n_x and n_y times respectively
  x_rows = rep(1:nrow(x), each = dim(x)[2])
  x_repeat = x[x_rows,]
  y_rows = rep(1:nrow(y), each = dim(y)[2])
  y_repeat = y[y_rows,]
  
  # compute x_bars and y_bars with one number left out
  # use the equation: x_bar_i = (n * x_bar - x_i) / (n - 1)
  x_bars = (dim(x)[2] * rowMeans(x_repeat) - as.vector(t(x))) / (dim(x)[2] - 1)
  y_bars = (dim(y)[2] * rowMeans(y_repeat) - as.vector(t(y))) / (dim(y)[2] - 1)
  
  # convert these means into matrix
  x_bars_mat = matrix(x_bars, ncol = dim(x)[2], byrow = TRUE)
  y_bars_mat = matrix(y_bars, ncol = dim(y)[2], byrow = TRUE)
  
  # make a theta matrix with col length = n_x + n_y, row length = mcrep
  theta_hat_i_mat = cbind(x_bars_mat / rowMeans(y), rowMeans(x) / y_bars_mat)
  theta_hat_i_centered = (theta_hat_i_mat - rowMeans(theta_hat_i_mat))^2
  n = dim(x)[2] + dim(y)[2]
  sigma2_jack = rowSums(theta_hat_i_centered) * ((n - 1) / n)
  sigma_jack = sqrt(sigma2_jack)
  
  # the point estimate
  theta_hat = rowMeans(x) / rowMeans(y)
  lwbs = theta_hat - qnorm(0.975) * sigma_jack
  upbs = theta_hat + qnorm(0.975) * sigma_jack
  
  res = list("lwbs" = lwbs, "upbs" = upbs)
  # res = c("lwbs" = lwbs, "upbs" = upbs)
  return(res)
}

#==============================================================================
# test case for function jackknife_ci_matrix
x = matrix(c(1:25), nrow = 5, byrow = TRUE)
y = matrix(c(40:59), nrow = 5, byrow = TRUE)
res = jackknife_ci_matrix(x, y)
#==============================================================================

## b.
## perform bootstrap on each of the replicate
generate_cis_matrix = function(x, y, B = 1e3) {
  # input: matrices x and y representing monte carlo replicates 
  #        B denotes the number of bootstrap samples
  # output: three cis for each monte carlo replicate after bootstrapping
  
  # each row should represent one monte carlo replicate
  # each row is a dataset
  
  mcrep = dim(x)[1]
  res = vector("list", mcrep)
 
  for (i in 1:mcrep) {
    n_x = dim(x)[2]
    n_y = dim(y)[2]
    boot_x = sample(x[i,], n_x * B, replace = TRUE)
    boot_y = sample(y[i,], n_y * B, replace = TRUE)
    dim(boot_x) = c(B, n_x)
    dim(boot_y) = c(B, n_y)
    x_bar = rowMeans(boot_x)
    y_bar = rowMeans(boot_y)
    theta_boot = x_bar / y_bar
    
    # percentile method
    percentile_ci = unname(quantile(theta_boot, c(0.025, 0.975)))
    percentile_ci2 = c(percentile_ci[1], percentile_ci[2])
    
    # the basic bootstrap
    theta_hat = mean(x[i,]) / mean(y[i,])
    basic_lwb = 2 * theta_hat - percentile_ci[2]
    basic_upb = 2 * theta_hat - percentile_ci[1]
    basic_ci = c(basic_lwb, basic_upb)
    
    # the normal approximation with bootstrap standard error
    standard_lwb = theta_hat - qnorm(0.975) * sd(theta_boot)
    standard_upb = theta_hat + qnorm(0.975) * sd(theta_boot)
    standard_ci = c(standard_lwb, standard_upb)
    
    list = list("percentile_ci" = percentile_ci2,
                "basic_ci" = basic_ci,
                "standard_ci" = standard_ci)
    res[[i]] = list
  }
  return(res)
}

#==============================================================================
# test case for function generate_cis_matrix
x = matrix(c(1:25), nrow = 5, byrow = TRUE)
y = matrix(c(40:59), nrow = 5, byrow = TRUE)
res = generate_cis_matrix(x, y)
#==============================================================================

## c.
## choose distributions to generate x and y with n_x and n_y
## carry out a monte carlo study on these two vectors
## compare the following three quantities for each of the four cis

mcrep = 1000 # number of monte carlo replicate
n_x = 20 # sample size of x
n_y = 30 # smaple size of y 
set.seed(123)
# generate x's from uniform distribution
xmat = runif(n = n_x * mcrep, min = 0, max = 1)
dim(xmat) = c(mcrep, n_x)

# generate y's from poisson distribution
ymat = rpois(n = n_y * mcrep, lambda = 4)
dim(ymat) = c(mcrep, n_y)

# the true value of the parameter
x_mean = 1 / 2
y_mean = 3
theta_true = x_mean / y_mean

#### i. the coverage probability

## jackknife coverage probability
jackknife_res = jackknife_ci_matrix(xmat, ymat)
lcb = jackknife_res[1]
upb = jackknife_res[2]
jackknife_cvrg_prob = mean( {unlist(lcb) < theta_true} & {theta_true < unlist(upb)} )
# jackknife_cvrg_prob = 0.456

## 3 bootstrap cis coverage probability
## form the matrix for these three different cis and label them correctly
boot3_res = generate_cis_matrix(xmat, ymat)
boot3_res_matrix = matrix(unlist(boot3_res), nrow = mcrep, byrow = TRUE)
colnames(boot3_res_matrix) = c("percentile_ci1", "percentile_ci2",
                     "basic_ci1", "basic_ci2", 
                     "standard_ci1", "standard_ci2")
head(boot3_res_matrix)

## percentile_ci coverage probability
percentile_ci_res = cbind(boot3_res_matrix[, "percentile_ci1"], 
                          boot3_res_matrix[, "percentile_ci2"])
colnames(percentile_ci_res) = c("percentile_ci1", "percentile_ci2")
head(percentile_ci_res)
percentile_cvrg_prob = mean({percentile_ci_res[, 1] < theta_true} &
                            {theta_true < percentile_ci_res[, 2]})
# percentile_cvrg_prob = 0.512

## the basic bootstrap coverage probability
basic_res = cbind(boot3_res_matrix[, "basic_ci1"],
                  boot3_res_matrix[, "basic_ci2"])
colnames(basic_res) = c("basic_ci1", "basic_ci2")
head(basic_res)
basic_cvrg_prob = mean({basic_res[, 1] < theta_true} &
                       {theta_true < basic_res[, 2]})
# basic_cvrg_prob = 0.384

## standard normal approximation coverage probability
std_normal_ci_res = cbind(boot3_res_matrix[,"standard_ci1"],
                          boot3_res_matrix[,"standard_ci2"])
colnames(std_normal_ci_res) = c("standard_ci1", "standard_ci2")
head(std_normal_ci_res)
std_normal_cvrg_prob = mean({std_normal_ci_res[, 1] < theta_true} &
                            {theta_true < std_normal_ci_res[, 2]})
# std_normal_cvrg_prob = 0.445

#### ii. the average length of the confidence intervals produced by each method 
####     (i.e. the difference between two values)

## jackknife CI average length
lcb = jackknife_res[1]
upb = jackknife_res[2]
jackknife_avg_len = mean(unlist(upb) - unlist(lcb))
# jackknife_avg_len = 0.07992586

## 3 bootstrap cis average length

## percentile ci average length
percentile_avg_len = mean(percentile_ci_res[, 2] - percentile_ci_res[, 1])
# percentile_avg_len = 0.07792532

## basic bootstrap average length
basic_avg_len = mean(basic_res[, 2] - basic_res[, 1])
# basic_avg_len = 0.07792532 (same as the percentile ci average length)

## standard normal approximation average length
std_normal_avg_len = mean(std_normal_ci_res[, 2] - std_normal_ci_res[, 1])
# std_normal_avg_len = 0.07828182

#### iii. compute the average shape of the confidence intervals, defined as 
####      (theta_hat_upper - theta_hat) / (theta_hat - theta_hat_lower)
theta_hats = rowMeans(xmat) / rowMeans(ymat)

## jackknife ci average shape
lcb = jackknife_res[1]
upb = jackknife_res[2]
diff_numerator1 = unlist(upb) - theta_hats
diff_denominator1 = theta_hats - unlist(lcb)
jackknife_shape = mean(diff_numerator1 / diff_denominator1)
# jackknife_shape = 1

## 3 bootstrap average shape

## percentile ci average shaple
diff_numerator2 = percentile_ci_res[, 2] - theta_hats
diff_denominator2 = theta_hats - percentile_ci_res[, 1]
percentile_shape = mean(diff_numerator2 / diff_denominator2)
# percentile_shape = 1.222945

## basic bootstrap ci average shape
diff_numerator3 = basic_res[, 2] - theta_hats
diff_denominator3 = theta_hats - basic_res[, 1]
basic_shape = mean(diff_numerator3 / diff_denominator3)
# basic_shape = 0.8227833

## standard normal approximation ci average shape
diff_numerator4 = std_normal_ci_res[, 2] - theta_hats
diff_denominator4 = theta_hats - std_normal_ci_res[, 1]
std_normal_shape = mean(diff_numerator4 / diff_denominator4)
# std_normal_shape = 1



