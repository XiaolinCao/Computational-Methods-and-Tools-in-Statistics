## Question 3
## Author: Xiaolin Cao (UM ID: 62251943)
## This assignment works with "mouse-tracking" data to represent 
## how decisive each participant was when he/she was making decisions.
## The data is in the form of (x, y, t) with x and y be the position and t 
## be the time.The goal is to describe the curvature of the moving pattern:
## the total Euclidean distance travelled, the max deviation from the direct path,
## the average deviation from the direct path and the area under the curve.

##a
#### function to translate trajectory to begin with time 0 at origin
translate_trajectory = function(x) {
  ####inputs: x - an n by 3 matrix
  ####outputs: return the same trajectory that begins with time 0 at origin
  return(cbind(x[, 1] - x[1, 1], x[, 2] - x[1, 2], x[, 3] - x[1, 3]))
}

## unit test for translate_trajectory
stopifnot(all(translate_trajectory(cbind(rnorm(3), rnorm(3), 1:3))[1, ] == 0))

##b
#### function to compute the angle theta formed by the origin 
#### and final position of trajectory
compute_theta = function(x) {
  ####inputs: x - an n by 3 matrix
  ####outputs: return the angle formed by the secant line
  final_pos = x[dim(x)[1],]
  x_final = as.numeric(final_pos[1])
  y_final = as.numeric(final_pos[2])
  theta = atan(y_final / x_final)
  return(theta)
}

##c
#### function to rotate the (x, y) coordinates in the trajectory 
#### so that the final point lines in the positive x-axis
rotate_coordi = function(x) {
  ####inputs:  x - an n??3 matrix
  ####outputs: rotate (x, y) to positive x-axis
  y = translate_trajectory(x)
  theta = compute_theta(y)
  
  for (i in 1:dim(y)[1]) {
      x[i, 1] = cos(theta) * y[i, 1] + sin(theta) * y[i, 2];
      x[i, 2] = -sin(theta) * y[i, 1] + cos(theta) * y[i, 2];
  }
  return(x)
}

##d
#### function to normalize an n??3 matrix so that it begins at origin
#### and ends on positive x-axis
normalize_trajectory = function(x) {
  ####inputs: x - an n??3 matrix
  ####outputs: return a normalized trajectory matrix x
  y1 = translate_trajectory(x)
  y2 = rotate_coordi(y1)
  return(y2)
}

##e
#### function to describe the curvature of a normalized trajectory
describe_curvature = function(x) {
  ####inputs: x - an n??3 normalized matrix that begins at origin and final point ends on positive x-axis
  ####outputs: description of the curvature
  
  ####i. compute the total Euclidean distance travelled
  dist_sum = 0
  x_coordi1 = x[, 1]
  y_coordi1 = x[, 2]
  x_diff1 = diff(x_coordi1, lag = 1, difference = 1)
  y_diff1 = diff(y_coordi1, lag = 1, difference = 1)
  dist_sum = sum(sqrt(x_diff1 * x_diff1 + y_diff1 * y_diff1))
  
  ####ii. compute the maximum absolute deviation from the secant line connecting the 
  ####    start and end position
  max_devi = 0
  for (j in 1:dim(x)[1]) {
    max_devi = max(max_devi, abs(x[j, 2]))
  }
  
  ####iii. compute the average absolute deviation of the observed trajectory from
  ####     the direct path
  total_devi = 0;
  for (k in 1:dim(x)[1]) {
    total_devi = total_devi + abs(x[k, 2])
  }
  average_devi = total_devi / dim(x)[1]
  
  ####iv. compute the absolute area under the curve relative to the secant line
  ####    connecting the start and end point by trapezoid rule
  y_coordi = abs(x[, 2])
  x_coordi = x[, 1]
  x_diff = diff(x_coordi, lag = 1, difference = 1)
  y_sum = diff(c(0, cumsum(y_coordi)), lag = 2)
  area = abs(sum(y_sum * x_diff / 2))
  
  result = list("total_dist" = dist_sum, "max_devi" = max_devi, "average_devi" = average_devi, "AUC" = area)
  return(result)
}

##f
#### apply the above functions to train_trajectories.csv and check the
#### answer against the train_measures.csv
train_dat = read.csv("/Users/caoxiaolin/Downloads/STATS506/hw1/train_trajectories.csv")
sub_num_1_dat = train_dat[train_dat$subject_nr == 1,]
count_trial_2_dat = sub_num_1_dat[sub_num_1_dat$count_trial == 2,]
count_trial_3_dat = sub_num_1_dat[sub_num_1_dat$count_trial == 3,]
count_trial_4_dat = sub_num_1_dat[sub_num_1_dat$count_trial == 4,]
normalized_mat_2 = normalize_trajectory(count_trial_2_dat[, c(3, 4, 5)])
describe_curvature(normalized_mat_2)
normalized_mat_3 = normalize_trajectory(count_trial_3_dat[, c(3, 4, 5)])
describe_curvature(normalized_mat_3)
normalized_mat_4 = normalize_trajectory(count_trial_4_dat[, c(3, 4, 5)])
describe_curvature(normalized_mat_4)

#### apply the above functions to the test_trajectories.csv and output the
#### results in a table
test_dat = read.csv("/Users/caoxiaolin/Downloads/STATS506/hw1/test_trajectories.csv")
sub1 = test_dat[test_dat$subject_nr == 6,]
normalized_sub1 = normalize_trajectory(sub1[, c(3, 4, 5)])
res_sub1 = describe_curvature(normalized_sub1)
#res_table = matrix(data=NA, nrow=5, ncol=4)

for (i in 6:10) {
  sub = test_dat[test_dat$subject_nr == i,]
  normalized_sub = normalize_trajectory(sub[, c(3, 4, 5)])
  # res = describe_curvature(normalized_sub)
  # res_table[i-5, 1] = res[["total_dist"]]
  # res_table[i-5, 2] = res[["max_devi"]]
  # res_table[i-5, 3] = res[["average_devi"]]
  # res_table[i-5, 4] = res[["AUC"]]
  print(as.data.frame(describe_curvature(normalized_sub)))
}
res_1 = c(1650.769, 464.8991, 90.38783, 275254.4)
res_2 = c(1252.55, 35.46823, 4.723562, 19981.2)
res_3 = c(1069.158, 18.4113, 1.757015, 10133.99)
res_4 = c(1092.076, 74.2055, 7.302945, 36134.4)
res_5 = c(1086.835, 85.33933, 12.48771, 51446.32)
res = rbind(res_1, res_2, res_3, res_4, res_5)
res_dat = as.matrix(res)
colnames(res_dat) = c("total_dist", "max_devi", "average_devi", "AUC")
res_dat
