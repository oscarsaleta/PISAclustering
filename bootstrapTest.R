# Determine whether the two groups of countries are considered similar in terms of PISA results using non-parametric bootstrap

# Get the PISA results for each group
res_gp1 = RESULTS_list[[1]]
res_gp2 = RESULTS_list[[2]]
diff_mean_org = means[1]-means[2]
diff_sd_org = sqrt(standardDev[1]^2+standardDev[2]^2)

# Number of permutation samples to compute
btsp_nb = 10000

# Declare the bootstrap mean value arrays
res_bt = numeric(btsp_nb)

# Number of samples for each group
res_gp1_n = length(res_gp1)
res_gp2_n = length(res_gp1)

# Generate bootstrap samples and store the mean value for each group
for (i in 1:btsp_nb)
{
  res_gp1_btsp = sample(res_gp1, res_gp1_n, replace=T)
  res_gp1_mean = mean(res_gp1_btsp)
  res_gp1_sd = sd(res_gp1_btsp)
  
  res_gp2_btsp = sample(res_gp2, res_gp2_n, replace=T)
  res_gp2_mean = mean(res_gp2_btsp)
  res_gp2_sd = sd(res_gp2_btsp)
  
  res_mean_diff = res_gp1_mean-res_gp2_mean
  res_sd_diff = sqrt(res_gp1_sd^2+res_gp2_sd^2)
  
  res_bt[i] = (res_mean_diff-diff_mean_org)/res_sd_diff
}

# hist(diff_mean, main = "Histogram of the difference of means")
# boxplot (diff_mean, main = "Boxplot of the difference of means")

# Compute the 95% confidence interval using the Bootstrap-t method
diff_CI = diff_mean_org + diff_sd_org*quantile(res_bt,c(0.025,0.975))

# If 0 is outside the confidence interval, consider that the 2 groups are not similar statistically speaking
groupsAreDifferent = (diff_CI[1]*diff_CI[2]) > 0