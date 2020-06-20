# Load pwr package to easily calculate the statistical power
if (!require(pwr)) {
  install.packages('pwr')
}
library(pwr)


##### Set conditions ####


# Disable scientific notation (1.05e10)
options(scipen = 999)
# Set number of simulations
number_of_experiments <- 100000 #number of simulated experiments

# Set mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
mean_of_samples <-  107
null_effect_expectation<-100

# Set sample size
number_of_samples <- 51
# Set SD of the simulated data
standard_deviation_for_simulation <- 15 

# With a mean difference of 6 (106 compared to 100), and SD of 15, and a sample size of 26, the test has 50% power)

# set up empty variable to store all simulated p-values
observed_p_value_for_each_experiment <-  numeric(number_of_experiments) 



##### Run simulations and calculate power ####

for (i in 1:number_of_experiments) {
  
  # Simulate data with specified mean, standard deviation, and sample size
  observations_for_this_experiment <-rnorm(n = number_of_samples,
                                     mean = mean_of_samples,
                                     sd = standard_deviation_for_simulation) 
  
  # Perform the t-test against mu (set to value you want to test against)
  t_test_results <-    t.test(observations_for_this_experiment,
                 mu = null_effect_expectation) 
  
  # Get the p-value and store it
  observed_p_value_for_each_experiment[i] <- t_test_results$p.value 
}


# Check power by summing significant p-values and dividing by number of simulations

alpha = 0.01
number_of_p_values_under_alpha <- sum(observed_p_value_for_each_experiment < alpha)
simulated_statistical_power = number_of_p_values_under_alpha/number_of_experiments

print(paste("This is the simulated statistical_power:", simulated_statistical_power))

# Calculate power formally by power analysis
# determines M when power > 0. When power = 0, will set  M = 100.

formal_statistical_power <-
  pwr.t.test(
    d = (mean_of_samples - null_effect_expectation) / standard_deviation_for_simulation,
    n = number_of_samples,
    sig.level = alpha,
    type = "one.sample",
    alternative = "two.sided"
  )$power 

print(paste("This is the formal_statistical_power:", formal_statistical_power))


###### Plot figure ######

#png(file="P-valueDist.png",width=4000,height=3000, , units = "px", res = 500)
op <- par(mar = c(5, 7, 4, 4)) #change white-space around graph


number_of_bars = 100
hist(x = observed_p_value_for_each_experiment,
  breaks = number_of_bars,
  xlab = "P-values",
  ylab = "number of p-values\n",
  axes = FALSE,
  main = paste(
    "P-value Distribution with",
    round(formal_statistical_power * 100, digits = 1),
    "% Power"
  ),
  col = "grey",
  xlim = c(0., 1)
)
axis(side = 1,
     at = seq(0, 1, 0.1),
     labels = seq(0, 1, 0.1))
axis(
  side = 2,
  at = seq(0, number_of_experiments, number_of_experiments / 4),
  labels = seq(0, number_of_experiments, number_of_experiments / 4),
  las = 2
)
abline(h = number_of_experiments / number_of_bars , col = "red", lty = 3)
#dev.off()

#Š Daniel Lakens, 2016.
# Adapted by Tiago Lubiana in 2020. 
# The original code was licensed in CC-BY-NC-SA and so is this one.
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
