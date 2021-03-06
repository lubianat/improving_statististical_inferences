
# Calculate the likelihood ratio ----

# Set total trials
total_trials<- 1000
# Set successes
successes<- 500


# Specify one hypothesis you want to compare with the likihood ratio
theta_H0 <-  .4


# Specify another hypothesis you want to compare with the likihood ratio (you can use 1/20, or 0.05).
# Or you can use the maximum likelihood estimate (considering absence of priors)
maximum_likelihood_estimate = successes/total_trials 
theta_H1 <- maximum_likelihood_estimate 

binomial_H0 <- dbinom(successes, total_trials, theta_H0)

binomial_H1 <- dbinom(successes, total_trials, theta_H1)

# Returns the likelihood ratio of H0 over H1
binomial_H0/binomial_H1

# Returns the likelihood ratio of H1 over H0
binomial_H1/binomial_H0

# Create 100 theta variables, from 0 to 1
# Theta is the probability of obtating a success
theta<- seq(0,1,len=100) 
likelihood <- dbinom(successes,total_trials,theta)
#png(file="LikRatio.png",width=4000,height=3000, , units = "px", res = 900)
plot(theta,likelihood,type='l',xlab=expression(theta), ylab='Likelihood', lwd=2)

points(theta_H0, binomial_H0)
points(theta_H1, binomial_H1)

segments(theta_H0, binomial_H0,successes/total_trials, binomial_H0, lty=2, lwd=2)
segments(theta_H1, binomial_H1,successes/total_trials, binomial_H1, lty=2, lwd=2)
segments(successes/total_trials, binomial_H0,successes/total_trials, binomial_H1, lwd=2)
title(paste('Likelihood Ratio H0/H1:',round(binomial_H0/binomial_H1,digits=2)," Likelihood Ratio H1/H0:",round(binomial_H1/binomial_H0,digits=2)))
#dev.off()

#© Daniel Lakens, 2016. 
# Modified and re-released in CC-BY-NC-SA by Tiago Lubiana, 2016
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/

