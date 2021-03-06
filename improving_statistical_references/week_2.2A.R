#Set the point null hypothesis you want to calculate the Bayes Factor for
theta_H0 <-0.5 

# Set total trials
total_trials <-365*24
#set successes
successes <-365*24

#Set the alpha for the Beta distribution for the prior
a_prior<-1
#Set the beta for the Beta distribution for the prior
bprior<-1

a_likelihood<-successes+1 #Calculate the alpha for the Beta distribution for the likelihood
b_likelihood<-total_trials-successes+1 #Calculate the beta for the Beta distribution for the likelihood
a_posterior<-a_prior+a_likelihood-1 #Calculate the alpha for the Beta distribution for the posterior
b_posterior<-bprior+b_likelihood-1 #Calculate the beta for the Beta distribution for the posterior

#create theta range from 0 to 1
theta<-seq(0,1,0.001)

#png(file="PriorLikelihoodPosterior.png",width=3000,height=3000, res = 500)
prior <- dbeta(theta, a_prior, bprior)
likelihood <- dbeta(theta, a_likelihood, b_likelihood)
posterior <- dbeta(theta, a_posterior, b_posterior)


plot(theta, posterior, ylim=c(0, 15), type = "l", lwd = 3, xlab = bquote(theta), ylab = "Density", las = 1)
lines(theta, prior, col="grey", lwd = 3)
lines(theta, likelihood, lty = 2, lwd = 3, col="dodgerblue")

points(theta_H0,dbeta(theta_H0, a_posterior, b_posterior), pch = 19)
points(theta_H0,dbeta(theta_H0, a_prior, bprior), pch = 19, col="grey")
segments(theta_H0, dbeta(theta_H0, a_posterior, b_posterior), theta_H0, dbeta(theta_H0, a_prior, bprior), lty=2)

BF10<-dbeta(theta_H0, a_posterior, b_posterior)/dbeta(theta_H0, a_prior, bprior)
title(paste('Bayes Factor:',round(BF10,digits=2)))
#dev.off()

#Š Daniel Lakens, 2016. 
# Modified by Tiago Lubiana, 2020
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/