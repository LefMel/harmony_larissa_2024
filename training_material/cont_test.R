# BLCMs for continuous tests

# Load packages
library("runjags")

## ROC Analysis

#  - ROC curves represent a plot of all possible pairs of *Se* and *(1-Sp)* values estimated at a range of cut-off values.

# - ROC can used to assess the accuracy of diagnostic tests measured on a continuous scale.

load("Cont.Rdata")
hist(S, breaks = 20)
abline(v=-1.8, col = "green")
abline(v=-1, col = "red")

# ROC Analysis with BLCMs 

## Point 1

# - In the absence of a gold standard the true disease status of each individual is unknown.

# - The available information we have is the continuous test output from each individual.

## Point 2

#  - Most studies dichotomize the continuous test output based on a pre-selected cut-off value and apply the BLCMs we discussed yesterday.

# - But this results in loss of valuable information. 
# - All positive results are equal, no matter how near or far they are from the cutoff.
# = Example - PCR CT value, scales of degree ( 8 - 10 --> A, 6.5 - 8 --> B ...)

## Model Specification - Mixture Normal Model

#  The data are best described by a mixture of two normal distributions (1 for the infected and 1 for the uninfected group):
# D (-) individuals with mean (mu1) and variance (1/tau_1)
# D (+) individuals with mean (mu2) and variance (1/tau_2)
# where * 1/tau = Precision

# The disease status for each indivudual is indicated by a latent variable.
# For identifiability we assume: mu1 < mu2  - diseased individuals are expected to have higher value of the continuous marker


## Mixture Normal Model explained
continuous_test <- "model {
  for (i in 1:481) {
	    #S[i] diagnostic test value for ith individual
	    S[i] ~ dnorm(mu[i],tau[i])
  
	    #Value of mu & tau depending on the group (diseased or disease-free)
	    mu[i] <- lambda[T[i]]           
	    tau[i] <- gamma[T[i]]
	    #dcat <- categorical #D(-) if T[i]=1, D(+) if T[i]=2
	    T[i] ~ dcat(P[]) 
  }
	P[1:2] ~ ddirch(alpha[])

	# lambda[1]-gamma[1] mean-precision of non-disease group   
	lambda[1] ~ dnorm(0,0.001)
	lambda[2] ~ dnorm(0,0.001)T(lambda[1],) 
	gamma[1] ~ dgamma(0.001,0.001)
	gamma[2] ~ dgamma(0.001,0.001)  

	# variance = 1/precision(tau)
	sigma[1] <- 1/gamma[1] 	
	sigma[2] <- 1/gamma[2]	

	# AUC
	AUC <- phi(-(lambda[1]-lambda[2])/sqrt(sigma[2]+sigma[1]))
	# ROC curve
	for(i in 1:111) {
	  c1[i] <-  ((-3.1+0.04*i)-lambda[2])/sqrt(sigma[2]) # grid is from -3 to 0.84
	  se[i] <- 1-phi(c1[i])
    c2[i] <-  ((-3.1+0.04*i)-lambda[1])/sqrt(sigma[1])
	  sp[i] <- phi(c2[i])
	  
	  J[i] <- se[i] + sp[i] - 1
	}
	#data# alpha, S
	#inits# lambda, gamma
	#monitor# AUC, se, sp, P, lambda, gamma, sigma, se, sp, J
}
"

## Data - Initial Values
alpha <- c(1,1)


summary(S)

# Initial values:
lambda <- list(chain1=c(-3, 0), chain2=c(-2,-2))
gamma <- list(chain1=c(10, 0.1), chain2=c(30, 5))


# Run the model
runjags.options(force.summary = TRUE)
results <- run.jags(continuous_test, n.chains = 2)

# Results interpretation and visualization
plot(results, vars=c('AUC', 'P', 'lambda', 'gamma', 'sigma'))
cut_off = summary(results)


# Estimate Se, Sp and plot ROC curve
se_est <- combine.mcmc(results, vars='se')
sp_est <- combine.mcmc(results, vars='sp')
ses_mu <- apply(se_est, 2, mean)
sps_mu <- apply(sp_est, 2, mean)
par(mfrow=c(1,1))
plot((1-sps_mu), ses_mu, type="l", col="darkblue", xlab = "1-Sp", ylab = "Se")

# AUC estimation
auc_est <- combine.mcmc(results, vars='AUC')
hist(auc_est, breaks=50, col="orange", main="AUC")

# Mean of infected and uninfected groups
lambda_est <- combine.mcmc(results, vars='lambda')
boxplot(as.matrix(lambda_est), col="red")

## Exercise 1

#  - Run the model and produce the same output.

# - Try to run the model under different prior specification for lambda

# - What happens if you remove T(lambda[1],)? Does the model converge?

# - Try and find the cut-off value that maximizes Youden's index?
cut_off <- data.frame(cut_off)
cut_off$Index = rownames(cut_off)
which.max(cut_off[grepl("J", cut_off[,"Index"]),]$Median) # 21
cut_off_opt = -3.1+0.04*21

hist(S, breaks = 20)
abline(v=cut_off_opt, col = "green")

cut_off[grepl("se", cut_off$Index),]$Median[21]
cut_off[grepl("sp", cut_off$Index),]$Median[21]
