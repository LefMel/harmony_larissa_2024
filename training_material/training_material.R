# HARMONY CA18208 Training School 

# Navigating through Bayesian approaches in disease measurement

# 25 - 27 October 2023
# Larissa, Greece

#################################
### Day 1 Material (Konstantinos Pateras, Polychronis Kostoulas)
#################################

# Konstantinos Pateras presentation - Yet another intro to MCMC Focusing on 2 main methods (pdf im the training_material folder)


# Bayesian Statistics

#In this session we'll see how we can estimate a probability of interest  but in a Bayesian framework, i.e. using Bayes theorem.

# Bayes' theorem 

#  P(A|B) = P(B|A)*P(A)/P(B)


# Components

# * P(A|B): Prob of event A occurring given that B is true - Posterior probability
# * P(B|A): Prob of event B occurring given that A is true - Likelihood ~ function of A
# * P(A): Prob of event A occurring - Prior probability
# * P(B): Prob of event B occurring - Marginal probability ~ sum over all possible values of A

# What we usually see/use


# theta: parameter of interest | y: observed data}
# P(theta|y) = P(y|\theta) * P(theta)/P(y) 
# Where:

#  * P(theta): Prior probability of parameter(s) of interest;
#  * P(y|theta): Likelihood of the data given the parameters value(s) 
#  * P(theta|y): Posterior probability of parameter(s) of interest given the data and the prior

# Bayesian Inference - Summary & Example

# To estimate the posterior distribution P(theta$|y) we need to:

#  Specify the Prior distribution: P(theta)
#  Define the Likelihood of the data: P(y|theta) 

# Example 1: Bayesian apparent prevalence (ap) estimation

# y out of n individuals test positive. Estimate the apparent prevalence.


# Parameter of interest: ap - [0,1]

# Data: n tested, y positive

# * Prior distribution for ap: ap ~ Beta(a,b)
# * Likelihood: y ~ Binomial(n,ap) 

# Let's write our first JAGS model

ap_model <- 
  'model {
  
  # Define likelihood distribution of the data
  # JAGS Binomial distribution Arguments: p, n 
  
  y ~ dbin(ap,n)
  
  # Specify prior distribution for par of interest 
  # Uniform (non-informative) prior distribution 
  ap ~ dbeta(1, 1)

  #data# n, y
  #monitor# ap
  #inits# ap
  }
  '


# Let's run our first JAGS model

# Call JAGS
library(runjags)

# Provide Data 
n = 40
y = 12

# Initial values for par of interest
ap <- list(chain1=0.05, chain2=0.95)


# Run the model
results <- run.jags(ap_model, n.chains=2, 
                    burnin=5000, sample=10000)

# View results

# Plot results
plot(results)
# Print results
summary(results)


# Example 2: Bayesian true prevalence (tp) estimation

# Assuming the absence of a perfect test we do not know how many individuals are truly positive/negative.

# Instead we know that n individuals are tested with an imperfect test and y have a positive result.


# Apparent/True prevalence: ap/tp 
# Sensitivity: Se 
# Specificity: Sp

# ap = P(T+) = P(T + & D+) + P(T+ &  D-) = P(D+) * P(T+|D+) + P(D-) * P(T+|D-) =>
# ap = tp * Se + (1 - tp) * (1 - Sp)

## Create a JAGS model for true prevalence estimation

# Parameters of interest - tp, Se, Sp 

# Prior distributions

# tp ~ dbeta(1,1) # Uniform (non-informative) prior distribution 
# Se ~ dbeta(25.4, 3.4) # 0.85 (0.7 - 0.95)
# Sp ~ dbeta(95, 5) # 0.77 (0.49 - 0.96)

# Data: n tested, y positive

# Likelihood: y ~ Binomial(n,ap), 
# ap = tp * Se + (1 - tp) * (1 - Sp)

## Write JAGS model
tp_model <- 
  'model {
  y ~ dbin(ap,n)
  ap <- tp*Se + (1-tp)*(1-Sp)
  
  # Uniform (non-informative) prior distribution 
  tp ~ dbeta(1,1)
  # Informative priors for Se and Sp
  Se ~ dbeta(25.4, 3.4)
  Sp ~ dbeta(95, 5)
  
  #data# n, y
  #monitor# tp, Se, Sp
  #inits# tp, Se, Sp
  }
  '


## Let's run our JAGS model
# Call JAGS
library(runjags)
# Provide Data 
n = 4072
y = 1210
# Initial values for pars of interest
tp <- list(chain1=0.05, chain2=0.95)
Se <- list(chain1=0.05, chain2=0.95)
Sp <- list(chain1=0.05, chain2=0.95)

# Run the model
results <- run.jags(tp_model, n.chains=2, 
                    burnin=5000, sample=10000)
## View results
# Plot results
plot(results) # Click backwards to view all plots
# Print results
summary(results)




#################################
### Day 2 Material (Konstantinos Pateras, Eleftherios Meletis, Xanthi Rousou)
#################################


# Konstantinos Pateras presentation - tPRiors:Bayesian prevalence estimation with elicited priors (pdf im the training_material folder)


# How to choose and generate prior distributions?
require(PriorGen)

# Exercise 1: Check if the codes below create the prior parameters for slides 1 to 3.

?findbeta
# Example 1
findbeta(themean = .9, percentile = .95, lower.v = F, percentile.value = .80)
# Example 2
findbeta(themean = .99, percentile = .95, lower.v = F, percentile.value = .90)
# Example 3
findbeta(themean = .01, percentile = .70, lower.v = T, percentile.value = .05)
# Example 4
findbeta_abstract(themean.cat = "Average",thevariance.cat = "High")

?findbeta_abstract
?findbeta_panel
?findbeta_raw


# Exercise 2: What if I wanted to create a beta prior for the specificity and I knew that 
# 1a. the mean specificity lies between (0.4-0.6) and 
# 1b. we are 95% certain that it is lower than 0.8
out1<-findbeta(themean=0.5, percentile = 0.95, lower.v = T, percentile.value = 0.8)
# 1b. we are 99% certain that it is lower than 0.6
out2<-findbeta(themean=0.5, percentile = 0.99, lower.v = T, percentile.value = 0.88)


# Exercise 3: Plotting of priors based on hyperparameters of Beta distribution.
x=seq(0,1,0.001)
plot(out1,type="l",lwd=3,ylab="Density (Specificity - 1 & 2)",ylim=c(0,5))
lines(out2,type="l",lwd=3, col="red")
legend("topright",legend = c("Prior1","Prior2"),fill=c("black","red"))




# Sensitivity - Specificity estimation with and without a gold standard

# Eleftherios Meletis presentation - Test_evaluation (pdf im the training_material folder)

## Hui-Walter paradigm/model (1980) 

# Link to Publication: https://doi.org/10.2307/2530508 
# Estimating the Error Rates of Diagnostic Tests S. L. Hui and S. D. Walter, 1980

#  - A particular model formulation that was originally designed for evaluating diagnostic tests in the absence of a gold standard

#- Not originally/necessarily Bayesian - implemented using Maximum Likelihood 

# - Evaluating an imperfect test against another imperfect test; is a bit like pulling a rabbit out of a hat

# If we don't know the true disease status, how can we estimate sensitivity or specificity for either test?

# https://www.youtube.com/watch?v=z6devQmW2xE&ab_channel=PolychronisKostoulas#



# We will use the data/observations from the manuscript published back in 1980.

## Hui-Walter (1980) dataset Table 1
pop_1 = matrix(nrow=3,ncol=3)
rownames(pop_1) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_1) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_1[1,1] = 14
pop_1[1,2] = 4
pop_1[2,1] = 9
pop_1[2,2] = 528
#Total rows and columns
pop_1[1,3] = pop_1[1,1] + pop_1[1,2]
pop_1[2,3] = pop_1[2,1] + pop_1[2,2]
pop_1[3,1] = pop_1[1,1] + pop_1[2,1]
pop_1[3,2] = pop_1[1,2] + pop_1[2,2]
N_1 = sum(pop_1[1,1] + pop_1[1,2] + pop_1[2,1] + pop_1[2,2])
pop_1[3,3] = N_1
pop_1

## Now let's do pop_2
pop_2 = matrix(nrow=3,ncol=3)
rownames(pop_2) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_2) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_2[1,1] = 887
pop_2[1,2] = 31
pop_2[2,1] = 37
pop_2[2,2] = 367
#Total rows and columns
pop_2[1,3] = pop_2[1,1] + pop_2[1,2]
pop_2[2,3] = pop_2[2,1] + pop_2[2,2]
pop_2[3,1] = pop_2[1,1] + pop_2[2,1]
pop_2[3,2] = pop_2[1,2] + pop_2[2,2]
N_2 = sum(pop_2[1,1] + pop_2[1,2] + pop_2[2,1] + pop_2[2,2])
pop_2[3,3] = N_2
pop_2

## Hui-Walter model

#  - A particular model formulation that was originally designed for evaluating diagnostic tests in the absence of a gold standard

# - Also known as the two_test - two_population setting/paradigm



## Model Specification ('hw_definition')

hw_definition <- c("model{
  Population_1 ~ dmulti(prob_1, N_1)
  Population_2 ~ dmulti(prob_2, N_2)
  
  #Population_1
  
  # Test1+ Test2+
	prob_1[1] <- (prev[1] * ((se[1])*(se[2]))) + ((1-prev[1]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_1[2] <- (prev[1] * ((se[1])*(1-se[2]))) + ((1-prev[1]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_1[3] <- (prev[1] * ((1-se[1])*(se[2]))) + ((1-prev[1]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_1[4] <- (prev[1] * ((1-se[1])*(1-se[2]))) + ((1-prev[1]) * ((sp[1])*(sp[2])))
	
	#Population_2
  
  # Test1+ Test2+
	prob_2[1] <- (prev[2] * ((se[1])*(se[2]))) + ((1-prev[2]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_2[2] <- (prev[2] * ((se[1])*(1-se[2]))) + ((1-prev[2]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_2[3] <- (prev[2] * ((1-se[1])*(se[2]))) + ((1-prev[2]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_2[4] <- (prev[2] * ((1-se[1])*(1-se[2]))) + ((1-prev[2]) * ((sp[1])*(sp[2])))

  prev[1] ~ dbeta(1, 1)
  prev[2] ~ dbeta(1, 1)
  
  se[1] ~ dbeta(1, 1)I(1-sp[1], )
  sp[1] ~ dbeta(1, 1)
  se[2] ~ dbeta(1, 1)I(1-sp[2], )
  sp[2] ~ dbeta(1, 1)

  #data# Population_1, Population_2, N_1, N_2
  #monitor# prev, prob_1, prob_2, se, sp
  #inits# prev, se, sp
  }
  ")
library('runjags')

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])


prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results <- run.jags(hw_definition, n.chains=2)


# Remember to check convergence and effective sample size!

plot(results)

pt <- plot(results)
pt$`prev[1].plot1`
pt$`prev[1].plot3`

print(pt[["prev[1].plot1"]])

print(pt[["se[1].plot1"]])
print(pt[["sp[1].plot1"]])
print(pt[["sp[1].plot3"]])

summary(results)

## Exercise 1 

#  Run the `hw_definition` model under the following different scenarios 
# and interpret the results in each case.

# 1. Change the priors for *Se[1]* and *Sp[1]* and try Beta(5,1).
curve(dbeta(x,5,1))  # Plots the distribution of Beta(2,1)


# Solution
## Model Specification ('hw_definition')

hw_definition_1 <- c("model{
  Population_1 ~ dmulti(prob_1, N_1)
  Population_2 ~ dmulti(prob_2, N_2)
  
  #Population_1
  
  # Test1+ Test2+
	prob_1[1] <- (prev[1] * ((se[1])*(se[2]))) + ((1-prev[1]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_1[2] <- (prev[1] * ((se[1])*(1-se[2]))) + ((1-prev[1]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_1[3] <- (prev[1] * ((1-se[1])*(se[2]))) + ((1-prev[1]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_1[4] <- (prev[1] * ((1-se[1])*(1-se[2]))) + ((1-prev[1]) * ((sp[1])*(sp[2])))
	
	#Population_2
  
  # Test1+ Test2+
	prob_2[1] <- (prev[2] * ((se[1])*(se[2]))) + ((1-prev[2]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_2[2] <- (prev[2] * ((se[1])*(1-se[2]))) + ((1-prev[2]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_2[3] <- (prev[2] * ((1-se[1])*(se[2]))) + ((1-prev[2]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_2[4] <- (prev[2] * ((1-se[1])*(1-se[2]))) + ((1-prev[2]) * ((sp[1])*(sp[2])))

  prev[1] ~ dbeta(1, 1)
  prev[2] ~ dbeta(1, 1)
  
  se[1] ~ dbeta(5, 1)I(1-sp[1], )
  sp[1] ~ dbeta(5, 1)
  se[2] ~ dbeta(1, 1)I(1-sp[2], )
  sp[2] ~ dbeta(1, 1)

  #data# Population_1, Population_2, N_1, N_2
  #monitor# prev, prob_1, prob_2, se, sp
  #inits# prev, se, sp
  }
  ")

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])


prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results_1 <- run.jags(hw_definition_1, n.chains=2)
# Remember to check convergence and effective sample size!
plot(results_1)  # Click backwards to view all plots

pt_1 <- plot(results_1)
pt_1$`prev[1].plot1`
pt_1$`prev[1].plot3`

print(pt_1[["prev[1].plot1"]])

print(pt_1[["se[1].plot1"]])
print(pt_1[["sp[1].plot1"]])
print(pt_1[["sp[1].plot3"]])

summary(results_1)

# 2. Remove the `I(1-sp[1], )` and 'I(1-sp[2])' from the model and run it again. What happens now?

# Solution
## Model Specification ('hw_definition')

hw_definition_2 <- c("model{
  Population_1 ~ dmulti(prob_1, N_1)
  Population_2 ~ dmulti(prob_2, N_2)
  
  #Population_1
  
  # Test1+ Test2+
	prob_1[1] <- (prev[1] * ((se[1])*(se[2]))) + ((1-prev[1]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_1[2] <- (prev[1] * ((se[1])*(1-se[2]))) + ((1-prev[1]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_1[3] <- (prev[1] * ((1-se[1])*(se[2]))) + ((1-prev[1]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_1[4] <- (prev[1] * ((1-se[1])*(1-se[2]))) + ((1-prev[1]) * ((sp[1])*(sp[2])))
	
	#Population_2
  
  # Test1+ Test2+
	prob_2[1] <- (prev[2] * ((se[1])*(se[2]))) + ((1-prev[2]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_2[2] <- (prev[2] * ((se[1])*(1-se[2]))) + ((1-prev[2]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_2[3] <- (prev[2] * ((1-se[1])*(se[2]))) + ((1-prev[2]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_2[4] <- (prev[2] * ((1-se[1])*(1-se[2]))) + ((1-prev[2]) * ((sp[1])*(sp[2])))

  prev[1] ~ dbeta(1, 1)
  prev[2] ~ dbeta(1, 1)
  
  se[1] ~ dbeta(1, 1)
  sp[1] ~ dbeta(1, 1)
  se[2] ~ dbeta(1, 1)
  sp[2] ~ dbeta(1, 1)

  #data# Population_1, Population_2, N_1, N_2
  #monitor# prev, prob_1, prob_2, se, sp
  #inits# prev, se, sp
  }
  ")

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])


prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results_2 <- run.jags(hw_definition_2, n.chains=2)
# Remember to check convergence and effective sample size!
plot(results_2)  # Click backwards to view all plots

pt_2 <- plot(results_2)
pt_2$`prev[1].plot1`
pt_2$`prev[1].plot3`

print(pt_2[["prev[1].plot1"]])

print(pt_2[["se[1].plot1"]])
print(pt_2[["sp[1].plot1"]])
print(pt_2[["sp[1].plot3"]])

print(pt_2[["se[2].plot1"]])

summary(results_2)

# 3. Try to run the model with different initial values, that explore the whole parameter space and removing `T(1-sp[1],)`.
# For example try it with:
# se <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))
# sp <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))

# Solution
## Model Specification ('hw_definition')
# The model is the same like as in the above scenario (hw_definition_2)

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])


prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))
sp <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))

results_3 <- run.jags(hw_definition_2, n.chains=2, sample = 100000)
# Remember to check convergence and effective sample size!
plot(results_3)  # Click backwards to view all plots

pt_3 <- plot(results_3)
pt_3$`prev[1].plot1`
pt_3$`prev[1].plot3`

print(pt_3[["prev[1].plot1"]])

print(pt_3[["se[1].plot1"]])
print(pt_3[["sp[1].plot1"]])
print(pt_3[["sp[1].plot3"]])
print(pt_3[["se[2].plot1"]])

summary(results_3)

# The model fails to converge to one solution, but reports two solution that are complementary

# 4. Run the model with only 1 population (either pop_1 or pop_2). What happens then?
# We'll remove population 2


# Solution
## Model Specification ('hw_definition')

hw_definition_4 <- c("model{
  Population_1 ~ dmulti(prob_1, N_1)

  #Population_1
  
  # Test1+ Test2+
	prob_1[1] <- (prev[1] * ((se[1])*(se[2]))) + ((1-prev[1]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_1[2] <- (prev[1] * ((se[1])*(1-se[2]))) + ((1-prev[1]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_1[3] <- (prev[1] * ((1-se[1])*(se[2]))) + ((1-prev[1]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_1[4] <- (prev[1] * ((1-se[1])*(1-se[2]))) + ((1-prev[1]) * ((sp[1])*(sp[2])))
	

  prev[1] ~ dbeta(1, 1)
  se[1] ~ dbeta(1, 1)I(1-sp[1],)
  sp[1] ~ dbeta(1, 1)
  se[2] ~ dbeta(1, 1)I(1-sp[2],)
  sp[2] ~ dbeta(1, 1)

  #data# Population_1, N_1
  #monitor# prev, prob_1,  se, sp
  #inits# prev, se, sp
  }
  ")

Population_1 <- as.numeric(pop_1[1:2,1:2])


prev <- list(chain1=c(0.05), chain2=c(0.95))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results_4 <- run.jags(hw_definition_4, n.chains=2)
# Remember to check convergence and effective sample size!
plot(results_4)  # Click backwards to view all plots

pt_4 <- plot(results_4)
pt_4$`prev.plot1`
pt_4$`prev.plot3`

print(pt_4[["prev.plot1"]])

print(pt_4[["se[1].plot1"]])
print(pt_4[["sp[1].plot1"]])
print(pt_4[["sp[1].plot3"]])
print(pt_4[["se[2].plot3"]])
print(pt_4[["se[2].plot4"]])
print(pt_4[["se[2].plot5"]])

summary(results_4)
# How do the results look? # Compare the results with the scenarios above? 
# Try also by removing population 2


curve(dbeta(x,5,1))
curve(dbeta(x,50000,1))




# Meta analysis of DTA studies using Bayesian latent class models

# Xanthi Rousou presentation - BAYES_CAMP_HARMONY (pdf in the training_material folder)






#################################
### Day 3 Material (Eleftherios Meletis)
#################################


# Alberto Gomez-Buendia - presentation _ Accuracy of tests for diagnosis of animal tuberculosis: moving away from the golden calf (and towards bayesian models) (pdf in the training_material folder)

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





#################################
### References for Reading
#################################

# Estimating the Error Rates of Diagnostic Tests, S. L. Hui and S. D. Walter, 1980 (https://doi.org/10.2307/2530508)
# Bayesian estimation of disease prevalence and the parameters of diagnostic tests in the absence of a gold standard, Joseph et al, 1995 (https://doi.org/10.1093/oxfordjournals.aje.a117428)
# STARD-BLCM: Standards for the Reporting of Diagnostic accuracy studies that use Bayesian Latent Class Models, Kostoulas et al, 2017 (https://doi.org/10.1016/j.prevetmed.2017.01.006)
# Diagnostic Accuracy Estimates for COVID-19 Real-Time Polymerase Chain Reaction and Lateral Flow Immunoassay Tests With Bayesian Latent-Class Models, Kostoulas et al, 2021 (https://doi.org/10.1093/aje/kwab093)
# |tPRiors |: a tool for prior elicitation and obtaining posterior distributions of true disease prevalence, Pateras - Kostoulas 2022 (https://doi.org/10.1186/s12874-022-01557-1)
# Estimation of diagnostic-test sensitivity and specificity through Bayesian modeling, Branscum et al., 2005 (https://doi.org/10.1016/j.prevetmed.2004.12.005)
# Conditional dependence between tests affects the diagnosis and surveillance of animal diseases, Gardner et al., 2000. (https://doi.org/10.1016/S0167-5877(00)00119-7)


# Take home message - Videos
# https://www.youtube.com/watch?v=1gQ-lLKl5CQ&ab_channel=PolychronisKostoulas
# https://www.youtube.com/watch?v=z6devQmW2xE&t=75s&ab_channel=PolychronisKostoulas








