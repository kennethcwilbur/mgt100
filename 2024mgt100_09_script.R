## Week 9

# clear workspace
rm(list=ls())
setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")

# Today we fit a Bass Model to estimate new product diffusion.
# The context is immaterial, but we could suppose the product is virtual reality 
#    goggles, like Oculus

library(tidyverse)

# --------------------
# BASS MODEL
# --------------------

#choose true parameters, we'll use these to simulate data
# then we'll use the simulated data to recover the known parameters
M<-100
p<-.02     # coefficient of innovation
q<-.2      # coefficient of imitation
Nstart <- p  # Number of sales in period 1
T<-40      # Number of periods to simulate

# Note: We'll denote sales as N because that's easier to write than dA/dt

# allocate memory
N <- vector(mode="numeric",length=T)
A <- vector(mode="numeric",length=T)
R <- vector(mode="numeric",length=T)
t <- vector(mode="numeric",length=T)

# initialize vectors for period 1
N[[1]] <- Nstart
A[[1]] <- N[[1]]
R[[1]] <- M-N[[1]]
t[[1]] <- 1.0
  
#simulate data for subsequent periods
for (i in 2:T){
  
  t[i] <- i*1.0
  # Bass model formula:
  N[i] <- ( p + q * A[[i-1]] / M ) * R[[i-1]]
  # Update accumulated sales
  A[i] <- A[[i-1]] + N[[i]]
  # Update Remaining market size
  R[i] <- M-A[[i]]
}

# Let's take a look . Combine simulated data into a dataframe & plot
sim <- cbind(t,N,A,R)
sim <- as.data.frame(sim)

# Let's look at Sales first
ggplot(sim) +
  geom_point(aes(t,N), color="black") 
  
# Now let's compare Sales to Accumulated Sales & Remaining market
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,A), color="red") +
  geom_point(aes(t,R), color="blue")


# This illustrates a general principle in model training, estimation and usage
# If we can simulate data from a model and known parameters,
#     and then use the simulated data and estimation code to recover known parameters,
#     then we can (a) check our understanding
#         (b) check our estimation code
#         (c) run a variety of simulation experiments to understand the 
#             model and estimator properties


# Now let's estimate the Bass model using OLS (the quadratic in A(t))
#    (good deeper dive if you want it: https://rpubs.com/BM07BAM/bass_model )

# run the linear regression: N_t = a + b*A_t + c*A_t^2
out_lm <- lm(N ~ A + I(A^2), data = sim)
summary(out_lm)

# What did we find? How well did the model fit the training data?

# Let's use our estimated model to predict sales in the training sample
sim$lmpred <- predict(out_lm)
# Let's pause for a moment to appreciate R's compact syntax on that

# Let's see how predicted sales compares to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") 


# Now let's estimate the Bass model a second way, using a different approach
# This time we'll use Nonlinear Least Squares and the exact solution

# First let's define our NLS objective function that we want to minimize, 
# as a function of p and q. This function calculates the sum of square errors
bass_sse <- function(pq, N_t, A_t, M) {
  p <- pq[1]
  q <- pq[2]
  
  N_hat <- M * p + (q - p) * A_t - (q / M) * A_t^2
  
  sse <- sum((N_t - N_hat)^2)
}

# We're going to search over p-q space to find the combination of p and q estimates
# that minimize the sum of square errors. The following defines the starting point for
# the search
init <- c(0.1, 0.1)   # this is where we start our search (p=.1, q=.1)

# 'optim' generically minimizes a multidimensional function (in this case, bass_sse)
# optim offers several search routines; defaults to Nelder-Mead https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
# init is the starting point, fn is the function to minimize
out2 <- optim(par = init, fn = bass_sse, N_t = sim$N, A = sim$A, M = M)
# type ?optim to learn more about the optim(ization) function

# What did we find?
out2$par
# How well are the parameters estimated?

# Let's use our NLS model to predict sales in the training sample
PredictBassInSampleN <- function(parms, T_horizon){

# allocate memory  
ISPredN <- vector(mode="numeric",length=T_horizon)

# Use the estimated model to predict sales in the training sample
for (i in 1:T_horizon) {
  ISPredN[i] <- M * parms[[1]] + (parms[[2]] - parms[[1]]) * A[i] - (parms[[2]] / M) * A[i]^2
  ISPredN[i] 
}

return(ISPredN)
}

sim$NLSInsamplePred<-PredictBassInSampleN(out2$par, 40)


# Let's see how in-sample sales predictions compare to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") +
  geom_point(aes(t,NLSInsamplePred), color="purple")

## Which model fits better? In what periods does each estimator fit better or worse?
nls_insample_r2<-1 - sum((sim$N-sim$NLSInsamplePred)^2)/sum((sim$N-mean(sim$N))^2)
nls_insample_r2


## Until now, we have used the entire T dataset to fit both models
## That means we can't do much predictive analytics--we used the entire dataset in estimation

##   (It's basically cheating to use training data to evaluate predictive performance)
##   (Do you understand why?)

## Now, suppose we only had the first (short_T) periods of data
## We'll estimate with the first short_T periods then retrodict periods 13-40
short_T <- 12

## Let's consider what estimation method would have yielded more accurate predictions
## This is a retrodiction exercise

# First let's create the restricted training sample 
shortsim <- sim[1:short_T,]

# Let's estimate the Bass model again using the quadratic linear model, using the 
# restricted data

# run the linear regression: N_t = a + b*A_t + c*A_t^2
out_lm2 <- lm(N ~ A + I(A^2), data = shortsim)
summary(out_lm2)

# Let's use our linear estimator to predict sales in the training sample
shortsim$lmpred <- predict(out_lm2)

# Let's see how predicted sales compares to actual sales
ggplot(shortsim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") 


# Now let's estimate the Bass model using NLS 

# Re-estimate NLS using the restricted sample data 
nls2short <- optim(par = init, fn = bass_sse, N_t = shortsim$N, A = shortsim$A, M = M)

# What did we find?
nls2short$par
# Are these parameter estimates better or worse than the first NLS estimates? Why?

# Let's use our NLS model to predict sales in the short sample
shortsim$NLSInsamplePred<-PredictBassInSampleN(nls2short$par, short_T)

# Let's see how predicted sales compares to actual sales in the restricted sample
ggplot(shortsim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") +
  geom_point(aes(t,NLSInsamplePred), color="purple") 

# How do the R-square statistics compare in the training sample?
nls_insample_r2<-1 - sum((shortsim$N-shortsim$NLSInsamplePred)^2)/sum((shortsim$N-mean(shortsim$N))^2)
nls_insample_r2


# Now let's use the 2 models to retrodict sales after the training data period

# For the quadratic linear model, we need to define a new function, which will 
#   make a series of iterated sales predictions, each building on the previous
lmPredOOS <- function(model,short_T){
  
  # first allocate the return vector in memory
  longpredN <- vector(mode="numeric",length=T)
  
  # Seed the first short_T periods with training data
  longpredN[1:short_T] <- N[1:short_T]
  
  # Seed the accumulated sales variable with the last in-sample training obs
  A <- A[short_T]
  
  # For periods between short_T and T,
  for (i in short_T+1:(T-short_T)){
    
    # Use the most recent A estimate to calculate the model's fitted value
    longpredN[i] <- model$coef[1]+model$coef[2]*A+model$coef[3]*A*A
    
    # Update the accumulated sales with the new predicted sales
    A <- A + longpredN[i]
  }  
  
  return(longpredN)
}

# Use our new function to retrodict using the OLS estimator
LMlong <- lmPredOOS(out_lm2,short_T)

# How do the OLS retrodictions compare to simulated data?
retrodictions <- cbind(t,N,LMlong) |>
  as.data.frame() 

ggplot(retrodictions) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,LMlong), color="green") 


# Now we need a function that uses the NLS estimator to predict out-of-sample
# This function is quite similar to lmPredOOS, but the prediction equation differs
NLSpredOOS <- function(M, p, q, short_T) {
  
  # first allocate the return vector in memory
  longpredN <- vector(mode="numeric",length=T)
  
  # Seed the first short_T periods with training data
  longpredN[1:short_T] <- N[1:short_T]
  
  # Seed the accumulated sales variable with the last in-sample training obs
  A <- A[short_T]
  R <- M-A
  
  for (i in short_T+1:(T-short_T)){
    
    longpredN[[i]] <- (p + q * A / M) * R
    A <- A + longpredN[[i]]
    R <- M - A
  }
  return(longpredN)
}

# Let's predict sales for all T periods given NLS estimates of p and q
retrodictions$NLSlong <- NLSpredOOS(M, nls2short$par[[1]], nls2short$par[[2]], short_T)

# Let's see how predicted sales compares to actual sales in the restricted sample
ggplot(retrodictions) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,LMlong), color="green") +
  geom_point(aes(t,NLSlong), color="purple") 

# Which estimation method looks like it retrodicted better in periods 13-T? 

# Let's sum the square prediction errors for out-of-sample periods
sspeLM <- sum((retrodictions$LMlong[(short_T+1):T]-retrodictions$N[(short_T+1):T])^2)
sspeNLS <- sum((retrodictions$NLSlong[(short_T+1):T]-retrodictions$N[(short_T+1):T])^2)
print(c(sspeLM, sspeNLS))


# Remember the bias-variance tradeoff: Increasing model fit can reduce predictive 
#     performance past some point   
# https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff

# The reason the NLS performs reasonably well, even with just a few data points, is that 
# its structure nests the true data generating process. 

# This is a common tradeoff in analytics: Purpose-built models using relevant theory
# can outperform robust general models with limited training data

# However, in most settings and with enough data, more flexible models will often 
# effectively learn the relevant structure without the need for context-specific
# theoretical assumptions

# Additionally, if the model embeds an irrelevant theory, it can perform worse than 
#     the general model, even with a lot of data. Typically, domain expertise is 
#     needed to inform the theory selection.

# You can vary p, q and short_T, and rerun the script, to learn more about 
# how various parameter values affect the retrodiction exercise

