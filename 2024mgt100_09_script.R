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


# Now let's estimate the Bass model using OLS (the quadratic in A(t))
#    (good deeper dive if you want it: https://rpubs.com/BM07BAM/bass_model )

# run the linear regression: N_t = a + b*A_t + c*A_t^2
out_lm <- lm(N ~ A + I(A^2), data = sim)
summary(out_lm)

# What did we find? How well did the model fit the data?

# Let's use our estimated model to predict sales
sim$lmpred <- predict(out_lm)
# Let's pause for a moment to appreciate R's compact syntax on that

# Let's see how predicted sales compares to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") 


# Now let's estimate the Bass model a second way, using a different approach
# This time we'll use Nonlinear Least Squares and the exact solution

# First let's define our NLS objective that we want to minimize, as a function of p and q
bass_sse <- function(pq, N_t, A_t, M) {
  p <- pq[1]
  q <- pq[2]
  
  N_hat <- M * p + (q - p) * A_t - (q / M) * A_t^2
  
  sse <- sum((N_t - N_hat)^2)
}

# Now we'll choose p and q to minimize the SSE
init <- c(0.1, 0.1)   # this is where we start our search (p=.1, q=.1)
# 'optim' generically minimizes a function 
# optim offers several search routines; defaults to Nelder-Mead https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
# init is the starting point, fn is the function to minimize
out2 <- optim(par = init, fn = bass_sse, N_t = sim$N, A = sim$A, M = M)
# type ?optim to learn more about the optim(ization) function

# What did we find?
out2$par
# How well are the parameters estimated?


# Let's define a function that will predict sales over every period given NLS 
#    estimates of p and q
predN <- function(M, p, q, Nstart, T) {
  
  NN <- vector(mode="numeric",length=T)
  NN[[1]] <- Nstart
  A <- Nstart
  R <- M-A
  
  for (i in 2:T) {
    
    NN[[i]] <- (p + q * A / M) * R
    A <- A + NN[[i]]
    R <- M - A
  }
  return(NN)
}

# Let's predict sales for all T periods given NLS estimates of p and q
sim$NLSpredN <- predN(M, out2$par[[1]], out2$par[[2]], Nstart, T)


# Let's see how predicted sales compares to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") +
  geom_point(aes(t,NLSpredN), color="purple") 

## Which model fits better? In what periods does each model fit better or worse?


## Until now, we have used the entire T dataset to fit both models
## That means we can't do much predictive analytics--we used the entire dataset in estimation

## Now, suppose we only had the first (short_T) periods of data
short_T <- 12

## Suppose we were back to just after period short_T (say, 12), and we wanted to predict 
## the following periods (short_T + 1 , T) of sales.
## Let's consider what estimation method would have yielded more accurate predictions
## This is a retrodiction exercise

# First let's create the restricted training sample 
shortsim <- sim[1:short_T,]

# Let's estimate the Bass model again using the quadratic linear model, using the 
# restricted data

# run the linear regression: N_t = a + b*A_t + c*A_t^2
out_lm2 <- lm(N ~ A + I(A^2), data = shortsim)
summary(out_lm2)

# Let's use our estimated model to predict sales
shortsim$lmpred <- predict(out_lm2)

# Let's see how predicted sales compares to actual sales
ggplot(shortsim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") 


# Now let's estimate the Bass model using NLS and the exact solution

# Re-estimate NLS using the restricted sample data 
nls2short <- optim(par = init, fn = bass_sse, N_t = shortsim$N, A = shortsim$A, M = M)

# What did we find?
nls2short$par

# Let's use our NLS model to predict sales in the short sample
shortsim$NLSpredN <- predN(M, nls2short$par[[1]], nls2short$par[[2]], Nstart, short_T)

# Let's see how predicted sales compares to actual sales in the restricted sample
ggplot(shortsim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") +
  geom_point(aes(t,NLSpredN), color="purple") 


# Now let's use the 2 models to retrodict sales after the restricted sample period

# For the NLS model, we have already written the function, we only need to call it:
NLSlong <- predN(M, nls2short$par[[1]], nls2short$par[[2]], Nstart, T)

# For the quadratic linear model, we need to define a new function, which will 
#   update our sales prediction period-by-period
predlonglm <- function(model,short_T,modelpred){

  # first allocate the return vector in memory
  longpred <- vector(mode="numeric",length=T)

  # Seed the T-period prediction vector with the in-sample data  
  longpred[1:short_T] <- modelpred
  
  # Seed the first accumulated sales 
  A <- longpred[short_T]

  # For periods between short_T and T,
  for (i in short_T+1:T-short_T){

    # Use the most recent A estimate to calculate the model's fitted value
    longpred[i] <- model$coef[1]+model$coef[2]*A+model$coef[3]*A*A

    # Update the accumulated sales with the new predicted sales
    A <- A + longpred[i]
  }  

    return(longpred)
}

# Use our new function to retrodict using linear quadratic estimation
LMlong <- predlonglm(out_lm2,short_T,shortsim$lmpred)

# Gather retrodictions into a dataframe for ggplot
retrodictions <- cbind(t,N,NLSlong,LMlong)
retrodictions <- as.data.frame(retrodictions)

# Let's see how predicted sales compares to actual sales in the restricted sample
ggplot(retrodictions) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,LMlong), color="green") +
  geom_point(aes(t,NLSlong), color="purple") 

# Which estimation method looks like it retrodicted better in periods 13-T? 
# Where did each estimation method retrodict better or worse?

# Let's sum the square prediction errors for out-of-sample periods
sspeLM <- sum((retrodictions$LMlong[(short_T+1):T]-retrodictions$N[(short_T+1):T])^2)
sspeNLS <- sum((retrodictions$NLSlong[(short_T+1):T]-retrodictions$N[(short_T+1):T])^2)
print(c(sspeLM, sspeNLS))

# Let's make vectors of prediction errors so we can graph them
peLM<- retrodictions$LMlong[(short_T+1):T]-retrodictions$N[(short_T+1):T-short_T]
peNLS <- retrodictions$NLSlong[(short_T+1):T-short_T]-retrodictions$N[(short_T+1):T-short_T]
tt <- retrodictions$t[(short_T+1):T] - 0.0
pe <- cbind(tt,peLM,peNLS)
pe <- as.data.frame(pe)

ggplot(pe) +
  geom_point(aes(tt,peLM), color="green") +
  geom_point(aes(tt,peNLS), color="purple") 

# For what values of short_T does NLS retrodict better than OLS?

# We just calculated summed prediction errors for short_T==12
# Let's repeat that exercise for every short_T from 3 to (T-1)

sspeLM_shortT<-vector(mode="numeric",length=(T-1))
sspeNLS_shortT<-vector(mode="numeric",length=(T-1))

# This loop varies short_T values from 3 to (T-1), then repeats ourprevious script contents
# and stores each model's sum of squared prediction errors for each short_T
for (i in 3:(T-1)){
  short_T<-i  
  shortsim <- sim[1:short_T,]
  out_lm2 <- lm(N ~ A + I(A^2), data = shortsim)
  shortsim$lmpred <- predict(out_lm2)
  nls2short <- optim(par = init, fn = bass_sse, N_t = shortsim$N, A = shortsim$A, M = M)
  shortsim$NLSpredN <- predN(M, nls2short$par[[1]], nls2short$par[[2]], Nstart, short_T)
  NLSlong <- predN(M, nls2short$par[[1]], nls2short$par[[2]], Nstart, T)
  LMlong <- predlonglm(out_lm2,short_T,shortsim$lmpred)
  sspeLM_shortT[[i]] <- sum((LMlong[(short_T+1):T]-N[(short_T+1):T])^2)
  sspeNLS_shortT[[i]] <- sum((NLSlong[(short_T+1):T]-N[(short_T+1):T])^2)
} 

# Graph the summed prediction errors as a function of short_T
ii<-1:(T-1)-0.0
gsspe<-as.data.frame(cbind(sspeLM_shortT,sspeNLS_shortT,ii))
ggplot(gsspe) +
  geom_point(aes(ii,sspeLM_shortT), color="green") +
  geom_point(aes(ii,sspeNLS_shortT), color="purple") +
  ggtitle("More training data -> Smaller retrodiction errors: OLS (green) vs NLS (purple)") +
  ylab("Sum of Squared Prediction Errors") +
  xlab("Training data size") 
  
# Remember the bias-variance tradeoff: Model fit can reduce predictive ability   
# https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff

# The reason the NLS performs reasonably well, even with just 6 data points, is that 
# its structure nests the true data generating process. 

# This is a common tradeoff in analytics: Purpose-built models using relevant theory
# can outperform robust general models with limited training data

# However, in most settings and with enough data, more flexible models will often 
# effectively learn the relevant structure without the need for context-specific
# theoretical assumptions

# You can vary p, q and short_T, and rerun the script, to learn more about 
# how various parameter values affect the retrodiction exercise

