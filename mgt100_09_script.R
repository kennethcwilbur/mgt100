## Week 9

# clear workspace
rm(list=ls())
setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")

# Today we fit a Bass Model to estimate new product diffusion.
# The context is immaterial, but we could suppose the product is virtual reality 
#    goggles, like the Oculus

library(tidyverse)

# --------------------
# BASS MODEL
# --------------------

#choose true parameters, we'll use these to simulate data
M<-100
p<-.02     # coefficient of innovation
q<-.2      # coefficient of imitation
Nstart <- p  # Number of sales in period 1

# Note: We'll denote sales as N because that's easier to write than dA/dt

# allocate memory
N <- vector(mode="numeric",length=40)
A <- vector(mode="numeric",length=40)
R <- vector(mode="numeric",length=40)
t <- vector(mode="numeric",length=40)

# initialize vectors for period 1
N[[1]] <- Nstart
A[[1]] <- N[[1]]
R[[1]] <- M-N[[1]]
t[[1]] <- 1.0
  
#simulate data for subsequent periods
for (i in 2:40){
  
  t[i] <- i*1.0
  # Bass model formula:
  N[i] <- ( p + q * A[[i-1]] / M ) * R[[i-1]]
  # Update accumulated sales
  A[i] <- A[[i-1]] + N[[i]]
  # Update Remaining market size
  R[i] <- M-A[[i]]
}

# combine simulated data into a dataframe
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


# Now let's estimate the Bass model using the quadratic in A(t)
#    (good deeper dive if you want it: https://rpubs.com/BM07BAM/bass_model )

# run the linear regression: N_t = a + b*A_t + c*A_t^2
out_lm <- lm(N ~ A + I(A^2), data = sim)
summary(out_lm)

# How well did the model fit the data?

# Let's use our estimated model to predict sales
sim$lmpred <- predict(out_lm)
# Let's pause for a moment to appreciate R's compact syntax on that

# Let's see how predicted sales compares to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") 


# Now let's estimate the Bass model using Nonlinear Least Squares and the exact solution

# First let's define our NLS objective that we want to minimize, as a function of p and q
bass_sse <- function(pq, N_t, A_t, M) {
  p <- pq[1]
  q <- pq[2]
  
  N_hat <- M * p + (q - p) * A_t - (q / M) * A_t^2
  
  sse <- sum((N_t - N_hat)^2)
}

# Now we'll choose p and q to minimize the SSE
init <- c(0.1, 0.1)   # this is where we start our search (p=.1, q=.1)
out2 <- optim(par = init, fn = bass_sse, N_t = sim$N, A = sim$A, M = M)

# What did we find?
out2$par


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

# Let's predict sales for all 40 periods given NLS estimates of p and q
sim$NLSpredN <- predN(M, out2$par[[1]], out2$par[[2]], Nstart, 40)


# Let's see how predicted sales compares to actual sales
ggplot(sim) +
  geom_point(aes(t,N), color="black") +
  geom_point(aes(t,lmpred), color="green") +
  geom_point(aes(t,NLSpredN), color="purple") 

## Which model fits better? In what periods does each model fit better or worse?


## Until now, we have used the entire 40 dataset to fit both models
## That means we can't do much predictive analytics--we used the entire dataset in estimation

## Now, suppose we only had the first (short_T) periods of data
short_T <- 12

## Suppose we were back to just after period short_T (say, 12), and we wanted to predict 
## the following periods (short_T + 1 , 40) of sales.
## Let's consider what estimation method would have yielded more accurate predictions

## This is called 'retrodiction' -- as in 'retroactive predictions'
## Common standard to evaluate and compare models' out-of-sample predictive performance 
## Especially for time-series models

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
NLSlong <- predN(M, nls2short$par[[1]], nls2short$par[[2]], Nstart, 40)

# For the quadratic linear model, we need to define a new function, which will 
#   update our sales prediction period-by-period
predlonglm <- function(model,short_T,modelpred){

  # first allocate the return vector in memory
  longpred <- vector(mode="numeric",length=40)

  # Seed the 40-period prediction vector with the in-sample data  
  longpred[1:short_T] <- modelpred
  
  # Seed the first accumulated sales 
  A <- longpred[short_T]
  
  # For periods between short_T and 40,
  for (i in short_T+1:40-short_T){

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

# Which estimation method looks like it retrodicted better in periods 13-40? 
# Where did each estimation method retrodict better or worse?

# Let's sum the square prediction errors for out-of-sample periods
sspeLM <- sum((retrodictions$LMlong[(short_T+1):40]-retrodictions$N[(short_T+1):40])^2)
sspeNLS <- sum((retrodictions$NLSlong[(short_T+1):40]-retrodictions$N[(short_T+1):40])^2)
print(c(sspeLM, sspeNLS))

peLM<- retrodictions$LMlong[(short_T+1):40]-retrodictions$N[(short_T+1):40-short_T]
peNLS <- retrodictions$NLSlong[(short_T+1):40-short_T]-retrodictions$N[(short_T+1):40-short_T]

tt <- retrodictions$t[(short_T+1):40] - 0.0
pe <- cbind(tt,peLM,peNLS)
pe <- as.data.frame(pe)

ggplot(pe) +
  geom_point(aes(tt,peLM), color="green") +
  geom_point(aes(tt,peNLS), color="purple") 

# Now, change short_T to 8 and re-run the script; what happens?

# Now, change short_T to 5 and re-run the script; what happens?

# Now, change short_T to 18 and re-run the script; what happens?

# The fundamental cause is the bias-variance tradeoff, along with the exact solution
# built into the NLS model, whereas the linear quadrati has to approximate the process. 
# Let's discuss. Learn more here: https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff

# A generalizable takeaway: actions you take to increase R-sq can reduce the model's
# predictive ability. You need to take this seriously if you care about prediction.

# You can also vary p, q and short_T, and rerun the script, to learn more about 
# how these properties manifest within this example

# Note: we can also do this comparison by feeding in short_T period data on A and N
# into the models, then running that forward until period 40. The results are similar.