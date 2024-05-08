## Week 6

rm(list=ls())
setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")

# Today we extend the MNL model from last week to include heterogeneity.

library(tidyverse)
library(mlogit)

# import datasets and functions

# import customer data
cust_dat <- read_csv("../data/smartphone_customer_data.csv", show_col_types = F)
n <- nrow(cust_dat)

# import data prepared for MNL models
load("../data/mnl_datasets.RData")

# import mnl performance functions
load("../data/mnl_performance_functions.RData")


# Calculate market shares

# We did this last class, but it's helpful to have these as reference points

brand_shares <- cust_dat |>
  filter(years_ago == 1) |>
  count(brand) |>
  mutate(shr = n / sum(n))

brand_shares

product_shares <- cust_dat |>
  filter(years_ago == 1) |>
  count(phone_id) |>         # count(a) is similar to group_by(a)|>summarise(n=n())
  mutate(shr = n / sum(n))

product_shares


# We're going to introduce a new graphic to represent demand model predictions

# This graphic will show customer heterogeneity in predicted market shares 

# We'll focus on one phone at a time, starting with A1

# On the x axis, we'll have the customer-specific predicted choice probability 
#     of A1

# Then we'll draw a histogram of how many customers are predicted to 
#     have each choice probability. This will be degenerate for the homogeneous 
#     model (why?) then it will get more interesting with heterogeneity

# We'll look at how these histograms change when different phones are offered on 
#    discount, and as we add heterogeneity to the demand model

myplot <- function(data, model, phone) {
  disc_levs <- c("A1", "A2", "S1", "S2", "H1", "H2", "")

  preds <- as_tibble(predict(model, newdata = data))
  colnames(preds) <- disc_levs[1:6]

  temp <- cust_dat |>
    filter(years_ago == 1) |>
    mutate(discount = factor(discount, levels = disc_levs)) |>
    mutate(discount = fct_recode(discount, "None" = "")) |>
    mutate(pr = preds[[phone]]) |>
    select(discount, pr)

  p <- temp |>
    filter(discount != "None") |>
    filter(pr>0) |>
    ggplot(aes(x = pr)) +
    geom_histogram(aes(fill = discount), binwidth = 0.01) +
    facet_wrap(. ~ discount) +
    xlab(paste("Pr(choose ", phone, ")")) +
    ylab("Count of customers ") +
    ggtitle(paste0("Histogram of Pr(choose ", phone, ") by Discount ")) +
    labs(fill = "Discounted Phone") +
    scale_x_continuous(limits = c(0,.6))+
    theme_bw()

  print(p) |>
    suppressWarnings()
  return(invisible(NULL))
}


# Fit homogeneous model based on product dummies and price from last week

out4 <- mlogit(choice ~ phone_id + price | 0, data = mdat1)

summary(out4)

brand_hit_rate(mdat1, out4)
product_hit_rate(mdat1, out4)
ll_ratio(mdat1, out4)

# Let's look at how histograms of choice probabilities for A1 change when phone 
# discounts change. 
myplot(mdat1, out4, "A1")
# X axis is predicted choice of A1, given a particular phone on discount
# Y axis shows how many customers were treated with each of 6 possible discounts
# Notice: Empirical probability of discount is not equal for all phones
# Run this command to verify:
cust_dat |> filter(years_ago==1) |> select(discount) |> table()

# Main point: Homogeneous demand model predicts a single response of choice 
#     probability to price discount, because it assumes all customers have the 
#     the same price responsiveness alpha. This will change shortly.

# Let's enrich this model to add heterogeneity 
# We'll start out by adding individual heterogeneity by customer attributes
#     via a price*minutes interaction

# What does the distribution of total_minutes look like?

ggplot(sub1, aes(x=total_minutes)) +
  geom_histogram(binwidth = 100) +
  xlab("Total Minutes") +
  ylab("Count of Customers") +
  ggtitle("Histogram of Total Minutes") +
  theme_bw()

# We will hypothesize that customers that use their phones more are people who are
# less price sensitive when buying a phone. To test this, we include the interaction
# of "price" and "total minutes" in the model. 
# We expect to see a positive estimated coefficient for this interaction

out5 <- mlogit(choice ~ phone_id + price + price:total_minutes | 0, data = mdat1)

summary(out5)

brand_hit_rate(mdat1, out5)
product_hit_rate(mdat1, out5)
ll_ratio(mdat1, out5)

# Let's look at how our heterogeneous demand model draws histograms of choice 
#     probabilities for A1 sales change when phone discounts change
myplot(mdat1, out5, "A1")
# We see that different customers have different probabilities of purchasing
# the same phone, even for customers facing the same discount. This is because
# we have included the interaction term in the model. 

# What does it mean to include the interaction?  It means the marginal disutility
# of price is a function of the customer's needs, as reflected in past usage behavior.  
# We can re-write part of model from:
#
# ... (alpha_int * price) + (alpha_minutes * price * totalminutes) ...
#
# to this
#
# ... (alpha_int + alpha_minutes*totalminutes) * price
#
# Recall that alpha_int < 0, so this should clarify that when alpha_minutes is 
# positive, customers that use their phones more are less price sensitive, i.e. 
# they experience less disutility from higher prices than customers who have 
# lower total_minutes

# Let's take a closer look at two customers to see how this heterogeneous model
# predicts choice behavior.

# grab the data for 2 customers (#1 & #13)
x1 <- sub1 |>
  filter(customer_id == 1) |>
  mutate(
    A2 = phone_id == "A2",
    S1 = phone_id == "S1",
    S2 = phone_id == "S2",
    H1 = phone_id == "H1",
    H2 = phone_id == "H2",
    ptm = price * total_minutes
  ) |>
  select(A2, H1, H2, S1, S2, price, ptm) |>
  as.matrix()

x2 <- sub1 |>
  filter(customer_id == 13) |>
  mutate(
    A2 = phone_id == "A2",
    S1 = phone_id == "S1",
    S2 = phone_id == "S2",
    H1 = phone_id == "H1",
    H2 = phone_id == "H2",
    ptm = price * total_minutes
  ) |>
  select(A2, S1, S2, H1, H2, price, ptm) |>
  as.matrix()

# notice how the interaction variable (ptm) is different for the two customers
x1
x2

# This is mainly driven by the variation in total minutes for the two customers.
cust_dat |>
  slice(1, 13) |>      # quick way to grab rows by row_index
  select(total_minutes)
# but also by the different A1 price faced by the two customers

# use our first het demand model to calculate purchase probabilities 
# for the six phones for each of the two customers
beta_hat <- coef(out5)

# let's calculate deterministic utility for each customer and each phone
xb1 <- t(x1 %*% beta_hat)    # t is the transpose function; it prints nicer
xb2 <- t(x2 %*% beta_hat)

# now we'll calculate purchase probability for each customer and each phone using the 
# MNL market share formula
example_shares <- rbind(
  round(exp(xb1) / rowSums(exp(xb1)), 3),
  round(exp(xb2) / rowSums(exp(xb2)), 3)
)

colnames(example_shares) <- c("A1", "A2", "S1", "S2", "H1", "H2")
example_shares

# Notice that the probabilities vary across phones.
# The first customer has a relatively high probability of purchasing the small
# Apple phone (A1), whereas this probability is lower for the second customer.
# Conversely customer 2 has a relatively high probability of purchasing the small 
# Huawei phone (H1), given their relatively high price sensitivity.

# Now back to modeling...

# Approach 2: Add heterogeneity via discrete segment/brand interactions

# compare brand-only model before/after adding discrete segment/brand interactions

out1 <- mlogit(choice ~ apple + samsung | 0, data = mdat1)
summary(out1)
brand_hit_rate(mdat1, out1)
product_hit_rate(mdat1, out1)
ll_ratio(mdat1, out1)

out6 <- mlogit(choice ~ apple:segment + samsung:segment | 0, data = mdat1)
summary(out6)
brand_hit_rate(mdat1, out6)
product_hit_rate(mdat1, out6)
ll_ratio(mdat1, out6)

# Let's focus on the comparison of coefficients between models 1 and 6.

# How do the segments vary in their Apple brand preference? Samsung?
#      (Remember, these are relative to Huawei preference)

# The homogeneous brand preference is like a weighted average of the segment-specific
# preferences, where segment sizes are the weights

# plot model predictions of each customer's A1 prob(choice==A1) with A1 discount
myplot(mdat1, out6, "A1")


# Next let's enrich brand+price model with discrete segment interactions

out2 <- mlogit(choice ~ apple + samsung + price | 0, data = mdat1)
summary(out2)
brand_hit_rate(mdat1, out2)
product_hit_rate(mdat1, out2)
ll_ratio(mdat1, out2)

out7 <- mlogit(choice ~ apple:segment + samsung:segment + price:segment | 0, data = mdat1)
summary(out7)
brand_hit_rate(mdat1, out7)
product_hit_rate(mdat1, out7)
ll_ratio(mdat1, out7)

# Let's focus on price sensitivity. Adding segment-specific price variables to the
# model enables us to estimate separate price sensitivities for each segment.

# How do segments differ in their price sensitivities?
# How do segments' price sensitivities compare to brand preferences?

# plot model predictions of each customer's A1 prob(choice==A1) with A1 discount
myplot(mdat1, out7, "A1")


# compare brand-dummy + price + size model before/after adding segment interactions

out3 <- mlogit(choice ~ apple + samsung + price + screen_size | 0, data = mdat1)
summary(out3)
brand_hit_rate(mdat1, out3)
product_hit_rate(mdat1, out3)
ll_ratio(mdat1, out3)

out8 <- mlogit(choice ~ apple:segment + samsung:segment + price:segment + screen_size:segment | 0, data = mdat1)
summary(out8)
brand_hit_rate(mdat1, out8)
product_hit_rate(mdat1, out8)
ll_ratio(mdat1, out8)

# plot model predictions of each customer's A1 prob(choice==A1) with A1 discount
myplot(mdat1, out8, "A1")


# Let's construct a model with both segment-specific and individual-specific heterogeneity

out9 <- mlogit(choice ~ apple:segment + samsung:segment + screen_size:segment +
                 price:segment + price:total_minutes:segment | 0, data = mdat1)

# Where is the individual specific heterogeneity?

summary(out9)
options(scipen=n)  # let's turn off scientific notation
summary(out9)
brand_hit_rate(mdat1, out9)
product_hit_rate(mdat1, out9)
ll_ratio(mdat1, out9)

# plot each individual's probability of choosing phone A1 on discount
myplot(mdat1, out9, "A1")


# let's incorporate the phone dummies with segment-specific beta parameters
out10 <- mlogit(choice ~ A1:segment + A2:segment + S1:segment +
                  S2:segment + H1:segment+
                 price:segment + price:total_minutes:segment | 0, data = mdat1)
summary(out10)
brand_hit_rate(mdat1, out10)
product_hit_rate(mdat1, out10)
ll_ratio(mdat1, out10)
myplot(mdat1, out10, "A1")


# OK now let's go a little nuts. Let's interact phone dummies with individual
#     heterogeneity. We don't have a good theory why total_minutes would drive
#     demand for individual phone models, but let's see what happens to the 
#     fit statistics



out11 <- mlogit(choice ~ A1:total_minutes:segment + A2:total_minutes:segment + S1:total_minutes:segment 
                + S2:total_minutes:segment + H1:total_minutes:segment 
                + price:segment:total_minutes + price:segment 
                + price:total_minutes 
                | 0, data = mdat1)
summary(out11)
brand_hit_rate(mdat1, out11)
product_hit_rate(mdat1, out11)
ll_ratio(mdat1, out11)
myplot(mdat1, out11, "A1")


# Which models predict better? We might worry about overfitting reducing predictive power

# Let's choose a model based on 10-fold cross validation
# mspe == 'mean square prediction error'

cv_mspe <- function(model, data, k=10, seed=4321) {
    # control psuedo random numbers
    set.seed(seed)
    
    # randomly assign each customer (J rows) to one of k folds
    N <- length(unique(data$customer_id))   # number of customers
    J <- length(unique(data$phone_id))      # number of products
    fold <- rep((1:N) %% k + 1, each=J)    # rep replicates list elements
    # %% is a modulus element, so what we're doing here is computing the remainder when
    # dividing 1:N by (k+1). So mod(customer_id,10)+1 is always an integer between 1
    # and 10. We repeat that 6 times for each customer, to match the mdat1 observations
        
    # preallocate the prediction storage
    preds <- vector(length=nrow(data))     

    # loop over folds 
    for(i in 1:k) {

    # create row indices for training & prediction observations      
        row_ids_train <- fold != i     # which rows to keep in for training
        row_ids_test  <- !row_ids_train  # which rows to hold out for prediction
        
        # fit/train model on training data
        out  <- mlogit(formula(model), data=data[row_ids_train,])
        
        # predict choice probabilities for prediction data
        yhat <- predict(model, newdata = data[row_ids_test,])
        
        # store yhat values for prediction data
        preds[row_ids_test] <- as.vector(t(yhat))
    }
    
    # calculate mse
    mse <- mean((data$choice - preds)^2)
    return(mse)
}

# calculate the MSE's for each model
mspe <- vector(length=11)
mspe[1] <- cv_mspe(out1, mdat1)
mspe[2] <- cv_mspe(out2, mdat1)
mspe[3] <- cv_mspe(out3, mdat1)
mspe[4] <- cv_mspe(out4, mdat1)
mspe[5] <- cv_mspe(out5, mdat1)
mspe[6] <- cv_mspe(out6, mdat1)
mspe[7] <- cv_mspe(out7, mdat1)
mspe[8] <- cv_mspe(out8, mdat1)
mspe[9] <- cv_mspe(out9, mdat1)
mspe[10] <- cv_mspe(out10, mdat1)
mspe[11] <- cv_mspe(out11, mdat1)

# plot the MSE's to compare them
tibble(mod_id=1:11, mspe=mspe) |> 
    ggplot(aes(x=mod_id, y=mspe)) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = 1:11)

  ylim(c(0, .16))  # adjustment to show the abs statistical differences
# that adjustment helps to show why mspe is only 1 factor among several when 
#    choosing a model specification
    
# which model has the lowest cross-validated mean-squared-error?
which.min(mspe)

# What does this tell us?



# Summary of R commands introduced

# one key coding thing we want you to learn:

# mlogit(y ~ x1:x2 | 0, data=yourdata)  # use colons to do interactions


# other commands

# ... many! ...
# don't worry too much about them
# the point is how the models work and how to interpret results

