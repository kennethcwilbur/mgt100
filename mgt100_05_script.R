## Week 5

# Today we fit a "workhorse" model in marketing and economics, the multinomial
# logit (MNL) model. We begin with the case of a homogeneous MNL model.

setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")

install.packages("mlogit")

library(tidyverse)
library(mlogit)

# import datasets

# In order to fit our Multinomial Logit (MNL) model, we need to know
# (1) what the customer chose and (2) the set of options from which the
# customer made his or her choice.  That's because MNL estimates demand by comparing 
# chosen options to unchosen options. We'll need to wrangle data to
# facilitate those comparisons

# import customer data
cust_dat <- read_csv("../data/smartphone_customer_data.csv", show_col_types = F)
n <- nrow(cust_dat)

# replace missing 'discount' values (currently NA) with empty string ("")
cust_dat <- cust_dat %>% mutate(discount = ifelse(is.na(discount), "", discount))

# We need to do some extensive data wrangling for the mlogit package
# We'll need customer segments next week, we'll identify those now, so we don't 
# have to re-wrangle the data next week
set.seed(1357)   # this sets random kmeans starting points to be deterministic
subk <- cust_dat %>% select(gaming, chat, maps, video, social, reading)
outk <- kmeans(subk, centers = 3, nstart = 10)
table(outk$cluster)
cust_dat$segment <- factor(outk$cluster)
rm(subk, outk)

# import phone attributes
phone_dat <- read_csv("../data/phone_dat.csv", show_col_types = F)


# create dataset for mnl

# Now we need to combine these datasets.  Our customer data has "n" rows and for
# each customer, there were 6 phones available on the market when the customer
# purchased their phone.  So we will construct a dataset that has n*6 rows. This 
# facilitates mlogit calculating a predicted utility for each available option for
# each available customer

# We're going to do this in three steps.

# Step 1: loop over customers and create a dataset of the 6 available phones
# for that customer. This is a very flexible way to structure the data.  It
# will be useful to us because we may need to adjust the price of a phone
# if it was on discount, and this adjustment is customer-specific. More
# generally, this is a good way to structure data for a MNL model since it would
# allow different customers to choose from different sets of products.

# step 2: we will stack (ie append) these n datasets on top of each other to
# create the dataset with the n*6 rows.

# Step 3: the 'mlogit' package that fits the MNL model requires us to create
# an mlogit-data object, so we'll do that, and then we'll feed that mlogit-dataset
# object into the mlogit() function to estimate the parameters of this model.

# ++++++++++
# Step 1
# ++++++++++

# So, let's loop over customers:

# create an empty list to store the n datasets (each dataset will have 6 rows)
dat_list <- vector(mode = "list", length = n)

# initialize progress bar
# this loop may be a little slow, so let's have a progress bar show us
# how fast the code is running and how long until the loop finishes
pb <- txtProgressBar(min = 1, max = n, style = 3)

# loop for step 1
for (i in 1:n) {
  # get cohort, minutes, brand, and size for customer i
  i_cohort <- cust_dat %>%
    slice(i) %>%   # this takes row (i) from cust_dat
    pull(years_ago)   # this takes the value of years_ago from row (i)
  i_brand <- cust_dat %>%
    slice(i) %>%
    pull(brand)
  i_size <- cust_dat %>%
    slice(i) %>%
    pull(screen_size)
  i_discount <- cust_dat %>%
    slice(i) %>%
    pull(discount)
  i_segment <- cust_dat %>%
    slice(i) %>%
    pull(segment)
  i_minutes <- cust_dat %>%
    slice(i) %>%
    pull(total_minutes)

  # subset the phone data to the 6 phones for the year when the customer purchased
  PD <- phone_dat %>% filter(years_ago == i_cohort)

  # adjust one of the phone's price for the 10% discount, if applicable
  PD <- PD %>% mutate(price = price - (phone_id == i_discount) * price * 0.1)

  # add customer id to PD
  PD <- PD %>% mutate(customer_id = i)

  # convert the one brand variable into a set of 3 brand dummies (ie, binary variables)
  PD <- PD %>% mutate(
    apple = as.integer(brand == "apple"),
    huawei = as.integer(brand == "huawei"),
    samsung = as.integer(brand == "samsung")
  )

  # create a binary variable to indicate the chosen phone
  # this is going to be the dependent variable in the MNL model (like "y" in OLS)
  PD <- PD %>%
    mutate(choice = (brand == i_brand) & (screen_size == i_size)) %>%
    mutate(choice = as.integer(choice))

  # add segment and total_minutes
  PD <- PD %>% mutate(segment = i_segment, total_minutes = i_minutes)

  # store this 6-row dataset in the i'th position of that list we initialized before the loop
  dat_list[[i]] <- PD %>% select(
    customer_id, phone_id, choice,
    apple, huawei, samsung,
    price, screen_size,
    segment, total_minutes
  )

  # update the progress bar to show progress
  setTxtProgressBar(pb, i)
}

# clean up -- delete temporary objects from the loop that we don't need anymore
rm(i, i_cohort, i_brand, i_size, i_discount, i_segment, i_minutes, PD, pb)

# Let's take a look at two (out of the n) 6-row datasets:
dat_list[1]
dat_list[100]

# ++++++++++
# Step 2
# ++++++++++

# Now we will stack the n 6-row customer-specific dataframes into one big
# dataframe (that will have n*6 rows)

# rbind operates on dataframes to concatenate rows
# we use do.call in order to concatenate rows within lists
mnl_dat <- as_tibble(do.call(rbind, dat_list))

rm(dat_list)

# Let's see how this n*6 row dataframe looks
head(mnl_dat, n = 20)

# We will estimate demand for each year separately, since customer preferences may
# have changed across product generations

# Let's split the big (n*6 row) dataframe into 3 dataframes, one for each year.
sub1 <- mnl_dat %>% filter(customer_id %in% which(cust_dat$years_ago == 1))
sub2 <- mnl_dat %>% filter(customer_id %in% which(cust_dat$years_ago == 2))
sub3 <- mnl_dat %>% filter(customer_id %in% which(cust_dat$years_ago == 3))

# ++++++++++
# Step 3
# ++++++++++

# Here, we will convert the 3 'sub' dataframes into mlogit.data objects.
# To do that, we have to specify the y variable (choice), whether our datasets
# have 6 times as many rows as the original data (shape="long") or 6 times as
# many columns (shape="wide"), and the id variable that groups the set of
# phones from one choice-occasion together (our "customer_id" variable).

mdat1 <- mlogit.data(sub1, choice = "choice", shape = "long", chid.var = "customer_id")
mdat2 <- mlogit.data(sub2, choice = "choice", shape = "long", chid.var = "customer_id")
mdat3 <- mlogit.data(sub3, choice = "choice", shape = "long", chid.var = "customer_id")

# Let's save these datasets to our hard disks so we don't need to re-run the big
# for-loop above in order to work with these data in the future

# The save() commands lets us save multiple R objects into one file on disk.
# We can then import these saved object in the future using the load() command.

save(sub1, sub2, sub3, mdat1, mdat2, mdat3, file = "../data/mnl_datasets.RData")

# For the rest of this script, we'll focus on the customer that bought phones
# last year (ie, where "years_ago" == 1).


# Calculate market shares

# As a point of comparison that we'll use later to understand the MNL model,
# we calculate product-level and brand-level market shares:

brand_shares <- cust_dat %>%
  filter(years_ago == 1) %>%
  count(brand) %>%
  mutate(shr = n / sum(n))

brand_shares

product_shares <- cust_dat %>%
  filter(years_ago == 1) %>%
  count(phone_id) %>%
  mutate(shr = n / sum(n))

product_shares


# Fit basic (brand-intercept only) model

# Always start simple. For our first model, we will fit a model where our "X"
# variables are just the binary dummy variables that indicate brand. We need
# to leave out one phone as a "baseline" and omit an intercept, so that this
# model is "identified" (ie, can be estimated). We omit the intercept by
# including the bar-zero ("|0") in the formula:

out1 <- mlogit(choice ~ apple + samsung | 0, data = mdat1)

summary(out1)

# We see that the coefficients for the Apple and Samsung brand dummies are
# positive and statistically significantly different from zero. Those are in
# comparison to the Huawai coefficient which is restricted to zero for identification.
# But what do those parameters mean?

# We'll use these coefficients to calculate the model's estimate of brand-level
# market shares. We'll learn two things by doing so.

# print the coefficients
coef(out1)

# print the brand market shares estimated from the model
coefs1 <- c(huawei = 0, coef(out1))
shares1 <- exp(coefs1) / sum(exp(coefs1))
round(shares1, 3)

# print the actual brand market shares
brand_shares

# print the actual product market shares
product_shares

# clean up
rm(coefs1, shares1)

# First, we learn that the model fits the intercepts in order to **exactly**
# match the brand-level market shares from the data. However, it does not match
# the product-level market shares.

# Second, and this is more subtle but general, the sign and magnitude of the
# coefficients inform of us the impact on estimated market shares: larger positive
# coefficients predict larger market shares. Apple has the largest coefficient
# and thus the largest estimated market share.

# Before moving on, let's calculate two measures of model fit/performance.
# We'll use custom functions to make the calculations easy to repeat.

# The first is the "hit rate" which is the percent of choices the model
# correctly predicts.  We'll create custom functions for the brand hit rate
# and the product hit rate. This measure is something that you would
# commonly encounter in industry, as it has a straightforward interpretation.

brand_hit_rate <- function(data, model) {
# here we use the model to predict which phone maximizes each customer's utility
  preds <- apply(predict(model, newdata = data), 1, which.max)
# here we construct a vector of customer choices for comparisons to predictions
  actuals <- apply(matrix(data$choice, ncol = 6, byrow = T), 1, which.max)
# here we compare the model's predictions to the data
  mean(ceiling(preds / 2) == ceiling(actuals / 2))
}

# now we'll do the same steps but at the phone level
product_hit_rate <- function(data, model) {
  preds <- apply(predict(model, newdata = data), 1, which.max)
  actuals <- apply(matrix(data$choice, ncol = 6, byrow = T), 1, which.max)
  mean(preds == actuals)
}

# The second measure of model fit is the likelihood ratio index 
# ( also called McFadden's pseudo # R-squared). 
# Like R^2 from linear regression, this metric ranges from
# zero to one, and the interpretation is the degree of improvement over the
# random guessing about consumer choices

ll_ratio <- function(data, model) {
  N <- nrow(model$probabilities)
  J <- ncol(model$probabilities)
  ll0 <- N * log(1 / J)   # this is our null model for comparison
  ll1 <- as.numeric(model$logLik)   # this is lnL(beta) from slides
  1 - ll1 / ll0
}

# Let's save these functions for future use

save(brand_hit_rate, product_hit_rate, ll_ratio, file = "../data/mnl_performance_functions.RData")

# Let's calculate the brand hit rate and the likelihood ratio index for
# our mnl model. 
brand_hit_rate(mdat1, out1)
product_hit_rate(mdat1, out1)
ll_ratio(mdat1, out1)

# The simple/naive "model" is that each brand is chosen 33.3% (=1/3).
# Our brand hit rate is about 35.6%, which is just a bit better than the naive approach.
# The likelihood ratio index confirms that the model is not much better than random guessing.


# One way we can improve a model's performance is to give it more complete data. 

# Let's add the price variable to the model and see what happens:

out2 <- mlogit(choice ~ apple + samsung + price | 0, data = mdat1)

summary(out2)

# First, notice that the magnitude of Apple and Samsung coefficient increased
# quite a bit. This indicates that, while ***holding price constant***, Apple
# and Samsung are preferred over Huawei. We saw that Apple and Samsung have
# larger market shares over Huawei, which result from both a brand effect and
# a price effect. Huawei phones are priced lower than Apple and Samsung phones.
# Thus the approximately-equal market shares result (in part) from a
# positive brand effect for Apple and Samsung relative to Huawei, which is
# tempered by a negative price effect because Apple and Samsung are priced higher.

# Second, we see that the price coefficient is negative, indicating that higher
# prices lead to lower utility and lower market shares. This makes sense.

# Third, p-values are smaller. This may suggest that this model fits the data
# better than the previous model.

# Let's test that "better fitting" hypothesis by calculating the hit rates and
# likelihood ratio index for this model.

brand_hit_rate(mdat1, out2)
product_hit_rate(mdat1, out2)
ll_ratio(mdat1, out2)

# We get a small improvement in brand hit rate which is now 35.6%, compared to
# the prior model's brand hit rate of 35.5%.

# We get a product hit rate of 24.8%, better than simpler model's product hit rate 
# of 24.8%

# That improvement is noticeable in the likelihood ratio statistic. .037 is much better
# than our previous fit of .002


# Let's see what has happened to our brands' market share predictions
# We have to predict phone shares, then we'll sum over phones to predict brand shares

# Here we predict phone shares
shares2p <- colMeans(predict(out2, newdata = mdat1))
names(shares2p) <- sub1 %>%
  head(6) %>%
  pull(phone_id)

# here we sum over phones to predict brand shares
shares2b <- colSums(matrix(shares2p, nrow = 2))
names(shares2b) <- c("apple", "samsung", "huawei")
round(shares2b, 3)
brand_shares

# ...we still exactly match actual brand-level market shares

round(shares2p, 3)
product_shares

# ...and now we have product-level market share estimates that better reflect
# the actual product-level market shares, albeit not perfectly.
# That's because we don't have any product-specific attributes or dummies.


# Let's improve the model further. We'll fit MNL with brand, price, and size

out3 <- mlogit(choice ~ apple + samsung + price + screen_size | 0, data = mdat1)

summary(out3)

brand_hit_rate(mdat1, out3)
product_hit_rate(mdat1, out3)
ll_ratio(mdat1, out3)

# The coefficient on the "size" variable is not statistically significant and
# our hit rates are essentially unchanged.  This suggests that "size" may not be
# adding much to the model.  Perhaps this is because screen size and price are
# highly correlated variables, and as a result, most of the information in the
# size variable is already included in the model via the price variable.

# However, note that the price coefficient increased from -0.006 to -0.005. This
# indicates that screen size is an (unobserved in the prior model) attribute that
# affects price. Thus, the prior model may have been affected by endogeneity.

# Specifically, the prior price coefficient of -0.006 was not a "pure" price effect.
# Instead it included both the true price effect and a screen size effect.
# Once we controlled for screen size, the price coefficient changed to -0.005.

# If we recall our market mapping exercise, we might recall that Samsung's large 
# phone had a very big screen, and a low market share. Maybe we should estimate
# phone-specific intercepts rather than specifying a common screensize parameter
# that applies to all phones



# Now let's fit the MNL model with product-specific intercepts and price

# Now, instead of brand dummy variables, let's use product dummy variables.
# So there will be 5 dummies (one phone has to be set to 0 for identification).
# Notice that because size does not vary for a given phone, we cannot include
# it in the model because it would be perfectly collinear with the phone dummies.

out4 <- mlogit(choice ~ phone_id + price | 0, data = mdat1)

summary(out4)

# Notice that many of the coefficients are negative. This is because the small
# Apple phone is the reference product (simply because it's listed first), so
# all phones with smaller market shares than Apple small have lower parameter estimates

# Notice also that our price coefficient has changed yet again to -0.007. Specifically,
# the screen size variable in the prior model was capturing the average effect
# of screen size across the 3 brands.  Now, we have specified a more flexible model
# in which screen size and all other product-specific differences are accounted
# for by the product dummies

# Let's also review our fit metrics

brand_hit_rate(mdat1, out4)
product_hit_rate(mdat1, out4)
ll_ratio(mdat1, out4)

# The brand hit rate improved 1% from 35.5% to 36.7%.
# The product hit rate improved 2% from 24.8% to 26.9%.
# The improvement comes from the flexibility of the model to allow for different
# preferences for small and large phones *within* a brand.
# LL Ratio is now up to .042, 21x larger than the .002 in the brand-only model

# Let's check the brand-level market shares

shares4p <- colMeans(predict(out4, newdata = mdat1))
names(shares4p) <- sub1 %>%
  head(6) %>%
  pull(phone_id)

shares4b <- colSums(matrix(shares4p, nrow = 2))
names(shares4b) <- c("apple", "samsung", "huawei")

round(shares4b, 3)
brand_shares

# we still exactly match actual brand-level market shares

round(shares4p, 3)
product_shares

# and now we are able to exactly match product-level market shares.


# What's next

# Our model is now performing well *in the aggregate*.  But we have not yet
# assessed how it does for each individual.  Adding customer-specific
# heterogeneity to our model is the next step. We'll focus on that next week.





# Summary of R commands introduced

# the ones you should know

# mlogit.data(data, choice, shape, chid.var)    # data prep for mlogit
# mlogit(y ~ x1 + x2 | 0, data=yourdata)        # estimate an MNL model
# summary()                                     # view model results
# predict()                                     # get fitted or predicted values

# also the new tidyverse commands

# slice(i)           # grabs row i
# pull(j)            # grabs column j

# other commands

# ... many! ...
# don't worry too much about them
# the point is how the model works and how to interpret results
# the point is not all the extra coding to demonstrate it
