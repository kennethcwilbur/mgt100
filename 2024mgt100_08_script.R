## Brand Investments

# Today we use our heterogeneous MNL demand model to assess whether we should
# hire a celebrity to affiliate with our brand.

# We're going to be Samsung in today's example

setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")
rm(list=ls())

library(tidyverse)
library(mlogit)

# Suppose we are Samsung, like last week, with the same market size and marginal costs.
# We are considering hiring our favorite well-known influencer to be a celebrity
# spokesperson for our brand. Should we do this?

# import datasets and functions
cust_dat <- read_csv("../data/smartphone_customer_data.csv", show_col_types = F)
n <- nrow(cust_dat)
load("../data/mnl_datasets.RData")
load("../data/mnl_performance_functions.RData")# import customer data

# fit het mnl model which performed best in 10-fold cross-validation
out10 <- mlogit(choice ~ A1:segment + A2:segment + S1:segment +
                  S2:segment + H1:segment+
                  price:segment + 
                  price:total_minutes:segment | 0, data = mdat1)# get the product names for the 6 products
prod_vec <- c("A1", "A2", "S1", "S2", "H1", "H2")


# calculate predicted profit without celebrity affiliation

# predicted product-specific market shares
pred1 <- colMeans(predict(out10, newdata = mdat1))
names(pred1) <- prod_vec
round(pred1 * 100, 1)

# get S1 and S2 prices
price_S1<- cust_dat |>
  filter(phone_id=="S1",years_ago==1, is.na(discount)) |>
  pull(price) |>
  mean()
price_S2<- cust_dat |>
  filter(phone_id=="S2",years_ago==1, is.na(discount)) |>
  pull(price) |>
  mean()

# store baseline prices and product-specific profits in a little 1x8 tibble
d1 <- tibble(
  price1 = price_S1,
  share1 = pred1[3],
  price2 = price_S2,
  share2 = pred1[4]
)

# assumed market size
M <- 10

# add quantities and revenues
d1 <- d1 |> mutate(
  q1 = share1 * M,
  q2 = share2 * M,
  rev1 = q1 * price1,
  rev2 = q2 * price2
)

# marginal cost for phones S1 and s2, from before
mc1 <- 470
mc2 <- 490

# add total costs and profits
d1 <- d1 |> mutate(
  cost1 = mc1 * q1,
  cost2 = mc2 * q2,
  profit1 = rev1 - cost1,
  profit2 = rev2 - cost2,
  total_profit = profit1 + profit2
)

# Let's look at what we have so far
d1

# Next, let's predict the pure demand effect of a celebrity endorsement

# suppose we learn from a conjoint market research study that our celebrity
# spokesperson will improve our brand image by an amount delta.
# And suppose we have done some work to convert delta into a "CCE"
# that is, an celebrity coefficient effect of size 0.14 utils
# and further, that the CCE applies equally to all 3 consumer segments
# (in reality, celebrity effects will be heterogeneous, but let's keep it simple
# for now)

# let's get an adjusted demand model that accounts for cce
cce <- 0.14
out10_adj <- out10
coef(out10_adj)
# coefficients 7:12 are the ones that relate to S1 and S2 phone utilities
out10_adj$coefficients[7:12] <- out10_adj$coefficients[7:12] + cce
# double check that it worked
coef(out10_adj)-coef(out10)


# now let's update our phone-specific market share predictions
pred2 <- colMeans(predict(out10_adj, newdata = mdat1))
names(pred2) <- prod_vec

# calculate change in predicted market shares. 
shr_change <- pred2 - pred1
names(shr_change) <- prod_vec

# print original predicted market shares, new with-celeb market shares,
# and their difference. What are we expecting to see?
round(pred1 * 100, 1)
round(pred2 * 100, 1)
round(shr_change * 100, 2)
# Did we see what we expect? Are S1 and S2 affected equally? Why?
# What price change does the CCE effect approximate? Do you remember from last week?

# predict profit with celebrity endorsement
d2 <- tibble(
  price1 = 799,
  share1 = pred2[3],
  price2 = 899,
  share2 = pred2[4]
)

# calculate quantities, revenues, costs, and profits
d2 <- d2 |> mutate(
  q1 = share1 * M,
  q2 = share2 * M,
  rev1 = q1 * price1,
  rev2 = q2 * price2,
  cost1 = mc1 * q1,
  cost2 = mc2 * q2,
  profit1 = rev1 - cost1,
  profit2 = rev2 - cost2,
  total_profit = profit1 + profit2
)

# print the result
rbind(d1, d2)

# Let's compare market shares on a graph
pdat <- rbind(
  tibble(celeb = "No", product = prod_vec, share = pred1),
  tibble(celeb = "Yes", product = prod_vec, share = pred2)
)

pdat <- pdat |> mutate(celeb = fct_inorder(factor(celeb))) 
ggplot(pdat) +
  geom_col(aes(product, share, fill = celeb), position = "dodge") +
  ggtitle("Market Shares with and without CCE") +
  ylab("Product-Specific Market Shares") +
  xlab("Product") +
  scale_fill_manual("Celebrity", values = c(No = "Firebrick", Yes = "Dodgerblue4")) +
  theme_bw()


# Find new optimal prices conditioning on CCE

# this mimics what we did last week, but instead of searching for the best price
# for one phone, we will search for the best prices for both Samsung phones at the
# same time. In other words, a 2-dimensional grid search

# get a vector of price changes to use for each phone
pvec <- seq(from = -100, to = 100, by = 10)

# get all combinations of price changes for the two phones
res <- expand.grid(pvec, pvec)
View(res)

# Construct empty matrix to store shares at each price
smat <- matrix(NA_real_, nrow = nrow(res), ncol = 6)
colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")

res <- cbind(res, smat)

# loop over the price change values
for (i in 1:nrow(res)){

  # print progress
  cat("Working on", i, "of", nrow(res), "\n")

  # get the price change amount for this gridpoint
  p1 <- res[i, 1]
  p2 <- res[i, 2]

  # change prices for S1 phones
  tempdat <- as_tibble(mdat1)
  tempdat <- tempdat |> mutate(price = ifelse(phone_id == "S1", price + p1, price))
  tempdat <- tempdat |> mutate(price = ifelse(phone_id == "S2", price + p2, price))

  # make market share predictions with CCE and counterfactual S1 and S2 prices
  preds <- predict(out10_adj, newdata = tempdat)

  # calculate and store market shares
  res[i, 3:8] <- colMeans(preds)
}

# gather prices and estimated shares into a dataframe
d3 <- tibble(
  scenario = 1:nrow(res),
  price1 = res[, 1] + 799,
  share1 = res[, 5],
  price2 = res[, 2] + 899,
  share2 = res[, 6]
)

# add components of profit function to the dataframe
d3 <- d3 |> mutate(
  q1 = share1 * M,
  q2 = share2 * M,
  rev1 = q1 * price1,
  rev2 = q2 * price2,
  cost1 = mc1 * q1,
  cost2 = mc2 * q2,
  profit1 = rev1 - cost1,
  profit2 = rev2 - cost2,
  total_profit = profit1 + profit2
)

# plot heat map of profit, inlcuding profit-maximizing scenario
ggplot(d3) +
  geom_tile(aes(x = price1, y = price2, fill = total_profit)) +
  scale_fill_gradient(low = "white", high = "blue") +
  xlab("S1 Price") +
  ylab("S2 Price") +
  ggtitle("Heat Map of Total Profits by S1 and S2 Prices") +
  geom_point(data = d3 |> filter(total_profit == max(total_profit)), aes(price1, price2), color = "white") +
  geom_point(data = d2, aes(price1, price2), color = "red") +
  theme_bw()

# select profit-maximizing price combination and compare to prior calculations
d3 <- d3 |>
  filter(total_profit == max(total_profit)) |>
  select(-scenario)
rbind(d1, d2, d3)


# Make the business decision of whether to hire celeb or not

# calculate willingness to pay to hire the celeb (with price optimization)
d3$total_profit - d1$total_profit

# How did price re-optimization change our wtp to hire the celeb?

# What if celeb threatens to affiliate with Huawei instead of us? 
# How would that affect our wtp?
# How could we calculate that?

# Summary of R commands introduced

# none!
