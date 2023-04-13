## Week 7

# Today we use the heterogeneous MNL from week 6 to optimize prices.

setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")

library(tidyverse)
library(mlogit)
library(gridExtra) # <-- for showing mutiple ggplots at the same time

# We'll start out by loading our data, getting the segments and fitting our 
# heterogeneous demand model. We need that to search for optimal prices

# import customer data
cust_dat <- read_csv("../data/smartphone_customer_data.csv", show_col_types = F)

# import data prepared for het MNL models
load("../data/mnl_datasets.RData")

# fit het mnl model
out <- mlogit(choice ~ apple:segment +
  samsung:segment +
  price:segment +
  screen_size:segment +
  price:total_minutes:segment | 0, data = mdat1)


# Demand curve for a phone

# Let's estimate the residual demand curve for the small Samsung phone (S1).
# That means we will vary the price of S1 while holding everything else constant,
# and track how S1's market share changes as we change its price.

# Recall that last year S1 had a price of $799 and a market share of 25.3%.

# We'll check prices between $599 and $999, in increments of $10
pvec <- seq(from = -200, to = 200, by = 10)

# We'll store market share predictions here in this empty matrix at each price
smat <- matrix(NA, nrow = length(pvec), ncol = 6)
colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")

# loop over the price change values
for (i in 1:length(pvec)) {

  # print progress
  cat("Working on", i, "of", length(pvec), "\n")

  # get the price change amount
  p <- pvec[i]

  # change prices but only for S1 phones
  tempdat <- as_tibble(mdat1) |> mutate(price = ifelse(phone_id == "S1", price + p, price))

  # make market share predictions with the temporarily-changed S1 prices
  preds <- predict(out, newdata = tempdat)
  ## Let's take a moment to appreciate how compact that one line of code is.
  ## What did it accomplish?
  ## Specialized design features like this help explain why many analytics professionals use R
  
  # calculate and store market shares
  smat[i, ] <- colMeans(preds)
}

# What's in smat? Does it make sense?
View(smat)
# Can you see what the optimal price for S1 is going to be?

# gather our prices and estimated shares into a dataframe
relcol <- which(colnames(smat) == "S1")   # "which" picks elements that are true
s1dat <- tibble(scenario = 1:length(pvec), price = pvec + 799, share = smat[, relcol])

# Looking at your raw data is a very good habit to cultivate
View(s1dat)
# Look OK?

# plot S1's inverse residual demand curve
ggplot(s1dat, aes(x = share, y = price)) +
  geom_point() +
  geom_line() +
  labs(x = "Share", y = "Price") +
  theme_bw()

# Congratulations! You just estimated your first demand curve!

# Note : we call it "residual" demand when it's product-specific and takes other 
# factors as given. That acknowledges that when other factors change, like say 
# prices of competing products A1 or H1, then S1 residual demand curve would change also.
 
# Notice that the model-predicted market share is not exactly the observed market share,
# because we have brand-specific coefficients but not product-specific coefficients.
# Just something to be aware of. We haven't optimized this demand model.

# actual market shares
cust_dat |>
  filter(years_ago == 1) |>
  count(phone_id) |>
  mutate(shr = n / sum(n))

# predicted market shares at 0 price change
smat[21, ] |> round(3)


# Convert shares to number of phones

# Suppose the US smartphone market size is 150 million units sold per year
# and further suppose that the college-age demographic that we've measured with our
# dataset comprises 1 out of every 15 smartphone sales, or 10 million phones.

M <- 10    # measured in millions

# Let's scale our demand curve to be in the price-quantity space instead of the price-share space

s1dat <- s1dat |> mutate(quantity = share * M)

ggplot(s1dat, aes(x = quantity, y = price)) +
  geom_point() +
  geom_line() +
  labs(x = "Quantity", y = "Price") +
  theme_bw()


# Let's consider pricing S1 based on own-price elasticity to maximize contribution 

# Marginal cost

# We need cost data else we cannot measure margin or contribution. Suppose a Samsung manager 
# informs us that MC is a constant $470 per S1 phone, invariant to quantity produced.
# (((Is this realistic? Let's discuss. Hint: SR v LR)))
# (((What would we do if it were convex or concave?)))

mc1 <- 470

# Calculate own-price elasticity at +/- $10 from actual price of $799

p1 <- s1dat |>
  slice(20) |>     # quick way to filter a row based on row index
  pull(price)       # quick way to select a single column
q1 <- s1dat |>
  slice(20) |>
  pull(quantity)

p2 <- s1dat |>
  slice(22) |>
  pull(price)
q2 <- s1dat |>
  slice(22) |>
  pull(quantity)

# this is an approximate price elasticity based on a $20 range around observed price, 
# from $789 to $809
elasticity <- ((q2 - q1) / q1) / ((p2 - p1) / p1)
elasticity

# Approximate optimal price using Tucker's elasticity heuristic
mc1 * 1 / (1 - 1 / abs(elasticity))

# this approach suggests that S1 price should be set much lower ($649) than its
# current value ($799). But, it is based on an assumption of constant demand elasticity,
# whereas our estimated demand model does not restrict price elasticity to be constant

# Let's calculate an arc elasticity over the full range of prices considered, from $599 to $999
p1 <- s1dat |>
  slice(1) |>
  pull(price)
q1 <- s1dat |>
  slice(1) |>
  pull(quantity)

p2 <- s1dat |>
  slice(41) |>
  pull(price)
q2 <- s1dat |>
  slice(41) |>
  pull(quantity)

elasticity <- ((q2 - q1) / q1) / ((p2 - p1) / p1)
elasticity
# how does it compare to the narrow arc elasticity?
# Note: arc elasticities are always approximations. Approximation error will depend 
# on the curvature of the demand function, which is based both on functional form 
# assumptions (e.g., constant elasticity, linear, mnl, etc) and also on the data.

# Let's calculate more accurate point elasticities at each of the 41 price points 
# we considered, and then we'll have a distribution of price elasticities
res_e <- vector(length = 39)
for (i in 2:40) {
  p1 <- s1dat |>
    slice(i - 1) |>
    pull(price)
  q1 <- s1dat |>
    slice(i - 1) |>
    pull(quantity)

  p2 <- s1dat |>
    slice(i + 1) |>
    pull(price)
  q2 <- s1dat |>
    slice(i + 1) |>
    pull(quantity)

  res_e[i - 1] <- ((q2 - q1) / q1) / ((p2 - p1) / p1)
}
summary(res_e)
# How much does price elasticity change along the demand curve?
# Should we assume constant elasticity?

# New approach: Grid search.
# Calculate S1 price to maximize S1 profit based on share predictions at each price point

# Calculate revenue at each price point

s1dat <- s1dat |> mutate(revenue = price * quantity)

p1 <- ggplot(s1dat) +
  geom_point(aes(x = quantity, y = price)) +
  theme_bw()
p2 <- ggplot(s1dat) +
  geom_point(aes(x = quantity, y = revenue)) +
  theme_bw()

grid.arrange(p2, p1, ncol = 1)

# notice that revenue increases, but starts to flatten out, as quantity increases

# margin

s1dat <- s1dat |> mutate(cost = mc1 * quantity)

p3 <- ggplot(s1dat) +
  geom_point(aes(x = quantity, y = cost)) +
  theme_bw()

grid.arrange(p3, p2, p1, ncol = 1)

# note how cost is linear, so unlike revenue, it does not start to flatten out
# at higher quantities

# profit, at a particular price, is the distance between the revenue and cost
# curves. Let's look at that distance graphically

ggplot(s1dat) +
  geom_line(aes(x = quantity, y = revenue), color = "blue") +
  geom_point(aes(x = quantity, y = revenue), color = "blue") +
  geom_line(aes(x = quantity, y = cost), color = "red") +
  geom_point(aes(x = quantity, y = cost), color = "red") +
  theme_bw()

# the distance/gap is largest somewhere around a quantity of 3-3.5 million phones,
# which roughly corresponds to price in $700-750.  Let's now calculate
# profit and the profit maximizing price more exactly.

# profit

s1dat <- s1dat |> mutate(profit = revenue - cost)

p4 <- ggplot(s1dat) +
  geom_point(aes(x = quantity, y = profit)) +
  theme_bw()

grid.arrange(p4, p3, p2, p1, ncol = 1)


# find S1-profit-maximizing quantity and price

s1dat |> filter(profit == max(profit))    # T/F condition filters row with max profit

# We see that the profit-maximizing price of $709 yields $834 million in contribution

# This demonstration has focused on finding the profit-maximizing price
# for phone S1, where we have only considered the profits of phone S1.

# Samsung, however, cares about the profit from its smartphone product line
# in total, and low S1 prices may cannibalize sales from its companion product S2.
# Let's now take S2 into account.

# Calculate S1 price to maximize total Samsung smartphone profit

# The manager at Samsung reports that S2 marginal costs are $490

mc2 <- 490

# Let's calculate quantity, revenue, cost, and profit for the S2 phone, as the
# price of S1 changes
# Recall that market price of S2 was $899

s2dat <- tibble(scenario = 1:length(pvec), price = 899, share = smat[, 4])
# share is demand model predictions of S2 share as price of S1 changes 

s2dat <- s2dat |> mutate(
  quantity = share * M,
  revenue = price * quantity,
  cost = mc2 * quantity,
  profit = revenue - cost
)

# now we will aggregate across phones to get total Samsung smartphone profit
# from both S1 and S2, as a function of price of S1

s2dat <- s2dat |> mutate(price = 0)   # just a placeholder for price of S1

sdat <- rbind(s1dat, s2dat)   # stack the 2 matrices

sdat <- sdat |>
  group_by(scenario) |>
  summarize_all(sum)          # add rev, cost and profit from S1 and S2 for each scenario

# find Samsung profit-maximizing price of S1, holding price of S2 fixed at 899

sdat |> filter(profit == max(profit))

# we get a profit maximizing price of $749 leading to 1,271 mil in profits.

# why do we get different answer than before?

# Which one is more profitable for Samsung, 1 unit of S1 or 1 unit of S2? 
# How big is the profit difference?

# As you decrease the price of S1, S1 is estimated to garner a larger
# market share. The increase to S1's share results from decreases to
# other phones' market shares.  These other phone include competitors'
# phones like A1, A2, H1, and H2, but also Samsung's other phone S2.

share_dat <- as_tibble(cbind(S1_price = pvec + 799, smat))

# pivot_longer transforms the data table which makes graphing easy
share_dat <- pivot_longer(share_dat, cols = A1:H2, names_to = "phone", values_to = "share")

ggplot(share_dat, aes(x = S1_price, y = share, color = phone)) +
  geom_line() +
  geom_point() +
  xlab("Price of S1 Phone") +
  ylab("Market Share") +
  ggtitle("Estimated Market Share Responses to S1 Price Change by Phone") +
  xlim(c(700, 900)) +
  ylim(c(0.05, 0.35)) +
  theme_bw()

# So the total-samsung profit-maximizing price for S1 needs to trade-off
# the increased revenue and profit from lowering S1 price, vs. the loss of
# revenue and profit from S2-to-S1 switchers that would occur if S1 was
# priced very low.



# Summary of R commands introduced

# colMeans()    - for taking the average of each column of a matrix or data.frame
