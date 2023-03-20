## Week 4

# Today we assess market mapping via dimension reduction with
# principal components analysis (PCA).

setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")

install.packages("ggrepel")

library(tidyverse)
library(ggrepel) # <-- helps label points on our plots

cust_dat <- read_csv("../data/smartphone_customer_data.csv")


# Let's map phones in 1D attribute space based on their screen sizes

# create a small dataset with one row per phone
sub <- cust_dat %>%
  select(years_ago, brand, screen_size, size_cat, phone_id) %>%
  arrange(years_ago, brand, screen_size, size_cat, phone_id) %>%
  distinct()   
# distinct discards duplicate observations, so we'll have just one row per product
# essentially we just recreated phone_dat from cust_dat
sub

# plot phones by size, one facet per years_ago
ggplot(sub, aes(x = screen_size, y = 0)) + # usual stuff, set y=0 to plot points on a horiz line
  facet_grid(rows = vars(years_ago)) + # use facet_grid to create separate plots by years_ago
  geom_point(size = 2) + # make paints a bit bigger with size=2
  geom_hline(yintercept = 0) + # add a horizontal line at y=0
  geom_text_repel(aes(label = phone_id), max.overlaps = Inf) + # from ggrepel package, adds texts to points
  scale_y_continuous(breaks = 0)+
  theme_bw() # change theme

# in this 1D space, we see:
# that Samsung's large phone are quite a bit bigger than Apple's and Huawei's in last 2 years
# that phone sizes are increasing each year
# that the range of phone sizes are increasing each year
# that the ordering of phones is stable year to year

# We'll come back to this chart as point of comparison later


# Let's calculate avg handsize per phone.
# We'll treat this as an unobserved product attribute

sub <- cust_dat %>%
  group_by(years_ago, brand, screen_size, size_cat, phone_id) %>%
  summarize(mhs = mean(handsize)) %>%
  arrange(years_ago, brand, screen_size, size_cat, phone_id)


# What would we expect to see in a screensize x handsize map?

# Are larger phones bought by people with larger hands?
# Should handsize be increasing year to year?
# Will phones of the same screen size be bought by people with the same handsize?
# Will people with the same handsize buy phones of different screen sizes?
# Should the handsize-to-screensize relationship change over time?

# Let's find out!

# Plot consumers' screensize vs handsize, facet by phone, for only years_ago==1
ggplot(cust_dat %>% filter(years_ago == 1)) +   # notice that we used tidy inside ggplot
  geom_histogram(aes(handsize), bins = 50) +
  facet_grid(rows = vars(screen_size)) +
  ggtitle("Hand Size Distributions by Screen Size") +
  theme_bw()

# Plot phones in screensize x handsize space, and show how it changes with years_ago
ggplot(sub, aes(x = screen_size, y = mhs)) + #
  geom_point() + #
  facet_grid(rows = vars(years_ago)) + # different plots for different years_ago
  geom_smooth(method = "lm", se = F) + # add linear trend (ie regression) line
  geom_text_repel(aes(label = phone_id), max.overlaps = Inf) + #Label the phone points
  theme_bw() #

# What do we see?
# There is a positive relationship between screen size and hand size
# The relationship isn't perfect.  Look at facet 3:
# S1 and H2 have similar screen sizes, but H2 tends to be bought by people with larger hands
# H2 and A2 have different screen sizes, but tend to be bought by people with the same hand size
# The relationship is softening slightly (trend line getting flatter) in recent years


#  Let's use PCA to reduce our 2D (screensize, avg handsize) to 1D
# In other words, we'll collapse this 2D map to a 1D map with minimal information loss
# To do this, we need a dimension reduction technique.
# Principal Components Analysis (PCA) is a useful, popular technique.
# In R, we can do PCA with prcomp()

# Recall that we grouped the data by phone attributes above
# We want to do PCA within each value of years_ago
# Therefore we are going to ungroup the data before filtering on years_ago

# In general, group_by() means that calculations(e.g., mean) will be performed within groups
# In general, we use ungroup() when we want to perform calculations without groups

pca_out1 <- sub %>%
  ungroup() %>%
  filter(years_ago == 1) %>%
  select(screen_size, mhs) %>%
  prcomp()
pca_out2 <- sub %>%
  ungroup() %>%
  filter(years_ago == 2) %>%
  select(screen_size, mhs) %>%
  prcomp()
pca_out3 <- sub %>%
  ungroup() %>%
  filter(years_ago == 3) %>%
  select(screen_size, mhs) %>%
  prcomp()

summary(pca_out1)
summary(pca_out2)
summary(pca_out3)

# Compare the cumulative proportion of variance explained under PC1 to that under PC2
# One component explains the majority of variance in the data
# This is because mean-handsize and screen size are highly correlated

sub %>%
  group_by(years_ago) %>%
  summarize(cor(mhs, screen_size))
# Notice that the correlation pattern changes with years_ago in a similar way to 
# how the cumulative proportion of variance explained changes with years_ago

# let's see what else is in pca_out

str(pca_out1)

# the rotated data is in pca$x
# we can use the first column of x for our 1D plot

# first, let's "extract" x into a tibble, to make it easier to work with
pcs1 <- as_tibble(pca_out1$x)
pcs2 <- as_tibble(pca_out2$x)
pcs3 <- as_tibble(pca_out3$x)

# and append these tibbles together
pcs <- bind_rows(pcs1, pcs2, pcs3, .id = "years_ago")
# .id creates a column to show the years_ago that each reduced data point is coming from
# try the following command and compare the output to understand:
pcs_compare<- bind_rows(pcs1, pcs2, pcs3)
# this is a generalizable technique to understand what function arguments do

# now we can plot in 1D space
ggplot(pcs, aes(x = PC1, y = 0)) +
  facet_grid(rows = vars(years_ago)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_text_repel(aes(label = sub$phone_id), max.overlaps = Inf) +
  scale_y_continuous(breaks = 0)+
  theme_bw()


# some observations
# H1 is further from the other phones now
# A1 and S1 are now: further apart in year 1, closer together in year 2, and unchanged in year 3

# Notice how H2 and A2 are nearly overlapping in years_ago==3. 
# Do you understand why?

# We have shown how to reduce a 2D attribute space to 1D with minimal information loss
# This idea generalizes from any XD space to any YD space for Y<X

# For product categories with 1000s of products and dozens of attributes, this is 
# a powerful way to represent product competition and understand markets




# Summary of R commands introduced

# principal components analysis (PCA)
# prcomp()

# plotting text that doesn't overlap
# ggrepel::geom_text_repel()

# new tidyverse commands
# ungroup()    # to override default tibble behavior created from group_by()
# distinct()   # to get unique combinations
# geom_hline   # to plot a horizontal line
# as_tibble()  # to convert a matrix to a tibble
# bind_rows()  # to "stack" tibbles
