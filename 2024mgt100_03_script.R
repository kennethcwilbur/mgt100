## Market Mapping

# Today we assess market mapping via dimension reduction with
# principal components analysis (PCA).

setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")
cust_dat <- read_csv("../data/smartphone_customer_data.csv")

library(tidyverse)
# ggrepel enables ggplot to label datapoints without overlap
install.packages("ggrepel")
library(ggrepel)


# Let's map phones in 1D attribute space based on their screen sizes

# create a small dataset with one row per phone
sub <- cust_dat |>
  select(years_ago, brand, screen_size, size_cat, phone_id) |>
  arrange(years_ago, brand, screen_size, size_cat, phone_id) |>
  distinct()   
# distinct discards duplicate observations, so we'll have just one row per product
# essentially we just recreated phone_dat from cust_dat
sub

# plot phones by size, one facet per years_ago
qq<-ggplot(sub, aes(x = screen_size, y = 0)) + # usual stuff, set y=0 to plot points on a horiz line
  facet_grid(rows = vars(years_ago)) + # use facet_grid to create separate plots by years_ago
  geom_hline(yintercept = 0, color="gray70") + # add a horizontal line at y=0
  geom_point(size = 2) + # make paints a bit bigger with size=2
  geom_text_repel(aes(label = phone_id), max.overlaps = Inf) +  # Labels for each point, avoid overlap
  scale_y_continuous(breaks = 0)+
  theme_bw() # change theme
print(qq)

# in this 1D space, we see:
# that Samsung's large phone are quite a bit bigger than Apple's and Huawei's in last 2 years
# that phone sizes are increasing each year
# that the range of phone sizes are increasing each year
# that the ordering of phones is stable year to year

# We'll come back to this chart as a point of comparison later


# Let's calculate avg handsize per phone.
# This might illustrate which customers prefer which phone products

sub <- cust_dat |>
  group_by(years_ago, brand, screen_size, size_cat, phone_id) |>
  summarize(mhs = mean(handsize)) |>
  arrange(years_ago, brand, screen_size, size_cat, phone_id)


# What would we expect to see in a screensize x handsize map?

# Are larger phones bought by people with larger hands?
# Should handsize be increasing year to year?
# Will phones of the same screen size be bought by people with the same handsize?
# Will people with the same handsize buy phones of different screen sizes?
# Should the handsize-to-screensize relationship change over time?

# Let's find out!

# Plot consumers' screensize vs handsize, facet by phone, for only years_ago==1
ggplot(cust_dat |> filter(years_ago == 1)) +   # notice that we used tidy inside ggplot
  geom_histogram(aes(handsize), bins = 50) +
  facet_grid(rows = vars(screen_size)) +
  ylab("Mean Hand Size")+
  xlab("Screen Size")+
  ggtitle("Hand Size Distributions by Screen Size, for Years_ago==1") +
  theme_bw()

# Plot phones in screensize x handsize space, and show how it changes with years_ago
ggplot(sub, aes(x = screen_size, y = mhs)) + #
  geom_point() + #
  facet_grid(rows = vars(years_ago)) + # different plots for different years_ago
  geom_smooth(method = "lm", se = F) + # add linear trend (ie regression) line
  geom_text_repel(aes(label = phone_id), max.overlaps = Inf) + #Label the phone points
  ylab("Mean Hand Size")+
  xlab("Screen Size")+
  ggtitle("Screen Size vs. Mean Customer Hand Size, by Phone Across Generations")+
  theme_bw() #

# What do we see?
# There is a positive relationship between screen size and hand size
# Not a perfectly linear relationship, though, some variance around it
# The relationship is softening slightly (trend line getting flatter) in recent years


#  Let's use PCA to reduce our 2D (screensize, avg handsize) to 1D
# In other words, we'll collapse this 2D map to a 1D map with minimal information loss
# To do this, we need a dimension reduction technique.
# We'll use PCA since it's easy, popular, and we studied it in class
# We'll use the Base-R function prcomp()

# Recall that we grouped the data by phone attributes above
# We want to do PCA within each value of years_ago
# Therefore we are going to ungroup the data before filtering on years_ago

# In general, group_by() means that calculations(e.g., mean) will be performed within groups
# Tidyverse retains the first level of grouping by default, until an ungroup() function is called
#     (but, it does not automatically retain subsequent groupings of groupings)
# So if we previously grouped, we have to use ungroup() if we want to perform calculations without groups

pca_out1 <- sub |>
  ungroup() |>
  filter(years_ago == 1) |>
  select(screen_size, mhs) |>
  prcomp()
pca_out2 <- sub |>
  ungroup() |>
  filter(years_ago == 2) |>
  select(screen_size, mhs) |>
  prcomp()
pca_out3 <- sub |>
  ungroup() |>
  filter(years_ago == 3) |>
  select(screen_size, mhs) |>
  prcomp()

# Not to brag, but you're getting pretty good at calling unsupervised machine learning algorithms

summary(pca_out1)
summary(pca_out2)
summary(pca_out3)

# Compare the cumulative proportion of variance explained under PC1 to that under PC2
# One component explains the majority of variance in the data
# This is because mean-handsize and screen size are highly correlated

sub |>
  group_by(years_ago) |>
  summarize(cor(mhs, screen_size))
# Notice that the correlation pattern changes with years_ago in a similar way to 
# how the cumulative proportion of variance explained changes with years_ago

# let's see what else prcomp() returned to us in pca_out

str(pca_out1)

# the rotated data is in pca$x
# we can use the first column of x for our 1D plot

# first, let's "extract" x into a tibble, to make it easier to work with in Tidy
pcs1 <- as_tibble(pca_out1$x)
pcs2 <- as_tibble(pca_out2$x)
pcs3 <- as_tibble(pca_out3$x)

# and append these tibbles together
pcs <- bind_rows(pcs1, pcs2, pcs3, .id = "years_ago")
# .id creates a column to show the years_ago that each reduced data point is coming from
# try the following command and compare the output to understand:
pcs_compare<- bind_rows(pcs1, pcs2, pcs3)
# this is a generalizable technique to understand what function arguments do
# the idea is to run little experiments with or without input parameters, then compare results
rm(pcs_compare)

# now we can plot phones in the PCA-compressed 1D space
q2<-ggplot(pcs, aes(x = PC1, y = 0)) +
  facet_grid(rows = vars(years_ago)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_text_repel(aes(label = sub$phone_id), max.overlaps = Inf) +
  scale_y_continuous(breaks = 0)+
  theme_bw()
print(q2)

# some observations
# H1 is further from the other phones now
# A1 and S1 are now: further apart in year 1, closer together in year 2, and unchanged in year 3

# we can compare this to qq, the original 1D plot based on screen size only
print(qq)
# why are they so similar?
# what are the key differences, and why did those occur?

# We have shown how to reduce a 2D attribute space to 1D with minimal information loss
# This idea generalizes from any X-dim space to any Y-dim space for Y<X

# We did this in a way where the information loss was truly minimal. The 2nd dimension
#    we added, hand size, was strongly related to the first dimension. Hence, eliminating
#    the second dimension using PCA did not subtract much info
# You would expect larger information losses when correlations between dimensions are smaller

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
