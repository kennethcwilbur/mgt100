## Week 2 : Customer Data & Data Viz

# Today we cover univariate (one variable) and bivariate (two variables)
# statistical and graphical summaries of data.

# Note: We stick to 1 & 2-variable summaries because those are what we can easily
# & effectively draw in 2 dimensions. 3-dimensional graphics are harder to 
# create effectively or interpret easily. 


# Let's load the code we'll be using 
library(tidyverse)

## Let's check that our default path is set to the right place
getwd()
## Remember, you can use setwd() or Session/Set Working Directory to change it

## import our datset (remember to customize the path to the dataset for your computer)
cust_dat <- read_csv("../data/smartphone_customer_data.csv")


## Univariate analysis on numeric data -----

# With numeric data, we often use mathematical functions that describe aspects
# of the data's distribution

# For example, we can assess the extremes 

cust_dat |>
  summarise(min(height))

cust_dat |>
  summarise(max(height))

## Note the British spelling - some other packages also use "summarize" which can create a conflict

# we can do both of these at the same time, and assign names to the calculated values
# with the syntax: data.frame |> summarize(new_var_name = f(var))

cust_dat |>
  summarise(
    min_height = min(height),
    max_height = max(height)
  )

# Do the min and max look reasonable?

# let's assess measures of central tendency
cust_dat |>
  summarise(
    avg = mean(height),
    med = median(height)
  )

# Are the results plausible?

# Or we can assess dispersion/concentration
cust_dat |>
  summarise(
    var = var(height),
    sd = sd(height)
  )
# Do those results make sense?

# Let's take a look at the distribution. Boxplots and histograms are often used for continuous measures
# To make a boxplot of customer height, you could call Base R's boxplot() command

boxplot(cust_dat$height)

# Boxplots show the median as a dark line, a box ranging from the first-to-third
# quartiles, and whiskers stretching 1.5x the IQR (inter-quartile range).
# Data points outside of the whiskers are "outliers" and plotted as individual points
# We can quickly get the median, quartiles, and min/max with the Base R summary() command

cust_dat |>
  select(height) |>
  summary()

# One occasional complaint about R is that there are "too many ways to do things"
# due to the abundance of packages. Of course, that could be reframed as giving you 
# lots of options.

# In general, we'll focus on ways that we think are generally good, cherry-picking useful
# stuff mostly from base R and tidyverse, or occasionally other packages where needed

# Next, We'll focus on Tidyverse "ggplot2" package using the "ggplot" command. 
# ggplot is popular and versatile because (1) offers a lot of control and (2) defaults are thoughtful
# Thoughtful defaults help facilitate interpretation and save time on coding 

# ggplot predates the tidyverse so its syntax is different

# All plots in ggplot2 start with "ggplot(data.frame)"
# and then you add visual plot elements as "geoms" as in "geom_boxplot()"
# The data enter as "aesthetic mappings" (or aes() for short)
# and we combine the steps with the + operator (rather than the pipe)

?ggplot

# this "grammar of graphics" allows you to make a very wide variety of plots
# to understand ggplot more deeply, read https://www.tandfonline.com/doi/pdf/10.1198/jcgs.2009.07098

# before we get to details, compare the base R boxplot command to the ggplot command

ggplot(cust_dat, aes(y = height)) +
  geom_boxplot()

# the first argument to ggplot() is "data" and
# the first argument to geom_xxx() is "mapping"
# we can often omit "data=" and "mapping=" because they are defaults
# Thus, the following code creates the same plot, but with less typing

# Arguments passed into ggplot, like aes, are "inherited" by subsequent properties, like geom_boxplot
# Or, subsequent properties can take their own inputs

# Let's try to make the plot pretty

ggplot(cust_dat, aes(y = height)) + 
  geom_boxplot(
    fill = "dodgerblue2",             # fill the box with a color
    outlier.colour = "firebrick",     # change the outlier point color
    outlier.alpha = 0.5               # make the outliers partially transparent
                ) + 
  ggtitle("My Second Boxplot!") +     # add a title
  xlab("Height") +                    # label the x axis
  ylab("Inches") +                    # label the y axis
  ylim(c(50, 85)) +                   # override the default y axis limits/range
  scale_x_discrete(labels = NULL, breaks = NULL) + # remove the tick marks and labels from the x axis
  theme_bw()                          # change the "theme" to one without a grey background

# you can also replace ggtitle with   
# labs(
# x="Height",
# y="Inches",
# title="My Second Boxplot!"
#  ))

# to dig into the plotting options (ie, function arguments) in more detail, either
# 1. call ?geom_boxplot in R, or
# 2. read the ggplot2 book, available online for free at: https://ggplot2-book.org/

# Boxplots mainly show quantiles and outliers, on purpose.  
# Histograms show us more details without the same focus on quantiles.
# Boxplots make a lot of sense if you know your data's distribution (e.g., Normal)
# Histogram give a sense of what the distribution shape might be
# To make a histogram, change the geom to geom_histogram

ggplot(cust_dat, aes(y = height)) +
  geom_boxplot()
ggplot(cust_dat, aes(x = height)) +
  geom_histogram()

# A very important setting with histograms is the bin width: bins that are too large
# can hide important detail, while bins that are too narrow can obscure overall shape
# We can change the number of bins with either the 'bins' or 'binwidth' arguments

ggplot(cust_dat, aes(height)) +
  geom_histogram(bins = 100)

ggplot(cust_dat, aes(height)) +
  geom_histogram(binwidth = 1)

# Does the distribution look Normal ?
# What are the pros and cons of binwidth vs count of bins ?

# Let's pretty this up with a few arguments

ggplot(cust_dat, aes(height)) +           # usual stuff on this line
  geom_histogram(
    binwidth = 1,                         # use binwidth to control binning behavior
    color = "grey",                       # change bar outline color
    fill = "dodgerblue4"
  ) + # change bar fill color
  ggtitle("My First Pretty Histogram!") + # add a title
  xlab("height") +                        # label the x axis
  ylab("Count") +                         # label the y axis
  theme_minimal()                         # use a different theme


## Univariate analysis on categorical data -----

# In this dataset, we have two categorical variables: gender and brand.
# We need to "convert" these variables from "strings" to "factors"
# A shortcut to overwrite a variable, ie instead of mutate(var = f(var)), is
# to use mutate_at(). The below code convert gender and brand into factor variables.

cust_dat <- cust_dat |> 
            mutate_at(vars(gender, brand), factor)

# Let's focus on brand. Brand is a categorical variable because it
# can only have 1 of K categories (for brand, K is 3)

# Let's first understand what values brand can take

cust_dat |> distinct(brand)

# One step better than getting the unique values, is to get a count of the number
# of rows that have each value. The count() command computes these frequency counts

cust_dat |> count(brand)   # notice n as the column header

# We can easily convert counts to proportions with n/sum(n)

cust_dat |>
  count(brand) |>
  mutate(pct = n / sum(n))    # n is the column header parameter

# Or we can plot the frequency counts as a barplot

ggplot(cust_dat,aes(x=brand)) +
  geom_bar()

# As before, let's make the plot pretty:

my3colors <- c("darkred", "darkgreen", "darkblue")
# the c() operator concatenates values into a vector

ggplot(cust_dat, aes(x=brand)) +                     # usual stuff here
  geom_bar(fill = my3colors) +                       # fill specifies colors
  xlab("Brand") +                                    # x axis label
  ylab("Number of Customers") +                      # y axis label
  ggtitle("How many customers bought each brand?") + # add a title
  scale_x_discrete(labels = c("Apple", "Huawei", "Samsung")) + # change the x axis labels
  theme_minimal()                                    # change theme


## Bi-variate plots -----

# Univariate summaries help to build our confidence and understanding of the 
# attribute data.

# After we become confident, we can start asking questions about how attributes 
# relate to each other. Bivariate plots are a great first step.

# Here we'll cover common plots / data summaries based on variable types:
# cont. vs cont. data: scatterplots
# cont. vs categorical dat: multiple boxplots, faceting
# categorical vs categorical: contingency tables, heat/bubble plots


# + Scatterplots -----

# Let's create a scatterplot of handsize and height
# Before we do it: What do you expect it to look like?

# geom_point is used to create scatterplots

ggplot(cust_dat,aes(x = height, y = handsize)) +
  geom_point()

# As before, let's make the plot pretty
ggplot(cust_dat, aes(height, handsize)) + # usual stuff here
  geom_point(color = "dodgerblue4", alpha = 0.3) + # transparency level, helps distinguish overlapping points
  ggtitle("Hand size vs. Height") +       # title
  xlab("Height") +                        # x axis label
  ylab("Hand Size") +                     # y axis label
  theme_classic()                         # yet another theme

# Does it look the way you expected? Exploring data is essentially a continuous 
# exercise of forming and testing little hypotheses

# We can observe the same relationship more simply by calculating the correlation 
cust_dat |> summarize(cor = cor(handsize, height))

# We can also add the linear regression line to the plot
ggplot(cust_dat, aes(height, handsize)) +
  geom_point(color = "dodgerblue4", alpha = 0.3) +
  geom_smooth(method = "lm", color = "firebrick")

# You can see ggplot options on this cheat sheet:
# https://drive.google.com/file/d/1m3R2uhd18UvJqWx7bqD6Bf8oGvy_LEBt/view?usp=share_link
# let's take a quick look. there is a lot! 


# + Multiple Boxplots -----

# To investigate the relationship between a continuous variable and a categorical
# variable, we can create one boxplot for each value of the categorical
# variable. This works well for a small number of categories.

# To do so, we just add another variable to our aesthetic -- simple!
ggplot(cust_dat,aes(brand, total_minutes)) + 
  geom_boxplot()

# Note: first arg in aes() is x by default, so we don't have to write x=brand

# Let's improve our plot
# Note: this color scheme isn't that great, but I want to show you variety
# Note: ylim() is used to customize the y axis range shown in the plot

ggplot(cust_dat, aes(brand, total_minutes)) +     # usual stuff
  geom_boxplot(outlier.color = "white", color = "white", fill = "grey90") + # playing with the colors quite a bit
  ggtitle("Usage Distributions by Brand") +       # add a title
  xlab("Brand") +                                 # x axis label
  ylab("Total Weekly Minutes of Phone Usage") +   # y axis label
  ylim(550, 1550) +                               # specify the vertial range
  theme_dark()                                    # not my favorite theme.. too spooky

# We see pretty similar usage distributions across the three brands.

# Alternatively, we could print the data for each brand to the console:

cust_dat |>
  group_by(brand) |>
  summarize(
    min = min(total_minutes),
    mean = mean(total_minutes),
    median = median(total_minutes),
    max = max(total_minutes)
  )

# You can see why we think graphics are easier to learn from than tables


# + Faceting -----

# A "facet" or "partition" is when When you make the same plot for non-overlapping subsets 

# Let's make brand-specific facets of minutes distributions

# the long way to do this is to subset the data, and then plot each subset

cust_dat |>
  filter(brand == "apple") |>
  ggplot(aes(total_minutes)) +
  geom_histogram()
cust_dat |>
  filter(brand == "samsung") |>
  ggplot(aes(total_minutes)) +
  geom_histogram()
cust_dat |>
  filter(brand == "huawei") |>
  ggplot(aes(total_minutes)) +
  geom_histogram()

# a better way is to use facet_wrap() or facet_grid()

ggplot(cust_dat, aes(total_minutes)) +
  facet_grid(rows = vars(brand)) +
  geom_histogram(binwidth = 25)

# and we can improve the facet histograms with some color and a theme

ggplot(cust_dat, aes(x=total_minutes, fill=brand)) + # usual stuff here
  facet_grid(rows=vars(brand)) +            # for facet_grid, let's do one brand per row
  geom_histogram(binwidth = 25) +           # customize our binwidth
  scale_fill_manual(values = my3colors) +   # override the default colors
  theme_minimal() +                         # pick a favorite theme
  theme(legend.position = "none")

# Side-by-side placement enable visual comparisons. What do you see?

# Facets work great for few categories. What do you do when you have lots of categories? 
# What if we had 20 brands? Could we visually interpret that many distributions?
# Some options to reduce the number of categories:
#   + Focus on the most frequent or most important categories (80/20 rule)
#   + Reduce comparisons by contrasting theoretically distinct categories
#   + Combine similar categories, e.g. nation of origin or price tier
#   + If possible, find a continuous representation of category membership

# In the end we'll build a model to control for many things simultaneously, but 
# we shouldn't invest in that until we have a solid understanding of the data.
# Do you know why? (GIGO)


# + Two-way Tables -----

# Two-way tables, AKA contingency tables, summarize relationships between categorical variables.  

# Does gender correlate with smartphone brand? What do you think?

# We can do this using count() or table():
# table() is a Base-R command that returns a "table" object rather than a
# data.frame, but it prints nicely
cust_dat |>
  select(gender, brand) |>
  table()

# count() is a tidyverse command that returns a tibble/data.frame, and it prints like one
cust_dat |> count(gender, brand)

# It may be easier to see these relationships as a bar chart
ggplot(cust_dat, aes(x = brand, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))

# Note: the default stacked bar chart makes some comparisons difficult, so 
# let's unstack using the position=dodge argument

ggplot(cust_dat, aes(x = brand, fill = gender)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("firebrick", "dodgerblue4"))

# We could do something similar with ggplot facets
ggplot(cust_dat) +
  facet_grid(rows = vars(brand)) +
  geom_bar(aes(gender, fill = gender)) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(xlab = "", ylab = "", 
       title = "Customer Count", 
       subtitle = "by Gender and Brand") +
  theme_bw() +
  theme(legend.position = "none")

# This is a really great blog post about the process and options in creating 
# beautiful,  publication quality plots in R -----
# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/#polish





# Summary of R commands introduced -----

# basic statistics
# min()
# max()
# mean()
# median()
# var()
# sd()

# correlation and frequencies
# cor() for correlation
# table() to get counts
# count() to get counts

# plotting with ggplot2:
# ggplot() +
# geom_boxplot()
# geom_histogram()
# geom_bar()
# geom_point()

# enhancing your plots:
# facet_wrap() or facet_grid()
# ggtitle()
# xlab()
# ylab()
# labs()

# tools for ggplot customization (knowing these is optional)
# theme_minimal()      # themes to customize look
# theme_bw()           #
# theme_classic()      #
# scale_x_discrete()   # can be used to change x-axis labels for categorical x variable
# scale_fill_manual()  # can be used to change fill color manually
# geom_smooth()        # can be used to add regression line to plot
