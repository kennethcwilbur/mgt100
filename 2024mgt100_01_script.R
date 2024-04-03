## Welcome to R !!!  

# Today we are going to 
#     get up and running with R and Tidyverse,
#     discuss how to import and export data from R.
#     introduce the 5 key "verbs" of the tidyverse.
#     cover univariate (one variable) and bivariate (two variables)
#           statistical and graphical summaries of data.

# Note: We stick to 1 & 2-variable summaries because those are what we can easily
# & effectively draw in 2 dimensions. 3-dimensional graphics are harder to 
# create effectively or interpret easily. 


# If you haven't already, install:
    # R from CRAN:  https://cran.r-project.org/
    # Rstudio from here:  https://www.rstudio.com/

# If you have installed them, now is a good time to reinstall or update


# We'll use Tidyverse packages frequently. 

    # This set of packages offer several nice extensions to "Base R"
    # The three most-used extensions are:
        # 1. a set of functions for working with data.frames
        # 2. a set of functions for producing graphs
        # 3. the "pipe" operator |> 
    # We'll discuss these extensions in detail as we begin to use them 
    # (so don't worry if you don't yet know what a "data.frame" is)

# Let's install and import the tidyverse:
# (make sure you're connected to the internet)
install.packages("tidyverse") # <-- do this only once 
update.packages("tidyverse")  # <-- do this periodically
library(tidyverse)            # <-- do this every time you restart R and/or open RStudio


## Let's import our customer dataset 
    
    # First, download the customer and phone datasets from 
    #       https://github.com/kennethcwilbur/mgt100/raw/main/mgt100-data.zip

    # Unzip the data into its own folder. A directory structure should look like:
    
      # Mgt 100
          # / class_scripts
          # / data
          # / homework_scripts
          # / slides
          # / syllabus

    # The Base-R function read.csv() imports data stored in csv format into R
    # The data are somewhere on your computer, so you call read.csv("path/to/file.csv")

    # Note that paths can be absolute, like "C:\Users\dyavorsky\documents\mgt100\data\smartphone_customer_data.csv"
    # Or that paths can be relative, like "../data/smartphone_customer_data.csv"

    # Relative paths are relative to your working directory.
    # You can see which directory R is using as your current working directory with
    
    getwd()
    
    # Notice whether the path includes slashes or backslashes; your input to R should match

    # And then you can change that directory with (update the path first)
    setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")
    
    
    # The imported dataset is called a "data.frame" in R
    # We assign that data.frame object to the name "cust.dat" with the assignment operator (<-)
        # You can type < and then - or use the shortcut alt-minus
        # You can get away with "=" instead but "<-" is safer and clearer
    
    cust_dat <- read_csv("../data/smartphone_customer_data.csv")

    ## (Mac users can try "./data/" if ".." doesn't work)
    ## R has many ways to do most things, e.g. read.csv in Base R
    
## Let's look at the data
    
    # In RStudio, you can click the object name or run View() to see the data
    
    View(cust_dat)
    
    # You can also print the data to the console by typing the object's name
    
    cust_dat

    # Let's practice assignment and removal
    
    cdat <- cust_dat
    rm(cdat)
    
    # Both the View() and print approaches can be overwhelming for large datasets
    # R has a number of built-in functions for quickly summarizing a dataset.
    
    # The structure command, str(), shows the variables, their data types, and the first few values
    
    str(cust_dat)
    
    # The summary command, summary(), shows min/max/mean and quartiles for numeric data
    
    summary(cust_dat)
    
    
## Description of variables
    
    # There are 3000 rows.  Each row describes one consumer and purchase occasion, including
    #     customer demographic and usage characteristics
    #     purchase environment data
    #     purchased phone characteristics  

    # Demographic variables
    
        # gender        - gender of phone owner {male, female}
        # height        - height in inches of phone owner
        # handsize      - inches from phone owner's left wrist to tip of middle finger
        # age           - age in years of phone owner
    
    # Customer usage variables
    
        # gaming        - minutes per week spent using apps like minecraft, call of duty, etc.
        # chat          - minutes per week spent using apps like messages, whatsapp, slack, discord, etc.
        # maps          - minutes per week spent using apps like apple maps, waze, google earth, etc.
        # video         - minutes per week spent using apps like youtube, tiktok, etc.
        # social        - minutes per week spent using apps like instagram, facebook, linkedin, etc.
        # reading       - minutes per week spent using apps like news, twitter, reddit, etc.
        # total_minutes - the sum of the 6 other usage variables (e.g., gaming + chat + ... + reading)
    
    # Purchase environment variables
    
        # days_ago      - integer number of days since phone was purchased
        # years_ago     - integer number of years {1, 2, 3} since phone was purchased
        # discount      - the phone (if any) that was on 10% price discount when the customer made their phone purchase

    # Purchased phone variables
    
        # phone_id      - the phone purchased {A1, A2, S1, S2, H1, H2}
        # brand         - one of {apple, samsung, huawei}
        # size_cat      - one of {s, l} to indicate either a small or large form factor (ie, screen size)
        # price         - price in dollars
        # screen_size   - measurement in inches of the diagonal of the phone's primary display
    
    
## Data Manipulation
    
    # The tidyverse has 5 key "verbs" (along with group_by) to work with a data.frame
    
    # It also has a "pipe" operator
    # The main idea of pipes is to transform g(f(x)) into x |> f() |> g()
    # so that your code reads left to right, instead of inner-parentheses to outer-parentheses
    
    #1. filter() selects rows
    
    filter(cust_dat, age == 24)
    
    cust_dat |> filter(age == 24) # <-- with the pipe (|>)
    
    
    #2. arrange() orders by row
    
    cust_dat |> arrange(height)
    cust_dat |> arrange(desc(height))
    
    
    #3. select() chooses columns
    
    cust_dat |> select(height, handsize)
    
    cust_dat |> select(-(gaming:reading))  # <-- get ranges via the colon (:) and use the "except" operator (-)

    
    #4. mutate() creates new columns
    
    cust_dat |> select(height) |> mutate(double_height = 2*height)
    
        # A quick note on the side: R often stores more precision than it prints
        # For example, row 2 appears to have height = 64.6 and double_height = 129, but
        # 64.6 * 2 = 129.2.  If we dig into the object, we can see that the precision is there.
    
        cust_dat |> 
            select(height) |> 
            mutate(double_height = 2*height) |> 
            filter(row_number() == 2) |> 
            unlist()    # Unlist just devolves a list object into a simpler object
            
    
    #5. summarize() with group_by() does aggregation
    
    cust_dat |> group_by(age) |> summarize(avg_height = mean(height))
    
    cust_dat |> group_by(gender) |> summarize(avg_hand = mean(handsize))
    
    # you'll find some handy additional things -- like n() -- as you work with the tidyverse functions
    
    cust_dat |> group_by(age) |> summarize(count = n())
    
    # we can group by more than one variable
        
        cust_dat |> 
            group_by(brand, size_cat) |> 
            summarize(num_consumers = n()) |> 
            arrange(desc(num_consumers))
        
        # if you don't group_by something, then summarize gives one aggregate number
        
        cust_dat |> 
            filter(brand=="samsung", size_cat=="l", years_ago==2) |> 
            summarize(avg_game_min = mean(gaming))
    
    
# Dataset Export
    
    # suppose we want to count customers (ie, rows) by age and save those results as a CSV file
    
    # start by doing the calculation and saving the result as a named R object (ie, the "n_by_age <-" part)
    
    n_by_age <- cust_dat |> group_by(age) |> summarize(count = n())
    
    # let's print this new tibble/data.frame to review it
    
    n_by_age
    
    # we can export this table to csv with the write_csv() function
    # write_csv() takes two main arguments: (1) the R object to write out and (2) the path/to/filename.csv 
    
    write_csv(n_by_age, file="../data/n_by_age.csv")
    
    # instead of specifying the full path/to/filename.csv, you can use a shortcut if you 
    # know the current "working directory".  The working directory is where R looks to read/write
    # files if you don't specify a complete path/to/filename.csv.  Let's figure out the current
    # working directory
    
    getwd()
    
    # you can change the working directory with setwd("path/to/new/directory")
    
    setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")
    
    # if you want to write to the current working directory, you can create the CSV file 
    # there using the shortcut notation: write_csv(r_object, file="filename.csv")
    
    write_csv(n_by_age, file="../data/n_by_age.csv")
    
    # I'll delete those files from my file system since I don't need to keep them
    
    file.remove("../data/n_by_age.csv")
    
    
# Getting help
    
    # Best source: R help files
    
    # you can precede a R command with a question mark to bring up R's help documentation
    ?getwd
    
    # you can use 2 question marks to quickly search local help files
    ??colors
    
    # if the R command is within an external package, you use the package name followed 
    # by a double-colon, then the command, for example
    ?dplyr::filter
    
    # more good sources : Google, Stackoverflow, your favorite Gen AI
    
    # If you have an error text you want to understand, google it

    # Often, Stackoverflow will have a relevant thread that is prominent in the 
    # search results

    # You will find multiple candidate answers that are ranked by helpfulness
    
    # 3rd best source: Large language models like chatgpt or copilot
    # These guys are helpful to understand what might terms might be related to your need.
    # They are usually partially accurate, but seldom fully accurate if your question is 
    # reasonably complicated.
    # They often help you surface the right terms for googling, which will help you 
    # find the right stackoverflow page.
    
    # 4th best source: Piazza. It's a good chance that an instructor or classmate can help
    # But please do not ask how to do the homework questions. 
    


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


# Summary of R commands introduced
    
    # key operators
        # <-
        # |> 
    
    # view/set your working directory
        # getwd()
        # setwd()
    
    # read/write in data
        # read_csv()
        # write_csv()
    
    # look at data
        # View()
        # str()
    
    # tidyverse data manipulation
        # select()
        # arrange() with possibly desc()
        # filter() 
        # mutate()
        # summarize() with group_by() and possibly n()
    
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
    
    
    
    