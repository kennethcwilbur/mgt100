## Welcome to R !!!

# Today we are going to get up and running with R and the Tidyverse packages,
# and we'll discuss how to import and export data from R.


# If you haven't already, install:
    # R from CRAN:  https://cran.r-project.org/
    # Rstudio from here:  https://www.rstudio.com/

# If you are unable to get things working natively on your machine, you can 
# use R via a browser with https://rstudio.cloud/ 
# However, we don't necessarily support that approach; we focus on desktop

# We'll use Tidyverse packages frequently. 

    # This set of packages offer several nice extensions to "Base R"
    # The three most-used extensions are:
        # 1. a set of functions for working with data.frames
        # 2. a set of functions for producing graphs
        # 3. the "pipe" operator |> 
    # We'll discuss these extensions in detail as we begin to use them 
    # (so don't worry if you don't yet know what a "data.frame" is)

# Let's install and import the tidyverse:

install.packages("tidyverse") # <-- do this only once 
update.packages("tidyverse")  # <-- do this periodically
library(tidyverse)            # <-- do this every time you restart R and/or open RStudio


## Let's import our customer dataset 
    
    # First, download the customer and phone datasets from 
    # https://github.com/kennethcwilbur/mgt100/raw/main/mgt100-data.zip

    # Unzip the data into its own folder. A directory structure should look like:
    
      # Mgt 100
          # / class_scripts
          # / data
          # / homework_scripts
          # / slides
  
    # The Base-R function read.csv() imports data stored in csv format into R
    # The data are somewhere on your computer, so you call read.csv("path/to/file.csv")

    # Note that paths can be absolute, like "C:\Users\dyavorsky\documents\mgt100\data\smartphone_customer_data.csv"
    # Or that paths can be relative, like "../data/smartphone_customer_data.csv"

    # Relative paths are relative to your working directory.
    # You can see which directory R is using as your current working directory with
    
    getwd()
    
    # Notice whether the path includes slashes or backslashes; your input to R should match

    # And then you can change that directory with (update the path first)
    setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")
    
    
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
    
    setwd("G:/My Drive/aaaCURRENT/2023mgt100/scripts")
    
    # if you want to write to the current working directory, you can create the CSV file 
    # there using the shortcut notation: write_csv(r_object, file="filename.csv")
    
    write_csv(n_by_age, file="../data/n_by_age.csv")
    
    # I'll delete those files from my file system since I don't need to keep them
    
    file.remove("../data/n_by_age.csv")
    
    
# Getting help
    
    # you can precede a R command with a question mark to bring up R's help documentation
    ?getwd
    
    # this website provides a nice overview on the structure of R's help documentation:
    # https://socviz.co/appendix.html#a-little-more-about-r
    
    # there is also a package under development (not by RStudio) that provides
    # more user-friendly help documentation for select R and Tidyvser commands
    # https://github.com/sjspielman/introverse
    install.packages("remotes")
    remotes::install_github("sjspielman/introverse")
    library(introverse)
    get_help("filter")
    get_help("summarize")
    
    # also works: copy/paste error code into stackoverflow or google
    
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
    
    
    
    
    