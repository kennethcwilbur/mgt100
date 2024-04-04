## Week 2

# Today we look at segmentation via the kmeans algorithm.

library(tidyverse)

## Let's import our customer dataset

setwd("G:/My Drive/aaaCURRENT/2024mgt100/scripts")
cust_dat <- read_csv("../data/smartphone_customer_data.csv")


## Today we'll use some abstract theory to pick 2 segmentation attributes
## Gaming minutes relate to desired smartphone attributes, e.g. performance
## Handsize relates to optimal screen size
## We'll limit to 2 variables so we can draw pictures as we go

# Let's subset the data to these two dimensions
sub <- cust_dat |> select(gaming, handsize)
sub

# and plot them

ggplot(sub, aes(gaming, handsize)) +
  geom_point() +
  theme_minimal()

# the gaming variable is "censored" at 0, meaning it cannot take on negative values

# Let's run a clustering algorithm (kmeans) on these data

# kmeans performs clustering based on distances.  We can get different distance
# results if we, e.g., measure handsize in inches or feet or millimeters (why?) 

# So first we standardize the data -- that is, we scale each variable to have a mean
# of zero and a standard deviation of one, so all data points are measured in standard
# deviations from the mean of the underlying univariate distribution

# Note: this is really common. It's often done so that regression parameters can 
# be interpreted as standard deviations and therefore become more comparable to each other

# note that scale() returns a matrix, so we use as_tibble() to convert the
# output of scale() into a data.frame/tibble

scl <- sub |>
  scale() |>
  as_tibble()
scl

# let's check that the scaling worked

scl |>
  summarize_all(mean) |>
  round(3) # check means
scl |> summarize_all(sd) # check std devs

# One final step before we run kmeans. R's built-in kmeans function starts from randomly
# chosen centers. But actually, computers use algorithms to draw quasi-random numbers. 
# The following command tells R what input to use into those algorithms. That enables 
# us to all choose the same "random" value, so we all start from the same place and
# arrive at the same result.
set.seed(4321)

# Now when we run the kmeans clustering algorithm, R handles all of the
# hard stuff for us!  All we need to do is call the kmeans() function
# and specify the "k" (ie, the number of clusters).  We do that with the "centers"
# argument. Suppose we believe there are 4 clusters in these data.

# We can also optionally do more than 1 start by specifying the 'nstart' argument.
# This is a good idea, since it makes it more likely the kmeans algorithm finds
# the global optimum, and not just a local optimum.

# Let's save the output from the kmeans() function into an object named "out"

out <- kmeans(scl, centers = 4, nstart = 10)

# congratulations! You just ran a fancy algorithm. We can call you a data scientist now

# "out" is a list.  This is common with model-fitting functions in R.  
# For example, "lm" fits a linear model and outputs a list 
# Let's run the structure function to see what is in "out"

str(out)

# We see that out$cluster is a vector of cluster-membership, i.e., a vector of
# length n that tells us which of the k clusters each of our n data points belongs to.

# Tangent: How to access list elements

# 3 ways to extract a list element -- returns the element
str(out$cluster)
str(out[["cluster"]]) # Double square brackets indicates list element name or position
str(out[[1]])

# 2 related ways to subset a list into a one-element list
str(out["cluster"])
str(out[1])

# double-square brackets is like the dollar sign; refers to list element content
# single-square brackets returns a list
# note the difference:
str(temp1<-out[[1]])
str(temp2<-out[1])
rm(temp1,temp2)

# We can also see that out$centers is a k-by-p matrix with the coordinates of the
# clusters' centers (k is # of clusters; p is # of variables)

str(out$centers)
out$centers

# We can use these elements of "out" to enhance our plot:

# First, let's grab the cluster membership as a variable and add it to our
# dataset as a factor/categorical variable
sub<-
  sub |> 
  mutate(cluster = factor(out$cluster))

sub |> count(cluster)

# Second, let's store the clusters' center locations in their own tibble/dataframe
centers <- as_tibble(out$centers)
centers

# These clusters are on the standardized data. To "unstandardize" them we can
# multiply by the variables' standard deviations and then add the means:

# calculate mean and sd
SD <- sub |>     # we use capitals to avoid duplicating case-sensitive R commands
  select(gaming, handsize) |>
  summarize_all(sd)
MEAN <- sub |>
  select(gaming, handsize) |>
  summarize_all(mean)

# create a matrix with the values so we can do some linear algebra
SD <- SD |>
  unlist() |>
  rep(4) |>
  matrix(nrow = 4, ncol = 2, byrow = T)
MEAN <- MEAN |>
  unlist() |>
  rep(4) |>
  matrix(nrow = 4, ncol = 2, byrow = T)

# unscale the centers (convert back into original units)
centers <- centers * SD + MEAN
round(centers, 1)

# Then we plot the points (colored by cluster membership) and the cluster centers
ggplot() +
  geom_point(data = sub, aes(x = gaming, y = handsize, color = cluster)) +
  geom_point(data = centers, aes(x = gaming, y = handsize), size = 4) +
  ggtitle("Kmeans cluster membership and centroids") +
  theme_minimal()

# why did we put data and aes into geom_point rather than ggplot?

# What do we think of this result?
# How useful is this segmentation?


# Second example with "iris" data

# load data
data(iris)
str(iris)

# plot petal length vs petal width by species
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, col = Species))

# run kmeans
out_iris <- iris |>
  select(Petal.Length, Petal.Width) |>
  kmeans(centers = 3, nstart = 10)   # why did we choose 3 segments?

# add segment membership
iris <- iris |> mutate(cluster = factor(out_iris$cluster))

# plot segmented data
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = cluster)) +
  geom_point(data = as_tibble(out_iris$centers), size = 4) +
  theme_minimal()

# "confusion matrix" -- ie, actual vs predicted
iris |>
  select(Species, cluster) |>
  table()

# what is a confusion matrix?
# Each row of a confusion matrix represents a classification model's prediction
# Each column of a confusion matrix represents true class memberships
# The cell contents count the numbers of observations that fall into each combination
# A good classification model will make high proportions of true classifications, 
#     and low proportions of false classifications

# for a lot more about the iris dataset, go here:
# https://en.wikipedia.org/wiki/Iris_flower_data_set


# Let's get back to analyzing phones and unpack the kmeans algorithm

# Run this function to show initial cluster points, in scaled space

fun1 <- function() {
  # specify a starting point for the cluster centroids
  # <<- is the super-assignment operator which assigns values to variables in the 
  # global environment, not just the local, within-function environment
  c1 <<- c(gaming = -1, handsize = 2)
  c2 <<- c(gaming = 1, handsize = 1)
  c3 <<- c(gaming = -1, handsize = -1)
  c4 <<- c(gaming = 2, handsize = -1)

  # convert to a data.frame
  cent_dat <<- data.frame(rbind(c1, c2, c3, c4))

  # pick colors
  col4 <- c("magenta", "green", "cyan", "purple")

  # plot
  p <- ggplot() +
    geom_point(data = scl, aes(gaming, handsize)) +
    geom_point(
      data = cent_dat, aes(gaming, handsize),   #what role does aes play here?
      shape = 21, fill = col4, color = "black", size = 5
    ) +
    ggtitle("Kmeans centroids") +
    theme_minimal()

  print(p)
  return(invisible())
}

fun1()   # note that R stores custom functions in the Environment pane

# Run this function to show assignment of points

fun2 <- function() {
  # calculate euclidean distance between each point and each of 4 centroids
  c1ssq <- apply(scl, 1, function(x) sqrt(sum((x - c1)^2)))
  c2ssq <- apply(scl, 1, function(x) sqrt(sum((x - c2)^2)))
  c3ssq <- apply(scl, 1, function(x) sqrt(sum((x - c3)^2)))
  c4ssq <- apply(scl, 1, function(x) sqrt(sum((x - c4)^2)))
# arguments inside apply: data, 1 for row-wise operation, function(x) 
# to indicate we'll define a function, then we specify euclidean distance function

  # assign each point to closest centroid using distances
  clust <<- factor(apply(cbind(c1ssq, c2ssq, c3ssq, c4ssq), 1, which.min))
  # which.min just picks the smallest of the 4 distances for each point
  # so, apply() assigns each data point to the segment of the nearest centroid
  # and factor stores it as a factor variable (AKA categorical variable)

  # plot
  p <- ggplot() +
    geom_point(data = scl, aes(gaming, handsize, color = clust)) +
    geom_point(data = cent_dat, aes(gaming, handsize), size = 4) +
    ggtitle("Kmeans cluster membership and centroids") +
    theme_minimal() +
    theme(legend.position = "none")


  print(p) # we have to print the stored graphic element because we're inside a function
  return(invisible())
}

fun2()   # run the newly-declared function fun2

# we'll run slightly-modified versions of these functions a few times to show convergence
# fun3 updates the centroids given the most recent cluster assignments
# fun4 updates the cluster assignments given the most recent centroids

fun3 <- function() {
  # Update cluster centers
  c1 <<- apply(scl[clust == 1, ], 2, mean)
  c2 <<- apply(scl[clust == 2, ], 2, mean)
  c3 <<- apply(scl[clust == 3, ], 2, mean)
  c4 <<- apply(scl[clust == 4, ], 2, mean)

  cent_dat <<- data.frame(rbind(c1, c2, c3, c4))

  # plot
  p <- ggplot() +
    geom_point(data = scl, aes(gaming, handsize, color = clust)) +
    geom_point(data = cent_dat, aes(gaming, handsize), size = 4) +
    ggtitle("Kmeans cluster membership and centroids") +
    theme_minimal() +
    theme(legend.position = "none")

  print(p)
  return(invisible())
}

fun4 <- function() {
  # get assignment criteria (euclidean distance to centroids)
  c1ssq <- apply(scl, 1, function(x) sqrt(sum((x - c1)^2)))
  c2ssq <- apply(scl, 1, function(x) sqrt(sum((x - c2)^2)))
  c3ssq <- apply(scl, 1, function(x) sqrt(sum((x - c3)^2)))
  c4ssq <- apply(scl, 1, function(x) sqrt(sum((x - c4)^2)))

  clust <<- factor(apply(cbind(c1ssq, c2ssq, c3ssq, c4ssq), 1, which.min))

  # plot
  p <- ggplot() +
    geom_point(data = scl, aes(gaming, handsize, color = clust)) +
    geom_point(data = cent_dat, aes(gaming, handsize), size = 4) +
    ggtitle("Kmeans cluster membership and centroids") +
    theme_minimal() +
    theme(legend.position = "none")

  print(p)
  print(sum(c1ssq)+sum(c2ssq)+sum(c3ssq)+sum(c4ssq))
  return(invisible())
}

fun3()
fun4()

# pay careful attention
# when you see centroids move, make a prediction about how the borders will change
# then run fun4() to test your prediction
fun3()
fun4()

fun3()
fun4()

fun3()
fun4()

fun3()
fun4()

fun3()
fun4()

fun3()
fun4()

fun3()
fun4()

stop("We could keep going, but there will be diminishing returns")

# clean up
rm(cent_dat, centers, c1, c2, c3, c4, clust, cluster)


# add segment memberships back into the original customer data
cust_dat <- cust_dat |> 
  mutate(cluster = factor(out$cluster))
# let's look at the dtaa to check it
View(cust_dat)


# Profile the segments by demographics.  Specifically:
# summarize the segments by age, gender, height, and time spent chatting

# For numeric variables, we can simply take means.
# For categorical variables, we calculate a proportion
# We'll look at some correlates of handsize and gaming minutes to understand how
#    they differ between segments
cdat <- cust_dat |>
  group_by(cluster) |>
  summarize(
    mean_age = mean(age),
    prop_female = mean(gender == "female"),
    mean_height = mean(height),
    mean_chat = mean(chat)
  )

# view results
cdat

# We see that some clusters chat more than others
# We can see that some clusters have more females than others

# We can plot some of these relationships
ggplot(cdat) +
  geom_col(aes(y = mean_chat, x = cluster, fill = cluster)) +
  ggtitle("Time spent in chat apps by segment") +
  theme_minimal()


# is k=4 the right number for k?

# we might want more information on which to base our choice of k
# One thing we might do is try many different values of k, and evaluate
# the performance of the algorithm for each k.  Here, our performance
# criterion will be the within-group sum of squares (WSS) from the model.
# As k increases, the WSS will decrease. The question is:
# how quickly does it decrease?

# let's try k=1, k=2, ..., k=10

# we'll create a vector named 'res' to store our results
res <- vector(length = 10)

# we loop over k=1 through k=10
# note: try entering 1:10 in your console to see what happens
for (i in 1:10) {
  # run k means
  out <- kmeans(scl, centers = i)

  # grab the WSS value, store it in the i'th position of res
  res[i] <- out$tot.withinss
}

# let's plot the WSS for each value of k
ggplot(data.frame(x = 1:10, y = res), aes(x, y)) +
  geom_line(color = "grey") +
  geom_point(size = 3) +
  xlab("Number of Clusters (K)") +
  ylab("Within-Group Sum of Squares (WSS)") +
  scale_y_continuous(limits = c(0, NA))+   # sets y-axis minimum to 0
  theme_minimal()
# this is called an "elbow graph"

# We see a lot of benefit--a substantial reduction in WSS--as
# we move from 1 to 2 clusters, and from 2 to 3. The marginal benefit levels off
# after 3 clusters.

# 3 would have been a better choice than 4 for these data, as it leads to more 
#    substantial segments. However, the elbow graph by itself is insufficient 
#    to make the judgment on the "right" number of segments; we also have to 
#    consider how measurable, accessible and actionable the segments are

# It is very hard to verifiably prove any optimal number of segments, given the 
#    multiplicity of objectives. Hence segmentation, like marketing in general,
#    is often called 'both an art and a science'


# Summary of R commands introduced

# kmeans algorithm
# out <- kmeans()     runs the algorithm, stores results in 'out'
# out$cluster         grabs each row's cluster membership
# out$centers         grabs each centroid's location
# out$tot.withinss    grabs the sum of squares

# for loop syntax
# for(var in vector) { do things in terms of var }

# pre-allocate a vector and fill it in
# vector(length=10)    to pre-allocate
# res[i] <- ...        to fill it in
