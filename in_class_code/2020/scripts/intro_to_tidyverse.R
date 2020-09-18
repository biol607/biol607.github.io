#'--------------------------------------
#'
#' @title Simulation, Dplyr, And All that
#' 
#' @author Jarrett Byrnes
#'--------------------------------------

# Pipes ####

vec <- 1:10

# we want the log of the sqrt of the length of vec
len <- length(vec)
sq <- sqrt(len)
log(sq)

# We could do a lot of nested functions
log(sqrt(length(vec)))

# neater
log(
  sqrt(
    length(vec)
  )
)


# Welcome to the pipe!
#if you haven't, install.packages("magrittr")
library(magrittr)

# a pipe for summation!
1:10 %>% sum() 

# we want a vector of 1:10
1:10 %>%
  
  # take its length
  length() %>%

  # take the sqrt of the length
  sqrt() %>%
  
  # take the log
  log()


# PIPE EXERCISES ####
# 1. Use pipes to sum the log of 100:200.

100:200 %>%
  log() %>%
  sum()

# 2. Use pipes to take the square root of the mean of 100 
# random uniform numbers. runif(100)

runif(100) %>%
  mean() %>%
  sqrt()


# 3. Let’s dive into the guts of R. Using the mtcars data frame, 
# get it’s summary and str that summary. What do you get back?

mtcars %>%
  summary() %>% 
  str()

# Base plot ####
vals <- runif(n = 1000, min = -10, max = 10)

hist(vals, xlim = c(-20, 20))

# scatter plot
my_df <- data.frame(x = 1:10, y = 1:10)

plot(y ~ x, data = my_df)


# Welcome to dplyr ####

#install.packages(c("dplyr", "purrr")
library(dplyr)

# mutate
mtcars2 <- mutate(mtcars, log_mpg = log(mpg))

head(mtcars2)

#base R
mtcars$log_mpg <- log(mtcars$mpg)

mtcars2 <- mtcars %>%
  mutate(log_mpg  = log(mpg),
         sqrt_cyl = sqrt(cyl))


# group_by
# i want to add a column that has the average
# mpg for each # of gears

mtcars_group <- mtcars %>%
  group_by(gear) %>%
  mutate(avg_mpg = mean(mpg)) %>%
  ungroup()

# summarize
# We want to create a derived data set
# Let's say we want the avg and sd of mpg by gear AND ONLY THAT

mtcars_summary <- mtcars %>%
  group_by(gear) %>%
  summarize(avg_mpg = mean(mpg),
            sd_mpg = sd(mpg)) %>%
  ungroup()

# filter
# remove all data where # of cyl = 3
mtcars_filter <- mtcars %>%
#  filter(cyl != 3)
  filter(!(cyl == 3))

unique(mtcars_filter$cyl)

#the ! and NOT
TRUE
FALSE
!TRUE
!FALSE 

# select
# only choose certain columns

#just MPG
mtcars %>%
  select(mpg) %>% 
  head()

# how about everything BUT mpg
mtcars %>%
  select(!mpg) %>%
  head()

#just a few columns
mtcars %>%
  select(gear, carb, disp) %>%
  head()

# all colums with an m in them
# and cyl
mtcars %>%
  select(contains("m"), cyl) %>%
  head()


# DPLYR EXERCISES
#Exercises
#1. Add some columns to mtcars to plot the log of mpg by the 
#square root of hp.
mtcars <- mtcars %>%
  mutate(log_mpg = log(mpg),
         sqrt_hp = sqrt(hp))

plot(log_mpg ~ sqrt_hp, data = mtcars)

#2. Get the average hp per gear and plot them against each other.

mtcars_hp_g <- mtcars %>%
  group_by(gear) %>%
  summarize(mean_hp = mean(hp))

plot(mean_hp ~ gear, data = mtcars_hp_g)

#3. Make a data fame for only 6 cylinder engines with only the 
# disp and carb columns. Create a boxplot of how carb influences disp.
# boxplot()

# Make a data fame
mtcars %>%
  
  # for only 6 cylinder engines
  filter(cyl == 6) %>%
  
  # with only the disp and carb columns
  select(disp, carb) %>%
  
  #boxplot
  boxplot(disp ~ carb, data = .)


# Simulation ####

# base::replicate()

replicate(n = 10, sum(1:100))

# purrr:map_dbl
library(purrr)

map_dbl(1:10, ~sum(1:100))

map_dbl(1:10, ~sum(1:.x))

map_df(1:10, ~data.frame(x = .x,
                         y = sum(1:.x)))

# Exercises
# 1. Use replicate() to repeatedly average the numbers 1:10 seven times.
# 
replicate(n = 7, mean(1:10))

# 1:10 %>%
#   replicate(n = 7, mean(.)) #NOPE NOPE NOPE

# 2. Do the same thing as #1 with map_dbl() - also what happens if 
#   you use other map functions?

map_dbl(1:7, ~mean(1:10))

#   
#   3. Start with a vector:
   my_vec <- c(11, 10, 10, 9, 10, 11, 10, 9, 10, 
               12, 9, 11, 8, 11, 11, 10, 11, 10, 
               11, 9)
# Use map_df() to make a data frame that, for the numbers 3 through 15, 
# returns two columns. One is the the average of the element of the vector 
# at indices 1 through the chosen number, the second is the standard 
# deviation.
# e.g. if .x = 10, 10.2 for a mean and 0.9189366 for a SD
#   

#use map df
map_df(3:15, ~data.frame(m = mean(my_vec[1:.x]),
                         s = sd(my_vec[1:.x])),
       .id = "x")

#if you are uncertain about an input
map_dbl(3:15, ~.x)
map_dbl(my_vec[3:15], ~.x)

#for loop
x <- numeric(length = 10)
for(i in 1:10){
 x[i] <- sum(1:i)
}
x


