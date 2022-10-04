#' -----------------------------------
#' Sampling and Simulation
#' 
#' @date 20220930
#' -----------------------------------

#### libraries for today ####
library(dplyr)
library(ggplot2)
library(purrr) # this is for iteration

#### setup code ####
theme_set(theme_classic(base_size = 12))

#### Replication and Iteration in Base R ####

replicate(5, rnorm(10) |> mean())

# or with a function
mean_from_sim <- function(n){
  rnorm(n = n) |>
    mean()
}

# iterate the function
replicate(5, mean_from_sim(10))

# what about sapply
sapply(1:5, mean_from_sim)

#replicate replicate
sapply(1:5, \(x) mean_from_sim(10)) #this works, but....

# for loop
out_vec <- rep(NA, 5)

for(i in 1:5){
  out_vec[i] <- mean_from_sim(10)
}

out_vec


#### Iteration with purrr ####

# let's get 5 means from simulations
# with a sample size of 10
map_dbl(1:5,
        mean_from_sim)

# what if you want to evaluate SPECIFICALLY 10
map_dbl(1:5,
        ~ mean_from_sim(10))

# use .x as our argument
map_dbl(1:5, 
        ~ mean_from_sim(.x))

# for example - what if we want sims
# with .x as your mean
map(1:5,
    ~ rnorm(n = 10, mean = .x))

#### Mapping and Data Frames ####
get_sim_properties <- function(n){
  samp <- rnorm(n)
  
  data.frame(n = n,
             m = mean(samp), 
             s = sd(samp))
}

map_df(3:30,
       get_sim_properties)


# EXERCISES #
# 1. Use replicate() to repeatedly average the 
# numbers 1:10 seven times.
mean(1:10)

replicate(7, mean(1:10))

# 2. Do the same thing with map_dbl() - 
#   with a ~
  
map_dbl(1:7, ~ mean(1:10))

#with a function
m_1_10 <- function(x) mean(1:10)
map_dbl(1:7, m_1_10)

# 3. What happens if you use other map functions?
map(1:7, ~ mean(1:10))
map_chr(1:7, ~ mean(1:10))
map_lgl(1:7, ~ mean(1:10)) #error!
map_df(1:7, ~ mean(1:10)) #error!
map_raw(1:7, ~ mean(1:10)) #error!

#### Simulation ####

# Our population
mean_pop <- 35
sd_pop <- 20

#set a seed
set.seed(607)
#rnorm(10)

# a data frame of sample sizes
samp_sim <- data.frame(samp_size = 3:50)


# rowwise()
samp_sim_1 <- samp_sim |>
  rowwise(samp_size) |> #like group_by(row)
  summarize(samp = rnorm(samp_size, mean_pop, sd_pop))


## How Jarrett Writes a Simulation

# 1. A function that gives me what I want from ONE simulation
get_one_sim <- function(samp_size,
                        pop_mean = mean_pop,
                        pop_sd = sd_pop){
  # sample a population at a given sample size
  samp <- rnorm(samp_size, mean = pop_mean, sd = pop_sd)
  
  # return a data frame with a mean and sd
  data.frame(sim_mean = mean(samp), sim_sd = sd(samp))
}

#2. A function that does the iteration
get_sims <- function(n_sims, 
                     samp_size,
                     pop_mean = mean_pop,
                     pop_sd = sd_pop){
  #do the iteration
  map_df(1:n_sims, 
         ~get_one_sim(samp_size, pop_mean, pop_sd))
}

# 3. Take your data frame of parameters, and run 
# simulations for EACH unique parameter set

sim_results <- samp_sim |>
  #for each row
  rowwise(samp_size) |>
  # run the simulations
  summarize(get_sims(n_sims = 1e3, 
                     samp_size = samp_size))

# look at means
ggplot(sim_results,
       mapping = aes(y = sim_mean,
                     x = samp_size)) +
  geom_point(alpha = 0.1)

# look at decrease in SE
sim_summary <- sim_results |>
  group_by(samp_size) |>
  summarize(mean_se = sd(sim_mean),
            sd_se = sd(sim_sd))

ggplot(sim_summary,
       aes(x = samp_size, y = sd_se)) +
  geom_point() +
  geom_line()

# EXERCISE
# Repeat this process for the sample median and IQR
# What is the optimal sample size? Feel free to reuuse code
# and modify it to learn how it works!
