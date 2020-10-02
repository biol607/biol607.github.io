#'---------------------------------
#'  Functions!!!
#'  2020-09-29
#'  
#'  ---------------------------------

# 1. Take a vector and sum after squaring

sum_squares <- function(vec){
  
  out <- vec^2
  
  out <- sum(out)
  
  return(out)
}

sum_squares(c(4,5,6))
sum_squares(c(1,1,1))


#2. take a number and combine it into a string with the word elephants
make_elephants <- function(a_number){
  
  out <- paste(a_number, "elephants", sep = " ")
  
  return(out)
}


make_elephants(6)

# Take a number, a string, and a separator, and combine
string_c <- function(a, b, sep){
  #add spaced around sep
  sep <- paste0(" ", sep, " ")
  
  #paste it all together
  out <- paste(a, b, sep = sep)
  
  #return to user
  return(out)
}

string_c("a", "b", "-")


# EC Write a function that takes a sample size, mean, SD, # of sims
# and returns a data frame with a mean and se of said mean. Default to 
# 100 sims

mean_and_se_sim <- function(n, m, s, sims = 100){
  # generate simulated samples from a population
  samps <- replicate(sims, rnorm(n = n, mean = m, sd = s))
  
  # take the means of those samples
  means <- colMeans(samps)
  
  # calculate the mean of the means, and the sd of the means
  out <- data.frame(mean = mean(means), se_mean = sd(means))
  
  # return that to the use
  return(out)
}

mean_and_se_sim(10, 5, 3)

### Modular functions ####
get_samp_rep <- function(n, m, s){
  # sample from a normal population
  samp <- rnorm(n, m, s)
  
  #return the mean from that sample
  mean(samp)
}

mean_and_se_sim <- function(n, m, s, sims = 100){
  
  means <- replicate(sims, get_samp_rep(n, m, s))
  
  out <- data.frame(mean = mean(means),
                    se_mean = sd(means))
  
  return(out)
}
# rewrite for EXTREME modularity ####

# A wrapper function to get the mean and SE
# of the estimate of the mean based on repeated
# draws from a normal population
mean_and_se_sim <- function(n, m, s, sims = 100){
  
  #draw sims number of means from a sampled population
  means <- get_sim_means(n, m, s, sims = sims)
  
  #make a data frame from those simulations with the mean and SE
  out <- make_mean_se_data_frame(means)
  
  #return
  return(out)
}

# This function get simulated means 
get_sim_means <- function(n, m, s, sims = 100){
  #repeat some number of times
  out <- replicate(sims,
            #drawing means from simulations
            get_mean_from_one_sim(n,m,s))
  
  return(out)
}

# This function gets the mean of ONE simulation
get_mean_from_one_sim <- function(n,m,s){
  #get one sample draw
  samp <- rnorm(n, m, s)
  
  #calculate a mean and return it
  out <- mean(samp)
  
  return(out)
}

# This function calculates a mean and SE based
# on simulation outputs
make_mean_se_data_frame <- function(sim_means){
  #calculate the mean of my simulations
  m <- mean(sim_means)
  
  #calculate the SE of my simulations
  se_mean <- sd(sim_means)
  
  #return the mean and SE in a single data frame
  return(data.frame(mean = m, se_mean = se_mean))
}

# # Exercises ####
# 1. Write a function that will get the mean, sd, median, and 
# IQR of a sample of a population.
library(dplyr)

samp_stats <- function(n,m,s){
  #get one sample
  samp <- rnorm(n,m,s)
  
  #return a data frame with relevant stats
  return(data.frame(
    mean_samp = mean(samp),
    median_samp = median(samp),
    sd_samp = sd(samp),
    IQR_samp = IQR(samp)
  ))
}

# 2. Write a function that uses this to get 1K resampled values 
# to get the statistic and its SE.

get_resampled_stats_and_se <- function(n,m,s,sims = 100){
  #get the samp stats from sims
  samp_stats <- data.frame(sims = 1:sims) %>%
    rowwise() %>%
    summarize(samp_stats(n,m,s))
  
  samp_stats %>%
    summarize_all(.funs = c(mean = mean, se = sd)) #a little trick
    #note, could also have done this with a big summarize
}

# 3. Wrap it all in dplyr magick to get these statistics for sample 
# sizes 3:5, means 2:5, and sd 1:3. Use tidyr::crossing to make the #
# initial data frame.
suppressMessages( #useful!
 stats_from_different_pops <-
  tidyr::crossing(n = 3:5,
                  m = 2:5,
                  s = 1:3) %>%
  rowwise(n,m,s) %>%
  summarize(get_resampled_stats_and_se(n,m,s))
)

# EC. Use ggplot2 to look at how the SE of these different statistics changes based on population mean, sample size, and SD
# 
library(ggplot2)

ggplot(stats_from_different_pops,
       aes(x = n, color = s, y = IQR_samp_se)) +
  geom_point()
