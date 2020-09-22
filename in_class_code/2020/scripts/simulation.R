#'--------------------------------------
#'
#' @title Simulation of Sampling to answer
#' HOW BIG SHOULD MY SAMPLE SIZE BE?!?!?!?!?!?!
#' 
#' @author Jarrett Byrnes
#'--------------------------------------

# libraries for this code
library(dplyr)
library(purrr)

# Random number generation ####
#draw from a normal population
rnorm(n = 5, mean = 3, sd = 2)
hist(rnorm(1000, mean = 3, sd = 2))

rnorm(1000, mean = 3, sd = 2) %>%
  hist()

rnorm(1000, mean = 3, sd = 2) %>%
  density() %>%
  plot()


# let's show this pseudo-randomness
set.seed(607)
rnorm(5, mean = 5, sd = 2)

#simulate flipping a coin - binomial
# how many heads do we get if we flip an unbiased coin
# 10 times
rbinom(1, prob = 0.5, size = 10)

#repeat the 10 flips 100 times (100 trials)
rbinom(100, prob = 0.5, size = 10)

# equal probability of any number in a range
runif(50, min = -5, max = 5)

#random whole numbers
runif(10, min = 0, max = 10) %>%
  round()

# Assuming we have intuition about a population, let's sample it ####

# Our assumptions
mean_pop <- 45
sd_pop <- 15

# Let's set up our sim!
# What sample sizes do we need?

samp_sim <- data.frame(samp_size = 3:50)


# NOW - we need to do some simulations!
#If we just wanted one random draw
samp_sim_one_replicate <- samp_sim %>%
  rowwise(samp_size) %>% #group by row number
  summarize(samp = rnorm(samp_size, mean = mean_pop, sd = sd_pop))

#what did we get
head(samp_sim_one_replicate)
plot(samp ~ samp_size, data = samp_sim_one_replicate)

# This is great - but, we want to estimate the mean at 
# each sample size, some huge number of times (1000?)
samp_sim_means <- samp_sim %>%
  rowwise(samp_size) %>% #group by row number
  summarize(samp_mean = replicate(1000,
                                  rnorm(samp_size, mean = mean_pop, 
                                        sd = sd_pop) %>% 
                                    mean()))

plot(samp_mean ~ samp_size, data = samp_sim_means)

# The comment first approach to simulation (or anything with dplyr)

# Assume a some population parameters
# Create a data frame with a variety of plausible sample sizes/properties
# For each sample size (set of params)...
# Replicate calculating estimated paramters from a random draw some # of times


# Let's get simulated mean and SDs to examine sample size ####
# Assume a some population parameters
mean_pop <- 45
sd_pop <- 15

# Create a data frame with a variety of plausible sample sizes/properties
sim_results <- data.frame(samp_size = 3:30) %>%
  # For each sample size (set of params)...
  rowwise(samp_size) %>%
  
  # Replicate calculating estimated parameters 
  # from a random draw 
  # some # of times
  summarize(samp_mean = replicate(100, 
                                  mean(rnorm(samp_size, mean_pop,sd_pop))),
            samp_sd = replicate(100, sd(rnorm(samp_size, mean_pop,sd_pop))))

# I replicated 100 simulations twice. Annoying!
# What if I only did it once, and made a DF for each sim
# Create a data frame with a variety of plausible sample sizes/properties
sim_results <- data.frame(samp_size = 3:30) %>%
  # For each sample size (set of params)...
  rowwise(samp_size) %>%
  
  # Replicate calculating estimated parameters 
  # from a random draw 
  # some # of times
  summarize(map_df(1:100,
                   ~data.frame(sim = .x,
                               samp_mean =  mean(rnorm(samp_size, mean_pop,sd_pop)),
                               samp_sd =  sd(rnorm(samp_size, mean_pop,sd_pop)))))
                               

# Faded Examples ####
#Some preperatory material
set.seed(42)
mean_pop <- 10
sd_pop <- 3
nsim <- 100
samp_sim <- data.frame(samp_size = 3:50)

#Mean simulations
sampSim %>%
  rowwise(samp_size) %>%
  summarize(samp_mean = 
              replicate(nsim, 
                        rnorm(samp_size, mean_pop, sd_pop) %>% mean()))

#Now the faded examples! Fill in the ___

#Median simulations
median_sims <- samp_sim %>%
  rowwise(samp_size) %>%
  summarize(samp_median = 
              replicate(nsim, 
                   rnorm(samp_size, mean_pop, sd_pop) %>% median()))


plot(samp_median ~ samp_size, data = median_sims)



#IQR simulations
#function for interquartile range - IQR()
iqr_sims <- samp_sim %>%
  rowwise(samp_size) %>%
  summarize(samp_iqr = 
         replicate(nsim, 
              rnorm(samp_size, mean_pop, sd_pop) %>% IQR()))

plot(samp_iqr ~ samp_size, data = iqr_sims)

### Sample and sift!

sim_results_common_sim <- samp_sim %>%
  rowwise(samp_size) %>%
  summarize(map_df(1:50,
                   ~data.frame(sim = .x,
                               sample_values = rnorm(samp_size, mean_pop, sd_pop)))) %>%
  group_by(samp_size, sim) %>%
  summarize(mean_samp = mean(sample_values),
            sd_samp = sd(sample_values),
            median_samp = median(sample_values))

### If we did this with functions

one_sim <- function(samp_size){
  samp <- rnorm(samp_size, mean_pop, sd_pop)
  
  out <- data.frame(mean_samp = mean(samp),
             sd_samp = sd(samp),
             median_samp = median(samp))
  
  return(out)
}


samp_fun_results <- samp_sim %>%
  rowwise(samp_size) %>%
  summarise(map_df(1:50, ~one_sim(samp_size)))


