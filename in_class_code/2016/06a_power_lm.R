### Power Analaysis for linear regression
### using the tidyverse
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

########
# Simulating data
#######
#1. get coefficients - either from data, or that you guess
#put together a set of parameters
sim_pop_n <- data.frame(
  intercept = 115.8,
  slope = 0.00237,
  resid_sd = 5.68,
  samp_size = 4:20
) %>%
  #crossing(resid_sd = 3:5)
  
#2. set up your sampling for each set of coefficients
  group_by(intercept, slope, resid_sd, samp_size) %>%
  expand(reps = 1:samp_size) %>%
  ungroup() %>%

#3. Replicate each 'study' for some number of simulations
  crossing(sim = 1:100) %>%

#4. Populate with predictors
  mutate(age.days = runif(n(), 1000, 8500)) %>%
  
#5. Based on your model populate with responses
  mutate(length.cm = rnorm(n(), intercept + slope*age.days, resid_sd))


######
# Fit a lot of models
######
fit_n <- sim_pop_n %>%
  
  #1. Group by simulation and parameters
  group_by(sim, slope, intercept, samp_size, resid_sd) %>%
  
  nest() %>%
  
  #2. Fit a model to this data
  mutate(mod = purrr::map(data, ~lm(length.cm ~ age.days, data = .))) %>%

  #3. Extract coefficients and p values
  mutate(coefs = purrr::map(mod, ~tidy(.))) %>%
  
  #4. Clean up
  unnest(coefs) %>%
  ungroup() %>%
  filter(term == "age.days")
  

######
# Look at how power relates to sample size
######
pow_n <- fit_n %>%
  #crossings(alpha = c(0.01, 0.05, 0.1))
  #1. Group by parameters that vary
  group_by(samp_size) %>%
  
  #2. Calcaulate type II error rate
  summarise(type_2_error = sum(p.value > 0.05)/n()) %>%
  ungroup() %>%
  
  #3. Calculate power
  mutate(power = 1 - type_2_error)


#Plot!
library(ggplot2)
ggplot(data = pow_n,
       mapping = aes(x=samp_size, y=power))+
  geom_point() + 
  geom_line()