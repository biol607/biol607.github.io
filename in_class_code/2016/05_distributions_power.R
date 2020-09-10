#####################
# Distributions, p vlaues, and power
#
# Sep 29th 2016
#####################

rnorm(100, mean = 0, sd = 1)

#I want the density of -0.5 from a standard normal
dnorm(-0.5, mean = 0, sd = 1)


#Plot a distribution
vals <- seq(-3, 3, length.out = 1000)
dens_vals <- dnorm(vals, mean = 0, sd = 1)

library(ggplot2)
ggplot(mapping = aes(x = vals, y = dens_vals)) +
  geom_line()


#visualizing a poisson, lambda = 10
dpois(3, lambda = 10)

vals_pois <- 0:20
dens_pois <- dpois(vals_pois, lambda = 10)

ggplot(mapping = aes(x = vals_pois, y = dens_pois)) +
  geom_bar(stat = "identity", width = 0.5)
#


# what is pbinom
#30% damaged, 70% healthy
# 500 reefs
# 300 are healthy

dbinom(300, size = 500, prob = 0.7)

#gimme a p value
pbinom(300, size = 500, prob = 0.7)

#upper tail - prob of observing this or a greater 
# level of damage
pbinom(200, size = 500, prob = 0.3, lower.tail=FALSE)

#Two-tailed test
2*pbinom(300, size = 500, prob = 0.7)

############# P-values by simulation

nose_length = seq(5, 15, length.out=2000)
prob <- c(rep(0.9, 1000), rep(0.1, 1000))/1000


#find the midpoint
which(nose_length > 10)[1]

#make a simulated population
pop <- sample(nose_length, size=10000,
              replace=TRUE, prob = prob)
hist(pop)

#what's the p value of 14.5?
#TRUE = 1, FALSE = 0
sum(pop>=14.5)/10000

########## Power Analysis
library(dplyr)

#1. create sample size data frame
samp_df <- data.frame(samp_size = rep(1:40, 500))

#2. Set a mean and pop SD
null_m <- 80
m <- null_m + 3
sd_pop <- 6
set.seed(80)

#3 Simulations!
sim_df <- samp_df %>%
  
  #group by row, as each is 1 sim
  group_by(sims = 1:n()) %>%
  
  #simulated draw from our population
  mutate(samp_mean = mean(rnorm(samp_size, m, sd_pop))) %>%
  
  #clean up
  ungroup()


#plto sampled mean by sample size
ggplot(data = sim_df, 
       mapping = aes(x=samp_size, y = samp_mean)) +
  geom_jitter(alpha = 0.4, size=3)


p_df <- sim_df %>%
  #first, sample SE
  mutate(se_y = sd_pop/sqrt(samp_size)) %>%
  
  #next calculate z
  mutate(z = (samp_mean - null_m)/se_y) %>%
  
  #last calculate p
  mutate(p = 2*pnorm(abs(z), lower.tail=FALSE))



#plot some p values

ggplot(p_df, mapping = aes(x=samp_size, y = p)) +
  geom_jitter(alpha=0.4)

#Calculate power!!!
power_df <- p_df %>%
  
  #for each sample size
  group_by(samp_size) %>%
  
  #calculae type 2 error rate
  summarise(error_rate = sum(p>0.05)/n()) %>%
  
  ungroup() %>%
  
  #calculate power
  mutate(power = 1 - error_rate)


ggplot(data = power_df, mapping = aes(x = samp_size, y = power)) +
  geom_line() + geom_point()


#look at multiple alphas
alpha <- seq(0.01, 0.1, .01)

#make all combos of p_df and alphas
library(tidyr)

alpha_df <- p_df %>%
  
  #do the combos!
  crossing(alpha = alpha)  %>%

  #for each sample size, and alpha
  group_by(samp_size, alpha) %>%
  
  #calculae type 2 error rate
  summarise(error_rate = sum(p>alpha)/n()) %>%
  
  ungroup() %>%
  
  #calculate power
  mutate(power = 1 - error_rate)

ggplot(data = alpha_df,
       mapping = aes(x = samp_size, y = power, 
                     color = factor(alpha))) +
  geom_point() + geom_line()