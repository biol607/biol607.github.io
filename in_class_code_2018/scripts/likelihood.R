#'--------------------
#'
#' Likelihood
#'
#'
#'--------------------

#Our libraries for today
library(MASS)
library(profileModel)
library(dplyr)
library(tidyr)
library(ggplot2)

## Single point likelihood

## Poisson distribution as our likelihood
## We have one observation of 10
## hairs per knuckle

#we want L(lambda | 10)
#which is p(10 | lambda)
#is just the PDF of poisson with lambda
#(probability density function)
dpois(10, lambda = 12)

#--------------
#for grid sampling, we search lambda ####
#--------------
pois_10 <- data.frame(lambda = 0:20) %>%
  mutate(lik = dpois(10, lambda = lambda),
         ll = dpois(10, lambda, log=TRUE))

#plot
ggplot(pois_10,
       aes(x = lambda, y = lik)) +
  geom_point()


ggplot(pois_10,
       aes(x = lambda, y = ll)) +
  geom_point() +
  geom_hline(yintercept = -2.078562 - 1.92, color = "red")

#get the MLE
pois_10 %>% filter(lik == max(lik))

#95% CI from a chisq?
qchisq(0.95, df = 1)/2

qchisq(0.50, 1)/2

#Get our CI
pois_10 %>% filter(ll >= max(ll)-1.92)

#---------------------------
# Multiple data points ####
#---------------------------

#skeleton of a likelihood function

my_lik_fun <- function(observations,
                       predictors,
                       parameters){
  
  
  #1. A data generating process
  # takes predictors and parameters
  # and calculates fitted values of observations
  
  
  
  #2. A likelihood function
  # compares fitted and observed values
  # and gets the likelihood parameters | observations
  
  
}

# example for a poisson
pois_log_lik_fun <- function(y, lambda_val){
  
  #Data Generating Process
  y_hat <- lambda_val
  
  #LogLik Function
  #use our observations (y) and fitted values (y_hat)
  sum(dpois(y, lambda = y_hat, log = TRUE))
  
  
}

#take it for a spin
#with simulated data
set.seed(607)
pois_data <- rpois(10, lambda = 10)

pois_log_lik_fun(pois_data, lambda_val = 3)

#grid sample
pois_mle <- data.frame(lambda_val = 0:20) %>%
  rowwise() %>%
  mutate(ll = pois_log_lik_fun(pois_data, lambda_val)) %>%
  ungroup()

#Look at the results
ggplot(pois_mle,
       aes(x = lambda_val, y = ll)) +
  geom_point()

#MLE
pois_mle %>% filter(ll == max(ll))

#CI
pois_mle %>% filter(ll >= max(ll) - 1.92)

#A problem!
#You have run 5 trials of flipping 20 coins. The number of heads in each trial is: 11, 10, 8, 9, 7. What's the maximum likelihood estimate of the probability getting heads? Use dbinom() here for the binomial distribution.

#one example of dbinom
dbinom(x = 10, size = 20, prob = 0.5)

#testing the LL of an even coin
sum(dbinom(x = c(11, 10, 8, 9, 7),
       size = 20,
       prob = 0.5,
       log = TRUE))


my_bin_ll <- function(observations,
                      size_val,
                      prob_val){
  
  #data generating process
  #prob_hat <- f(predictors, parameters)
  #e.g. linear regression y_hat <- a + b*x
  prob_hat <- prob_val
  
  #log lik
  sum(dbinom(observations, size = size_val, prob = prob_hat,
             log = TRUE))
  
}

bin_dat <- c(11,10,8,9,7)

#grid sampling
my_bin_grid <- data.frame(prob_val = seq(0, 1, b = 0.1)) %>%
  rowwise() %>%
  mutate(ll = my_bin_ll(bin_dat, size_val = 20, 
                        prob_val = prob_val)) %>%
  ungroup()


my_bin_grid_fine <- data.frame(prob_val = 
                                 seq(0.4, 0.5, b = 0.001)) %>%
  rowwise() %>%
  mutate(ll = my_bin_ll(bin_dat, size_val = 20, 
                        prob_val = prob_val)) %>%
  ungroup()

ggplot(my_bin_grid_fine,
       aes(x = prob_val, y = ll)) +
  geom_point()


my_bin_grid_fine %>% filter(ll == max(ll))


#--------------
# Two parameter grid sampling ####
#--------------

seals <- read.csv("../data/17e8ShrinkingSeals Trites 1996.csv")
  
  
norm_ll <- function(obs, m, s){
  #DGP
  obs_hat <- m
  
  #Log Likelihood
  sum(dnorm(obs, mean = obs_hat, sd = s, log=TRUE))
  
}  
  
#some starting values to search
mean(seals$age.days)  
sd(seals$age.days)  

#setup our grid sampling
seal_age_grid <- crossing( m = seq(3710, 3740, .1),
                           s = seq(1280, 1300, .1)) %>%
  rowwise() %>%
  mutate(ll = norm_ll(seals$age.days, m = m, s = s)) %>%
  ungroup()

  
#MLE
seal_age_grid %>% filter(ll == max(ll))
  
#visualize
ggplot(seal_age_grid,
       aes(x = m, y = s, fill = ll)) +
  geom_raster() +
  xlim(3725, 3735) + ylim(1290, 1296)
  

ggplot(seal_age_grid,
       aes(x = m, y = s, z = ll)) +
  geom_contour(aes(color = ..level..))
  
##profile
##for the mean
mean_prof <- seal_age_grid %>%
  group_by(m) %>%
  filter(ll == max(ll)) %>%
  ungroup()

#plot
ggplot(mean_prof,
       aes(x = m, y = ll)) +
  geom_line()

#profile
mean_prof %>%
  filter(ll >= max(ll) - 1.92) %>%
  arrange(m) %>%
  filter(row_number() ==1 | row_number() == n()) %>%
  as.data.frame()

#-----------------------
# Linear models with GLM
#-----------------------

#linear regression with least squares
seal_lm <- lm(length.cm ~ age.days,
              data = seals)

seal_mle <- glm(length.cm ~ age.days,
                family = gaussian(link = "identity"),
                data = seals)

plot(seal_mle, which = 1)
plot(seal_mle, which = 2)
hist(residuals(seal_mle))

#profile
seal_prof <- profile(seal_mle)

plot(seal_prof)

library(profileModel)
seal_prof_mod <- profileModel(seal_mle,
                              objective = "ordinaryDeviance")

plot(seal_prof_mod)


#coefficients
summary(seal_mle)

#model comparison
seal_null_mle <- glm(length.cm ~ 1,
                     family = gaussian(link = "identity"),
                     data = seals)

anova(seal_null_mle, seal_mle, test = "LRT")

#plotting
ggplot(seals,
       aes(x = age.days, y = length.cm)) +
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = gaussian(link = "identity")))
