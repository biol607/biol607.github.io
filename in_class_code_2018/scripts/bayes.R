#'-----------
#'
#' Bayes in R
#'
#'
#'-----------

library(tidyverse)
set.seed(607)
pois_data <- rpois(10, lambda = 10)


## Make a poisson model
pois_bayes <- function(y, lambda_est,
                       min_prior = 2, 
                       max_prior = 20){
  
  #DGP
  lambda <- lambda_est
  
  #Prior for lambda est
  prior <- dunif(lambda_est, 
                 min = min_prior, 
                 max = max_prior)
  
  #likelihood
  lik <- dpois(y, lambda)
  
  return(prod(lik) * prior)
  
}

#find our posterior!
pois_post <- data.frame(lambda_vals = 0:30) %>%
  rowwise() %>%
  mutate(numerator_posterior = pois_bayes(pois_data, lambda_vals)) %>%
  ungroup() %>%
  mutate(posterior = numerator_posterior / sum(numerator_posterior))


library(ggplot2)
ggplot(pois_post,
       aes(x = lambda_vals, y = posterior)) +
  geom_col()


#look for our CIs
View(pois_post)

#get bayesian estimate with a strong posterior
pois_post <- pois_post %>%
  rowwise() %>%
  mutate(num_post_strong = pois_bayes(pois_data,
                                      lambda_vals,
                                      min_prior = 7,
                                      max_prior = 12)) %>%
  ungroup() %>%
  mutate(post_strong = num_post_strong / sum(num_post_strong))

#-----------------
# Fitting a line ####
#-----------------

library(brms)
library(bayesplot)
library(tidybayes)


seals <- read_csv("../data/17e8ShrinkingSeals Trites 1996.csv")

options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)

#fit our model using brms

seal_lm_bayes <- brm(length.cm ~ age.days,
                     family = gaussian(link = "identity"),
                     data = seals,
                     file = "./seal_lm_bayes.brms")

#look at chains
plot(seal_lm_bayes)


#poorly behaved chains
b8.2 <-
  brm(data = list(y = c(-1, 1)), 
      family = gaussian,
      y ~ 1,
      prior = c(prior(uniform(-1e10, 1e10), class = Intercept),
                prior(uniform(0, 1e10), class = sigma)),
      inits = list(list(Intercept = 0, sigma = 1),
                   list(Intercept = 0, sigma = 1)),
      iter = 4000, warmup = 1000, chains = 2, 
      file = "bad_model.Rds")

plot(b8.2)



#Convert model to a data frame
seal_posterior <- posterior_samples(seal_lm_bayes,
                                    add_chain = TRUE)
head(seal_posterior)
mcmc_trace(seal_posterior) +
  scale_color_manual(values = c("red", "black", "purple", "orange"))


#look at one parameter
plot(seal_lm_bayes, par = "b_Intercept")

#Gelman-Rubin diagnost, aka rhat
#want a value of one
rhat(seal_lm_bayes)
mcmc_rhat(rhat(seal_lm_bayes))

#look at autocorrelation
mcmc_acf(seal_posterior)

#Look at all of the diagnostics
library(shinystan)
launch_shinystan(seal_lm_bayes)


#-------
# linear diagnostics
#-------


seal_fit <- fitted(seal_lm_bayes) %>% as.data.frame()
seal_res <- residuals(seal_lm_bayes) %>% as.data.frame()


plot(seal_fit$Estimate, seal_res$Estimate)

#qq plot
qqnorm(seal_res$Estimate)
qqline(seal_res$Estimate)

#Let's work with the whole model for diagnostics
pp_check(seal_lm_bayes, type = "error_hist", bins = 10, fill = "red")

#look at fitted v. observed values
pp_check(seal_lm_bayes, type = "scatter")


#look at the match between posterior predictions
#and summary statistics
pp_check(seal_lm_bayes, type = "stat", stat = "mean")
pp_check(seal_lm_bayes, type = "stat", stat = "sd")

pp_check(seal_lm_bayes, type = "stat_2d")


#posterior distributions
pp_check(seal_lm_bayes, type = "dens")
pp_check(seal_lm_bayes, type = "dens_overlay")


#--------
# Coefficients
#--------

summary(seal_lm_bayes)

seal_lm_bayes %>%
  gather_draws(b_Intercept) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = c(0.4, 0.75))


#with ggridges
coef_chains <- seal_lm_bayes %>%
  gather_draws(b_Intercept, b_age.days, sigma)

library(ggridges)
#all coefs
ggplot(coef_chains, 
       aes(x = .value, y = .variable, fill = factor(.chain))) +
  geom_density_ridges()


#One coef
ggplot(coef_chains %>% filter(.variable=="b_age.days"), 
       aes(x = .value, y = .chain, fill = factor(.chain))) +
  geom_density_ridges(alpha = 0.8)

posterior_interval(seal_lm_bayes, prob = 0.8)

head(seal_posterior)

#What is the probabilit that intercept < 100?
sum(seal_posterior$b_Intercept < 110) / nrow(seal_posterior)

#What is the probability that the slope is between 2.3 e - 03 and 2.4 e-03?
sum(seal_posterior$b_age.days > 2.3e-03 &
      seal_posterior$b_age.days < 2.4e-03)/nrow(seal_posterior)


#---------
# Visualization ####
#---------

seal_plot <- ggplot(seals,
                    mapping = aes(x = age.days, y = length.cm)) +
  geom_point()


#Adding a fitted line
seal_plot +
  geom_abline(intercept = fixef(seal_lm_bayes)[1,1],
              slope = fixef(seal_lm_bayes)[2,1],
              color = "orange")


#add uncertainty
seal_plot +
  geom_abline(intercept = seal_posterior$b_Intercept,
              slope = seal_posterior$b_age.days,
              alpha = 0.5,
              color = "grey") +
  geom_abline(intercept = fixef(seal_lm_bayes)[1,1],
              slope = fixef(seal_lm_bayes)[2,1],
              color = "orange")


#adding predicition uncertainty
seal_predict <- posterior_predict(seal_lm_bayes,
                                  newdata = data.frame(age.days = c(1000, 8500))) %>%
  as.data.frame() %>%
  mutate(x = 1000, xend = 8500) %>%
  rename(y_1000 = V1, y_8500 = V2)


seal_plot +
  geom_segment(data = seal_predict,
               aes(x = x, xend = xend, 
                   y = y_1000, yend = y_8500),
               alpha = 0.1, color = "purple")+
  geom_abline(intercept = seal_posterior$b_Intercept,
              slope = seal_posterior$b_age.days,
              alpha = 0.5,
              color = "grey") +
  geom_abline(intercept = fixef(seal_lm_bayes)[1,1],
              slope = fixef(seal_lm_bayes)[2,1],
              color = "orange")


#---------
#priors
#---------

prior_summary(seal_lm_bayes)

seal_lm_bayes_prior <- brm(length.cm ~ age.days,
                           family = gaussian(link = "identity"),
                           data = seals,
                           prior = c(
                             prior(normal(115, 5), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(uniform(3,10), class = sigma)
                           ),
                           file = "seal_lm_bayes_prior.Rds")

