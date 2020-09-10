###########
# Fit bayesian linear models in R
###########

#Load our seal data
seals <- read.csv("data/06/17e8ShrinkingSeals Trites 1996.csv")

#load your library
library(rstanarm)

#set our cores
options(mc.cores = parallel::detectCores())

#Fit our model!
set.seed(607)

seal_lm_bayes <- stan_glm(length.cm ~ age.days,
                          data = seals,
                          family = gaussian())


#######Diagnostics
plot(seal_lm_bayes, plotfun = "stan_trace")
plot(seal_lm_bayes, show_density = TRUE, pars = "(Intercept)")
plot(seal_lm_bayes, show_density = TRUE, pars = "age.days", ci_level = 0.2)
plot(seal_lm_bayes, show_density = TRUE, pars = "sigma")

#autocorrelation in our chains
plot(seal_lm_bayes, plotfun = "stan_ac")

#more standard diagnostics
plot(fitted(seal_lm_bayes), residuals(seal_lm_bayes))
qqnorm(residuals(seal_lm_bayes))
qqline(residuals(seal_lm_bayes))

#dealing with posterior predictive dist
pp_check(seal_lm_bayes, nreps = 3)

pp_check(seal_lm_bayes, check = "residuals")

pp_check(seal_lm_bayes, check="scatter")

pp_check(seal_lm_bayes, check="test")
pp_check(seal_lm_bayes, check="test", 
         test = c("mean", "sd"))

#diagnostics
#library(shinystan)
#launch_shinystan(seal_lm_bayes)

######## Coefficients!
plot(seal_lm_bayes, pars = "age.days")
summary(seal_lm_bayes, digits = 5)

posterior_interval(seal_lm_bayes)


#play with posterior
seal_posterior <- as.data.frame(seal_lm_bayes)
head(seal_posterior)

#Wht is the probability that age.days < 0.0023
sum(seal_posterior$age.days < 0.0023) / nrow(seal_posterior)

#visualize uncertainty
library(ggplot2)

seal_plot <- ggplot(data = seals,
       mapping = aes(x = age.days, y = length.cm)) + 
  geom_point() +
  geom_abline(intercept = seal_posterior[,1],
              slope = seal_posterior[,2],
              color = "blue")
seal_plot

#make new predictions
six_k_predict <- posterior_predict(seal_lm_bayes,
                  newdata = data.frame(age.days = 6000))

new_predictions <- 
  posterior_predict(seal_lm_bayes,
            newdata = data.frame(age.days = c(1000, 19000)))
new_predictions <- as.data.frame(new_predictions)
head(new_predictions)

new_predictions$x1 <- 1000
new_predictions$x2 <- 19000

seal_plot +
  geom_segment(data = new_predictions,
               mapping = aes(x = x1, y = V1,
                             xend = x2, yend = V2),
               alpha = 0.1, 
               color = "orange")
