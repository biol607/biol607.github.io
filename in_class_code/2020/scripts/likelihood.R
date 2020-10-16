#'--------------------------
#' @title Likelihood!
#' 
#' @date 2020-10-16
#'--------------------------

#libraries ####

library(dplyr)
library(ggplot2)
library(tidyr)

# Likelihood of a single data point ####

# our population is gaussian with a mean of 10
# and a SD of 3

# let's say we have a single data point
x <- 10

# our likelihood for any given hypothesis is
# the density of our data given our hypothesis
# p(D | H)

# digression: how I plot a normal curve
norm <- data.frame(x = seq(-3,3, length.out = 100)) %>%
  mutate(p = dnorm(x, mean = 0, sd = 1))

qplot(x, p, data = norm, geom = "line")

# if we want the likelihood of a hypothesized mean of 15
# and SD of 3 with our data
p(D|H) = p(D|theta)
dnorm(x, mean = 15, sd = 3)

# So, to get a maximum likelihood estimate, we need to test
# a lot of possible means
lik_vec <- dnorm(x, 
                 mean = seq(8, 12, length.out = 100),
                 sd = 3)

which(lik_vec == max(lik_vec))

# vectors gets ugly with lots of which statements

lik_df <- data.frame(mean = seq(-4, 20, length.out = 100)) %>%
  mutate(lik = dnorm(x, mean = mean, sd = 3))

ggplot(lik_df,
       aes(x = mean, y = lik)) +
  geom_point()

# Likelihood with multiple data points ####

# our population has a mean of 15 and SD of 3
set.seed(607)
samp <- rnorm(20, mean = 15, sd = 3)

#functions for likelihood and log-likelihood
norm_lik <- function(m, s = 3){
  dnorm(samp, mean = m, sd = s, log = FALSE) %>% prod()
}

norm_loglik <- function(m, s = 3){
  dnorm(samp, mean = m, sd = s, log = TRUE) %>% sum()
}

# let's get our likelihood surface
lik_df_mult <- tibble(mean = seq(10, 20, length.out = 100)) %>%
  rowwise(mean) %>%
  mutate(lik = norm_lik(mean),
           loglik = norm_loglik(mean)) %>%
  ungroup()

ggplot(lik_df_mult,
       aes(x = mean, y = lik)) +
  geom_point()

ggplot(lik_df_mult,
       aes(x = mean, y = loglik)) +
  geom_point()

# get our MLE
lik_df_mult %>% filter(loglik == max(loglik)) %>%as.data.frame()

# 95% CI - that the points that are 1.92 away from the MLE's loglik
# remember, we want the quantile of the chisq divided by 2 so we
# get both tails
qchisq(0.95, df = 1)/2
qchisq(0.68, df = 1)/2

lik_df_mult %>%
  filter(loglik >= max(loglik) - qchisq(0.95, df = 1)/2) %>%
  as.data.frame()


# Two-dimensional likelihood surfaces ####

# Remember, our population has a mean of 15 and SD of 3
# What if we need to estimate both the mean and SD!

#crossing to make a grid!
crossing(1:3, 4:6, 7:9)


#let's make our likelihood surface!
lik_df_norm <- crossing(m = seq(10, 20, length.out = 300),
                        s = seq(1, 5, length.out = 300)) %>%
  group_by(m, s) %>%
  mutate(lik = norm_lik(m, s),
         loglik = norm_loglik(m,s),
         deviance = -2 * loglik) %>%
  ungroup


# visualize!
ggplot(data = lik_df_norm %>% filter(loglik > max(loglik) - 5),
       mapping = aes(x = m, y = s, fill = loglik)) +
  geom_raster() +
  scale_fill_viridis_c()

cont_plot <- ggplot(data = lik_df_norm %>% filter(loglik > max(loglik) - 3),
       mapping = aes(x = m, y = s, z = loglik)) +
  geom_contour_filled(bins = 20) +
  guides(fill = "none")

cont_plot

# rayshader

#MLE of our parameters
lik_df_norm %>% filter(deviance == min(deviance))

# Get our profiles by slicing across values of m or s
# We want to, over all values of a parameter of interest
# get the likelihood at optimized values of all other parameters

#for one values of m...
lik_df_norm %>% 
  filter(m == m[16900]) %>%
  filter(deviance == min(deviance))

# our profile for m
lik_prof_m <- lik_df_norm %>%
  group_by(m) %>%
  filter(deviance == min(deviance)) %>%
  ungroup()

# our profile for s
lik_prof_s <- lik_df_norm %>%
  group_by(s) %>%
  filter(deviance == min(deviance)) %>%
  ungroup()

# here's my profile
ggplot(lik_prof_m %>% filter(loglik > max(loglik) - 4),
       aes(x = m, y = loglik)) +
  geom_point()

#let's see that profile line
cont_plot +
  geom_line(data = lik_prof_m %>% filter(loglik > max(loglik) - 1.92),
            aes(x = m, y = s), color = "red")+
  geom_line(data = lik_prof_s %>% filter(loglik > max(loglik) - 1.92),
            aes(x = m, y = s), color = "orange")


#rayshade to show a profile

raster_plot <- ggplot(data = lik_df_norm %>% filter(loglik > max(loglik) - 3),
                    mapping = aes(x = m, y = s, fill = loglik)) +
  geom_raster() +
  scale_fill_viridis_c()


  
rayshader::plot_gg(raster_plot + 
                     geom_line(data = lik_prof_m %>% filter(loglik > max(loglik) - 1.92),
                               aes(x = m, y = s), color = "red"))


# Regression with Likelihood ####
seals <- read.csv("./data/17e8ShrinkingSeals Trites 1996.csv")

# dirty secret, you can do **most** of this with lm
seal_lm <- lm(length.cm ~ age.days, data = seals)
logLik(seal_lm)

# if we want to be 'strict', we'll use glm
seal_mle <- glm(length.cm ~ age.days,
                data = seals,
                family = gaussian(link = "identity"))


# assumptions!
plot(seal_mle, which = 1)
plot(seal_mle, which = 2)
hist(residuals(seal_mle))

# The new thing - make sure our profiles are well behaved!
library(MASS)
library(profileModel)

prof <- profileModel(seal_mle,
                     objective = "ordinaryDeviance")

plot(prof)
plot(prof, print.grid.points = TRUE)

prof <- profileModel(seal_mle,
                     objective = "ordinaryDeviance",
                     quantile = qchisq(0.95,1))
plot(prof)

#----
#let's do this with MASS
# tau is the signed square root of the deviance
# so, a parabaola should become a straight line
# if it's not, you have a problem!
sqrt(4)
-sqrt(4)
prof_mass <- profile(seal_mle)
plot(prof_mass)

prof_mass
confint(prof_mass)

# Model evaluation
summary(seal_mle) #dispersion parameter for gaussian = variance
summary(seal_lm)

# Mode comparison

seal_null <- glm(length.cm ~ 1,
                 data = seals,
                 family = gaussian(link = "identity"))

# Compare seal_mle and seal_null
anova(seal_null, seal_mle, test = "LRT")

anova(seal_mle, test = "LRT")

# for plotting, we just use method = "glm" instead of 
# method = "lm" with stat_smooth
ggplot(seals,
       aes(x = age.days, y = length.cm)) +
  geom_point() +
  stat_smooth(method = "glm",
              method.args = list(family = gaussian(link = "identity")))
