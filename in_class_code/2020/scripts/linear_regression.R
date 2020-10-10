#'--------------------------------
#' @title Linear Regression!
#' 
#' @date 2020-10-09
#'--------------------------------


# The Fundamental Steps of executing a regression ####
# 1. Load the data
# 2. Visualize the data - just to detect problems and perform a cursory 
#    test of assumptions!
# 3. Fit the model.
# 4. Use the fit model to test assumptions
# 5. Evaluate the model
# 6. Visualize the fit model

# Seal Model ####
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Load the data ####
seals <- read.csv("data/17e8ShrinkingSeals Trites 1996.csv")

# what's here?!

str(seals)
summary(seals)
#visdat
skimr::skim(seals)

# 2. plot it! ####
seal_plot <- ggplot(data = seals,
       mapping = aes(x = age.days, y = length.cm)) +
  geom_point(alpha = 0.5)

seal_plot


# 3. Fit the model

# formulae are in the y ~ x format
seal_lm <- lm(length.cm ~ age.days, data = seals)

seal_lm
coef(seal_lm)

# 4. Use the fit model to test assumptions ####

# Does the distribution of our predictions match our data?
seal_sims <- simulate(seal_lm, nsim = 20) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sim",
    values_to = "length.cm"
  )

ggplot() +
  geom_density(data = seal_sims,
               mapping = aes(x = length.cm, group = sim), 
               size = 0.2)  +
  geom_density(data = seals,
               mapping = aes(x = length.cm),
               size = 2, color = "blue")

# Is there a relationship between fitted and residual values?
plot(seal_lm, which = 1)

# with ggplot
library(ggfortify)
autoplot(seal_lm, which = 1, ncol = 1)

# Did we satisfy normality and homoskedacticity using a qq plot and 
# levene test
residuals(seal_lm) %>% hist()

plot(seal_lm, which = 2)

# residuals(seal_lm) %>% shapiro.test() # doesn't work due to large sample size

# Look for outliers with leverage
plot(seal_lm, which = 4) #over 1 is a problem maybe?
plot(seal_lm, which = 5)



#leverage
dat <- tibble(x = c(1:10, 100),
              y = c(rnorm(10, x), 50))

ggplot(data = dat[,-11],
       aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm")

fit <- lm(y ~ x, data = dat)
fit_no_outlier <- lm(y~x, data = dat[-11,])
plot(fit, which = 4)
plot(fit, which = 5)


coef(fit)
coef(fit_no_outlier)


# 5. Evaluate the model ####
library(broom)

# F-test
# Did we explain any variation in the data other than noise?
# Null hypothesis - our model should have just as much explanatory 
# power as the noise we observe - var(model)/var(error) = F ratio
# If we get a small probability value, we reject the null

anova(seal_lm)

anova(seal_lm) %>%
  tidy()

# T-test of parameters
# If we divide a parameter by it's SE = t
# We can use that to see if we can reject the hypothesis that our
# paramter = 0

summary(seal_lm)

#tidy output from broom
tidy(seal_lm)
glance(seal_lm)

# just the r2
summary(seal_lm)$r.squared
summary(seal_lm)$coef


# 5. Visualize your model ####

#Fit interval
seal_plot +
  stat_smooth(method = "lm") #shows error around our FIT

fit_seals <- predict(seal_lm, 
                     interval = "confidence") %>%
  as_tibble() %>%
  rename(lwr_ci = lwr,
         upr_ci = upr)

seals <- cbind(seals, fit_seals)

ggplot(seals,
       aes(x = age.days, 
           ymin = lwr_ci, 
           ymax = upr_ci,
           y = fit)) +
  geom_ribbon() +
  geom_line(color = "blue")


# Predicition Interval
predict_seals <- predict(seal_lm,
                         interval = "prediction") %>%
  as_tibble() %>%
  rename(lwr_pi = lwr,
         upr_pi = upr)

seals <- cbind(seals, predict_seals) 

# fix the names - but, note, . now is _ 
# sorry! This is an argument for using readr::read_csv
seals <- janitor::clean_names(seals)

ggplot(seals,
       aes(x = age_days,
           y = fit,
           ymin = lwr_pi,
           ymax = upr_pi)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(color = "blue", size = 2) +
  geom_point(mapping = aes(y = length_cm))


# let's visually compare the fit interval and the prediction intervals
ggplot(data = seals,
       mapping = aes(x = age_days,
                     y = length_cm)) +
  #prediction interval
  geom_ribbon(mapping = aes(ymin = lwr_pi,
                            ymax = upr_pi),
              alpha = 0.5) +
  # fit interval - just coefficient error (precision)
  geom_ribbon(mapping = aes(ymin = lwr_ci,
                            ymax = upr_ci),
              color = "red",
              alpha = 0.5)
#-----------
# Faded examples

#load the data
fat <- read.csv("./data/17q04BodyFatHeatLoss Sloan and Keatinge 1973 replica.csv")

#initial visualization to determine if lm is appropriate
fat_plot <- ggplot(data=fat, aes(x=leanness, y=lossrate)) + 
  geom_point()
fat_plot

fat_mod <- lm(lossrate ~ leanness, data=fat)

#assumptions
simulate(fat_mod, nsim = 100) %>%
  pivot_longer(cols = everything(), 
               names_to = "sim", values_to = "lossrate") %>%
  ggplot(aes(x = lossrate)) +
  geom_density(aes(group = sim), size = 0.2) +
  geom_density(data = fat, color = "blue", size = 2)

plot(fat_mod, which=1)
plot(fat_mod, which=2)

#f-tests of model
anova(fat_mod)

#t-tests of parameters
summary(fat_mod)

#plot with line
fat_plot + 
  stat_smooth(method=lm, formula=y~x)

#--- add some fade

deet <- read.csv("./data/17q24DEETMosquiteBites.csv")

deet_plot <- ggplot(data=deet, aes(x=dose, y=bites)) + 
  geom_point()

deet_plot

deet_mod <- lm(bites ~ dose, data=deet)

#assumptions
simulate(deet_mod, nsim = 100) %>%
  pivot_longer(cols = everything(), 
               names_to = "sim", values_to = "bites") %>%
  ggplot(aes(x = bites)) +
  geom_density(aes(group = sim), lwd = 0.2) +
  geom_density(data = deet, color = "blue", lwd = 2)


plot(___, which=1)
plot(___, which=2)

#f-tests of model
anova(___)

#t-tests of parameters
summary(___)

#plot with line
deet_plot + 
  stat_smooth(method=lm, formula=y~x)




