#'----------------------
#' Linear Regression!
#' 
#' @author Jarrett Byrnes
#' @date 2018-10-12
#' 
#'----------------------

library(readr)
library(tidyverse)
library(ggplot2)

seals <- read_csv("../data/17e8ShrinkingSeals Trites 1996.csv")

head(seals)

seals_base <- ggplot(seals,
                     mapping = aes(x = age.days,
                                   y = length.cm)) +
  geom_point()

seals_base


#### THIS IS OUR WORKFLOW!
#1. Fit a model

#2. Evaluate the assumption of that model

#3. Evaluate the model itself (testing!)

#4. Visualization

#----
#Applying the workflow to the seal data ####
#----

#1. Fit a model

seal_mod <- lm(length.cm ~ age.days, data = seals)

#2. Evaluate the assumption of that model

plot(seal_mod)

par(mfrow = c(2,2))
plot(seal_mod)
par(mfrow = c(1,1))

#or one by one
plot(seal_mod, which = 1) #residual v fitted
plot(seal_mod, which = 2) #qq plot
plot(seal_mod, which = 4) #cook's distance
plot(seal_mod, which = 5) #leverage v. residual

hist(residuals(seal_mod))

#assumptions with modelr
library(modelr)

seals <- seals %>%
  add_residuals(seal_mod)

head(seals)

ggplot(data = seals, aes(x = resid)) +
  geom_histogram()


ggplot(data = seals, aes(sample = resid))  +
  geom_qq(shape = 1) +
  geom_qq_line(color = 'red')

#3. Evaluate the model itself (testing!)

# f-test ####
anova(seal_mod)

anova(seal_mod) %>% broom::tidy()

# t-tests and r^2 ####
library(broom)
summary(seal_mod)

summary(seal_mod) %>% broom::tidy()
summary(seal_mod) %>% broom::glance()

#4. Visualization
seals <- seals %>%
  add_predictions(seal_mod)


seals_base +
  geom_line(data = seals,
            mapping = aes(y = pred), 
            color = "blue", size = 2)

#with ggplot
seals_base +
  stat_smooth(method = "lm",
              color = "blue", 
              size = 2)

#how do we add prediction confidence interval?
pred_frame <- predict(seal_mod, interval = "prediction")

head(pred_frame)

#bind some columns
pred_frame <- cbind(seals, pred_frame)

seals_base +
  stat_smooth(method = "lm",
              color = "blue", 
              size = 2) +
  geom_ribbon(data = pred_frame,
              mapping = aes(x = age.days,
                      ymin = lwr,
                      ymax = upr),
              fill = "grey", alpha = 0.6)

#------------------
## Faded examples ####
#------------------

fat <- read_csv("../data/17q04BodyFatHeatLoss Sloan and Keatinge 1973 replica.csv")

#initial visualization to determine if lm is appropriate
fat_plot <- ggplot(data=fat, 
                   mapping = aes(x=leanness, y=lossrate)) + 
  geom_point()
fat_plot

#fit a model
fat_mod <- lm(lossrate ~ leanness, data=fat)

#assumptions
plot(fat_mod, which=1)
plot(fat_mod, which=2)

#f-tests of model
anova(fat_mod)

#t-tests of parameters
summary(fat_mod)

#plot with line
fat_plot + 
  stat_smooth(method=lm)

#----------
## DEET AND MOSQUITOS
#----------

deet <- read.csv("../data/17q24DEETMosquiteBites.csv")

deet_plot <- ggplot(data=deet, aes(x=dose, y=bites)) + 
  geom_point()

deet_plot

deet_mod <- lm(bites ~ dose, data=deet)

#assumptions
plot(deet_mod, which=1)
plot(deet_mod, which=2)

#f-tests of model
anova(deet_mod)

#t-tests of parameters
summary(deet_mod)

#plot with line
deet_plot + 
  stat_smooth(method=lm, formula=y~x)


### Dealing with a nonlinearity???
deet_mod_log <- lm(log(bites) ~ dose, data=deet)

plot(deet_mod_log, which  = 2)


deet_plot +
  scale_y_continuous(trans = "log") +
  stat_smooth(method = "lm")

#------
# mortality and homerange
#------

zoo <- read.csv("../data/17q02ZooMortality Clubb and Mason 2003 replica.csv")

zoo_plot <- ggplot(data=___, aes(x=mortality, y=homerange)) + 
  ___()

___

zoo_mod <- lm(___, data=___)

#assumptions
plot(___, which=1)
plot(___, which=2)

#f-tests of model
anova(___)

#t-tests of parameters
summary(___)

#plot with line
zoo_plot + 
  stat_smooth(method=___)

#What would you do to fix nonlinearity issues