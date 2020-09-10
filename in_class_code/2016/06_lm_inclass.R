####################
#linear regression
####################

#libraries
library(dplyr)
library(broom)
library(modelr)
library(readr)
library(ggplot2)

seals <- read_csv("data/06/17e8ShrinkingSeals Trites 1996.csv")

system.time(read_csv("data/06/17e8ShrinkingSeals Trites 1996.csv"))
system.time(read.csv("data/06/17e8ShrinkingSeals Trites 1996.csv"))

str(seals)
seals

#visualize
seal_plot <- ggplot(seals, mapping = aes(x = age.days, y = length.cm)) +
  geom_point()

seal_plot

#fit model
seals_lm <- lm(length.cm ~ age.days, 
               data = seals)

#what is this object?
seals_lm
str(seals_lm)
names(seals_lm)

#Test our assumptions
plot(seals_lm)

#cook's distance
plot(seals_lm, which=4)

#distribution of residuals
res_seals <- residuals(seals_lm)
hist(res_seals)

#put it to the test!
anova(seals_lm)

#coefficients.. r^2
summary(seals_lm)

#Let's look at this with broom
library(broom)
tidy(seals_lm)
tidy(anova(seals_lm))
glance(seals_lm)

#modelr
library(modelr)
seals <- seals %>%
  add_residuals(seals_lm) %>%
  add_predictions(seals_lm)

seals


###Predictions
tibble(age.days = 50:100) %>%
  add_predictions(seals_lm)

predict(seals_lm, 
        newdata=data.frame(age.days=50:100),
        interval="confidence")


###VISUALIZATION
seal_fit_plot <- seal_plot +
  stat_smooth(method = "lm") +
  theme_bw()

seal_fit_plot

#prediction confidence interval
pred_data_frame <- data.frame(
  age.days = min(seals$age.days):
    max(seals$age.days)
)

head(pred_data_frame)

pred_seals <- predict(seals_lm, 
          newdata=pred_data_frame,
          interval="predict")

head(pred_seals)

pred_data_frame <- cbind(pred_data_frame,
                         pred_seals) %>%
  rename(length.cm = fit)

head(pred_data_frame)

#visualize the prediction interval
seal_fit_plot +
  geom_ribbon(data = pred_data_frame,
              mapping = aes(x=age.days,
                            ymax = upr,
                            ymin = lwr),
              fill = "grey", alpha = 0.5)

#Examples
deet <- read.csv("./data/06/17q24DEETMosquiteBites.csv")
deet_mod <- lm(bites ~ dose, data=deet)
tidy(deet_mod)
