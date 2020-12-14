#'------------------------------------------------
#' Multiple Linear Regrrression
#'------------------------------------------------


library(dplyr)
library(car)
library(ggplot2)
library(broom)
library(modelr)


keeley <- read.csv("data/Keeley_rawdata_select4.csv")

head(keeley)


#exploratory data viz ####

ggplot(data = keeley,
       aes(x = cover, y = rich, color = firesev)) +
  geom_point() +
  scale_color_gradient(low="yellow", high = "red") +
  facet_wrap(~cut_number(firesev, 4))

pairs(keeley)

# Fit a MLR
keeley_mlr <- lm(rich ~ firesev + cover, data = keeley)

#standard diagnostics
plot(keeley_mlr, which = 1)
plot(keeley_mlr, which = 2)
shapiro.test(residuals(keeley_mlr))
plot(keeley_mlr, which = 5)

#look at residuals by individual predictors
residualPlots(keeley_mlr)

#are predictors too correlated?
keeley %>%
  select(firesev, cover) %>%
  cor()

#variance inflation factor
vif(keeley_mlr)

#Evaluation ####
#f-test
Anova(keeley_mlr)

summary(keeley_mlr)


# Visualization  ####
library(visreg)
visreg(keeley_mlr, gg = TRUE)


# Our own homeade visreg!
#create a data frame of predictors
k_cover_pred <- data.frame(firesev = mean(keeley$firesev),
                           cover = seq(0, 1.6, length.out = 100))

#get fit and CI
k_cover_fit <- cbind(k_cover_pred,
                     predict(keeley_mlr,
                             newdata = k_cover_pred,
                             interval = "confidence"))

#plot our fit(s) with the data
ggplot(keeley,
       aes(x = cover, y = rich)) +
  geom_point() +
  geom_line(data = k_cover_fit, aes(y = fit), color = "blue") +
  geom_ribbon(data = k_cover_fit,
              aes(y = fit, ymin = lwr, ymax = upr),
              alpha = 0.3, color = "lightgrey")


# Visualize with multiple different combinations of predictors

k_firev_explore <- data_grid(keeley,
                             cover = seq_range(cover, 100),
                             firesev = seq_range(firesev, 4)) %>%
  add_predictions(model = keeley_mlr, var = "rich")

ggplot(data = keeley,
       aes(x = cover, y = rich, color = firesev)) +
  geom_point() +
  geom_line(data = k_firev_explore) +
  scale_color_gradient(low="yellow", high = "red") +
  facet_wrap(~cut_interval(firesev, 4)) 

# Add an interaction effect ####
keeley_mlr_int <- lm(rich ~ cover*firesev, data = keeley)

residualPlots(keeley_mlr_int)
vif(keeley_mlr_int)


#fit a centered model
keeley <- keeley %>%
  mutate(firesev_c = firesev - mean(firesev),
         cover_c = cover - mean(cover))

keeley_mlr_int_c <- lm(rich ~ cover_c*firesev_c, data = keeley)
vif(keeley_mlr_int_c)


Anova(keeley_mlr_int)
Anova(keeley_mlr_int_c)


# visualize interactions ###

visreg(keeley_mlr_int, "cover", by = "firesev", gg = TRUE)

keeley_int_explore <- data_grid(keeley,
                                cover = seq_range(cover, 100),
                                firesev = seq_range(firesev, 4)) %>%
  add_predictions(model = keeley_mlr_int, var = "rich")


ggplot(data = keeley, 
       aes(x = cover, y = rich, color = firesev)) +
  geom_point() +
  geom_line(data = keeley_int_explore, aes(group = firesev)) +
  scale_color_gradient(low = "yellow", high = "red") +
  facet_wrap(~cut_interval(firesev, 4))


# surface
keeley_surf <- data_grid(keeley,
                         cover = seq_range(cover, 100),
                         firesev = seq_range(firesev, 100)) %>%
  add_predictions(model = keeley_mlr_int, var = "rich")


ggplot(data = keeley_surf,
       aes(x = cover, y = firesev, color = rich, fill = rich)) +
  geom_raster() +
  scale_fill_viridis_c(option = "B") +
  scale_color_viridis_c(option = "B") +
  geom_point(data = keeley)


