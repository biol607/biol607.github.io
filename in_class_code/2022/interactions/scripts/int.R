#' ------------------------------------------
#' Interaction Effects and Nonlinearities with
#' the General Linear Model! Hooray!
#' ------------------------------------------

##### Libraries ####

# data loading and manipulation
library(readr)
library(dplyr)

# libraries to help with stats
library(car)
library(broom)
library(performance) # model checking!
library(emmeans) #posthocs

# libraries for data viz and querying my model
library(ggplot2)
library(visreg)
library(GGally)
library(modelr) #using data_grid and other functions

# set a theme for today
theme_set(theme_bw(base_size = 16))

#### Interacting Cateogrical Variables ####
intertidal <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18e3IntertidalAlgae.csv")


# plot the data
ggplot(data = intertidal,
       mapping = aes(x = herbivores, y = sqrtArea,
                     color = height, fill = height))+
  geom_boxplot() #+
  #geom_point(size = 2, position = position_dodge(width = 0.4), alpha = 0.4)


ggplot(data = intertidal,
       mapping = aes(y = herbivores, x = sqrtArea,
                     color = height, fill = height))+
  ggridges::geom_density_ridges()


# Model this with an interaction

intertidal_add_lm <- lm(sqrtArea ~ herbivores + height, data = intertidal)

intertidal_lm <- lm(sqrtArea ~ herbivores + height +
                      herbivores : height, data = intertidal)

intertidal_lm <- lm(sqrtArea ~ herbivores * height, data = intertidal)

#lm(a ~ b*c*d*e) - all expansions done for you!
#lm(a ~ (b+c)*d) - would expand do b + c +d + b*d + c*d

# check assumptions!
check_model(intertidal_lm)

# look at what the model tells us!

tidy(intertidal_lm)
r2(intertidal_lm)

# Let's look at the means and some differences
emmeans(intertidal_lm, specs =  ~ herbivores)

# BUT - let's look at simple effects!
emmeans(intertidal_lm, specs = ~ herbivores + height)

emmeans(intertidal_lm, specs = ~ herbivores | height)

emmeans(intertidal_lm, specs = ~ height | herbivores)

# pairwise comparisons
emmeans(intertidal_lm, specs = ~ herbivores + height) |>
  contrast(method = "pairwise") |>
  confint(adjust = "none") |> plot()

# a more targeted question - does the effect of herbivores
# vary by height?
emmeans(intertidal_lm, specs = ~ herbivores | height) |>
  contrast(method = "pairwise") |>
  confint() |> plot()

# does the effect of tide height vary by herbivore presence?

emmeans(intertidal_lm, specs = ~ height | herbivores) |>
  contrast(method = "pairwise") |>
  confint() |> plot()

# plot this
intertidal_em <- 
  emmeans(intertidal_lm, specs = ~ herbivores + height) |> tidy()

ggplot(data = intertidal,
       aes(x = height, y = sqrtArea, group = herbivores)) +
  geom_point(color = "lightgrey", 
             position = position_dodge(width = 0.2),
             alpha = 0.5) +
  geom_pointrange(data = intertidal_em,
                  mapping = aes(ymin = estimate - 2*std.error,
                      ymax = estimate + 2*std.error,
                      y = estimate,
                      color = herbivores),
                  position = position_dodge(width = 0.2))


# A Kelpy example
kelp <- read_csv("https://biol607.github.io/lab/data/kelp_pred_div_byrnesetal2006.csv")


## Check and correct for non-factors
## (this is some dplyr fun)
kelp <- kelp |>
  mutate(Trial = as.character(Trial))


#Visualize
ggplot(data = kelp,
         aes(Treatment, Porp_Change, fill=Trial)) +
  geom_boxplot()

#fit
kelp_lm <- lm(Porp_Change ~ Treatment * Trial, data = kelp)

#assumptions
performance::check_model(kelp_lm)

# look at the model
broom::tidy(kelp_lm)

# how well did we explain the data?
r2(kelp_lm)

#Pairwise Comparison of simple effects
emmeans(kelp_lm, specs = ~ Trial |Treatment) |>
  contrast(method = "pairwise") |>
  confint() |> plot()

# What I actually did
emmeans(kelp_lm, specs = ~ Treatment) |>
  contrast(method = "trt.vs.ctrl", ref = "No Predators") |>
  confint(adjust = "none") |> plot()

# ----------------------------------------- #

#### Interactions between continuous variables ####
keeley <- read_csv("data/Keeley_rawdata_select4.csv")

#visualize
ggpairs(keeley)

# let's examine how richness is affected by firesev and if that changes 
# stand age

ggplot(data = keeley,
       mapping = aes(x = firesev,
                     y = rich,
                     color = age)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(vars( cut_number(age, 4)))

# Let's fit that model!
keeley_mlr_int <- lm(rich ~ firesev * age, data = keeley)


# check assumptions
check_model(keeley_mlr_int)
check_collinearity(keeley_mlr_int)

keeley |>
  select(firesev, age) |>
  cor()

#check vifs on additive scale
lm(rich ~ firesev + age, data  = keeley) |> check_collinearity()


# fit a centered model
keeley <- keeley |>
  mutate(age_c = age - mean(age),
         firesev_c = firesev - mean(firesev))

keeley_centered <- lm(rich ~ age_c * firesev_c, data = keeley)
vif(keeley_centered)

# r2 is...
r2(keeley_centered)
r2(keeley_mlr_int)

# Evaluate what the model is telling us!
tidy(keeley_mlr_int)

# visualize! with visreg
visreg(keeley_mlr_int, xvar = "firesev", by = "age", 
       gg = TRUE, breaks = 4)

# How do slopes differ at different ages?
slope_by_age <- emtrends(keeley_mlr_int, specs = ~age, var = "firesev",
         at = list(age = c(5,20,30, 60))) 

slope_by_age

slope_by_age |>
  contrast(method = "pairwise")

# see the change in slope
slope_curve_age <- emtrends(keeley_mlr_int, specs = ~age, var = "firesev",
                         at = list(age = seq(0,50, length.out = 100)))|>
  tidy()

ggplot(slope_curve_age,
       aes(x = age, y = firesev.trend)) +
  geom_line()

# Counterfactual Plots

# explore how richness changes by firesev at different levels of age
k_int_explore <- keeley |>
  data_grid(firesev = seq_range(firesev, n = 100),
            age = seq_range(age, n = 4)) |>
  augment(keeley_mlr_int, newdata = _, interval = "confidence") |>
  rename(rich = .fitted)

count_plot <- ggplot(data = k_int_explore, 
       mapping = aes(x = firesev, y = rich,
                     color = age)) +
  geom_line(mapping = aes(group = age)) +
  scale_color_viridis_c() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = age),
              fill = "lightgrey", alpha = 0.4, color = NA) 

count_plot+
  facet_wrap(vars(age)) 

# add the data
count_plot+
  geom_point(data = keeley) +
  facet_wrap(vars(cut_interval(age,4))) 


# what about a surface?
k_int_surf_data <- keeley |>
  data_grid(firesev = seq_range(firesev, n = 100),
            age = seq_range(age, n = 100)) |>
  augment(keeley_mlr_int, newdata = _, interval = "confidence") |>
  rename(rich = .fitted)

ggplot(data = k_int_surf_data,
       mapping = aes(x= firesev, y = age, fill = rich)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C")

# visreg for the surface
visreg2d(keeley_mlr_int, xvar = "firesev", yvar = "age")

# Example time
# Load the data
# 1. plankton from Baikal!
# 2. data(meadows, pacakge = "piecewiseSEM")
# 3. data(tips, package = "reshape")
# 4. OR - look at 1.4 in the lab - kelp has has a "diversity" column - 
#   and you get diferent answers if div is continuous or categorica

# Perform a preliminary visualization. 
# Play with this and choose two predictors (or go hog wild)

# Fit a MLR model with an interaction

# Test Asssumptions and modify model if needed

# Evaluate results

# Visualize results and dig in!

# Let us know when you have something cool!
