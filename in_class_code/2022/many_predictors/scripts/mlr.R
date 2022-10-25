#' --------------------------------------------------
#' Multiple predictor types lab
#' 
#' @date 2022-10-21
#' --------------------------------------------------

##### Libraries ####
library(readr)
library(dplyr)
library(broom)
library(emmeans)
library(modelr)
library(performance)
library(car) #companion to applied regression
library(ggplot2)
library(GGally)

theme_set(theme_bw(base_size = 14))

#### Multiple Linear Regression ####
keeley <- read_csv("data/Keeley_rawdata_select4.csv")

# quick look!
skimr::skim(keeley)
pairs(keeley)
ggpairs(keeley) # from GGally

# Our intention is to model species richness as a function
# of fire severity and plant cover in a plot

# visualize
ggplot(data = keeley,
       aes(x = firesev, y = rich,
           color = cover, size = cover)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(vars(cut_interval(cover, 4)))



ggplot(data = keeley,
       aes(x = cover, y = rich,
           color = firesev, size = firesev)) +
  geom_point() +
  scale_color_viridis_c(option = "B") +
  facet_wrap(vars(cut_interval(firesev, 4)))

# I feel comfortable modeling this!

## Model!

keeley_lm <- lm(rich ~ cover + firesev, 
                data = keeley)

## Assumptions!
check_model(keeley_lm)

#VIFs for collinearity
vif(keeley_lm)

# correlation of predictors
keeley |>
  select(firesev, cover) |>
  cor()

#normality
check_normality(keeley_lm) |> plot()
check_normality(keeley_lm) |> plot("qq")

# car::residualPlots
residualPlots(keeley_lm)

# evaluate our model
tidy(keeley_lm)

# if we had centered
keeley <- keeley |>
  mutate(cover_cent = cover - mean(cover),
         firesev_cent = firesev - mean(firesev))

lm(rich ~ cover_cent + firesev_cent, data = keeley) |>
  tidy()

# R2 - how much variation in richness is explained?
r2(keeley_lm)

# old skool
summary(keeley_lm)

# If we want partial correlations between predictors
# and responses - how strong is the association we can
# z-transorm EVERYTHING and refit out lm.
library(effectsize)
effectsize(keeley_lm)
effectsize(keeley_lm, method = "basic")

# Viz #

# Look at each predictor setting the others to their mean
library(visreg)

visreg(keeley_lm)

keeley_vr <- visreg(keeley_lm, plot = FALSE)

# Roll your own plot
keeley_pred <- data_grid(keeley,
                         cover = mean(cover),
                         firesev = 
                           seq_range(firesev, 100))

keeley_pred <- augment(keeley_lm, 
                       newdata = keeley_pred,
                       interval = "confidence")

ggplot(data = keeley,
       aes(x = firesev, y = rich)) +
  geom_point() +
  geom_line(data = keeley_pred,
            mapping = aes(y = .fitted),
            color = "red") +
  # CI
  geom_ribbon(data = keeley_pred,
              mapping = aes(y = .fitted,
                            ymin = .lower,
                            ymax = .upper),
              alpha = 0.3, color = "lightgrey")


# let's say we want to show the quartiles of cover
# to understand how the line changes with cover
visreg2d(keeley_lm, xvar = "firesev", yvar = "cover")

keeley_pred_2d <- data_grid(keeley,
                            cover = seq_range(cover, 3),
                            firesev = seq_range(firesev, 100))

keeley_pred_2d <- augment(keeley_lm,
                          newdata = keeley_pred_2d)

ggplot(data = keeley,
       aes(x = firesev, y = rich)) +
  geom_point() +
  geom_line(data = keeley_pred_2d,
            mapping = aes(y = .fitted,
                          color = as.character(cover))) +
  facet_wrap(vars(cut_interval(cover, 3)))


#### The Plankton Data ####

# Load the data
plankton <- read_csv("data/planktonSummary.csv")
skimr::skim(plankton)

# Perform a preliminary visualization. 
# Play with this and choose two predictors
ggpairs(plankton |> select(-Station))

# Fit a MLR model
plank_lm <- lm(log(CHLFa) ~ DIN + DIP + SAL, data = plankton)

# Test Asssumptions and modify model if needed
check_model(plank_lm)

# Evaluate results
r2(plank_lm)

tidy(plank_lm)

standardize_parameters(plank_lm, method = "basic")

# Visualize results
visreg(plank_lm, ylim = c(-3,8))

# A back-transform example
plank_pred <- data_grid(plankton,
                        SAL = mean(SAL),
                        DIN = seq_range(DIN, 100),
                        DIP = seq_range(DIP, 100))

plank_pred <- augment(plank_lm,
                      newdata = plank_pred) |>
  mutate(CHLFa_fitted = exp(.fitted)) #back-transform

ggplot(data = plank_pred,
       mapping = aes(x = DIN,
                     y = DIP,
                     fill = CHLFa_fitted)) +
  geom_raster() +
  scale_fill_viridis_c()

# Let's look at a few lines

plank_pred_lines <- data_grid(plankton,
                        SAL = mean(SAL),
                        DIN = seq_range(DIN, 100),
                        DIP = seq_range(DIP, 3))

plank_pred_lines <- augment(plank_lm,
                            newdata = plank_pred_lines,
                            interval = "confidence") |>
  mutate(CHLFa_fitted = exp(.fitted),
         CHLFa_lower = exp(.lower),
         CHLFa_upper = exp(.upper))

ggplot(data = plank_pred_lines,
        mapping = aes(x = DIN,
                      y = CHLFa_fitted,
                      ymin = CHLFa_lower,
                      ymax = CHLFa_upper,
                      color = as.character(DIP),
                      group = DIP)) +
  geom_ribbon(fill = "lightgrey", color = NA) +
  geom_line()

#### Multiple Categorical Variables ####
zooplankton <- read_csv("data/18e2ZooplanktonDepredation.csv") |>
  mutate(block = as.character(block))

skimr::skim(zooplankton)

# Perform a preliminary visualization. 
zoop_plot <- ggplot(data = zooplankton,
       aes(x = treatment,
           y = zooplankton)) +
  geom_point()

ggplot(data = zooplankton,
       aes(x = block,
           y = zooplankton)) +
  geom_point()

zoop_plot

# Fit a MLR model
zoop_lm <- lm(zooplankton ~ treatment + block,
              data = zooplankton)

# Test Assumptions and modify model if needed
check_model(zoop_lm)

# Evaluate results
tidy(zoop_lm) #do not want
zoop_em <- emmeans(zoop_lm, specs = ~treatment)
zoop_em |> confint()

emmeans(zoop_lm, ~block) |> confint()

# fit
r2(zoop_lm)

# differences
contrast(zoop_em, "pairwise") |> confint(adjust = "none")

# Visualize results
zoop_em_for_plotting <- zoop_em |> 
  confint() |> 
  as_tibble()

zoop_plot +
  geom_pointrange(data = zoop_em_for_plotting,
                  aes(y = emmean,
                      ymin = lower.CL,
                      ymax = upper.CL),
                  color = "red")

# Faded Example
#load the data
bees <- read.csv("./data/18q07BeeGeneExpression.csv") |>
  mutate(colony = as.character(colony))

#Visualize
ggplot(data=bees,
         aes(x = type, y = Expression)) +
  geom_jitter()

#fit
bees_lm <- lm(Expression ~ type + colony, data=bees)

#assumptions
check_model(bees_lm)
plot(bees_lm)

#pairwise comparison
emmeans(bees_lm, spec = ~ type) |>
  contrast(method = "pairwise") |>
  confint()

#### Mixing Categorical and Continuous Predictors ####

# Load the data
neand <- read_csv("data/18q09NeanderthalBrainSize.csv")
str(neand)


# Perform a preliminary visualization. 
ggplot(neand, 
       mapping = aes(x = species, y = lnbrain)) +
  geom_boxplot()

ggplot(neand, 
       mapping = aes(x = species, y = lnmass)) +
  geom_boxplot()

ggplot(neand, 
       mapping = aes(x = lnmass, y = lnbrain,
                     color = species)) +
  geom_point()

# Fit a  model
neand_lm <- lm(lnbrain ~ lnmass + species, data = neand)

# Test Asssumptions and modify model if needed
check_model(neand_lm)

# parallel slope assumption
neand_lm_int <- lm(lnbrain ~ lnmass + species +
                    lnmass : species, data = neand)

tidy(neand_lm_int) # our slopes ARE parallel

# Evaluate results
tidy(neand_lm)

neand_means <- emmeans(neand_lm, specs = ~ species | lnmass)
neand_means

contrast(neand_means, method = "pairwise") |>
  confint()


# Visualize results

# this is bad
badplot <- ggplot(neand, 
       mapping = aes(x = lnmass, y = lnbrain,
                     color = species)) +
  geom_point() + 
  stat_smooth(method = "lm")


# roll your own with data_grid and augment
neand_pred <- data_grid(neand,
                        lnmass = seq_range(lnmass, 100),
                        species = unique(species))


neand_pred <- augment(neand_lm, newdata = neand_pred,
                      interval = "confidence") |>
  rename(lnbrain = .fitted)

# the real plot
goodplot <- ggplot(neand, 
       mapping = aes(x = lnmass, y = lnbrain,
                     color = species)) +
  geom_point() +
  geom_line(data = neand_pred) +
  geom_ribbon(data = neand_pred,
              mapping = aes(ymin = .lower,
                            ymax = .upper, 
                            group = species),
              alpha = 0.5, fill = "lightgrey",
              color = NA)


library(patchwork)
badplot + goodplot
