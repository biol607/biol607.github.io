#'---------------------------------------------
#' @title General Linear Models
#' 
#' @date 2020-11-20
#' 
#'---------------------------------------------

library(dplyr)
library(tidyr)
library(modelr)
library(car)
library(emmeans)
library(brms)
library(tidybayes)
library(ggdist)
library(ggplot2)

intertidal <- read.csv("data/18e3IntertidalAlgae.csv")

ggplot(data = intertidal,
       mapping = aes(x = herbivores, y = sqrtarea,
                     color = height)) +
  geom_point(position = position_dodge(width = 0.5),
             alpha = 0.5)


ggplot(data = intertidal,
       mapping = aes(x = herbivores, y = sqrtarea,
                     fill = height)) +
  geom_boxplot(position = position_dodge(width = 0.75), 
               width = 0.5)

ggplot(data = intertidal,
       mapping = aes(x = height, y = sqrtarea,
                     fill = herbivores)) +
  geom_boxplot(position = position_dodge(width = 0.5), 
               width = 0.5)

ggplot(data = intertidal,
       mapping = aes(y = herbivores, x = sqrtarea,
                     fill = height)) +
  stat_halfeye(position = position_dodge(width = 0.5))


ggplot(data = intertidal,
       mapping = aes(y = herbivores, x = sqrtarea,
                     color = height)) +
  stat_summary()


ggplot(data = intertidal,
       mapping = aes(x = height, y = sqrtarea,
                     color = herbivores)) +
  stat_summary() +
  stat_summary(fun = mean, geom = "line",
               aes(group = herbivores))



# Model our factorial design ####
intertidal_lm <- lm(sqrtarea ~ herbivores + height +
                      herbivores:height,
                    data = intertidal)

intertidal_lm <- lm(sqrtarea ~ herbivores*height, 
                    data = intertidal)

# assumptions
plot(intertidal_lm, which = 1)
plot(intertidal_lm, which = 2)
plot(intertidal_lm, which = 4)


residualPlots(intertidal_lm)

# get residuals and add them to our data frame
intertidal <- intertidal %>%
  modelr::add_residuals(intertidal_lm)

#evaluate our assumptions in terms of no group effect on residuals
ggplot(intertidal,
       aes(x = height, y = resid)) +
  geom_boxplot()

ggplot(intertidal,
       aes(x = herbivores, y = resid)) +
  geom_boxplot()

ggplot(intertidal,
       aes(x = herbivores, y = resid, fill = height)) +
  geom_point(position = position_dodge(width = 0.5))

#evaluate our model ####
Anova(intertidal_lm)
Anova(intertidal_lm, type = "III")

# post-hocs ####
#we are comparing all possible means
intertidal_em <- emmeans(intertidal_lm,
                         specs = ~ herbivores + height)


contrast(intertidal_em, "tukey", adjust = "none") %>% 
  plot() +
  geom_vline(xintercept = 0, color = "red")

# does herbivore treatment matter at high or low heights?
intertidal_em_2 <- emmeans(intertidal_lm,
                           ~ herbivores | height)


contrast(intertidal_em_2, "tukey", adjust = "none")

contrast(intertidal_em_2, "tukey", adjust = "none") %>%
  plot()

#----------------------#
# Faded example
# interested in Predator_Diversity and Trial
# and their effect on Porp_Change
# Might also want to look at Treatment
kelp <- read.csv("./data/kelp_pred_div_byrnesetal2006.csv")


## Check and correct for non-factors
str(kelp)
kelp <- kelp %>%
  mutate(Predator_Diversity = as.factor(Predator_Diversity),
         Trial = as.factor(Trial))

#Visualize
qplot(Porp_Change,Treatment, data=kelp, geom="boxplot", fill=Trial)

ggplot(kelp,
       aes(x = Predator_Diversity,
           y = Porp_Change,
           fill = Trial)) +
  geom_boxplot(position = position_dodge())


#fit
kelp_lm <- lm(Porp_Change ~ Predator_Diversity * Trial, data=kelp)

#assumptions
plot(kelp_lm, which=c(1,2,4))

#residualPlots(_________) #might not work - play!

#ANOVA
Anova(kelp_lm)

#Tukey's HSD for simple effects
contrast(emmeans(kelp_lm, ~Predator_Diversity), method = "tukey")
                 
#------
# General Linear Models with both categorical and continuous
# predictors

neand <- read.csv("data/18q09NeanderthalBrainSize.csv")

#exploring the data
ggplot(neand,
       aes(x = species, y = lnbrain)) +
  geom_boxplot()

ggplot(neand,
       aes(x = lnmass, y = lnbrain)) +
  geom_point() +
  stat_smooth(method ="lm")

ggplot(neand,
       aes(x = lnmass, y = species)) +
  stat_halfeye()



ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  stat_smooth(method ="lm", fill = NA)

# fit a model
neand_glm <- glm(lnbrain ~ lnmass + species,
                 data = neand,
                 family = gaussian(link = "identity"))

# assumptions
plot(neand_glm, which = 1)
plot(neand_glm, which = 2)
plot(neand_glm, which = 4)

residualPlots(neand_glm)

neand <- neand %>%
  add_residuals(neand_glm)

qplot(species, resid, data = neand, geom = "boxplot")

#test of an interaction between mass and species
neand_int <- glm(lnbrain ~ lnmass * species,
                 data = neand,
                 family = gaussian(link = "identity"))
#assumption
Anova(neand_int)

#model
Anova(neand_glm)

#post-hocs
# neand_em <- emmeans(neand_glm, specs = ~ species | lnmass,
#                     at = list(lnmass = 4))
neand_em <- emmeans(neand_glm, specs = ~ species)
neand_em

contrast(neand_em, method = "tukey")


#look at slopes
emtrends(neand_glm, specs = ~species, var = "lnmass")

#if we had an interaction
emtrends(neand_int, specs = ~species, var = "lnmass")

emmeans(neand_int, specs = ~species|lnmass,
        at = list(lnmass = c(1,2,4,6,8))) %>%
  contrast(method = "tukey") %>% 
  plot()


# visualize!

neand_newdat <- modelr::data_grid(neand,
                                  species = unique(species),
                                  lnmass = seq_range(lnmass, n = 100))

neand_predict <- predict(neand_glm,
                         newdata = neand_newdat,
                         type = "response") 

neand_newdat <- neand_newdat %>%
  mutate(lnbrain = neand_predict)

ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  geom_line(data= neand_newdat)

#use emmeans for predictions

neand_newfit <- emmeans(neand_glm, specs = ~ species + lnmass,
                        at = list(lnmass = seq(4,4.5, length.out = 100))) %>%
  as_tibble() %>%
  rename(lnbrain = emmean)

#make a plot
ggplot(neand,
       aes(x = lnmass, y = lnbrain, color = species)) +
  geom_point() +
  geom_line(data= neand_newfit) +
  geom_ribbon(data = neand_newfit,
              aes(ymin = asymp.LCL, ymax = asymp.UCL, group = species),
              alpha = 0.1, color = "lightgrey") +
  theme_classic()

#rsq
piecewiseSEM::rsquared(neand_glm)
