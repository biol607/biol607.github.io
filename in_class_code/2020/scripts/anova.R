#'-----------------------------------------------
#' @title Looking at models with multiple means
#' 
#' @description We're using linear models, but with
#' categorical descriptors (so, lots of 1s and 0s)
#'-----------------------------------------------

#libraries
library(dplyr)
library(ggplot2)
library(readr)
library(brms)
library(tidybayes)
library(ggdist)

# one or two newer libraries for ANOVA
library(emmeans)
library(car) #companion to applied regression by John Fox

#One-Way ANOVA-style model
knees <- read_csv("data/15e1KneesWhoSayNight.csv")
head(knees)

skimr::skim(knees)
unique(knees$treatment)

# plot the data
ggplot(data = knees,
       mapping = aes(x = treatment, y = shift)) +
  geom_point() +
  stat_summary(color = "red")



# Fit the model!!!!

knees_lm <- lm(shift ~ treatment, data = knees)

knees_glm <- glm(shift ~ treatment, 
                 data = knees,
                 family = gaussian(link = "identity"))

knees_brm <- brm(shift ~ treatment,
                 data = knees,
                 family = gaussian(link = "identity"))


# check assumptions!!!
plot(knees_lm, which = 1)
plot(knees_lm, which = 2)
shapiro.test(residuals(knees_lm))

plot(knees_lm, which = 4)
plot(knees_lm, which = 5)

#add residuals to our data frame
knees <- knees %>%
  modelr::add_residuals(knees_lm)

#plotting residuals by group with car
residualPlots(knees_lm) #not sure why this isn't working

ggplot(knees,
       aes(x = treatment, y = resid)) +
  geom_boxplot()

#check for HOV
leveneTest(knees_lm)

#check our density match
pp_check(knees_brm, nsamples = 100)


# Let's evaluate our model! ####
anova(knees_lm)
anova(knees_glm, test = "LRT")

# car::Anova
Anova(knees_lm)
Anova(knees_glm)

# let's look at some coefficients and other info ####
#r2
summary(knees_lm)
bayes_R2(knees_brm)

#means contrasts
knees_lm_means <- lm(shift ~ treatment - 1, data = knees)
summary(knees_lm_means)

#the means
emmeans(knees_lm, ~treatment)
emmeans(knees_glm, ~treatment)
knees_brm_em <- emmeans(knees_brm, ~treatment)

#bayesian emmeans
knees_draws<- gather_emmeans_draws(knees_brm_em)

ggplot(knees_draws,
       aes(x = treatment, y = .value)) +
  stat_halfeye() + coord_flip()

#compare means
knees_em <- emmeans(knees_lm, ~treatment)

knees_cont <- contrast(knees_em, method = "tukey", adjust = "none")

plot(knees_cont) +
  geom_vline(xintercept = 0, color = "red")


library(multcompView)
CLD(knees_em)
pwpp(knees_em)


knees_brm_cont <- emmeans(knees_brm, ~treatment) %>%
  contrast(method = "tukey") %>%
  gather_emmeans_draws()

# contrasts with Region of Practical Equivalence
ggplot(knees_brm_cont, 
       aes(y = contrast, x = .value)) +
  stat_halfeye() +
  geom_vline(xintercept = c(-0.5, 0.5), color = "red")

knees_brm_cont %>%
  group_by(contrast) %>%
  summarize(rope_density = sum(.value > -0.5 & .value < 0.5)/n())



# Faded Examples

#General workflow for an ANOVA_style model with least squares
# 1
plants <- read.csv("./data/15q01PlantPopulationPersistence.csv")

#Visualize
qplot(treatment, generations, data=plants, geom="boxplot")

#fit
plant_lm <- lm(generations ~ treatment, data=plants)

#assumptions
plot(plant_lm, which=c(1,2,4,5))

#ANOVA
Anova(plant_lm)

#Tukey's HSD
contrast(emmeans(plant_lm, ~treatment), method = "tukey")
contrast(emmeans(plant_lm, ~treatment), method = "tukey", adjust = "none")
contrast(emmeans(plant_lm, ~treatment), method = "dunnett", adjust = "none")


#Second, how do different host types affect 
# nematode longevity?

worms <- read.csv("./data/15q19NematodeLifespan.csv")

#Visualize
qplot(treatment, lifespan, data=worms, geom="point")

#fit
worm_lm <- lm(lifespan ~ treatment, data=worms)

#assumptions
plot(worm_lm, which=c(1,2,4,5))

#ANOVA
Anova(worm_lm)

#Tukey's HSD
contrast(emmeans(worm_lm, ~treatment), method = "tukey") %>% 
  plot() + geom_vline(xintercept = 0)



# And last, how about how number of genotypes affect 
# eelgrass productivity. Note, THERE IS A TRAP HERE. 
# Look at your dataset before you do ANYTHING.

eelgrass <- read.csv("./data/15q05EelgrassGenotypes.csv") %>%
  mutate(treatment.genotypes = factor(treatment.genotypes))

#Visualize
qplot(treatment.genotypes, shoots, data=eelgrass, geom="point") +
  stat_smooth(method = "lm")

#fit
eelgrass_lm <- lm(shoots ~ treatment.genotypes, data=eelgrass)

#assumptions
plot(eelgrass_lm, which=c(1,2,4,5))

#ANOVA
anova(eelgrass_lm)

#Tukey's HSD
contrast(emmeans(eelgrass_lm, ~treatment.genotypes), method = "tukey") %>%
  plot() + geom_vline(xintercept = 0)


# Multiway models ####
zoop <- read_csv("data/18e2ZooplanktonDepredation.csv",
                 col_types = "fnf") #make sure block is a character

ggplot(zoop,
       aes(x = treatment, y = zooplankton)) +
  geom_boxplot() +
  facet_wrap(~block)

ggplot(zoop,
       aes(x = treatment, y = zooplankton, color = block)) +
  geom_point()

ggplot(zoop,
       aes(x = block, y = zooplankton, color = treatment)) +
  geom_point()


# Fit a model with lm
zoop_lm <- lm(zooplankton ~ treatment + block,
              data = zoop)

# Look at assumptions
plot(zoop_lm)

#plot residuals by treatment and test for non-additivity
residualPlots(zoop_lm)


# F-test
Anova(zoop_lm)
anova(zoop_lm)

summary(zoop_lm)


zoop_trt_em <- emmeans(zoop_lm, ~ treatment)
zoop_trt_em

contrast(zoop_trt_em, method = "dunnett")
contrast(zoop_trt_em, method = "tukey", adjust = "none")
contrast(zoop_trt_em, method = "tukey", adjust = "tukey")
contrast(zoop_trt_em, method = "tukey", adjust = "bonferroni")

#----
zoop_no_block <- lm(zooplankton ~ treatment,
                    data = zoop)

anova(zoop_lm, zoop_no_block)

anova(zoop_lm)

# what if we lost a sample?
#drop two rows
zoop_loss <- zoop[-c(1:2),]

loss_lm_1 <- lm(zooplankton ~ treatment + block,
                data = zoop_loss)


loss_lm_2 <- lm(zooplankton ~ block + treatment,
                data = zoop_loss)

anova(loss_lm_1)
anova(loss_lm_2)


loss_lm_int <- lm(zooplankton ~ 1, data = zoop_loss)
loss_lm_trt <- lm(zooplankton ~ treatment, data = zoop_loss)
loss_lm_blk <- lm(zooplankton ~ block, data = zoop_loss)

#type I comparisons
anova(loss_lm_trt, loss_lm_int)
anova(loss_lm_1, loss_lm_blk)

#type II
car::Anova(loss_lm_1)
car::Anova(loss_lm_2)

#type II comparisons
anova(loss_lm_1, loss_lm_blk)
anova(loss_lm_1, loss_lm_trt)

