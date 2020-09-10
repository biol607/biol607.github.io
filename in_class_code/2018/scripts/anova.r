#'-------------------------------
#' ANOVA!!!!!
#' 
#'-------------------------------

library(tidyverse)
library(car)
library(emmeans)
library(multcompView)
library(broom)
library(ggplot2)
library(modelr)

#data
knees <- read_csv("../data/15e1KneesWhoSayNight.csv")

#visualize the data! ####
ggplot(knees,
       aes(x = treatment, y = shift)) +
  geom_boxplot()

ggplot(knees,
       aes(x = treatment, y = shift)) +
  geom_point()

#model it! ####
knees_lm <- lm(shift ~ treatment, data = knees)

#assumptions of ANOVA ####
#no variance-treatment/estimated value relationship
#(i.e. fitted-residual)
plot(knees_lm, which = 1)
residualPlots(knees_lm, groups = treatment)

library(modelr)
knees <- knees %>%
  add_residuals(knees_lm)

ggplot(knees,
       aes(x = treatment, y = resid)) +
  geom_boxplot()

#qq plot
plot(knees_lm, which = 2)
leveneTest(knees_lm) #for HOV
shapiro.test(residuals(knees_lm)) #for normality

#leverage plot
plot(knees_lm, which = 5)
plot(knees_lm, which = 4)


# ANOVA - f-test ##
anova(knees_lm)
Anova(knees_lm)

# Evaluate our model and do posthocs! ###
summary(knees_lm)


knees_lm_noint <- lm(shift ~ treatment - 1,
                     data = knees)
anova(knees_lm_noint)
summary(knees_lm_noint)

#coefficients using emmeans
knees_em <- emmeans(knees_lm, ~treatment)

knees_em

emmeans::contrast(knees_em)

#posthocs
knees_tukey <- emmeans::contrast(knees_em,
                                 method = "tukey")

knees_tukey
plot(knees_tukey) +
  geom_vline(xintercept = 0, color = "red")

#LSD test - no adjusted p-values
emmeans::contrast(knees_em,
                  method = "tukey",
                  adjust = "none")

#other adjustments
emmeans::contrast(knees_em,
                  method = "tukey",
                  adjust = "bonferroni")

emmeans::contrast(knees_em,
                  method = "tukey",
                  adjust = "fdr")

#Dunnett
emmeans::contrast(knees_em,
                  method = "dunnett")

#change your control

emmeans::contrast(knees_em,
                  method = "dunnett",
                  ref = 2)

#visualize groupings
knees_ctab <- CLD(knees_em)

ggplot(knees_ctab,
       aes(x = treatment, y = emmean,
           ymin = lower.CL, ymax = upper.CL,
           color = factor(.group))) +
  geom_pointrange()

ggplot(knees,
       aes(x = treatment, y = shift)) +
  geom_point() +
  geom_pointrange(data = knees_ctab,
                  mapping = aes(x = treatment, y = emmean,
                                ymin = lower.CL, ymax = upper.CL,
                                color = factor(.group)))


#### Faded Examples ####
#Plant isolation
plants <- read.csv("../data/15q01PlantPopulationPersistence.csv")

#Visualize
qplot(treatment, generations, data=plants, geom="boxplot")

#fit
plant_lm <- lm(generations ~ treatment, data=plants)

#assumptions
plot(plant_lm, which=c(1,2,4,5))

#ANOVA
anova(plant_lm)

#Tukey's HSD
contrast(emmeans(plant_lm, ~treatment), method = "tukey")

CLD(emmeans(plant_lm, ~treatment), adjust = "none")

## How does host type affect nematode longevity
worms <- read.csv("../data/15q19NematodeLifespan.csv")

#Visualize
qplot(treatment, lifespan, data=worms, geom="boxplot") +
  stat_summary()

#fit
worm_lm <- lm(lifespan ~ treatment, data=worms)

#assumptions
plot(worm_lm, which=c(1,2,4,5))
leveneTest(worm_lm)

#ANOVA
anova(worm_lm)

#Tukey's HSD
contrast(emmeans(worm_lm, ~treatment), method = "tukey")

#plot the differences
CLD(emmeans(worm_lm, ~treatment)) %>%
  ggplot(aes(x = treatment, y = emmean, 
             ymin = lower.CL, ymax = upper.CL,
             color = factor(.group))) +
  geom_pointrange()

## Eelgrass genetic diversity
library(visdat)
eelgrass <- read.csv("../data/15q05EelgrassGenotypes.csv")
str(eelgrass)

vis_dat(eelgrass)
vis_miss(eelgrass)
vis_cor(eelgrass)

eelgrass <- eelgrass %>%
  mutate(treatment.genotypes = factor(treatment.genotypes))

#Visualize
qplot(treatment.genotypes, shoots, data=eelgrass, geom="point")

#fit
eelgrass_lm <- lm(shoots ~ treatment.genotypes, data=eelgrass)

#assumptions
plot(eelgrass_lm, which=c(1,2,4,5))

#ANOVA
anova(eelgrass_lm)

#Tukey's HSD
contrast(emmeans(eelgrass_lm, ~treatment.genotypes), method = "tukey")

CLD(emmeans(eelgrass_lm, ~treatment.genotypes))  %>%
  ggplot(aes(x = treatment.genotypes, y = emmean, 
             ymin = lower.CL, ymax = upper.CL,
             color = factor(.group))) +
  geom_pointrange()
  
# ANODEV - likelihood + ANOVA ####
knees_anodev <- glm(shift ~ treatment,
                    family = gaussian(link = "identity"),
                    data = knees)

#assumptions
plot(profile(knees_anodev))
plot(knees_anodev, which=c(1,2,4,5))

#Chis square likelihood ratio test
anova(knees_anodev, test = "LRT")
Anova(knees_anodev)

#posthocs
knees_anodev_em <- emmeans(knees_anodev, ~treatment)

knees_anodev_em

contrast(knees_anodev_em, method = "tukey")

# BANOVA ####
library(brms)
library(tidybayes)

knees_banova <- brm(shift ~ treatment,
                    family = gaussian(link = "identity"),
                    data = knees,
                    file = "knees_banova.rds")

#Evaluate all of your assumptions, and convergence....

#still treatment contrasts
knees_banova

#look at coefficients
knees_b_em <- emmeans(knees_banova, ~treatment)

knees_b_em

#some fun plots!
gather_emmeans_draws(knees_b_em) %>%
  ggplot(aes(y = treatment, x = .value, fill = treatment)) +
  geom_halfeyeh()

gather_emmeans_draws(knees_b_em) %>%
  ggplot(aes(x = treatment, y = .value)) +
  geom_line(aes(group = .draw), alpha = 0.01) +
  stat_pointinterval(color = "red")

gather_emmeans_draws(knees_b_em) %>%
  ggplot(aes(x = treatment, y = .value)) +
  stat_lineribbon(alpha = 0.25) +
  stat_pointinterval()

gather_emmeans_draws(knees_b_em) %>%
  ggplot(aes(x = treatment, y = .value)) +
  geom_jitter(alpha = 0.05)


#posthocs
contrast(knees_b_em, method = "tukey")


contrast(knees_b_em, method = "tukey") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(y = contrast, x = .value)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, color = "red")

#BANOVA - look at variance components####
group_sd <- knees_b_em %>%
  gather_emmeans_draws() %>%
  group_by(.draw) %>%
  summarize(value = sd(.value)) %>%
  mutate(type = "sd_treatment")

resd_sd <- add_predicted_draws(knees_banova, newdata = knees) %>%
  group_by(.draw) %>%
  summarise(value = sd(.prediction - shift)) %>%
  mutate(type = "sd_resid")

banova_tibble <- bind_rows(group_sd, resd_sd)

ggplot(banova_tibble,
       aes(y = type, x = value)) +
  geom_halfeyeh()

#make a table
banova_wide <- tibble(sd_treatment = group_sd$value,
                      sd_resid = resd_sd$value)

tidyMCMC(banova_wide, conf.int = TRUE,
         conf.method = "HPDinterval")

banova_percent <- banova_wide / rowSums(banova_wide)*100

tidyMCMC(banova_percent, conf.int = TRUE,
         conf.method = "HPDinterval")
