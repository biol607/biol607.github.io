#'--------------------------------------------------
#'
#' T-tests As a Linear Model
#' 
#'--------------------------------------------------

# libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) #Companion to Applied Regression - by John Fox
library(emmeans) #Expected Means

# Paired Data
# Data where we have paired samples
# (two plots next to each other, two cells from the same body,
#  two measurements before and after an intervention, etc.)
blackbird <- read_csv("data/12e2BlackbirdTestosterone.csv")

str(blackbird)

#visualize and with a paired test see assumptions
ggplot(blackbird,
       aes(x = `dif in logs`)) +
  geom_histogram(bins = 10)

#viz before and after
b_tidy <- blackbird %>%
  mutate(id = 1:n()) %>%
  pivot_longer(-id,
               names_to = "When",
               values_to = "Antibody") %>%
  filter(When %in% c("Before", "After"))
ggplot(b_tidy,
       aes(x = When, y = Antibody, group = id)) +
  geom_point() + geom_line()

# fit a lm
# dif ~ b0 + error
blackbird_mod <- lm(dif ~ 1, data = blackbird)

#assumptions
plot(blackbird_mod, which = 1)
plot(blackbird_mod, which = 2)
shapiro.test(residuals(blackbird_mod))

# are my means different?
summary(blackbird_mod)

library(brms)
blackbird_brm <- brm(dif ~ 1,
                    data = blackbird,
                    family = gaussian(link = "identity"),
                    chains = 1)


fixef(blackbird_brm, probs = c(0.025, 0.1, 0.9, 0.975))
plot(blackbird_brm)

blackbird_post <- as.data.frame(blackbird_brm)

#how much is > 0
sum(blackbird_post$b_Intercept>0)/nrow(blackbird_post)

# AIC
blackbird_mod_really_null <- lm(dif ~ 0, data = blackbird)

AIC(blackbird_mod)
AIC(blackbird_mod_really_null)

# classically....
t.test(blackbird$dif)



# Comparing Two Separate Means ####
salmon <- read_csv("data/12e4BrookTrout.csv") %>%
  janitor::clean_names()

names(salmon)

ggplot(salmon,
       aes(x = brook_trout,
           y = mean_chinook_survival)) +
  geom_boxplot()

# fit a model with dummy encoded categorical variables
# OR - ONE HOT ENCODING!
# y = b0 + b1 * trout + e
# b0 = mean of whatever group is 0
# b1 = difference between groups

#THIS IS THE MODEL
salmon_mod <- lm(mean_chinook_survival ~ brook_trout,
                 data = salmon)

#what's happening inside
model.matrix(mean_chinook_survival ~ brook_trout,
             data = salmon)

model.matrix(mean_chinook_survival ~ brook_trout - 1,
             data = salmon)


# Evaluate the model
summary(salmon_mod)


# oops - assumptions
plot(salmon_mod, which = 1)

# Levene's test from car
# Is the variance different between groups
leveneTest(salmon_mod)

#non-paramtric test - we analyze ranks
salmon_rank <- lm(rank(mean_chinook_survival) ~ 
                    brook_trout,
                  data = salmon)

summary(salmon_rank)

# let's go big and model variance
# y = b0 + b1 * trout + e
# e = g0 + g1 * trout

library(nlme)
salmon_gls <- gls(mean_chinook_survival ~ brook_trout,
                  data = salmon,
                  weights = varIdent(form = ~1|brook_trout))


summary(salmon_gls)

#same thing, different library
library(glmmTMB)

salmon_tmb <- glmmTMB(mean_chinook_survival ~ brook_trout,
                      dispformula = ~ brook_trout,
                      data = salmon
                      )
summary(salmon_tmb)

#same thing, bayes style!

salmon_brm_mod <- brmsformula(mean_chinook_survival ~ brook_trout,
                              sigma ~ brook_trout)

salmon_brm <- brm(salmon_brm_mod, data = salmon,
                  chain = 2)

fixef(salmon_brm)

plot(salmon_brm)

#Welch's t-test
t.test(mean_chinook_survival ~ brook_trout,
       data = salmon,
       unequal.var = TRUE)

# what are our means?
# y = b1* no trout + b2 * trout + e
# b1 = mean of no trout, b2 = mean of trout
# with emmeans
salmon_em <- emmeans(salmon_brm, ~ brook_trout)
emmeans(salmon_mod, ~ brook_trout)

salmon_em

plot(salmon_em)


