#########
#Likelihood and the liner model
##########
library(bbmle)
library(ggplot2)
library(broom)

seals <- read.csv("data/06/17e8ShrinkingSeals Trites 1996.csv")


ggplot(data = seals, mapping=aes(x=age.days, y=length.cm))+
  geom_point()

##### Fit a mle2 object


seal_fit <- mle2(length.cm ~ dnorm(b0 + b1 * age.days, resid_sd),
                 data = seals,
                 start = list(b0 = 5, b1 = 5, resid_sd = 500))

#If you want to use a sensible box constraint
seal_fit_bfgs <- mle2(length.cm ~ dnorm(b0 + b1 * age.days, resid_sd),
                 data = seals,
                 start = list(b0 = 5, b1 = 5, resid_sd = 5),
                 method = "L-BFGS-B",
                 lower = c(b0 = -Inf, b1 = -Inf, resid_sd = 0.01),
                 upper = c(b0 = Inf, b1 = Inf, resid_sd = Inf))

seal_fit_sann <- mle2(length.cm ~ dnorm(b0 + b1 * age.days, resid_sd),
                 data = seals,
                 start = list(b0 = 5, b1 = 5, resid_sd = 5),
                 method = "SANN")

##### Test our assumptions!!!!

#check out the distribution of our residuals!
res <- residuals(seal_fit)
qqnorm(res)
qqline(res)

#fitted v. resiuals
fitted_vals <- predict(seal_fit)
qplot(fitted_vals, res)

#plot the profile likelihoods
plot(profile(seal_fit))

plot(profile(seal_fit, which="b0", std.err=0.05))

#### Evaluate output
summary(seal_fit)
confint(seal_fit)
confint(seal_fit, method="quad")

library(broom)
summary(seal_fit)
tidy(summary(lm(length.cm ~ age.days, data=seals)))

### Fit an alternate model

seal_fit_null <- mle2(length.cm ~ dnorm(b0, resid_sd),
                 data = seals,
                 start = list(b0 = 5, resid_sd = 500))

logLik(seal_fit)
logLik(seal_fit_null)

#LRT
anova(seal_fit, seal_fit_null)
