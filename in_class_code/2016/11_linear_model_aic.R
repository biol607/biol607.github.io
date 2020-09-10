###########################
#### General linear models
####
###########################
library(dplyr)
library(ggplot2)
library(car)

neand <- read.csv("data/11/18q09NeanderthalBrainSize.csv")

#visualize
qplot(lnmass, lnbrain, color=species, data = neand) +
  stat_smooth(method = "lm")

## Fit!
neand_ancova <- lm(lnbrain ~ lnmass + species, data=neand)

##test assumptipns
plot(neand_ancova, which = c(1,2,5))

## residuals by treatment
residualPlots(neand_ancova, test=FALSE)

## homogeneity of slopes
neand_int <- lm(lnbrain ~ lnmass * species, data=neand)
Anova(neand_int)


#### Assessment
Anova(neand_ancova)

summary(neand_ancova)

#postHoc
library(lsmeans)

neand_ls <- lsmeans(neand_ancova, specs = "species")
neand_ls

#where are we
ref.grid(neand_ancova)

contrast(neand_ls, method="tukey")


#visualization
crPlots(neand_ancova, smooth=FALSE)

# visreg
library(visreg)
visreg(neand_ancova)


#use fitted values
neand <- cbind(neand,
  predict(neand_ancova, interval="confidence"))


ggplot(neand) +
  geom_point(mapping = aes(x=lnmass, y=lnbrain,
                           color=species)) +
  geom_line(mapping = aes(x=lnmass, y=fit,
                          color=species)) +
  geom_ribbon(mapping = aes(x = lnmass,
                            ymin = lwr,
                            ymax = upr,
                            group = species),
              color = "lightgrey", alpha=0.5)

#########
## Multiple Linear Regression
###########

keeley <- read.csv("data/11/Keeley_rawdata_select4.csv")
head(keeley)
pairs(keeley)

###Visualize
qplot(cover, rich, color = firesev, size = firesev, data = keeley) +
  scale_color_gradient(low = "yellow", high = "red") +
  facet_wrap(~cut_number(firesev,4)) +
  stat_smooth(method="lm")



qplot(firesev, rich, color = cover, size = cover, data = keeley) +
  scale_color_gradient(low = "brown", high = "green") +
  facet_wrap(~cut_number(cover,4)) +
  stat_smooth(method="lm")



###### Additive model
keeley_mlr <- lm(rich ~ cover + firesev, data=keeley)

#Assumptions
plot(keeley_mlr, which = c(1,2,5))
residualPlots(keeley_mlr,  test=FALSE)

#multicollinearity
vif(keeley_mlr)

keeley %>%
  select(firesev, cover) %>%
  cor()

#Evaluation
Anova(keeley_mlr)
summary(keeley_mlr)

#Viz
crPlots(keeley_mlr, smooth=FALSE)

visreg(keeley_mlr)

k_pred <- data.frame(cover = mean(keeley$cover),
                     firesev = seq(2,8, length.out=100))

k_pred <- cbind(k_pred, 
                predict(keeley_mlr,
                        newdata = k_pred,
                        interval = "confidence"))

head(k_pred)


ggplot() +
  geom_point(data = keeley, 
             mapping = aes(x= firesev, y = rich,
                           color = cover)) +
  scale_color_gradient(low = "brown", high = "green") +
  geom_line(data = k_pred, mapping = aes(x=firesev, 
                                         y = fit)) +
  geom_ribbon(data = k_pred, mapping = (aes(x= firesev,
                                            ymin = lwr,
                                            ymax = upr)),
              alpha=0.1)


######### Ineraction
keeley_int <- lm(rich ~ cover * firesev, data=keeley)
residualPlots(keeley_int)
Anova(keeley_int)

######## AIC
AIC(keeley_int)
AIC(keeley_mlr)

keeley_plant <- lm(rich ~ cover, data = keeley)
keeley_firesev <- lm(rich ~ firesev, data = keeley)
keeley_null <- lm(rich ~ 1, data=keeley)

########AIC table
library(AICcmodavg)

modelList <- list(int = keeley_int, 
                  fire = keeley_firesev, 
                  plant = keeley_plant,
                  mlr = keeley_mlr, 
                  null = keeley_null)
aictab(modelList)


#Model averaged prediction
avg_pred <- modavgPred(modelList, newdata = k_pred)

k_pred <- cbind(k_pred, avg_pred)



ggplot() +
  geom_point(data = keeley, 
             mapping = aes(x= firesev, y = rich)) +
  #linear model fit
  geom_line(data = k_pred, mapping = aes(x=firesev, 
                                         y = fit), color = "red") + 
  #ensemble fit
  geom_line(data = k_pred, mapping= aes(x = firesev,
                                        y = matrix.output.mod.avg.pred), color="blue")

