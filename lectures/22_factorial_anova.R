## @knitr lecture22prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)
library(car)
library(QuantPsyc)
source("./3dplotting.R")
neand <- read.csv("./data/22/18q09NeanderthalBrainSize.csv")
algae <- read.csv("./data/22/18e3IntertidalAlgae.csv")
keeley <- read.csv("./data/22/Keeley_rawdata_select4.csv")

## @knitr algae_plot
algae_plot <- qplot(height, sqrtarea,  data=algae, geom="boxplot", fill=herbivores) + theme_bw(base_size=16)
algae_plot

## @knitr graze_linear
graze_linear <- lm(sqrtarea ~ height + herbivores, data=algae)
Anova(graze_linear)

## @knitr graze_linear_diagnostics
residualPlot(graze_linear, variable="fitted", type="response")


## @knitr graze_linear_tukey
residualPlots(graze_linear, plot=F)


## @knitr graze_interaction
graze_int <- lm(sqrtarea ~ height + herbivores + herbivores:height, 
                data=algae)

## @knitr graze_interaction2
#Or, more compact syntax
graze_int <- lm(sqrtarea ~ height*herbivores, data=algae)


## @knitr graze_int_resplot
residualPlot(graze_int, variable="fitted", type="response")

## @knitr graze_interaction_anova
Anova(graze_int)


## @knitr graze_interaction_coefs
summary(graze_int)$coefficients


## @knitr graze_posthoc_contrast
library(contrast)

#compare plus in each height to minus in each height
contrast(graze_int,
         a=list(height=levels(algae$height), herbivores="plus"),
         b=list(height=levels(algae$height), herbivores="minus"))

## @knitr graze_posthoc_visalize
with(algae,
     interaction.plot(height, herbivores, sqrtarea)
)

## @knitr graze_posthoc_TUKEY
graze_aov <- aov(sqrtarea ~ height*herbivores, data=algae)
TukeyHSD(graze_aov)

## @knitr graze_posthoc
algae$int <- with(algae, interaction(height, herbivores))
graze_int2 <- lm(sqrtarea ~ int, data=algae)
#
library(multcomp)
summary(glht(graze_int2, linfct=mcp(int = "Tukey")))


## @knitr graze_unbalance
algae_unbalanced <- algae[-c(1:5), ]

graze_int_unbalanced <- lm(sqrtarea ~ height*herbivores, 
                           data=algae_unbalanced)


## @knitr graze_unbalance_anova12
anova(graze_int_unbalanced)
Anova(graze_int_unbalanced)

## @knitr graze_unbalance_anova23
Anova(graze_int_unbalanced, type="III")

## @knitr keeley_data_3d
with(keeley, scatterPlot3d(age,elev,firesev, 
                           col="black", xlab="age", ylab="elev", zlab="firesev",
                           phi=20, theta=-45))


## @knitr keeley_model
keeley_lm <- lm(firesev ~ age*elev, data=keeley)
Anova(keeley_lm)

## @knitr keeley_III
Anova(keeley_lm)
Anova(keeley_lm, type="III")


## @knitr keeley_int_plot
keeley$egroup <- keeley$elev<600
k_plot <- qplot(age, firesev, data=keeley, color=elev, size=elev)  + theme_bw() +
  scale_color_continuous(low="blue", high="red")
k_plot 

## @knitr keeley_int_plot2
k_plot + stat_smooth(method="lm", aes(group=egroup))

## @knitr keeley_coef
summary(keeley_lm)$coef
summary(keeley_lm)$r.squared

## @knitr keeley_vif
vif(keeley_lm)

## @knitr keeley_visreg1
library(visreg)
visreg(keeley_lm, "elev", by="age")

## @knitr keeley_visreg2
visreg(keeley_lm, "age", by="elev")


## @knitr keeley_meanCenter
keeley$int <- with(keeley, meanCenter(age)* meanCenter(elev))
keeley_lm2 <- lm(firesev ~ age + elev + int, data=keeley)

## @knitr keeley_meanCenter_anova
Anova(keeley_lm2)

## @knitr keeley_meanCenter_coef
summary(keeley_lm2)$coef

## @knitr keeley_3d
source("./3dplotting.R")
abcSurf(keeley_lm, phi=20, theta=-65, col="lightblue") -> p
with(keeley, scatterPlot3d(age,elev,firesev, 
                           add=T, background=p, col="black", alpha=0.4))

## @knitr keeley_predict_df
pred.df <- expand.grid(age = quantile(keeley$age),
                       elev = quantile(keeley$elev))
pred.df <- cbind(pred.df, 
                 predict(keeley_lm, pred.df, interval="confidence"))
#
pred.df$firesev <- pred.df$fit


## @knitr keeley_predict_lines
keeley_fit <- ggplot(data=pred.df, aes(x=age, y=firesev, 
                                       ymin=lwr, ymax=upr, 
                                       group=elev)) +
  geom_line(mapping=aes(color=elev)) +
  scale_color_continuous(low="blue", high="red") + theme_bw()
#
keeley_fit


## @knitr keeley_predict_error
keeley_fit+geom_ribbon(alpha=0.1)


## @knitr keeley_predict_lines_data
k_plot2 <- k_plot+geom_line(data=pred.df, aes(x=age, y=firesev, 
                                              ymin=lwr, ymax=upr, 
                                              group=elev), size=1) 
k_plot2

## @knitr keeley_predict_lines_data_error
k_plot2 + geom_ribbon(data=pred.df, aes(x=age, y=firesev, 
                                        ymin=lwr, ymax=upr, group=elev), 
                      color="grey", size=0, alpha=0.1)


## @knitr keeley_surf
kelev <- seq(min(keeley$elev), max(keeley$elev), 1)
kage <- seq(min(keeley$age), max(keeley$age), .1)
#
firesevMat <- outer(kelev, kage, 
                    function(x,y) predict(keeley_lm, 
                                          data.frame(elev=x, age=y)))
#
filled.contour(kelev, kage, firesevMat,  
               color.palette=heat.colors, 
               xlab="Elevation", ylab="Age", 
               key.title=title(main="Fire\nSeverity"))
