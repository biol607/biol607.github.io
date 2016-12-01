########################
#########ANOVA
########################
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)



knees <- read.csv("./data/10/15e1KneesWhoSayNight.csv")

###Make some sort of plot
ggplot(data = knees,
       mapping = aes(x = treatment, y = shift)) +
  geom_boxplot()
  #stat_summary(color = "red") +
  #geom_point(alpha = 0.9) 


### Fit an ANOVA model
knees_lm <- lm(shift ~ treatment, data=knees)

#AOV?
aov(shift ~ treatment, data=knees)
  
#Evaluate assumptions
par(mfrow = c(2,2))
plot(knees_lm, which = c(1,2,5))
par(mfrow=c(1,1))

#Look at the residuals v. treatment
residualPlots(knees_lm)

# F-test
anova(knees_lm)


#So - which treatment matters
summary(knees_lm)
summary(update(knees_lm, . ~ .-1))

#Tukey's HSD
contrast(lsmeans(knees_lm, specs="treatment"),
         method = "tukey")

#LSD test
contrast(lsmeans(knees_lm, specs="treatment"),
         method = "tukey", adjust = "none")

#Dunnett's test
contrast(lsmeans(knees_lm, specs="treatment"),
         method = "dunnett")


#Dunnett's test against eyes
contrast(lsmeans(knees_lm, specs="treatment"),
         method = "dunnett", ref = 2)


########## EELGRASS
eelgrass <- read.csv("./data/10/15q05EelgrassGenotypes.csv")
eelgrass$genotype_factor <- factor(eelgrass$treatment.genotypes)


####### Two-way ANOVA
zooplankton <- read.csv("./data/10/18e2ZooplanktonDepredation.csv")
zooplankton$block <- factor(zooplankton$block)

qplot(treatment, zooplankton, geom="boxplot", data=zooplankton)
qplot(block, zooplankton, geom="boxplot", data=zooplankton)

#fit the model
zooplankton_lm <- lm(zooplankton ~ treatment + block, data=zooplankton)

#assumptions and tukey's test of nonadditivity
plot(zooplankton_lm, which=c(1,2,4,5))
residualPlots(zooplankton_lm)

#Type II sums of suqares for our F-test
Anova(zooplankton_lm)

##Tukey tests
contrast(lsmeans(zooplankton_lm, spec="treatment"),
          "tukey")


contrast(lsmeans(zooplankton_lm, spec="block"),
         "tukey")

########### Factorial
intertidal <- read.csv("./data/10/18e3IntertidalAlgae.csv")

qplot(herbivores, sqrtarea, data = intertidal, geom="boxplot",
      fill = height)

qplot(height, sqrtarea, data = intertidal, geom="boxplot",
      fill = herbivores)

#Factorial model
intertidal_fac <- lm(sqrtarea ~ herbivores * height, data = intertidal) 

intertidal_fac <- lm(sqrtarea ~ herbivores + height +
                       herbivores:height, data = intertidal) 

plot(intertidal_fac, which=c(1,2,4,5))
residualPlots(intertidal_fac)

#F-test
Anova(intertidal_fac)

#Tukey test
contrast( lsmeans(intertidal_fac, specs = c("herbivores", "height")),
          method = "tukey")