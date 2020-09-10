######################
# In Class code for 
# sampling and simulation
#
# Jarrett Byrnes
# 9/15/16
##################

#Load libraries
#install.packages("magrittr")
#install.packages("dplyr")

library(magrittr)

#old way
vec <- 1:10
len <- length(1:10)
sq <- sqrt(10)
log(sq)

log(sqrt(length(1:10)))

#simple pipes
1:10 %>% sum()
sum(1:10)

#functional pipeline
1:10 %>%
  length() %>%
  sqrt() %>%
  log()



#Use pipes to sum the log of 
#100:200.
100:200 %>% 
  log() %>%
  sum()

#Use pipes to take the square 
#root of the mean of 100 
#random uniform numbers.
100 %>%
  runif() %>%
  mean() %>%
  sqrt()

#Let’s dive into the guts of R.
#Using the mtcars data frame, 
#get it’s summary and str that 
#summary. What do you get back?

mtcars %>%
  summary() %>%
  str()

##Plot
hist(mtcars$mpg)

plot(mpg ~ cyl, data=mtcars)

plot(mpg ~ cyl, data=mtcars,
     col="red", pch=19)

## Dplyr

library(dplyr)

#mutate
mtcars$log_mpg <- log(mtcars$mpg)

mtcars2 <- 
  mutate(mtcars, log_mpg = log(mpg))

#pipe notation
mtcars2  <- mtcars %>%
  mutate(log_mpg = log(mpg),
         log_cyl = log(cyl))

#group_by
mtcars3 <- mtcars %>%
  group_by(carb) %>%
  mutate(avg_mg = mean(mpg)) %>%
  ungroup()

View(mtcars3)

###summarize

mtcars %>%
  group_by(cyl) %>%
  summarize(avg_mpg = mean(mpg),
            sd_mpg = sd(mpg)) %>%
  ungroup()

###1) With the iris data, get the 
#average Petal.Length and Petal.Width 
#by species

#take the iris data set
iris %>%
  
  #group it by species
  group_by(Species) %>%
  
  #summarise it - take the mean
  #of peta length and width
  summarize(mean_length = mean(Petal.Length),
            mean_width = mean(Petal.Width))





##########Sample sizes
set.seed(20160915)
samp <- rnorm(n = 40, mean = 10, sd = 3)

#sample function
sample(samp, size = 5, replace = TRUE)

#sampling from a data frame
mtcars[sample(1:nrow(mtcars), 5),]

#using dplyr
sample_n(mtcars, 5)
sample_frac(mtcars, 0.1)


#Simulation!
# I try n = 3:50, 10 simulation per
#sample size

sampSim <- data.frame(
  samp_size = rep(5:30, times=10)
)

sampSim$sim <- 1:nrow(sampSim)

#take my data frame sampSim
sampSim <- sampSim %>%
  
  #for each simulation
  group_by(sim) %>%
  
  #draw from a random normal dist, 
  #mean=10, sd=3, and calculate the
  #mean of the sample
  mutate(mean_sim = mean(rnorm(samp_size,
                               mean = 10,
                               sd = 3))) %>%
  ungroup()

#evaluate
head(sampSim)
plot(mean_sim ~ samp_size, data = sampSim)


#Look at variability of estimates
sampSimSummary <- sampSim %>%
  #for each sample size
  group_by(samp_size) %>%
  
  #calculate the sd (or var or whatnot)
  summarize(sd_mean_sim = sd(mean_sim)) %>%
  
  ungroup() %>%
  
  #sort by sd, to see the lowest values
  arrange(sd_mean_sim)


#Filtering
sampSimSummary %>%
  filter(sd_mean_sim < 0.7)

#Faded examples


#Some preperatory material
set.seed(42)
samp <- rnorm(100, 10, 3)
sampSim <- data.frame(samp_size = rep(3:50, times = 10))
sampSim$sim_number = 1:nrow(sampSim)

#Mean simulations
sampSim %>%
  group_by(sim_number) %>%
  mutate(mean_pop = mean(rnorm(samp_size, mean = 10, sd = 3))) %>%
  ungroup()



#Median simulations
sampSim %>%
  group_by(sim_number) %>%
  mutate(median_pop = ____(rnorm(samp_size, mean = 10, sd = 3))) %>%
  ungroup()

#SD simulations
sampSim %>%
  group_by(____) %>%
  ____(sd_pop = ____(rnorm(____, mean = 10, sd = 3))) %>%
  ____()


#IQR simulations
#function for interquartile range is IQR()
sampSim %>%
  ____(____) %>%
  ____(IQR_pop = ____(____(____, mean = 10, sd = 3))) %>%
  ____()


################



#Some preperatory material
set.seed(42)
samp <- rnorm(100, 10, 3)
sampSim <- data.frame(samp_size = rep(3:50, times = 10))
sampSim$sim_number = 1:nrow(sampSim)


#Median simulations
sampSim %>%
  group_by(sim_number) %>%
  mutate(median_pop = median(rnorm(samp_size, mean = 10, sd = 3))) %>%
  ungroup()


#1 bootstrap replicate draw
one_boot <- sample(samp, size = 100, 
       replace=TRUE)

median(one_boot)

#create a data fraeme for a simulation
#with 100 bootstrap reps
median_sims <- data.frame(sims = 1:100)

#start with my data
median_sims %>%
  
  #for each simulation
  group_by(sims) %>%
  
  #calculate the median of a bootstrap
  mutate(sim_median = median(sample(samp, 
                                    size = 100, 
                                    replace=TRUE))) %>%
  #ungroup
  ungroup() %>%
  
  #calculate the sd of my estimates of the median
  summarize(median_se = sd(sim_median))




