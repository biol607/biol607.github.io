#for some distribution dist
dist
#density
ddist 
#pvalues
pdist
#specify a p value, return observation that will produce it
qdist
#random numbers that follow the dist
rdist
##################
#density of 5 in a normal distribution with a mean of 8
# and an sd of 3
dnorm(5, mean = 8, sd = 3)

#to plot density of a distribution
#make data
dens_frame <- data.frame(x = seq(0,16, length.out = 200))
str(dens_frame)
#calculate density for each value of x
dens_frame$density <- dnorm(dens_frame$x, mean = 8, sd = 3)
#library(ggplot2)

norm_dens <- ggplot(dens_frame, mapping = aes(x = x,
                                              y = density)) +
  geom_line()
norm_dens
#######
#use rnorm to generate random numbers!
rnorm(5, mean = 8, sd = 3)

######Poisson and discrete densities
#Use dpois() to get the density of a number
dpois(3, lambda = 10)
#discrete density: must be an integer 
dpois(3.5, lambda = 10)
#negative numbers have a probability of 0
dpois(-3, lambda = 10)

pois_frame = data.frame(x = 0:40)
#adjust lambda to see how the distribution is affected
pois_frame$density <- dpois(pois_frame$x, lambda = 20)

ggplot(data = pois_frame, mapping = aes(x = x, y = density)) +
  geom_bar(width = 0.1, stat = "identity")

########
#what is the density of 4 in a standard lognormal
dlnorm(4, meanlog = 0, sdlog = 1)
#instead of a data frame, use some vectors
vals <- seq(0,10, length.out = 500)
dens <- dlnorm(vals, meanlog = 0, sdlog = 1)
#plot
ggplot(mapping = aes(x = vals, y = dens)) +
  geom_line() +
  geom_vline(xintercept = 4, color = "red")


#Gamma - Spruce budworms take 2 hours to eat a single leaf. You have given your budworm 5 leaves. What is the probability that you will wait 15 hours for it to finish eating?
  
dgamma(15, scale = 2, shape = 5)
#plot the density
vals <- seq(0,20, length.out = 200)
dens <- dgamma(vals, scale = 2, shape = 5)
ggplot(mapping = aes(x=vals, y=dens)) +
  geom_line() +
  geom_vline(xintercept = 15)

#Exponential - The distribution of time between events happening, 
#with rate = 1/(average time to next event). 
#If the mean time to mortality of a super-corgi is 20 years, 
#what's the probability that a super-corgi will live to 30 

dexp(30, rate = 1/20)
#plot the density
vals <- seq(0,50, length.out=200)
dens <- dexp(vals, rate = 1/20)
ggplot(mapping = aes(x=vals, y=dens)) +
  geom_line() +
  geom_vline(xintercept = 30)

#Binomial - You flip a coin 100 times, 
#and think that you have 50:50 chance of heads or tails. 
#What's the probability of obtaining 40 heads?
dbinom(40, size = 100, prob = 0.5)
#plot the density
vals <- 0:100
dens <- dbinom(vals, size = 100, prob = 0.5)
ggplot(mapping = aes(x = vals, y = dens)) +
  geom_bar(width = 0.6, stat = "identity") +
  geom_vline(xintercept = 40)
######
#surveying 500 reefs post storm
#did storm have an effect?
#baseline chance to be damaged is 30%
#70% healthy
#300 undamaged
#what is the probability of observiung 300 or fewer undamaged reefs?

#one tailed test of undamaged reefs
pbinom(300, size = 500, prob = 0.7, lower.tail = TRUE)

#one tailed test of damaged reefs: we are interestested in the upper tail
pbinom(200, size = 500, prob = 0.3, lower.tail = FALSE)

#multiply p by 2 for a two tailed test
2 * pbinom(300, size = 500, prob = 0.7)
########
#simple z test
#Sample of 10 flieas
#sample length = 0.4mm
#pop mean of 0.3, sd = 0.1

#calc standard error
sigma_ybar <- 0.1/sqrt(10)

#z score
z <- (0.4-0.3)/sigma_ybar

2 * pnorm(abs(z), lower.tail = FALSE)
#############
#create the null distribution
#tapirs havea 90% chance of nose 5-10cm (uniform)
#10% have a nose that is 10-15 cm (uniform)

nose_length <- seq(5, 15, length.out = 2000)

which(nose_length>10)[1]

#90% chance of being between 5:10, 10% chance of being between 10:15
prob_observed <- c(rep(0.9/1000, 1000), 
                   rep(0.1/1000, 1000))

samp <- sample(nose_length, 10000, replace = TRUE, 
               prob = prob_observed)


#tapir with nose of 14.5 cm

length(which(samp >= 14.5))/length(samp)
#################
##Side note on functions
#################
#use functions to automate repetative tasks

#name it, and specify the arguments it will need

std_error <- function(x){ #in this case, x will be a vector of numbers
  #Calculate Standard error of the mean.
  #note that we are nesting other functions inside of here 
  sqrt(var(x)/length(x))
}



#Excercise
#test it on iris! 
#Group by species and summarise std. error of the Sepal.Length column, using your function

#############
#population normally distributed mean = 80, sd = 6
#testing a drug, effect size = 10
#what sample size to get power = 0.8

sim_data <- data.frame(samp_size = rep(1:10, 500)) %>%
  #use n() to avoid hardcoding 
  group_by(1:n()) %>%
  #get a sample mean
  mutate(sample_mean = mean(rnorm(samp_size, 90, 6))) %>%
  ungroup()

sim_data <- sim_data %>%
  #calculate SE
  mutate(se_y = 6/(sqrt(samp_size))) %>%
  #calculate Z
  mutate(z = (sample_mean-80)/se_y) %>%
  #calculate p
  mutate(p = 2* pnorm(abs(z), lower.tail = FALSE))

#plot it!
ggplot(data = sim_data, mapping = aes(x = samp_size,
                                      y = p)) +
  geom_jitter(alpha = 0.4)

#######
#power = 1 - beta
#beta is the fraction of p values that are > alpha
#we are using alpha = 0.05

#If we want to see how power is related to sample size
sim_power <- sim_data %>%
  group_by(samp_size) %>%
  summarise(power = 1-sum(p>0.05)/n()) %>%
  ungroup()

ggplot(data = sim_power, mapping = aes(x = samp_size,
                                       y = power)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8, lty = 2)

#adjusting alpha
#install.packages("tidyr")
sim_tradeoff <- sim_data %>%
  crossing(alpha = 10^(-1:-10)) %>%
  group_by(samp_size, alpha) %>%
  summarise(power = 1-sum(p>alpha)/n()) %>%
  ungroup()

ggplot(data =  sim_tradeoff, mapping = aes(x = samp_size,
                                           y = power,
                                           #set alpha as a factor for a discrete color scale
                                           color = factor(alpha))) +
  geom_point() +
  geom_line() +
  #change axis labels
  xlab("Sample Size") +
  #make legend pretty
  scale_color_discrete(guide = guide_legend(title = expression(alpha)))


