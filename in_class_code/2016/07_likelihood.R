library(dplyr)
library(tidyr)
library(ggplot2)

############
# Likelihood via simulation
# (brute force!)
############
library(dplyr)
library(tidyr)
library(ggplot2)

#Start with Poisson
#We have one sample, and it's 10 flowers
#What is lambda? Mean and variance

#What is my range of paramater values?
lambda_vals <- 0:20

#Likelihood function
lik_pois <- dpois(x = 10, lambda = lambda_vals)

#Max likelihood
max(lik_pois)

#visualize likelihood surface
qplot(lambda_vals, lik_pois)

#What is my MLE of lambda?
lambda_vals[which.max(lik_pois)]

##########
#Likelihood with dplyr
##########

pois_10 <- data.frame(lambda_vals = 0:20) %>%
  mutate(lik = dpois(x = 10, lambda = lambda_vals)) %>%
  mutate(log_lik = dpois(x = 10, lambda = lambda_vals, log=TRUE)) 


#Get my MLE
pois_10 %>%
  filter(lik == max(lik))

qplot(lambda_vals, log_lik, data=pois_10) +
  geom_hline(yintercept = max(pois_10$log_lik) - 1.92,
             color = "blue", lty=2)

#Get CIs
pois_10 %>%
  filter(log_lik > max(log_lik) - 1.92) %>%
  
  #Get the biggest and smallest values
  #arrange things by parameter value
  arrange(lambda_vals) %>%
  
  #get just the first and last row
  filter(row_number() == 1 | row_number() == n())



#Multiple data points
set.seed(607)
pois_data <- rpois(10, lambda = 10)

pois_mle <- data.frame(lambda_vals = 0:20) %>%
  rowwise() %>% #group by row
  mutate(lik = prod(dpois(x = pois_data, lambda = lambda_vals)),
         log_lik = sum(dpois(x = pois_data, lambda = lambda_vals, log=TRUE))
  ) %>%
  ungroup()


#Visualize
qplot(lambda_vals, log_lik, data=pois_mle)

#MLE
pois_mle %>%
  filter(log_lik == max(log_lik))

#CI
pois_mle %>%
  filter(log_lik > max(log_lik)-1.92) %>%
  filter(row_number() == 1 | row_number() == n())


## You have run 5 trials of flipping 20 coins. The number of heads in each 
## trial is: 11, 10, 8, 9, 7. What’s the maximum likelihood estimate of the 
## probability getting heads?

?dbinom

seq(0, 1, by = 0.1)

#1. What is our data?
heads <- c(11, 10, 8, 9, 7)

#2. Setup a data frame with a lot of possible parameter values
heads_mle <- data.frame(p = seq(0, 1, .001)) %>%
  
  #3. Estimate likelihood and log likelihood for each paramter
  
  rowwise() %>%
  
  mutate(
    lik = prod( dbinom( x= heads, size = 20, prob = p)),
    log_lik = sum( dbinom( x= heads, size = 20, prob = p, log=TRUE))
  ) %>%
  
  ungroup()


#4. Visualize
qplot(p, log_lik, data=heads_mle)
qplot(p, lik, data=heads_mle)

#5. Get the MLE and CI
heads_mle %>%
  filter(log_lik == max(log_lik))

heads_mle %>%
  filter(log_lik > max(log_lik) - 1.92) %>%
  filter(row_number() == 1 | row_number() == n())

############
### Look at multiple parameters
### with seals!
############
#1. What is our data?

seals <- read.csv("data/06/17e8ShrinkingSeals Trites 1996.csv")

hist(seals$age.days)


#2. Setup a data frame with a lot of possible parameter
#   values
library(tidyr)

seal_dist <- crossing(m = seq(3720, 3740, by=0.1),
                      s = seq(1280, 1300, by = 0.1)) %>%
  
  #3. Estimate likelihood and log likelihood for 
  #each paramter
  rowwise() %>%
  mutate(log_lik = sum(dnorm(x = seals$age.days, mean = m, sd = s, log = TRUE))) %>%
  ungroup()

#visualize
ggplot(data = seal_dist, 
       mapping = aes(x = m, y = s, z =log_lik)) +
  geom_contour(aes(color = ..level..))


base_lik <- ggplot(data = seal_dist, 
                   mapping = aes(x = m, y = s, color =log_lik))

base_lik + geom_point() + 
  scale_color_gradient(low = "blue", high = "red")


base_lik + geom_raster(aes(fill=log_lik)) + 
  scale_fill_gradient(low = "blue", high = "red")




persp(x = seq(3720, 3740, by=0.1),
      y = seq(1280, 1300, by = 0.1),
      z = matrix(seal_dist$log_lik, ncol = sqrt(nrow(seal_dist))),
      phi = 0.2, theta = 0.4)

library(rgl)
rgl_init()
rgl.spheres(x = seal_dist$m, y = seal_dist$s, z = seal_dist$log_lik, r = 0.2, color = "yellow") +
  rgl_add_axes(x, y, z, show.bbox = TRUE)
aspect3d(1,1,1)

###MLE
seals <- read.csv("data/06/17e8ShrinkingSeals Trites 1996.csv")

seal_dist <- crossing(m = seq(3720, 3740, by = 0.1), 
                      s=seq(1280, 1310, by = 0.1)) %>%
  rowwise() %>%
  mutate(log_lik = sum(dnorm(seals$age.days, mean = m, sd = s, log=TRUE))) %>%
  ungroup()

#MLE
seal_dist %>%
  filter(log_lik == max(log_lik))

#CI via profile likelihoods
 #CI for our mean

seal_profile_mean <- seal_dist %>%
  group_by(m) %>%
  filter(log_lik == max(log_lik)) %>%
  ungroup()

qplot(m, log_lik, data=seal_profile_mean)

seal_profile_mean %>%
  filter(log_lik > max(log_lik) - 1.92) %>%
  arrange(m) %>%
  filter(row_number() == 1 | row_number() == n())


## CI for SD
seal_profile_sd <- seal_dist %>%
  group_by(s) %>%
  filter(log_lik == max(log_lik)) %>%
  ungroup()

seal_profile_sd %>%
  filter(log_lik > max(log_lik) - 1.92) %>%
  arrange(s) %>%
  filter(row_number() == 1 | row_number() == n())

#Visualize profiles
ggplot(seal_dist, mapping=aes(x=m, y=s, z= log_lik)) +
  geom_contour() +
  geom_line(data = seal_profile_sd)+
  geom_line(data = seal_profile_mean)

