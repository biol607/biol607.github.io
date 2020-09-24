#'--------------------------------------
#'
#' @title Bootstrapping of Sampling to answer
#' HOW BIG SHOULD MY SAMPLE SIZE BE?!?!?!?!?!?!
#' 
#' @author Jarrett Byrnes
#'--------------------------------------


# libraries for this code
library(dplyr)
library(purrr)

# Bootstrapping is based on repeated draws from a SAMPLE

# Create the sample we are going to work with ####
set.seed(2020)
samp <- rnorm(40, mean = 10, sd = 3)

# One Bootsrap Sample ####

# A bootstrap sample is a resampled set of values
# with the same n, sampled with replacement
one_boot <- sample(samp,
                   size = length(samp),
                   replace = TRUE)

#show that they are drawn from each other
one_boot %in% samp

#bootstrapped medians! ####

#1000 times, sample from samp and calculate a median
boot_med <- replicate(1000,
                      sample(samp, 
                             size = length(samp),
                             replace = TRUE) %>% median)

length(boot_med)

#let's look at it!
hist(boot_med)

# the SE of the median is the SD of these bootstraps
sd(boot_med)

#2/3 confidence interval is
mean(boot_med) + sd(boot_med)
mean(boot_med) - sd(boot_med)

median(samp)
mean(boot_med)

# SE of the mean ####
boot_mean <- replicate(1000,
                      sample(samp, 
                             size = length(samp),
                             replace = TRUE) %>% mean)

# compare SE
sd(boot_mean)
sd(samp)/sqrt(length(samp))


####
#First, make sure you feel comfortable calculating the 
# bootstrapped SE of the IQR from samp. Repeat what we did
# above with IQR instead of median.


boot_iqr <- replicate(1000,
                      sample(samp,
                             size = length(samp),
                             replace = TRUE) %>% IQR)

sd(boot_iqr) #bootstrapped standard error of the IQR
mean(boot_iqr)
IQR(samp)

#Now, write out in comments what you will do to end up 
#with a data frame that has a column of sample sizes and 
#a column of IQRs calculated from sampling our samp vector.


# start with a data frame with a sample size column
# for each sample size
# replicate 1000 times
# bootstrap draws 
# and get an IQR
# now I have 1K bootrapped IQRS and different sample sizes 

# start with a data frame with a sample size column
boot_iqrs <- data.frame(samp_size = 10:50) %>%
  # for each sample size
  rowwise(samp_size) %>%
  # replicate 1000 times
  summarize(boot_iqr = replicate(1000,
                                 # bootstrap draws 
                                 sample(samp,
                                        size = samp_size,
                                        replace = TRUE) %>%
                                   # and get an IQR
                                   IQR()
                                 ))

boot_se_iqrs <- boot_iqrs %>%
  group_by(samp_size) %>%
  summarize(se_iqr = sd(boot_iqr))

plot(se_iqr ~ samp_size, data = boot_se_iqrs,
     type = "l")



