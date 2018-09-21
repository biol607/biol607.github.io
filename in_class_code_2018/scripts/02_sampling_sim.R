#--------------------------
#' Lab 2 - Sampling and simulation
#' 
#' @author Jarrett Byrnes
#' @date 2018-09-14
#' 
#' This is the code for sampling and simulation
#--------------------------

#load a library
library(dplyr) 


#How to not not display warnings in markdown
#```{r, warning = FALSE}
#```
#


#----
# Pipes
#----

vec <- 1:10
sq <- sqrt(vec)
log(sq)

log(sqrt(1:10))

v <- 1:10
v <- sqrt(v)
log(v)


sum(1:10)

1:10 %>% sum()

#log of the sqrt of 1:10 with pipes!
1:10 %>%
  sqrt() %>%
  log() 

# Get the numbers 1 through 10 
out <- 1:10 %>%
  
  # then take the square root of them
  sqrt() %>%
  
  # then take the log of the result
  log()

out 

#some people like weird output
1:10 %>%
  sqrt() %>%
  log() -> out
  

1:10 %>%
  sqrt() %>%
  log() %>%
  sum()

1:100 %>% sum()
  

#fun1(fun2(fun3(fun4(fun5(1:10)))))
 
#-------------
# dplyr
#-------------

#transformation
mtcars2 <- mtcars

#old skool
mtcars2$log_mpg <- log(mtcars2$mpg)
mtcars2$log_cyl <- log(mtcars2$cyl)

mtcars2$log_mpg <- log(mtcars2$mpg)
log_mpg <- log(mtcars2$mpg)

###mutate
mutate(mtcars2, 
       log_mpg = log(mpg), 
       log_cyl = log(cyl))

#now with pipes and overwriting!
mtcars2 <- mtcars2 %>%
  mutate(log_mpg = log(mpg),
         log_cyl = log(cyl))

head(mtcars2)


#write a new mtcars, starting with mtcars
mtcars2 <- mtcars %>%
#create a new column with the log of mpg
  mutate(log_mpg = log(mpg),
#create a new column with the log of cyl
  log_cyl = log(cyl))


## summarize
mt_summary <- mtcars2 %>% 
  summarize(avg_mpg = mean(mpg),
            avg_cyl = mean(cyl),
            sd_mpg = sd(mpg))

str(mt_summary)
class(mt_summary)

## group_by
unique(mtcars$cyl)

#get the average and sd of the mpg by cyl
cyl_summary <- mtcars %>%
  group_by(cyl) %>%
  summarize( avg_mpg = mean(mpg),
             sd_mpg = sd(mpg))

## select
mtcars_select <- mtcars %>%
  select(cyl, mpg)

#examine
head(mtcars_select,10)
tail(mtcars_select)
mtcars_select

mtcars_negative_select <- mtcars %>%
  select(-disp)

head(mtcars_negative_select)

## filter

#booleans
# == is this equal?
# != are these not equal?
# >, < >=, <= common comparisons
# is.____ checks for an object type
# ! means not
mtcars_4_gears <- mtcars %>%
  filter(gear == 4)

mtcars_4_gears

mtcars_not_4_gears <- mtcars %>%
  filter(gear != 4)

mtcars_not_4_gears <- mtcars %>%
  filter(!(gear == 4))

#filtering NAs
#remove all rows in mtcars
#where cyl is NA
mtcars_no_nas <- mtcars %>%
  filter(!is.na(cyl))

#####
#plotting
#####
plot(mpg ~ cyl, data = mtcars)
boxplot(mpg ~ cyl, data = mtcars)
hist(mtcars$mpg)


#######
# simulation with rowwise()
#######

set.seed(2018)
runif(2)


#random number generation
runif(10, 0, 10)
samp <- rnorm(n = 40, mean = 10, sd = 3)
hist(samp)

sample(samp, size = 3)
sample(samp, size = 300, replace = TRUE)


#useful for data subsampling
mtcars[sample(1:nrow(mtcars), size = 3),]

mtcars %>% sample_n(size = 3, replace = TRUE)

mtcars %>% sample_frac(size = 0.1, replace = TRUE)


####
# simulate means from a sample
####

samp_sim <- data.frame(
  samp_size = rep(3:50, time = 10)
)

#take this sample data frame
samp_sim <- samp_sim %>%
  #for each row (for each simulation)
  rowwise() %>%
  # draw a random normal sample with n = samp_size and take a mean
  mutate(samp_mean = mean(rnorm(samp_size, mean = 10, sd = 3)),
         samp_mean_from_samp = mean(sample(samp, size = samp_size, replace=TRUE))) %>%
  ungroup()

  
plot(samp_mean ~ samp_size, data = samp_sim)

#what is our optimal sample size?


#take our simulations
samp_sim %>%
  #for each sample size
  group_by(samp_size) %>%
  #calculate the SD of sample means
  summarize(sd_mean = sd(samp_mean)) %>%
  #order by SD
  arrange(sd_mean)



