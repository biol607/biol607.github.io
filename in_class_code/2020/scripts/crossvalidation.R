#'---------------------------------------------
#' @title Cross-Validation Intro
#'---------------------------------------------

#load some libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

#libraries for CV
library(rsample)
library(boot)
library(modelr)

# the data
roaches <- read.csv("data/chap17f5_4CockroachNeurons.csv")

# plot it with a fit linear regression
ggplot(data = roaches,
       aes(x = temperature, y = rate)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  stat_smooth(method = "lm", fill = NA,
              formula = y ~ 1, fill = NA,
              color = "red", linetype = 2)

# Simplest case for CV is one point ###
roaches_no_15 <- roaches[-15,]
roaches_15 <- roaches[15,]

# CV in a nutshell
# 1. fit a model(s) with your training data
roach_lm_no_15 <- lm(rate ~ temperature, data = roaches_no_15)
roach_int_no_15 <- lm(rate ~ 1, data = roaches_no_15)

# 2. evaluate the out of sample deviance (MSE) for your 
# test data

# Get that RMSE!
rmse(model = roach_lm_no_15, data = roaches_15)
rmse(model = roach_int_no_15, data = roaches_15)


# K-Fold Cross-Validation ####

# make a folded dataset object
roach_five_fold <- vfold_cv(roaches, v = 5)

#analysis(roach_five_fold$splits[[1]])
#assessment(roach_five_fold$splits[[1]])

# Fit a model to each fold
#start with our tibble
set.seed(2020)
roach_five_fold <- roach_five_fold %>% 
  
  #create a new column mods, which we make with map
  # iterating over all of our splits
  mutate(mods = map(splits, 
                    
                    #for each split, fit a model using
                    #the training data set
                    ~lm(rate ~ temperature,
                        data = analysis(.x))))

# extract the out of sample rmse for each model at each fold
# using a function called map2
x <- 1:10
y <- 11:20

map2_dbl(x, y, ~.x + .y)

# start with our tibble
roach_five_fold <- roach_five_fold %>%
  
  # create a new column, rmse, which we make with map2
  # iterating over all splits AND fit models
  mutate(rmse = map2_dbl(.x = splits, .y = mods,
                         ~rmse(model = .y,
                               data = assessment(.x))))

# Calculate the average root mean square error for
# out of sample prediction
mean(roach_five_fold$rmse)


### What does one fold look like?
ggplot(analysis(roach_five_fold$splits[[1]]),
       aes(x = temperature, y = rate)) +
  #geom_point() +
  stat_smooth(method = "lm", fill = NA) +
  geom_point(data = assessment(roach_five_fold$splits[[1]]),
             color = "red") +
  geom_segment(data = assessment(roach_five_fold$splits[[1]]),
               aes(xend = temperature, 
                   yend = predict(roach_five_fold$mods[[1]],
                                  newdata = assessment(roach_five_fold$splits[[1]]))),
             color = "red", 
             lty = 2 )
  
#for one fold
p <-  predict(roach_five_fold$mods[[1]],
              newdata = assessment(roach_five_fold$splits[[1]]))

o <- assessment(roach_five_fold$splits[[1]])$rate

#mse
sum((p-o)^2)/length(o)

#rmse
sqrt(sum((p-o)^2)/length(o))

# LOOCV ####

# Start by making a LOO tibble
roach_loo <- roaches %>%
  loo_cv() %>%
  
  # fit the temperature model and intercept only model
  # for each LOO split
  mutate(temp_mod = map(splits,
                        ~lm(rate ~ temperature, 
                           data = analysis(.x))),
         
         # create a new column
         #using map to iterate over all splits
         int_mod = map(splits,
                       #fit a linear model where rate
                       #is predicted by an intercept
                       ~lm(rate ~ 1,
                           #fit that model on the training
                           #data from each split
                           data = analysis(.x))))


# Get the RMSE of each model and each model TYPE 
# for each LOO split

# start with our tibble
roach_loo <- roach_loo %>%
  # pivot to put ALL models in one column
  pivot_longer(cols = c(temp_mod, int_mod),
               names_to = "model_name",
               values_to = "fit_model") %>%
  
  # get our rmse just like before with map2!
  mutate(rmse = map2_dbl(.x = splits, .y = fit_model, #what we're using
                     ~mse(data = assessment(.x), #our test data
                          mod = .y))) #out model to generate preds

# the answer!
roach_loo %>%
  group_by(model_name) %>%
  summarize(loo_mse = mean(rmse))


ggplot(data = roach_loo,
       mapping = aes(x = id, y = rmse, color = model_name)) +
  geom_point() +
  scale_x_discrete(labels = NULL)


# Using boot::cv.glm() for LOO or k-fold ####

roach_glm <- glm(rate ~ temperature, data = roaches,
                 family = gaussian(link = "identity"))

loo_roach <- cv.glm(data = roaches,
                    glmfit = roach_glm,
                    K = nrow(roaches))

#what is our LOO CV score?
loo_roach$delta[1] %>% sqrt() #rmse

# AIC ####
roach_lm <- lm(rate ~ temperature, data = roaches)
roach_int <- lm(rate ~ 1, data = roaches)
roach_sq <- lm(rate ~ poly(temperature, 2), data = roaches)
roach_cub <- lm(rate ~ poly(temperature, 3), data = roaches)


AIC(roach_lm)
AIC(roach_int)

library(AICcmodavg)

mod_list <- list(roach_int, roach_lm, roach_sq, roach_cub)
name_vec <- c("int", "linear", "quad", "cube")

aictab(cand.set = mod_list, modnames = name_vec)


#what are coefs?
broom::tidy(roach_cub)



