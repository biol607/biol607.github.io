#'-----------------------------
#' Chisq and t-tests
#' 
#' @author Jarrett Byrnes
#' @date 2018-10-05
#' 
#'-----------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

births <- read_csv("../data/08e1DayOfBirth.csv")
births_oldskool <- read.csv("../data/08e1DayOfBirth.csv")

expected <- sum(births$`Number of births`)/nrow(births)

expected

#visualize the data
ggplot(births,
       mapping = aes(x = Day, y = `Number of births`)) +
  geom_col() +
  geom_hline(yintercept = expected, color = "red",
             lty = 2)

#run a chisq test
chisq.test(births$`Number of births`)

### Chisq test with non-null expectation
extinct <- read_csv("../data/08e6MassExtinctions.csv")


ggplot(extinct,
       mapping = aes(x = `Number of extinctions`,
                     y = Frequency)) +
  geom_col()

#calculate some expectations
freqs <- dnbinom(0:20, mu = 3.6, size = 3)
freqs <- freqs/sum(freqs)

extinct <- extinct %>%
  mutate(Expected = freqs * sum(Frequency))

#visualize observed v. predicted
ggplot(extinct,
       mapping = aes(x = `Number of extinctions`,
                     y = Frequency)) +
  geom_col() +
  geom_point(mapping = aes(y = Expected), color = "red")

#chisq test
chisq.test(extinct$Frequency, p = freqs)
#--------------


#faded examples
#Load
births <- read_csv("data/05/08e1DayOfBirth.csv")

#calculate expected values
expected <- sum(births$`Number of births`)/nrow(births)

#visualize
ggplot(data=births,
       mapping = aes(x=Day, y=`Number of births`)) +
  geom_point() +
  geom_hline(yintercept = expected)

#test
chisq.test(births$`Number of births`)
#----------------


#load
powerball <- read_csv("../data/08q02Powerball.csv")

#calculate expected values
expected_powerball <- sum(powerball$`Millions of tickets sold`)/nrow(powerball)

#visualize
ggplot(data=powerball, mapping = aes(x=Day, y=`Millions of tickets sold`)) +
  geom_col() +
  geom_hline(yintercept = expected_powerball, color="red")

##test
chisq.test(powerball$`Millions of tickets sold`)

#------------------
#load
boys <- read_csv("../data/08e5NumberOfBoys.csv")

#calculate expected values
freq_boys <- c(0.25, 0.5, 0.25)
boys$expected_boys <- freq_boys * sum(boys$Frequency)

#visualize
ggplot(data=boys, 
       mapping = aes(x=expected_boys, y=Frequency)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red")

##test
chisq.test(boys$Frequency,
           p = freq_boys)
#------------------
#Contingency tables

parasite <- read_csv("../data/09e3ParasiteBrainWarp.csv")

parasite

#visualization
inf_x <- ggplot(data = parasite,
                mapping = aes(x = `infection status`,
                              y = frequency,
                              fill = eaten)) +
  geom_col()

inf_x


eaten_x <- ggplot(data = parasite,
                mapping = aes(fill = `infection status`,
                              y = frequency,
                              x = eaten)) +
  geom_col()

eaten_x

#install patchwork
#devtools::install_github("thomasp85/patchwork")

library(patchwork)
inf_x + eaten_x

#rasters
ggplot(data = parasite,
       mapping = aes(x = `infection status`,
                     fill = frequency,
                     y = eaten)) +
  geom_raster() +
  scale_fill_gradient(low = "white", high = "green") +
  geom_text(mapping = aes(label = frequency))


#make a contingency table
cont_table <- xtabs(frequency ~ eaten + `infection status`,
                    data = parasite)

cont_table

chisq.test(cont_table)



#legend titles
ggplot(data = parasite,
       mapping = aes(x = `infection status`,
                     fill = frequency,
                     y = eaten)) +
  geom_raster() +
  geom_text(mapping = aes(label = frequency)) +
  scale_fill_gradient(low = "white", high = "green",
                      guide = guide_colorbar("Number"))

ggplot(data = parasite,
       mapping = aes(x = `infection status`,
                     fill = frequency,
                     y = eaten)) +
  geom_raster() +
  geom_text(mapping = aes(label = frequency)) +
  scale_fill_gradient(low = "white", high = "green") +
  guides(fill = guide_colorbar("Number"))

#------------
# T-Test ####
#------------

blackbird <- read_csv("../data/12e2BlackbirdTestosterone.csv")

ggplot(data = blackbird,
       mapping = aes(x = dif)) +
  geom_density()

t.test(blackbird$dif)

library(broom)
tidy(t.test(blackbird$dif))

## two sample
chinook <- read_csv("../data/12e4BrookTrout.csv")

#visualize the data
ggplot(data = chinook,
       mapping = aes(x = `brook trout?`,
                     y = `Mean chinook survival`)) +
  geom_boxplot()

chin_test <- t.test(`Mean chinook survival` ~ `brook trout?`,
                    data = chinook,
                    unequal.var = TRUE)


chin_test

#Check our residuals by group
chinook <- chinook %>%
  group_by(`brook trout?`) %>%
  mutate(resid = `Mean chinook survival` - 
           mean(`Mean chinook survival`)) %>%
  ungroup()


##evaluate residuals visually
ggplot(chinook,
       mapping = aes(x = resid)) +
  geom_density()

#qq plot
qqnorm(chinook$resid)
qqline(chinook$resid)

#put it to the test
shapiro.test(chinook$resid)

##visualize t-test
ggplot(data = chinook,
       mapping = aes(x = `brook trout?`,
                     y = `Mean chinook survival`)) +
  stat_summary(fun.data = mean_cl_boot) +
  geom_jitter(color = "red")

#----- faded examples

#Load and visualize data
cichlid <- read_csv("../data/12q09Cichlids.csv")

ggplot(data=cichlid, 
       mapping=aes(x=Genotype, y=preference)) +
  geom_boxplot() 

## test assumptions
cichlid <- cichlid %>%
  group_by(Genotype) %>%
  mutate(resid = 
           preference - mean(preference)) %>%
  ungroup()

#qq
qqnorm(cichlid$resid)
qqline(cichlid$resid)
shapiro.test(cichlid$resid)

cichlid %>%
  group_by(Genotype) %>%
  do(tidy(shapiro.test(.$resid)))

#put it to the test!
t.test(preference ~ Genotype, data = cichlid,
           unequal.var = TRUE)

ggplot(data=cichlid, mapping=aes(x=Genotype,y=preference)) +
  stat_summary(size=1.5)

#------
#Load and visualize data
monogomy <- read_csv("../data/12q05MonogamousTestes.csv")

ggplot(data=monogomy, 
       mapping=aes(x=`Column 1` , y=`Testes area`)) +
  geom_boxplot() 

## test assumptions
monogomy <- monogomy %>%
  group_by(`Column 1`) %>%
  mutate(resid = 
         `Testes area` - mean(`Testes area`)) %>%
  ungroup()

#qq
qqnorm(monogomy$resid)
qqline(monogomy$resid)

shapiro.test(monogomy$resid)

#put it to the test!
t.test(`Testes area` ~ `Column 1` , data = monogomy,
           unequal.var = FALSE)

ggplot(data=monogomy, mapping=aes(x=`Column 1` ,y=`Testes area`)) +
  stat_summary(size=1.5) +
  geom_point(color = "red")



#--------------
# Power of a t-test
#--------------


## Simulate a a data set

make_t_data <- function(m1, m2, s, n){
  
  data.frame(group = c(rep("A", n), rep("B", n)),
             
             value = rnorm(n*2,
                           mean = c(rep(m1, n), rep(m2, n)),
                           sd = s),
             
             stringsAsFactors = FALSE)
  
  
}

make_t_data(0,5,3,4)

## fit a model with that data and get p
get_p_from_t <- function(a_dataset){
  test <- t.test(value ~ group,
                 data = a_dataset)
  
  test$p.value
  
}

get_p_from_t(make_t_data(0,5,3,4))

## Repeat getting p some # of times
get_lots_of_p_from_t <- function(m1, m2, s, n, nsim){
  replicate(nsim, get_p_from_t(make_t_data(m1, m2, s, n)))
}

get_lots_of_p_from_t(0,5,3,4,nsim = 100)

## Calculate power from those p values
get_power_from_t <- function(m1, m2, s, n, nsim, alpha = 0.05){
  #get p values
  p <- get_lots_of_p_from_t(m1, m2, s, n, nsim)
  
  #figure out how many are wrong
  num_wrong <- sum(p > alpha)
  
  #get power
  1 - num_wrong/nsim 
  
}

#get power from simulations
pow_frame <- crossing(m1 = 0, m2 = 3, s = 1:8, n = 4:8) %>%
  rowwise() %>%
  mutate(power = get_power_from_t(m1, m2, s, n, nsim = 100)) %>%
  ungroup()

ggplot(pow_frame,
       mapping = aes(x = s, y = power, color = factor(n))) +
  geom_point() +
  geom_line()









