### Chi square test

library(readr)
library(dplyr)
library(ggplot2)
library(broom)

#birth data
births <- read_csv("./data/05/08e1DayOfBirth.csv")

#check that the data is OK
births

#calculate my expectation
expected_births <- mean(births$`Number of births`)

ggplot(data= births) +
  aes(x = Day, y = `Number of births`) +
  geom_point() +
  geom_hline(yintercept = expected_births)

#put it to the test
ch_births <- chisq.test(births$`Number of births`)
ch_births

names(ch_births)
tidy(ch_births)


# chisq with 6df
x <- seq(0, 30, length.out=1000)
ch_6 <- dchisq(x, df = 6)
plot(ch_6 ~ x, type="l", xlab="chisq")



################ 
# T-tests
################ 
chinook <- read_csv("./data/05/12e4BrookTrout.csv")

# look at our data
ggplot(data = chinook) +
  aes(x = `brook trout?`, y = `Mean chinook survival`) +
  geom_boxplot() + 
  geom_point()


## calculate residuals

chinook <- chinook %>%
  group_by(`brook trout?`) %>%
  mutate(mean_resid = `Mean chinook survival` - 
           mean(`Mean chinook survival`))

#histogram
hist(chinook$mean_resid)

##qq
quantile(chinook$mean_resid)

qqnorm(chinook$mean_resid)
qqline(chinook$mean_resid)

# Shapiro-Wilks test of Normality
shapiro.test(chinook$mean_resid)

#put it to the test
chinook_t <- t.test(`Mean chinook survival` ~ `brook trout?`,
       data = chinook, var.equal = FALSE)

chinook_t

tidy(chinook_t)

#visualize results

ggplot(data = chinook) +
  aes(x = `brook trout?`, y = `Mean chinook survival`) +
  stat_summary(fun.data = "mean_cl_boot")
