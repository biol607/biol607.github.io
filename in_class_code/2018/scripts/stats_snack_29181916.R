#'-----------------
#'
#' Stats snack 2018-10-16
#'
#'---------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(visdat)
library(Hmisc)

trout <- read_csv("../data/12e4BrookTrout.csv", na = ".")

vis_dat(trout)


#what is in here
str(trout)
trout

#plot
ggplot(data = trout,
       mapping = aes(x = `brook trout?`, y = `Mean chinook survival`)) +
  geom_boxplot() #+
  #stat_summary(color = "red", fun.data = mean_sdl)

trout9293 <- trout %>%
  select(`chinook survival 1992`, `chinook survival 1993`) %>%
  rowwise() %>%
  mutate(Mean_survival_92_93 = mean(`chinook survival 1992`, 
                                    `chinook survival 1993`))


trout <- trout %>%
  rowwise() %>%
  mutate(Mean_survival_92_93 = mean(`chinook survival 1992`, 
                                    `chinook survival 1993`))

chinook_test <- t.test(`Mean chinook survival` ~ `brook trout?`,
                      data = trout,
                      unequal.var=TRUE)


chinook_test
broom::tidy(chinook_test)
broom::glance(chinook_test)
