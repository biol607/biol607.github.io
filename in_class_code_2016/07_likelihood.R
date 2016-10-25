library(dplyr)
library(tidyr)
library(ggplot2)

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

