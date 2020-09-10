#'------------------------------------
#'
#' Loading Files and Ggplot2
#' 
#' @Date 2018-09-21
#' 
#' @author Jarret Byrnes
#' 
#'------------------------------------

#---
# Loading Files ####
#---

#working directory
getwd()

# ../ takes you up one directory
# ./ is in the directory right here!
#e.g. my_data <- read.csv("../data/03/my_data.csv")
my_data <- read.csv("../data/my_data.csv")

str(my_data)

## two other ways of loading files
library(readr)
my_data_readr <- read_csv("../data/my_data.csv")


class(my_data)
class(my_data_readr)


#What if I like excel!
library(readxl)
my_data_excel <- read_excel("../data/my_data.xlsx")

#string v. factors
my_data_readr$Column_3
my_data$Column_3

#--------
# ggplot2
#--------
library(dplyr)
library(ggplot2)
library(forcats)

#load hadcrut_temp_anomoly_1850_2015.csv
hadcrut_temp <- read_csv("../data/hadcrut_temp_anomoly_1850_2015.csv")

#get month names in order
hadcrut_temp <- hadcrut_temp %>%
  mutate(month_name = factor(month_name)) %>%
  mutate(month_name = fct_inorder(month_name))

hadcrut_temp$month_name  
  
  
##
# let's plot things ###
##

had_dens <- ggplot(data = hadcrut_temp,
                   mapping = aes(x = anomaly))

had_dens +
  geom_histogram(bins = 100)

#many aesthetics - fill, color, group, linetype, 
# alpha, shape, and more

#make a new ggplot - and make month_name either fill or color, use geom_histogram or geom_density

had_dens_fancy <- ggplot(hadcrut_temp,
       aes(x = anomaly, group = month_name,
           color = month_name, fill = month_name,
           alpha = month_name)) 

had_dens_fancy+
  geom_histogram(bins = 5)

had_dens_fancy + 
  geom_density()

#----
#Two dimensions ####
#----

had_plot_base <- ggplot(data = hadcrut_temp,
                        mapping = aes(x = month_name,
                                      y = anomaly))

had_plot_base +
  geom_point()


### This looks terrible
### Try - geom_jitter(), geom_violin, 
### geom_boxplot(), stat_summary(),
### and for funsies, load library(ggridges)
### and try geom_density_ridges()
### look at arguments - particularly for geom_jitter

had_plot_base +
  geom_jitter(alpha = 0.5, height = 0,
              width = 0.25)

had_plot_base +
  aes(fill = month_name) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3)

had_plot_base +
  geom_violin()


library(ggridges)
ggplot(data = hadcrut_temp,
         mapping = aes(x=anomaly, y = month_name,
             fill = month_name)) +
  geom_density_ridges(alpha = 0.4)


#install.packages("ggbeeswarm")
library(ggbeeswarm)
had_plot_base +
  aes(color = month_name) +
  geom_beeswarm(alpha = 0.4)


#----
# lines and color ###
#----

had_plot_base +
  geom_line(mapping = aes(group = year))

#create a basic colored line plot
had_lines <- had_plot_base +
  geom_line(mapping = aes(group = year,
                          color = year))

had_lines


#playing with color scales
had_lines +
  scale_color_gradient(low = "blue", high = "red")

had_lines +
  scale_color_gradient2(low = "blue", high = "red",
                        mid = "yellow", midpoint = 1925)


had_lines +
  scale_color_gradientn(colors = rainbow(7))


#nice palattes
library(RColorBrewer)
display.brewer.all(n = 10, exact.n=FALSE)


had_lines +
  scale_color_gradientn(colors = brewer.pal(7,
                                            "BrBG"))
library(viridis)
had_lines + 
  scale_color_viridis(option = "A") +
  guides(color = "none")


##EXERCISE:
# Line plot with year on the x, anomaly on the y
# and use month as a group and/or color - do something
# cool as your palette! OR color by anomaly

had_months <- ggplot(hadcrut_temp,
                     aes(x = year,
                         y = anomaly,
                         color = month_name)) +
  geom_line()


had_months +
  #scale_color_gradient(low = "red", high = "blue")
  #scale_color_manual(values = rainbow(12))
  scale_color_brewer(palette = "Accent")

#----
# Facets ####
#----
had_months +
  facet_wrap(~month_name)

had_lines +
  facet_wrap(~cut_width(year, 10))

## EXERCISE = try cut_interval and cut_number


#summarization

had_plot_base +
 # geom_jitter(alpha = 0.2) +
  stat_summary(fun.data = mean_se, color = "red")


had_months +
  facet_wrap(~month_name) +
  stat_smooth(color = "black")


had_months +
  facet_wrap(~month_name) +
  stat_smooth(color = "black", method = "lm")

#---
#themes ####
#---

had_lines +
  theme_bw()


had_lines +
  theme_bw(base_size = 8)

had_lines +
  theme_void() +
  guides(color = "none")

library(ggthemes)
had_lines +
  theme_excel()

had_lines +
  theme_fivethirtyeight()

had_lines +
  theme_solarized(light = FALSE)

had_lines +
  theme(legend.position = c(.95, .95))

theme_set(theme_fivethirtyeight(base_size = 9))

had_lines
