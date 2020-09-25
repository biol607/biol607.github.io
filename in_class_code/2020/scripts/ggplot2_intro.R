#'--------------------------------------
#'
#' @title: Intro to ggplot2
#' @author: Jarrett Byrnes
#' @date 2020-09-25
#' 
#' 
#' @description: This code is my first foray
#'  into ggplot2!
#'
#'
#'--------------------------------------

# libraries
library(ggplot2)
library(dplyr)
library(palmerpenguins)

head(penguins)

# Look at the distribution of bill lengths ####

#properties of a figure to map to
# x, y, color, size, shape, alpha.....

bill_dens <- ggplot(data = penguins,
                    mapping = aes(x = bill_length_mm))


bill_dens +
  geom_histogram(bins = 40)

bill_dens +
  geom_freqpoly()

# what happens if you add geom_density()?
bill_dens + 
  geom_density()

# Look at the distribution of bill lengths by species ####

bill_dens_group <- ggplot(data = penguins,
                          mapping = aes(x = bill_length_mm,
                                        group = species))


bill_dens_group +
  geom_density()

# let's add some fill colors!
bill_dens_group +
  geom_density(mapping = aes(fill = species))

# let's make *all* of these density plots a little transparent
# We are going to specify something about an aesthetic
# but ***NOT*** have it mapped to the data
bill_dens_group +
  geom_density(mapping = aes(fill = species), #maps to data
               alpha = 0.3) #does not map to data - it's fixed


bill_dens_group +
  geom_density(mapping = aes(fill = species), #maps to data
               position = "stack") #does not map to data - it's fixed

# Exercise: Now you try geom_histogram. How’s it look? 
# Bad, right? Try different colors, alphas, and fills 
# to see if you can improve. Maybe a different position?
# What works best?


# do a little x and y as your two dimensions ####
pen_plot_base <- ggplot(data = penguins,
                        mapping = aes(x = body_mass_g,
                                      y = species,
                                      color = species))

pen_plot_base


pen_plot_base +
  geom_point(size = 3,
             alpha = 0.3)

# geom_jitter - move the points around
pen_plot_base +
  geom_jitter(size = 2, 
              alpha = 0.6)

pen_plot_base +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_jitter(width = 0,
                                        height = 0.4))
# Exercise
# 1. Try out the following geoms - geom_boxplot(), 
#    geom_violin(), stat_summary(). Which do you prefer?
pen_plot_base +
  geom_boxplot()

pen_plot_base +
  geom_violin()

pen_plot_base +
  stat_summary()

# 2. Try adding multiple geoms together. 
#     Does order matter?
# EC. If you want to get saucy, install ggridges and 
#   try out geom_density_ridges()


# Continuous values on both axes
pen_mass_depth <- ggplot(data = penguins,
                         mapping = aes(x = body_mass_g,
                             y = bill_depth_mm,
                             color = species))

pen_mass_depth +
  geom_point()


# Multi-panel plots! ####

# faceting!

#facet_wrap - we just split things up 
# and let R arrange (or specify row, cols)
pen_mass_depth +
  geom_point() +
  facet_wrap(~species)

pen_mass_depth +
  geom_point() +
  facet_wrap(~ species + island)

# 1. Given that we have the same species of penguin on different 
# islands, what do you see if you use facet_grid() with both 
# island and species?
  
# 2. Incorporate other faceting variables - sex, year, etc. Or 
# mix up what is a facet and what is a color. What do you learn?

# Make our plots sing!!! ####

pen_scatter <- pen_mass_depth +
  geom_point()


# Labels!
pen_scatter <- pen_scatter +
  labs(title = "Penguin Bill Depth verus Body Mass",
       subtitle = "Data from Palmer LTER",
       x = "Body Mass (g)",
       y = "Bill Depth (mm)",
       color = "Species of\nPenguin")


pen_scatter +
  theme_bw(base_size = 14,
           base_family = "Times")

# my fav package of themes
library(ggthemes)
pen_scatter +
  theme_tufte()

pen_scatter +
  theme_excel()


pen_scatter +
  theme_fivethirtyeight()


#if you want to set a theme
theme_set(theme_classic(base_size = 12, base_family = "Times"))


# Colors! ####

pen_scatter +
  scale_color_manual(values = c("orange", "purple", "darkblue"))

# Many pre-built palattes
pen_scatter +
  scale_color_brewer(palette = "Dark2")

pen_scatter +
  scale_color_viridis_d()

pen_scatter +
  scale_color_viridis_d(option = "A")

pen_scatter +
  scale_color_manual(values = rainbow(3))

#packages with color palettes
library(wesanderson)
pen_scatter +
  scale_color_manual(values = wes_palette("BottleRocket2"))


# Continuous color scales ####
pen_mass_col <- ggplot(data = penguins,
                       mapping = aes(x = bill_depth_mm,
                                     y = bill_length_mm,
                                     color = body_mass_g)) +
  geom_point() +
  facet_wrap(~species)

pen_mass_col


# VIRIDIS!!!
pen_mass_col +
  scale_color_viridis_c(option = "B")


pen_mass_col + 
  scale_color_gradient(low = "blue", high = "red")


pen_mass_col + 
  scale_color_gradientn(colors = c("blue", "green", "orange", "red"))

pen_mass_col + 
  scale_color_gradientn(colors = c("blue", "green", "orange", "red")) +
  theme_bw(base_family = "NewCenturySchoolbook")

# Exercise: OK - let’s look at how flipper length relates to 
# bill length and depth. Feel free to choose what gets to be a 
# color and what gets to be a coordinate. Combine other aspects - 
# sex, year, etc., to find something more interesting. 
# But above all, make this plot have a great color scale with 
# matching theme to make it pop.

