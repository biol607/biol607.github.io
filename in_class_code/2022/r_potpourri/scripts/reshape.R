#' ------------------------------------
#' Data reshaping with tidyr
#' 
#' @date 2022-09-22
#' ------------------------------------

setwd(here::here()) # LOVE THIS LINE

#### Loading some libraries ####
library(dplyr) #data manipulation
library(tidyr) #reshaping tidy data
library(readr) #read in some data

#### Load in data ####
# load up data/combined.csv
# +1 on the etherpad once loaded
surveys <- read_csv("data/combined.csv")

#### Let's make a dataset that is the mean weight of
#### each genus in each plot ID
surveys_gw <- 
  surveys |>
  filter(!is.na(weight)) |>
  group_by(plot_id, genus) |>
  summarise(mean_weight = mean(weight)) |>
  ungroup()

# Let's pivot wide
surveys_gw_wide <- 
  surveys_gw |>
  pivot_wider(names_from = genus,
              values_from = mean_weight)

View(surveys_gw_wide)

#let's pivot wide but fill in 0 for NAs

surveys_gw_wide <- 
  surveys_gw |>
  pivot_wider(names_from = genus,
              values_from = mean_weight,
              values_fill = 0,
              names_prefix = "weight_",)

# pivoting LONG
# pivot_longer you need quoted names for new columns
surveys_gw_long <- 
  surveys_gw_wide |>
  pivot_longer(names_to = "measure_genus",
               values_to = "mean_weight",
               cols = -plot_id)

# Reshape the surveys data frame with year as columns, 
# plot_id as rows, and the number of genera per plot as the values.
# You will need to summarize *before reshaping*, and use the 
# function n_distinct() to get the number of unique genera within a 
# particular chunk of data. It’s a powerful function! See ?n_distinct for more.

#make a new object
annual_genera <- 
  # start with surveys
  surveys |>
  # for each year and plot
  group_by(plot_id, year) |>
  # get the unique number of genera
  summarize(num_genera = n_distinct(genus)) |>
  # then pivot wide with year as column
  pivot_wider(names_from = year,
              values_from = num_genera)

# EXERCISE - pivot back to the long format
# so each row is a unique year and plot_id combination
annual_genera_long <-
  annual_genera |>
  pivot_longer(cols = -plot_id,
               names_to = "year",
               values_to = "num_genera")

# I want to plot the AVERAGE hindfoot_length per plot_id and plot_type
# against the AVERAGE weight
library(ggplot2)
ggplot(surveys,
       mapping = aes(x = hindfoot_length,
                     y = weight)) +
  geom_point()

lw_surveys <- surveys |>
  # we want to work with just these four variables
  select(plot_id, plot_type, hindfoot_length, weight) |>
  # then, filter out NAs
  filter(!is.na(hindfoot_length),
         !is.na(weight))|>
  # pivot longer so that MEASUREMENT TYPE is a column
  pivot_longer(cols = c(hindfoot_length, weight),
               names_to = "measurement",
               values_to = "value") |>
  # group by plot id, type, and measurement, 
  group_by(plot_id, plot_type, measurement) |>
  # and get averages
  summarize(mean_value = mean(value))


ggplot(lw_surveys,
       mapping = aes(x = plot_type,
                     y = mean_value)) +
  stat_summary() +
  facet_wrap(vars(measurement), scale = "free")

# let's look at relatinoships
lw_wide <- 
  lw_surveys |>
  pivot_wider(names_from = measurement,
              values_from = mean_value)

#the plot from before, but with averages
ggplot(lw_wide,
       mapping = aes(x = hindfoot_length,
                     y = weight,
                     color = plot_type)) +
  geom_point()
