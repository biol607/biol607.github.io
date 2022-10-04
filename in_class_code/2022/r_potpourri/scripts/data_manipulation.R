#' ------------------------------------
#' Data manipulation with dplyr and tidyr
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

##### Look at the data #####
surveys
str(surveys)
visdat::vis_dat(surveys)

#### SELECTing certain columns ####

# we want the plot_id, species_id, and weight column
select(surveys, plot_id, species_id, weight)

# equivalent subset
surveys[,c("plot_id", "species_id", "weight")]

# with a pipe
surveys |>
  select(plot_id, species_id, weight) 

# remove record_id and species_id from surveys
#surveys[, !c("")]
surveys |>
  select(-record_id, -species_id) #use - to get rid of

#### What if we want to FILTER the dataset down #####

# We want only 1995 data
surveys[surveys$year == 1995, ]

surveys_1995 <- surveys |>
  filter(year == 1995)

#### We can now build complex data manipulation pipelines #####

# we want to create an object
surveys_sml <- 
  # from surveys
  surveys |>
  # where weight is less than 5 grams
  filter(weight < 5) |>
  # and we only have species_id, sex, and weight
  select(species_id, sex, weight)

#### EXERCISE ####
# Subset surveys
surveys |>
  # to include animals collected before 1995
  filter(year < 1995) |>
  # and only give back year, sex, and weight
  select(year, sex, weight)
  #select(year, sex, weight, everything()) #just reorder

# defensive test programming
# surveys_2 <- surveys |>
#   select(everything()) |>
#   head()
surveys <- surveys |>
  select(everything()) 

#### What if you want to change or create a new Value? ####
# we want to add a column weight_kg
surveys$weight_kg <- surveys$weight / 1000

# MUTATE your way to health and happiness
surveys <- surveys |>
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) #|>
# pull(weight_lb) #debug and check yourself by PULLING out a vector




# Challenge 
# Create a new data frame from the surveys data that 
# meets the following criteria: 
# contains only the species_id column and a new column
# called hindfoot_cm containing the hindfoot_length values 
# (currently in mm) converted to centimeters. 
# In this hindfoot_cm column, there are no NAs and all
# values are less than 3.

# create a new data frame 
surveys_hindfoot_cm <- 
  # starting with surveys
  surveys |>
  # calculate hindfoot_cm based on hindfoot_length (in mm) /10
  mutate(hindfoot_cm = hindfoot_length / 10) |>
  # select down to specied_id and hindfoot_cm
  select(species_id, hindfoot_cm) |>
  # make sure there are no NA hindfoot_cm
  filter(!is.na(hindfoot_cm)) |>
  # make sure that all hindfoot_cm < 3
  filter(hindfoot_cm < 3)
  
visdat::vis_dat(surveys_hindfoot_cm)
summary(surveys_hindfoot_cm)

# if we REALLY want detail about our data
skimr::skim(surveys_hindfoot_cm)

#### Split - Apply - Combine ####

# What is the average weight of a rodent by sex?
#surveys |> filter(sex == "M") %>% mean()....
#surveys |> filter(sex == "F") %>% mean()....

#this is NOT QUITE what we want
surveys |>
  group_by(sex) |>
  mutate(mean_weight = mean(weight, na.rm = TRUE)) |>
  pull(mean_weight)

#this IS what we want
surveys |>
  group_by(sex) |>
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys |>
  filter(!is.na(weight)) |>
  group_by(sex) |>
  summarize(mean_weight = mean(weight))


# Let's say we want to get the mean and minimum
# of weight by species and sex
# and then sort by minimum

# start with surveys
surveys |>
  # filter out NA weights
  filter(!is.na(weight)) |>
  # group by species and sex
  group_by(sex, species_id) |>
  # calculate mean and min weights
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) |> #,
            #.groups = "drop") |>
  ungroup() |>
  # ARRANGE by minimum
  arrange(min_weight,
          desc(mean_weight))

#### What if you just want to COUNT things ####

# how many individuals of each sex and species do you have?
surveys |>
  group_by(sex, species_id) |>
  summarize(count = n()) #n() gives you the number of entries (rows)

# more compact
surveys |>
  count(sex, species_id)


# Why use ungroup?
# Let's say we wanted to look at
# how each species/sex combination's minimum weight
# differed from the SMALLEST minimum weight in the whole data set
surveys |>
  filter(!is.na(weight)) |>
  group_by(species_id, sex) |>
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight, na.rm = TRUE)) |>
  #ungroup() |>
  mutate(deviation_of_min = min_weight - min(min_weight)) 
### OOPS _ this mutate was done RELATIVE to species, not RELATIVE
### to the entire dataset


# What is the difference between mutate and summarize?
surveys |>
  mutate(mean_weight = mean(weight, na.rm = TRUE)) |>
  pull(mean_weight)

surveys |>
  summarize(mean_weight = mean(weight, na.rm = TRUE))



#### Exercises
#1. How many animals were caught in each plot_type surveyed?
surveys |>
  count(plot_type)

# 2 Use group_by() and summarize() to find the mean, min, 
# and max hindfoot length for each species (using species_id). 
# Also add the number of observations of each species (hint: see ?n)

surveys_hl <- surveys |>
  filter(!is.na(hindfoot_length)) |>
  group_by(species) |>
  summarize(mean_hl = mean(hindfoot_length),
            min_hl = min(hindfoot_length),
            max_hl=  max(hindfoot_length),
            count = n())

# 3. What was the heaviest animal measured in each year? 
# Return the columns year, genus, species_id, and weight hint - 
# filter is your friend!

# take surveys
surveys |>
  # filter NAs of weight
  filter(!is.na(weight)) |>
  # for each year
  group_by(year) |>
  # get the heaviest
  filter(weight == max(weight)) |>
  # AND THEN select out the year, genus, species ID and weight
  select(year, genus, species_id, weight) |>
  # ungroup and arrange by year
  ungroup() |>
  arrange(year)
