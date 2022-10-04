#' ----------------------------------------------
#' Ggplot2 intro with palmerpenguins
#' 
#' @date 2022-09-15
#' ----------------------------------------------

#### Libraries ####
library(ggplot2)
library(palmerpenguins)
library(visdat)
library(ggridges)

# In general - if you don't have a library
# replace library with install.packages
# and after that, switch it back to library
#install.packages("palmerpenguins")

#### Let's look at the data penguins ####
head(penguins)
str(penguins)
summary(penguins)
vis_dat(penguins)

# how to run a function from a library without loading it
# library_name::function()
visdat::vis_miss(penguins)

#### Our First Ggplot! ####

# Look at the distribution of bill length
bill_dist <- ggplot(data = penguins,
                    mapping = aes(x = bill_length_mm))

class(bill_dist)
str(bill_dist)

# Look at our masterpiece!
bill_dist

# Add a geom_()
bill_dist +
  geom_histogram(bins = 40)

# geom_density
# fill, color for outlines, alpha for transparency
bill_dist +
  geom_density(fill = "goldenrod",
               color = "blue",
               alpha = 0.5)

#### Visualizing multiple distributions ####
bill_dist_byspecies <- 
  ggplot(data = penguins,
         mapping = aes(x = bill_length_mm,
                       group = species))

bill_dist_byspecies +
  geom_density()

# add fill as an aesthetic
bill_dist_byspecies +
  geom_density(mapping = aes(fill = species),
               alpha = 0.5)

# arguments for positioning
bill_dist_byspecies +
  geom_density(mapping = aes(fill = species),
               alpha = 0.5,
               position = "stack")

# Exercise: Now you try geom_histogram. How’s it look? 
# Bad, right? Try different colors, alphas, and fills 
# to see if you can improve. Maybe a different position? 
# What works best?
bill_dist_byspecies +
  geom_histogram(mapping = aes(fill = species),
                 alpha = 0.75,
                 bins = 50)

# identity
bill_dist_byspecies +
  geom_histogram(mapping = aes(fill = species),
                 alpha = 0.75,
                 bins = 50,
                 position = "identity")

bill_dist_byspecies +
  geom_histogram(mapping = aes(fill = species),
                 alpha = 0.75,
                 bins = 30,
                 position = "fill")

#### Visualizing Two Dimensions in Space ####
bill_dist_xy <- ggplot(data = penguins,
                       mapping = aes(x = bill_length_mm,
                                     y = species,
                                     color = species))
# geom_point
bill_dist_xy +
  geom_point()

bill_dist_xy +
  geom_point(size = 5, alpha = 0.1)

# OTOH - what about a jitter
bill_dist_xy +
  geom_jitter(size = 3, alpha = 0.5)

#Exercise: 
#  1. Try out the following geoms - geom_boxplot(), 
#     geom_violin(), stat_summary(). Which do you prefer?

bill_dist_xy +
  geom_boxplot() +
  coord_flip() # a quick fix

bill_dist_xy +
  geom_violin(alpha = 0.5) 

bill_dist_xy +
  stat_summary()

ggplot(data = penguins,
       aes(x = species, y = bill_depth_mm, color = species)) +
  stat_summary(fun.data = "mean_sdl", 
               geom="linerange", size = 4, 
               color = "black") +
  stat_summary(fun.data = "mean_se", size = 2 ) +
  coord_flip()

bill_dist_xy +
  ggdist::stat_halfeye()

#  2. Try adding multiple geoms together. Does order matter?
#  3. Try out geom_density_ridges() from ggridges
bill_dist_xy +
  geom_density_ridges(alpha = 0.5,
                      mapping = aes(fill = species))

# If you finish, and want more fun while you wait
# Try out ggdist and it's geoms https://mjskay.github.io/ggdist/

#### Scatterplots ####

pen_mass_length <- 
  ggplot(data = penguins,
         mapping = aes(x = body_mass_g,
                       y = bill_length_mm,
                       color = species))
# basic scatter
pen_scatter <- pen_mass_length +
  geom_point(alpha = 0.6)

pen_scatter

#### Facet by a data property ####
pen_scatter +
  facet_wrap(vars(species))

pen_scatter +
  facet_wrap(vars(species, sex), ncol = 2)


# Exercise: Take 10-15 on this, and we will break until 10:30
#   1. Given that we have the same species of penguin on 
# different islands, what do you see if you use facet_grid() 
# with both island and species?
pen_scatter +
  facet_grid(rows = vars(island),
             cols = vars(species))

pen_scatter + 
  facet_wrap(vars(island, species))

#   2. Incorporate other faceting variables - sex, year, etc. 
# Or mix up what is a facet and what is a color. What do you 
# learn?
ggplot(data = penguins[!is.na(penguins$sex),],
       mapping = aes(x = body_mass_g,
                     y = bill_length_mm,
                     color = sex)) +
  geom_point(alpha = 0.5) +
  facet_grid(rows = vars(island),
             cols = vars(species))

# facets and scales
pen_scatter +
  facet_wrap(vars(sex),
             scales = "free")

#removing NAs
penguins_clean <- penguins[ !is.na(penguins$bill_depth_mm), ]

idx <- which(is.na(penguins$bill_depth_mm))

penguins[-idx, ]

vec <- c(3,4,NA)
vec[-3]

# what to write if you want to make future you
# hate current you
`[`(vec, -3)

idx_sp <- which(is.na(penguins$species))
penguins[-idx_sp, ]
#penguins[!is.na(penguins$species),]


#### Add a little spice ####

pen_scatter

## LABS()
# add a title
# rename axes
# captions
pen_scatter_beautiful <- pen_scatter +
  labs(title = "Penguin Bill Length by Mass",
       subtitle = "Data from Palmer LTER",
       x = "Body Mass (g)",
       y = "Bill Length (mm)",
       color = "Species of\nPenguin") #\n is a line break 

pen_scatter_beautiful
# redundant coding with shape
pen_scatter_beautiful <- pen_scatter_beautiful +
  aes(shape = species) +
  labs(shape = "Species of\nPenguin") #make sure it shares the label with color

pen_scatter_beautiful

# make it colorblind friendly
pen_scatter_beautiful +
  scale_color_manual(values = c("red", "blue", "orange"))

pen_scatter_beautiful +
  scale_color_brewer(type = "qual")

pen_scatter_beautiful +
  scale_color_brewer(palette = "Dark2")

pen_scatter_beautiful <- pen_scatter_beautiful +
  ggthemes::scale_color_colorblind()


# change the background theme/color
pen_scatter_beautiful +
  theme_light()

# whole packages of themes
pen_scatter_beautiful +
  ggthemes::theme_excel()

pen_scatter_beautiful <- pen_scatter_beautiful +
  theme_minimal(base_size = 16,
                base_family = "Times")# +
 # theme(panel.background = element_rect(fill = "blue"))

# if you want to set a theme for a whole code file
#theme_set(theme_minimal())


# continuous scales

pen_scatter +
  aes(color = bill_depth_mm) +
  scale_color_viridis_c()

# change range of axes
# add sample counts


#### stats ####
pen_mass_length +
  geom_point()

# smoother
pen_mass_length +
  stat_smooth()

pen_mass_length +
  stat_smooth(method = "lm") +
  geom_point()

ggplot(data = penguins,
       mapping = aes(x = body_mass_g,
                     y = bill_length_mm)) +
  stat_bin_2d()

ggplot(data = penguins,
       mapping = aes(x = body_mass_g,
                     y = bill_length_mm)) +
  stat_density_2d_filled()


# Load the Bigfoot data for Tidytuesday!
# Explore what is there
# sketch our a plot you would want to make
# Make a cool plot! Make it fancy
# Post the plot to slack!

# Get the Data

# Or read in the data manually
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-09-13
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')


ggplot(data = bigfoot |> dplyr::filter(season != "Unknown"),
       aes(x = cloud_cover,
           fill = season,
           y = season)) +
  stat_density_ridges(alpha = 0.5)


####
library(dplyr)
library(geofacet)

bigfoot_bystate <- bigfoot |>
  filter(season != "Unknown") |>
  group_by(state, season) |>
  summarize(n = n()) |>
  group_by(state) |>
  mutate(frac = n / sum(n)) |>
  ungroup()|>
  mutate(season = forcats::fct_relevel(.f = season, 
                         levels = c("Winter",
                                    "Spring",
                                    "Summer",
                                    "Fall")))

#histogram by state of seasonal appearance
ggplot(data = bigfoot_bystate,
       aes(y = season,
           x = n,
           fill = season)) +
  geom_col() +
  facet_geo(~state) +
  theme_void() 

#histogram by state of fractional seasonal appearances
ggplot(data = bigfoot_bystate,
       aes(x = season,
           y = frac,
           fill = season)) +
  geom_col() +
  coord_flip() +
  facet_geo(~state) +
  theme_void() 
