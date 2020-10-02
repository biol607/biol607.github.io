#'---------------------------------
#' @title wide and long data with tidyr!
#' --------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# some data on mammals ####
mammals <- data.frame(site = c(1,1,2,3,3,3), 
                      taxon = c('Suncus etruscus', 'Sorex cinereus', 
                                'Myotis nigricans', 'Notiosorex crawfordi', 
                                'Suncus etruscus', 'Myotis nigricans'),
                      density = c(6.2, 5.2, 11.0, 1.2, 9.4, 9.6))

mammals #is long!

# Long to wide ####
# what if we want wide mammals!
m_wide <- mammals %>%
  pivot_wider(names_from = taxon,
              values_from = density)

#what do we have?
m_wide
visdat::vis_dat(m_wide)

# filling in NAs with a value (0)
m_wide_0 <- mammals %>%
  pivot_wider(names_from = taxon,
              values_from = density,
              values_fill = list(density = 0))

visdat::vis_dat(m_wide_0)

# what if oops! We had already pivoted and wanted to move forward
# Fill in those 0s without pivoting

mammals_0 <- mammals %>%
  complete(site, taxon,
           fill = list(density = 0))

# Pivoting long! ####
m_long_0 <- m_wide_0 %>%
  pivot_longer(cols = -site,
               names_to = "taxon",
               values_to = "density")

m_long_0

# use the cols we WANT
m_long_0 <- m_wide_0 %>%
  pivot_longer(cols = `Suncus etruscus`:`Notiosorex crawfordi`,
               names_to = "taxon",
               values_to = "density")

# why wide versis long?
ggplot(m_wide_0,
       aes(x = `Suncus etruscus`, y = `Notiosorex crawfordi`)) +
  geom_jitter()

ggplot(m_long_0,
       aes(x = taxon, y = density)) +
  geom_violin() +
  stat_summary(color = "red")


# Exercise
# Using palmer penguins, which is wide - make a long
# data frame so that we can see the average of all measurements
# split up by year, species, island, and sex (faceting, colors, etc.)
# get rid year if you want
library(palmerpenguins)
penguins
#pivot_longer(cols = -c(a, b, c, d), ...)

p_long <- penguins %>%
  pivot_longer(cols = bill_length_mm:body_mass_g,
               names_to = "measure",
               values_to = "value")

ggplot(data = p_long,
       mapping = aes(x = species,
                     y = value,
                     fill = sex)) +
  geom_boxplot() +
  facet_wrap(~measure, scales = "free_y")
