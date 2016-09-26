library(readr)
library(dplyr)
library(ggplot2)

#Load the data
hadcrut_temp_anomaly <- read_csv("./data/04/hadcrut_temp_anomoly_1850_2015.csv")

head(hadcrut_temp_anomaly)
str(hadcrut_temp_anomaly)

hadcrut_temp_anomaly <- hadcrut_temp_anomaly %>%
  mutate(month_name = factor(month_name))

#install.packages("forcats")
library(forcats)
hadcrut_temp_anomaly<- hadcrut_temp_anomaly %>%
  mutate(month_name = fct_inorder(month_name))

hadcrut_temp_anomaly$month_name


#COMMENCE PLOTTING!!!!
had_base_plot <- ggplot(data = hadcrut_temp_anomaly,
                        mapping = aes(x = month_name, y = anomaly))

had_base_plot
### Geoms
had_base_plot +
  geom_point(color = "blue", alpha = 0.3, size=4)

had_base_plot + 
  geom_boxplot() +
  geom_jitter(width = 0.3, height = 0)

#summaries

had_base_plot + 
  geom_jitter(width = 0.3, height = 0) +
  stat_summary(color="red", size=1.4)


#Lines
had_lines <- had_base_plot +
  geom_line(mapping = aes(group = year))

had_lines

#2.6 Exercises

#Using geom_histogram, what does the distribtion of anomolies
#look like.
ggplot(data = hadcrut_temp_anomaly) +
  aes(x = anomaly) +
  geom_histogram()

#Plot y = anomoly by x = year. First, plot the points.

#Last, try a line plot, using group = month.
had_year_lines <- ggplot(data=hadcrut_temp_anomaly) +
  aes(x = year, y = anomaly, group = month) +
  geom_line()


#### COLOR

had_lines  <-  had_base_plot +
  geom_line(mapping = aes(group = year, color = year))

had_lines +
  scale_color_continuous(low = "blue", high = "red")

had_lines +
  scale_color_gradient2(low = "blue", mid = "yellow",
                        high = "red", midpoint = 1925)


had_lines +
  scale_color_gradientn(colors = rainbow(7))

library(RColorBrewer)
display.brewer.all(n=10, exact.n = FALSE)

had_lines +
  scale_color_gradientn(colors = brewer.pal(7, "BrBG"))

install.packages("viridis")
library(viridis)

had_lines +
  scale_color_viridis()

#What happens to our histogram if we add a fill argument. 
#Try fill = factor(year) (note that without the factor, 
#nothing happens!).

ggplot(data = hadcrut_temp_anomaly, mapping = aes(x = anomaly, fill = factor(year))) +
  geom_histogram() +
  guides(fill = FALSE)

#Returning to our line plot of year by anomoly, 
#try coloring also by anomoly. First use the default palatte, 
#and then try a blue-white-red.
ggplot(hadcrut_temp_anomaly) +
  aes(x = year, y = anomaly, group = month,
      color = anomaly) +
  geom_line() +
  scale_color_gradient2(low="blue", high = "red", 
                        mid = "white", midpoint = 0)


#Now color by month. Try at least two different palettes
# either of your own devising or from an additional package.#
#Note, if you use month_name, you’ll be mucking about with 
#scale_color_manual. RColorBrewer handles discrete groups 
#easily.

had_years <- ggplot(hadcrut_temp_anomaly) +
  aes(x = year, y = anomaly, group = month,
      color = month_name) +
  geom_line() 


###Facets
had_years +
  facet_wrap(~month_name)

had_years +
  facet_grid(. ~ month_name)

had_years +
  facet_grid(month_name ~ .)

#Facet by decade
hadcrut_temp_anomaly <- hadcrut_temp_anomaly %>%
  mutate(decade = floor(year/10)*10)

ggplot(hadcrut_temp_anomaly) +
  aes(x = year, y = anomaly, group = month,
      color = month_name) +
  geom_line() + facet_wrap(~decade)



#stat_smooth for trends

had_years +
  facet_wrap(~month_name) +
  stat_smooth(color="black")


had_years +
  facet_wrap(~month_name) +
  stat_smooth(method="lm", color="black")


##########
#install.packages("ggthemes")
library(ggthemes)

had_lines +
  scale_color_viridis() +
  theme_void(base_size = 14)

?theme
had_lines_color <- had_lines +
  scale_color_viridis() 

had_lines_color +
  theme_excel()


had_lines_color +
  theme_fivethirtyeight()


had_lines_color +
  theme_tufte()

had_lines_color_theme <- 
  had_lines_color +
  theme_bw()

had_lines_color_theme

### Axes
had_lines_color_theme  +
  scale_y_log10()

had_lines_color_theme +
  coord_polar()


had_lines_color_theme +
  ylim(c(-0.5, 0.5))

had_lines_color_theme +
  scale_x_discrete(expand = c(0,0)) +
  coord_polar()




########## Where did we start?
head(hadcrut_temp_anomaly)

ggplot(data = hadcrut_temp_anomaly) +
  aes(x = month_name, y = anomaly, 
      color = year, group = year) +
  geom_line() +
  scale_color_continuous(low = "brown", high = "green")