#'------------------------------------------------
#' @title: Galton Board
#' @source: https://raw.githubusercontent.com/EvaMaeRey/little_flipbooks_library/master/xaringan_reveal_parentheses_balanced.R
#'------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)

# initial params
n <- 70  # number of ball bearnings 
stop_level <- 10 # number of perturbation levels 
# make it an even number
levels <- 44 # greater than stop_levels


#init
set.seed(2020)
crossing(unit_id = 1:n,
         level = 1:levels - 1) %>%
  mutate(perturbation =  # moves
           sample(c(-1,1),  # left or right
                  n(),  # each ball at each level
                  replace = T)) %>%
  group_by(unit_id) %>%  # operations on each ball
  mutate(perturbation =
           ifelse(row_number() == 1,
                  yes = 0,  # start centered
                  no = perturbation)) %>%
  # each ball should release one at a time
  mutate(time =  # displacing them in time w/
           row_number() +
           # using unit id
           unit_id * 3 - 1) %>%
  filter(time > 0) %>%
  mutate(x_position =  # we get the x position
           # by summing the cumulative distributions
           cumsum(perturbation)) %>%
  # if ball is beyond the perturbation levels
  mutate(x_position =  # we overwrite the x position
           ifelse(level <= stop_level,
                  yes = x_position,
                  no = NA)) %>%
  # then fill in with the last x position
  fill(x_position) %>%
  ungroup() ->
  ball_bearings

# ball bearings
ball_bearings %>%
  filter(level == (levels - 1) ) %>%
  rename(final_time = time) %>%
  crossing(time = as.numeric(1:max(ball_bearings$time))) %>%
  group_by(time, x_position) %>%
  summarise(x_position_count = sum(time > final_time)) ->
  ball_bearings_collect

# pegs and walls
crossing(unit_id =
           -stop_level:stop_level,
         level = 1:stop_level) %>%
  mutate(transparent =
           (unit_id + level) %% 2) ->
  pegs
# Lets make walls
crossing(unit_id =
           -(stop_level + 1):(stop_level + 1),
         level = stop_level:levels) %>%
  mutate(transparent =
           unit_id %% 2) ->
  walls

#params
ball_bearings_size <- 2
peg_size <- 3


#the plot

ggplot(ball_bearings) +
  aes(y = level) +
  aes(x = x_position) +
  scale_y_reverse() +
  aes(group = unit_id) +
  geom_point(data = walls,
             aes(x = unit_id, alpha = transparent),
             col = "grey30", size = peg_size) +
  geom_point(data = pegs,
             aes(x = unit_id, alpha = transparent),
             col = "grey30", size = peg_size) +
  geom_segment(x = -sqrt(n), xend = -1.5,
               y = 0, yend = 0) +
  geom_segment(x = sqrt(n), xend = 1.5,
               y = 0, yend = 0) +
  geom_abline(intercept = 1.5,
              slope = -1) +
  geom_abline(intercept = 1.5,
              slope = 1) +
  annotate(geom = "tile",
           height = 2, width = 2,
           x = 0 , y = -1.5) +
  annotate(geom = "tile",
           height = 2, width = 1.75,
           x = 0 , y = -1.5, fill = "white") +
  geom_rect(data = ball_bearings_collect,
            mapping = aes(xmin = x_position - .35,
                          xmax = x_position + .35,
                          ymax = max(ball_bearings$level) + 1 - x_position_count*1.5,
                          ymin = max(ball_bearings$level) + 1,
                          group = x_position,
                          y = NULL, x = NULL),
            fill = "darkgrey") +
  geom_point(size = ball_bearings_size,
             col = "steelblue") +
  coord_equal() +
  geom_hline(yintercept = stop_level,
             linetype = "dotted") +
  scale_alpha_continuous(range = c(0, 1), guide = F) +
  theme_void() ->
  g

#the animation
g +
  gganimate::transition_time(time = time) +
  gganimate::shadow_wake(wake_length = .05) +
  gganimate::ease_aes("bounce-in-out")