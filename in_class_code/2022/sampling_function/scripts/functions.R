#' -------------------------------
#'  Functions
#'  @date 2022-09-27
#' -------------------------------

##### Libraries #####

##### Working dir and other options ####

##### load in helper code ####
source("scripts/helpers.R")


#### Our First Function ####

# we want a function that adds 1 to a number
add_one <- function(x) {
  ret_value <- x + 1
  
  return(ret_value)
}


add_one(1)
add_one(3)
add_one()

# other quick ways to write a function
add_one <- function(x){
  return(x+1)
}

add_one <- function(x) x+1

# max - min
max_minus_min <- function(x){
  
  ret_value <- max(x) - min(x)
  
  return(ret_value)
}

max_minus_min(c(4,7,1,6,8))

# to see what is happening inside of a function
debugonce(max_minus_min)
max_minus_min(c(4,7,1,6,8))

# What if we want a named object
c(hello = 3)

max_minus_min_detailed <- function(x){
  max_val = new_max(x)
  min_val = new_min(x)
  diff_val = max_val - min_val
  
  ret <- list(max_val = max_val,
              min_val = min_val,
              diff_val = diff_val)
  
  return(ret)
}

max_minus_min_detailed(c(3,4,5))

# write to a global variable
foo <- 1

clock_tick <- function() foo <<- foo + 1
clock_tick()
foo


#### Multiple arguments

# a function that adds THREE numbers together
# but, if any of them are missing, assume 0
add_three_numbers <- function(x = 0,
                              y = 0,
                              z = 0){
  return(x + y + z)
}

add_three_numbers()
add_three_numbers(3, 4, 5)
add_three_numbers(y = 3, z = 4, x = 5)


