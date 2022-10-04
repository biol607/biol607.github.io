#' --------------------------------
#' Helper functions
#' --------------------------------

# some max and min with no NA
new_max <- function(x) max(x, na.rm = TRUE)

new_min <- function(x) min(x, na.rm = TRUE)

# a function that uses length.out as it's 3rd qrgument
# to seq
seq_by_length <- function(from = 1, to = 1,
                          length.out = 1,
                          ...){
  ret <- seq(from = from, to = to, 
             length.out = length.out,
             ...)
  
  return(ret)
}
