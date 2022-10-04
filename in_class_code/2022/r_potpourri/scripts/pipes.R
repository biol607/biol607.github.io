#' ---------------------------------------------
#' 
#' Pipes
#' 
#' ---------------------------------------------

# sum 1 through 10 and turn
# into a character
sum(1:10)

# nesting
as.character( sum(1:10))


as.character(
  sum(1:10)
)

# take sum of 1:10
# turn into a character
# paste the phrase "this is the answer"
# print
# Style of a language called LISP
print(
  paste("This is the answer",
        as.character(
          sum(1:10)
        ))
)

#Lots of objects
my_answer <- sum(1:10)
my_answer <- as.character(my_answer)
my_answer <- paste("This is the answer", my_answer)
print(my_answer)


# Enter THE PIPE
# take an output and pipe it as the first argument to the
# next function

sum(1:10) |> #cmd-shift-m
  as.character() |>
  paste("is the answer") |>
  print()

#### If you don't know how to code something write it out! ####
# Print out the phrase that is the sum of 1:10 and "this is the answer"

# take sum of 1:10
sum(1:10) |>
  # turn into a character
  as.character() |>
  # add the phrase "this is the answer"
  paste("this is the answer") |>
  # print
  print()


# take sum of 1:10
sum(1:10) |>
  # turn into a character
  as.character() |>
  # add the phrase "this is the answer"
  {\(x) paste("this is the answer", x)}() |>
  # print
  print()

# this is the pipe from magrittr
library(magrittr)
sum(1:10) %>%
  as.character()

# use the magrittr pipe to make 55 the 2nd argument
sum(1:10) %>%
  as.character() %>%
  paste("this is the answer:", .)

# the native pipe requires a named argument
# AND uses _
sum(1:10) |>
  as.character() |>
  paste("this is the answer", a = _) # we can name it anything

# Let's sum 1:10
# and then get 100 random numbers
# with a mean of sum(1:10) - using a normal dist

sum(1:10) |>
  rnorm(n = 100, mean = _)

