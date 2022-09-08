#' -----------------------------------------
#' @title My First R File
#' 
#' This script is my first attempt at
#' coding in R.
#'
#' @date 2022-09-08
#'
#'
#' -----------------------------------------

#### Here is the first code block ####

#### Here is my second code block ####

#### Arithmetic ####
3
3 + 4
4 * 5

#### More complex mathematical functions ####
sqrt(4)
ln(2)
log(5)
log(exp(1))

log10(10)
log(x = 10, base = 10)
log(x = 10, base = exp(1))

#### Variables ####

# This is what you DON'T want
sqrt(3 + 4 + 5) * 2 +10 /3

# You want to work with calculated values
pi

# make foo equal the square root of 2
foo <- sqrt(2)
sqrt(2) -> foo
#foo = sqrt(2) #ugh

# square root of 2 + 5
foo + 5

#### More than a number ####
class(foo)

"hello"
class("hello")
bar <- "Hello. My name is Jarrett."
class(bar)
bar
bar + 5

# Boolean!
TRUE
FALSE
class(TRUE)

3 < 4
3 == 4

TRUE + 5
FALSE + 10

TRUE + TRUE + TRUE + FALSE

# integers
class(1)
class(1L)

# What about NOTHING!
NA # missing value
NA + 5
NaN

4/0
0/0
-4/0

class(NA)
class(NaN)
class(Inf)


# the IS functions
biz <- NA + 5
is.na(biz)
is.nan(biz)
is.finite(biz)
