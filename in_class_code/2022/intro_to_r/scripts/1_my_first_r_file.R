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


#### Vectors ####

my_numbers <- c(5, 6, 7, 8)

# get me the third element of my_numbers

my_numbers[3]

# a sequence of numbers from 1 to 100
my_seq <- 1:100
my_seq


c(1:100, 50)

# sequence of numbers from 1 to 100, step by .5
seq(from = 1, to = 100, by = 0.5)

my_numbers + 5

# vectors of random numbers are common
runif(100, min = 0, max = 1)

sum(c(3,4,5))
mean(c(3,4,5))

#### Digging into more complex objects ####
class(my_numbers)
class(c("hello", "goodbye"))
class(c(TRUE, FALSE))
class(c(3, 4, "5"))

combo_vec <- c(my_numbers, my_seq)

#### Our hero str ####
# (ok, and summary, too) #

str(combo_vec)
summary(combo_vec)

vec_with_na <- c(1:100, 3,4,NA, 200:1000)
str(vec_with_na)
sum(vec_with_na)
summary(vec_with_na)


# here's how to ignore NAs
sum(vec_with_na, na.rm = TRUE)


#### Matrix ####
my_matrix <- matrix(data = 1:50,
                    nrow = 10)

my_matrix

# look at indices
my_matrix[2,2]

# what is the index of the number 16?
which(my_matrix == 16, arr.ind = TRUE)

# what if I want a whole row?
my_matrix[4,]

# what if I want a whole column?
my_matrix[,4]

#### Get more info about this matrix ####
str(my_matrix)
summary(my_matrix)
nrow(my_matrix)
ncol(my_matrix)
dim(my_matrix)

rowMeans(my_matrix)

#### numerics and characters ####
my_vec <- c(1, 2, "hello")
my_vec
as.numeric(my_vec)

# hunt for bad values
my_bad_vec <- c("3", "5", "6,")
as.numeric(my_bad_vec)
which(is.na(as.numeric(my_bad_vec)), 
      arr.ind=TRUE)

#### Lists ####

my_list <- list(First = 1:5,
                Second = letters[1:5])

my_list
class(my_list)
str(my_list)
summary(my_list)

# reference objects in a list (or subset)
my_list$First
my_list[1]

# weirdness ahoy!
class(my_list$First)
class(my_list[1])

my_list[[1]]
class(my_list[[1]])


my_list[["First"]]


nested_list <- list(a = list(
  First = 1:10,
  Second = "hello"
))

nested_list$a$Second
nested_list[["a"]][["Second"]]


# other list tools
names(my_list)
names(nested_list)
my_list[c("First", "Second")]

my_names <- names(my_list)
my_list[my_names]


#### What if a list and matrix had a baby? ####
# A DATA FRAME #

# load up some data on cars
data(mtcars)
head(mtcars)
str(mtcars)
dim(mtcars)
names(mtcars)

# subset
mtcars$cyl
mtcars[,2]
mtcars[1:5, c("qsec", "vs")]

###### morley
data(morley)
