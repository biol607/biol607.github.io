#'-------------------------------------------
#'
#' @title: My first R script for Biol 607
#' @author: Jarrett Byrnes
#'
#' @date: 2009-09-10
#'
#' @Changelog:
#'
#'-------------------------------------------




# Three mathematical things to show ####
# that R works as a calculator

1

1 + 2

1 * 2 + (3*4)


# next thing?

3/4
10 ^ 2
-1 + 1

# Arithmetic is cool - but what about complex functions? ####
sqrt(4)
log10(10)
log(5)

?log

log(x = 10, base = 10) #this is the most careful
log(10, base = 10) #this is more common usage
log(10, 10) #what does it mean?

??"logarithm"

# We have variables! ####
pi
exp(1)

foo <- sqrt(2)
sqrt(2) -> foo #not generally done
foo = sqrt(2) #this works, but, there are reasons not to

# why no = ?
# 1. History: <- 
# 2. Unambiguity: <- gives us a sense of what is being
#  assigned to what
# 3. Understand when we are using functions
#    = is reserved for use in specifying function args
# can use option/alt and - for an arrow


foo
foo^2
foo + 2
log(foo)

# variable naming!
# 1. be meaningful
sqrt2 <- sqrt(2)
thesquarerrotoftwo <- sqrt(2)

# be short but meaningful!
# not start with a number
# if you use multiple words, use either snake or 
#  camel case?
# BE CONSISTENT
square_root_of_two <- sqrt(2) #snake case
square.root.of.two <- sqrt(2) # . has a use in R
#plot.lm and plot.xy are both functions
squareRootOfTwo <- sqrt(2) #camel case

# more than a number ####
# classes of objects
class(square_root_of_two)

#character
text_string <- "this is text"
class(text_string)

#booleans
TRUE
FALSE
true
class(TRUE)

TRUE + 0
square_root_of_two == 1 # equality is a ==
square_root_of_two < 1
square_root_of_two <= 1
square_root_of_two >= 1


# what about missing values?
NA
class(NA)
NA + 0

#related special variables
NaN #this is not missing, it's just not a number
3/0
Inf
-Inf

class(NaN)
class(Inf)
Inf + 1

#EXERCISE: Make a variable. Now make a variable out of 
# some math equation. Try adding variables of 
# different classes together - what happens?


NA + foo
foo + "foo"
foo + TRUE
foo + NaN

#there are other other classes!
1L
class(1L)


# Vectors! ####

my_vec <- c(1,1,2,3,5,8)
my_vec

class(my_vec)

class(c("a", "b", "c"))

#a useful character vector
letters
LETTERS


# what is the 12th letter of the alphabet?
letters[12]

# a range of values
# use :
1:10

# the first 10 letters of the alphabet
letters[1:10]

# the letters of the alphabet corresponding to my_vec
letters[my_vec]

# EXERCISE: Make two vectors and add them together. 
# First try it with numbers. Then try vectors of 
# different object types. What happens?

c(1, 2, 3, 4, "a")

# vectors are great, and there are many functions that we use
# to make them!

1:10

seq(from = 1, to = 10, by = .1)
seq(from = 1, to = 10, length.out = 100)

#random numbers
my_unif <- runif(n = 100, min = 13.5, max = 200)

#many functions work on vectors
sum(my_unif)
mean(my_unif)


# str and summarize - what is in that object! ####

str(my_unif) # functions in one!

class(my_unif)
length(my_unif)
head(my_unif)

#sometimes problems are not obvious
na_vec <- c(1:100, NA, 10:100)

str(na_vec)

summary(na_vec)


# The difference between [] ()
# parentheses are for functions!
# [] are for things we can subset

letters[c(1:5)]

letters(c[1:5])

letters()
log()

c[1:5]
log[1:5]

letters[1:5]

## [[]] {} - that's for the future!

