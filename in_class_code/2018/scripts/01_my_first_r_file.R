#---------------------
#' my_first_r_file.R
#'
#' This is my first R script
#' There are lots of commands
#' 
#' @author Jarrett Byrnes
#' 2018-09-07
#'
#-----------------------

#-------
#Arithmetic ####
#-------
3 + 4
3 * 4
3 / 4

12 / 3

a + b
pi
c

#-------
# more complex math ####
#-------

log(10)
log(10, base = 10)
log(x = 10, base = 10)

#--------
# variables ####
#--------

pi

a <- 3
a  
#3 -> a
#a = 3
b <- 4

a + b 

# Name our variables
diameter_of_a_sea_urchin <- 3
diameterOfASeaUrchin <- 3
#diameter.of.a.sea.urchin <- 3
#diameter-of-a-sea-urchin <- 3

urchin_dia <- 3
urchin_dia <- log(10)

#overwriting
value <- 1
value <- value + 1
value

#----
# more than a number
#----

#numerics
3
class(3)
str(3)

#characters
"hello"
class("hello")
str("hello")

#boolean
TRUE
class(TRUE)
3 == 3
3 < 3
TRUE + TRUE + FALSE

#Oops classes!
NA
1 + NA

NULL

Inf
-Inf
NaN


# ------------
# Vectors ####
# ------------

my_vec <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
my_vec

my_vec[5]


my_vec[3] + my_vec[5]

my_vec + 1
log(my_vec)
sum(my_vec)
mean(my_vec)

#create vectors
1:5
seq(from = 1, to = 5, by = 0.5)
seq(from = 1, to = 5, length.out = 100)
my_rand <- runif(n = 100, min = 0, max = 100)

class(my_rand)
str(my_rand)
summary(my_rand)


vec <- c(1,2,3,NA,5)
str(vec)
summary(vec)

c(T,T,T,F)
c("hello", "goodbye")  
c(3, "goodbye")  

# -----------
# Matrices ####
# -----------

my_matrix <- matrix(1:50,
                    nrow = 10,
                    byrow=TRUE)

my_matrix

my_matrix[3,4]
my_matrix[3,]
my_matrix[,3]

my_matrix[1:3, 4:5]
my_matrix[1:3, c(5,4)]

# matrix information
class(my_matrix)
str(my_matrix)
summary(my_matrix)

nrow(my_matrix)
ncol(my_matrix)
dim(my_matrix)


#makes a warning!
a_seq <- seq(0,81,by = 3)
a_mat <- matrix(a_seq[-28], nrow=9)

#what went wrong
length(a_seq)

# ------
# lists ####
# ------


my_list <- list(first = 1:10,
                second = letters[1:5])

my_list
str(my_list)
summary(my_list)


my_list[1]
my_list$first
class(my_list[1])
class(my_list$first)


# double brackets
my_list[["first"]]

a <- "first"
my_list[[a]]

names(my_list)


# ----------
# data frames ####
# ----------

data(mtcars)
View(mtcars)

str(mtcars)
summary(mtcars)

#subset
mtcars[1,3]
mtcars$mpg

#Try out names() mean() and colMeans() on mtcars
