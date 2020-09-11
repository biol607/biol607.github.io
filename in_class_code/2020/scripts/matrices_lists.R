#'-------------------------------------------------
#'
#' @title Matrices, lists, and more!
#' 
#' @author Jarrett Byrnes
#' @date 2020-09-11
#'-------------------------------------------------

my_vec <- 1:50

my_matrix <- matrix(my_vec, ncol = 10)
my_matrix

my_matrix[2,2]

#byrow
matrix(my_vec, ncol = 10, byrow = FALSE)
matrix(my_vec, ncol = 10, byrow = TRUE)

#let's look more at indices

#a_matrix[row, column]
my_matrix[2 , 2]
my_matrix[2 , ] #rows
my_matrix[ , 2] #columns

# explore the matrix!
str(my_matrix)
summary(my_matrix)
length(my_matrix)

#functions for dimensions
dim(my_matrix)
nrow(my_matrix)
ncol(my_matrix)
######


unif_vec <- runif(n = 100, min = 5, max = 50)

# Lists ####

my_list <- list(first = 1:10,
                second = letters[10:25])

my_list

#what's in it!
str(my_list)
names(my_list)

my_list$first
my_list[first] #doesn't work
my_list["first"]

class(my_list["first"])

#to get a vector back
my_list[["first"]]
class(my_list[["first"]])

# if you don't know names
my_list[1]
my_list[[1]]

#nested and mixed lists!

big_list <- list(first = 1:10,
                 second = NA,
                 third = list(a = letters[1:5],
                              b = list(one = 1,
                                       two = "2")))

big_list$first

#dig into the nest!
big_list$third$b$one
big_list[[3]][[2]][[2]]


# DATA FRAMES - what if lists and matrices had  baby! ####
View(my_matrix)
View(big_list)

data(mtcars)
str(mtcars)
View(mtcars)

#list-like
mtcars$mpg
names(mtcars)


#tabular
mtcars[1,5] #[row, col]
mtcars[1, "drat"]

mtcars[3:10, c("mpg", "wt")]

colMeans(mtcars)
summary(mtcars)
