###############################
# This is our first in class
# script file
#
# Created on 9/98/16
# By: Jarrett Byrnes
###############################

#I am doing some math here
3 + 4 #this is 3+4
3 - 7
log(10, base=10)

# add 3 and 4
3 + 4

# subtract 7 from 10
10 - 7

#take log of 2
log(2)


#variables
pi
#functions are objects, too
log(10)
log

#defining a variable
cat <- 2
2 -> cat #use sparingly
cat = 2 #don't use 


#use a variable 
cat+2


#save output into a variable
dog <- sqrt(2)
dog


#xample
biz <- 3 + 4
biz+1

#stylistic aside
a <- 5
#descriptive variable names
value_of_pi <- 3.141593
#value.of.pi - generally not good
#valueOfPi - hard to read



#Different kinds of objects in R
"hello"
hello <- 'hello'

# a common problem
num <- "3"

TRUE
T
FALSE
F
TRUE + 5

cat == 2
cat == dog
cat < dog
cat > dog
cat <= dog

# ! = NOT!
cat != dog
!TRUE

#is
is.numeric(3)
is.numeric("3")

#Missing values
NA

NA + 1
is.na(NA)
is.na(2)

as.numeric("Tree")


NULL
NULL +1

5/0
-5/0

is.finite(Inf)


#############
col_1 <- c(1, 2, 3, 4, 5)
1:5



#look at the 3rd value of col_1
col_1[3]

col_1[2]

col_1[c(1,3)]

#do some addition
col_1[c(1,3)] + 1

col_1

#modify a cell
col_1[5] <- 7

big_vec <- 1:1000
big_vec

#random vector
random_vec <- runif(100, 
                    min=0, 
                    max=100)
random_vec


#remove something
col_1[-3]
col_1a <- col_1[-3]
col_1a

#work with a vector
sum(col_1)
mean(col_1)
#hat's in a vector
summary(col_1)
summary(c("cat", "dog"))

#find out what is in an object
str(col_1)
str(c("cat", "dog"))
str(c(cat, dog))


########### Matricies
my_matrix <- matrix(1:50, nrow=10)
my_matrix

#I want to see the first value
my_matrix[1,1]

#second value in the fourth col
my_matrix[2,4]

#third row
my_matrix[3,]

#properties of a matrix
str(my_matrix)
nrow(my_matrix)
ncol(my_matrix)
dim(my_matrix)

#what to do with a matrix
sum(my_matrix)
rowSums(my_matrix)

#########Lists
my_list <- list(
  col_1 = 1:5,
  col_2 = 11:15
)

my_list
str(my_list)
names(my_list)


#access individual pieces
my_list$col_1
my_list[["col_1"]]

my_key <- "col_2"
my_list[[my_key]]

#listscan contain a lot of things
weird_list <- list(
  mat_1 = my_matrix,
  str_1 = "cat",
  vec_1 = 1:10
)

str(weird_list)

############# Data Frame
my_df <- data.frame(
  col_1 = 1:5,
  col_2 = 11:15
)


my_df
View(my_df)
str(my_df)

#columns are list-like
my_df$col_1
my_df[["col_2"]]

#or they are matrix-like
my_df[,1]
my_df[2,]


#more information about our data
summary(my_df)
