#make my data frame
my_data <- read.csv("./data/03/my_data.csv")


#what's here?

str(my_data)

#factors
my_data$Column_2

my_data_chr <- read.csv("./data/03/my_data.csv", 
                    stringsAsFactors=FALSE)

#let's go with readr
#install.packages("readr")
library(readr)
my_data_readr <- read_csv("./data/03/my_data.csv")

my_data_readr
str(my_data_readr)


#install.packages("readxl)
library(readxl)

my_data_excel <- read_excel("./data/03/my_data.xlsx")
