#' -----------------------------------------------------------------
#' 
#' Loading in Data and Other Miscellany
#' 
#' @date 2022-09-20
#' -----------------------------------------------------------------

# What directory does R think it is in? ####
getwd()
#setwd("/Users/jebyrnes/Dropbox (Byrnes Lab)/Classes/biol_607_biostats/in_class_code/2022/r_potpourri")

# look around a bit more
list.files()

# Let's look HERE!
library(here)
here()

#this will set my working directory to where the Rproj file is
setwd(here::here()) # LOVE THIS LINE

#### Different Ways to Load Data ####
my_data <- read.csv("data/my_dataset_clean.csv",
                    stringsAsFactors = TRUE)

str(my_data)
my_data$column_3

# use something else to load data
library(readr)
my_data_readr <- read_csv("data/my_dataset_clean.csv")

my_data_readr

# if you have big data....
library(vroom)
my_data_vroom <- vroom("data/my_dataset_clean.csv", delim = ",")

# load straight from excel
library(readxl)
#library(googlesheets4)
my_data_excel <- read_excel("data/my_dataset.xlsx",
                            sheet = 1)

str(my_data_excel)
####