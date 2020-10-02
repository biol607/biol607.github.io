#'-------------------------------
#'
#' @title Loading Data
#' 
#' @date 2020-10-02
#' -------------------------------
#here for good practice ####
setwd(here::here())

# Navigating our filesystem with R! ####

# where does R think it is looking for files on my computer?
getwd()

# what files can R see right now?
list.files()

# ok, what is in data?
list.files("data")

# install.packages("here")
# http://here.r-lib.org
library(here)
here()

# one last neat directory trick
# sometimes you will see ./ or ../
# ./ means relative to the current working directory
list.files("./data")

# ../ go up one directory
list.files("../")
list.files("../2020/")

# READING IN A FILE!!!! ####
# old school file reading
sheet1 <- read.csv("data/test_data_sheet_1.csv")

# what to do once you read in a file
names(sheet1)
str(sheet1)

# some people have NAs as . or NA or -
sheet1 <- read.csv("data/test_data_sheet_1.csv",
                   na.strings = c("NA", ".", "-"))
sheet1[2,2] <- NA
summary(sheet1)

# Looking at our data at scale
library(visdat)
vis_dat(sheet1)

# looking for missing data
library(naniar)
miss_var_summary(sheet1)

# skimming your data
library(skimr)
skim(sheet1)


# Loading data the new school way
library(readr)
sheet2 <- read_csv("data/test_data_sheet_2.csv",
                   col_types = "ddc")

sheet2$`Group ID`


# To clean names
library(janitor)
sheet2  <- sheet2 %>% clean_names()

# Loading from excel or other things ####
library(readxl)

sheet1_xl <- read_excel("data/test_data.xlsx",
                        sheet = "Sheet1") %>%
  janitor::clean_names()

sheet1_xl

# for googlesheets use googlesheets4
# if you have YUGE data! Look at vroom::vroom() or data.table::fread()


