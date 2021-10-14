## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Drydock
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## A drydock is a place where ships go to get repairs they cannot get at sea
  ## Similarly, this code does operations that the app will eventually do
  ## But in a context where the added complication of Shiny is avoided

# Clear environment
rm(list = ls())

# Make an object of our broadest working directory
getwd()
myWD <- getwd()

## ----------------------------------------------- ##
       # Reading in Multiple Excel Sheets ####
## ----------------------------------------------- ##
# The Shiny App will want to read in several Excel sheets
# BUT we *do not* want to read them in one-by-one
# So, we want to supply a vector and read in all of the sheets at once

# Required libraries
library(tidyverse); library(stringr); library(readxl); library(purrr)

# Read in the data
#siteData <- as.data.frame(
#  read_excel(path = "shiny-test.xlsx",
#             sheet = "siteData",
#             col_types = 'text')
#  )

# Check it out
#str(siteData)

# Okay, but how about a vector of desired sheets?
wants <- c("siteData", "reproData", "plantData")

# Iterate through each sheet name
wants %>%
  purrr::map(function(sheet){ 
    assign(x = sheet,
           value = as.data.frame(
             readxl::read_xlsx(path = "Test Data/shiny-test.xlsx",
                               sheet = sheet)),
           envir = .GlobalEnv)
    })




## ----------------------------------------------- ##
# Other Task...
## ----------------------------------------------- ##







# END ####

# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##
