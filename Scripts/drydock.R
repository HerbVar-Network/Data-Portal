## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Drydock
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## A drydock is a place where ships go to get repairs they cannot get at sea
  ## Similarly, this code does operations that the app will eventually do
  ## But in a context where the added complication of Shiny is avoided

# Required libraries
library(tidyverse); library(stringr); library(readxl)
library(purrr); library(stringr)
library(googlesheets4); library(googledrive)

# Clear environment
rm(list = setdiff(ls(), c('myWD')))

# Make an object of our broadest working directory
  ## Should end in "/HerbVar-Data-Portal"
getwd()
#myWD <- getwd()

# Set the working directory to a testing ground folder
setwd(paste0(myWD, "/Test Data"))
getwd()

## ----------------------------------------------- ##
  # Splitting a Vector w/o Special Characters ####
## ----------------------------------------------- ##
# Make a bad vector
badVec <- c("siteData   reproData plantData")
badVec

# Try to split it with str_split
str_split(string = badVec, pattern = "\\s+")

# Make that not a list?
as.data.frame(x = str_split(string = badVec, pattern = "\\s+"))

# Call that as an object
hacked <- as.data.frame(str_split(string = badVec, pattern = "\\s+"))
names(hacked) <- "elements"
hacked

# What if we make it a matrix first?
as.data.frame(as.matrix(str_split(string = badVec, pattern = "\\s+")))
as.data.frame(str_split(string = badVec, pattern = "\\s+"))

# What does a for loop on the dataframe version do?
for (i in 1:nrow(hacked)){
  print(hacked[i, ])
}

## ----------------------------------------------- ##
       # Reading in Multiple Excel Sheets ####
## ----------------------------------------------- ##
# The Shiny App will want to read in several Excel sheets
# BUT we *do not* want to read them in one-by-one
# So, we want to supply a vector and read in all of the sheets at once

# Read in the data
#siteData <- as.data.frame(
#  read_excel(path = "Shiny Test Data.xlsx",
#             sheet = "siteData",
#             col_types = 'text')
#  )

# Check it out
#str(siteData)

# Okay, but how about a vector of desired sheets?
wants <- c("siteData", "reproData", "plantData")

# Iterate through each sheet name

# Option 1: Automatically bring them in as their own objects
#wants %>%
#  purrr::map(function(sheet){ 
#    assign(x = sheet,
#           value = as.data.frame(
#             readxl::read_xlsx(path = "Test Data/Shiny Test Data.xlsx",
#                               sheet = sheet)),
#           envir = .GlobalEnv)
#    })

# Option 2: List the elements using purrr
file_list <- wants %>%
  purrr::map(.f = function(sheet){ 
  assign(x = sheet,
         value = as.data.frame(
           readxl::read_xlsx(path = "Shiny Test Data.xlsx",
                             sheet = sheet)))
  }) %>%
  setNames(wants)

# Look at what that yields
str(file_list)

# Option 3: List elements using a for loop
  ## Make empty list
data_list <- list()

  ## For loop to import data
for (i in 1:length(wants)) {
    # Import
  data_list[[i]] <- as.data.frame(
    readxl::read_xlsx(path = "Shiny Test Data.xlsx",
                      sheet = wants[i]))
  
  # Name element after contents
  names(data_list)[i] <- wants[i]
}

  ## Look at what we're left with
str(data_list)

## ----------------------------------------------- ##
    # For Loops & Operations within a List ####
## ----------------------------------------------- ##
# For each element of a list, we want to do some operation
for (item in data_list) {
  print(item)
  print('end of loop')
}

# Return the name for each element in the list
for (name in names(data_list)) {
  print(name)
}

# Is "name" interchangeable with "item"?
  ## No
for (item in names(data_list)) {
  print(name)
}

# What if we want to save each item in the list by its name?
  ## This works but is commented out to not needlessly save files
#for (i in 1:length(data_list)) {
#  write.csv(x = data_list[[i]],
#            file = paste0(names(file_list)[i], ".csv"),
#            row.names = F)
#}

## ----------------------------------------------- ##
    # Adding Rows to Existing GoogleSheets ####
## ----------------------------------------------- ##
# Pre-emptively solve an issue with an HTTP2 error
httr::set_config(httr::config(http_version = 0))

# Authorize GoogleDrive and GoogleSheets
googledrive::drive_auth(email = "herbvar@gmail.com")
gs4_auth(email = "herbvar@gmail.com")

# Identify a googlesheet I made to test this exact thing
TestFileID <- drive_get(path = "Test Data - Phase 2", corpus = 'allDrives')

# Add a row to that datasheet
  ## Without specifying columns
sheet_append(ss = TestFileID$id,
             data = data.frame(c("x", "y", "z")),
             sheet = "Completed Surveys")

# What if we created a dataframe with correctly named columns
  ## This works but is commented out so it doesn't run more than needed
#gg_test_df = data.frame("fileName" = 'x',
#                        "PI_lastName"	= 'x',
#                        "PI_firstName" = 'x',
#                        "plantGenus" = 'x',
#                        "plantSpecies" = 'x')
#gg_test_df

# And get a GoogleSheet with those columns
CSfileID <- drive_get(path = "Phase 2 Completed Surveys - Primary",
                      corpus = 'allDrives')

# Add a row to that datasheet
  ## Without specifying columns
  ## This works but is commented out so it doesn't run more than needed
#sheet_append(ss = CSfileID$id,
#             data = gg_test_df,
#             sheet = "completedSurveys")

## ----------------------------------------------- ##
        # Preliminary QA/QC per Sheet ####
## ----------------------------------------------- ##
# Each sheet will get preliminary QA/QC so need to handle that separately
# The 'wishlist' of errors to check is provided under each sub-heading

# Read in all sheets
  ## This way auto-names each one by its sheet name
c('siteData', 'densityData', 'plantData', 'reproData',
  'herbivoreData', 'newColumns', 'notes') %>%
  purrr::map(function(sheet){ 
    assign(x = sheet,
           value = as.data.frame(
             readxl::read_xlsx(path = "Nick HV Fake Data.xlsx",
                               sheet = sheet)),
           envir = .GlobalEnv)
    })

## ------------------------------ ##
   # QA/QC: siteData Checks ####
## ------------------------------ ##
# Error wishlist:
  ## 1) If "datum" is empty for any row, return the "variable" that matches

# Examine data
head(siteData)

#siteData %>%
#  dplyr::mutate(
#    Warning = ifelse(is.na(siteData$datum)[1:19],
#                     yes = paste0("Bad. You're missing ",
#                                  siteData$variable[is.na(siteData$datum)[1:19]]),
#                     no = "Looks good!")
#  ) %>%
#  as.data.frame()

rbind(
data.frame("Errors" =
  ifelse(is.na(siteData$datum)[1:19],
                                yes = paste0("No entry detected for '",
                                             siteData$variable,
                                             "'. Please enter that value and re-attach data"),
                                no = NA)),
data.frame("Errors" = 'test')
) %>%
  dplyr::filter(!is.na(Errors))


## ------------------------------ ##
  # QA/QC: densityData Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(densityData)


## ------------------------------ ##
# QA/QC: plantData Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(plantData)


## ------------------------------ ##
# QA/QC: reproData Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(reproData)



## ------------------------------ ##
# QA/QC: herbivoreData Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(herbivoreData)



## ------------------------------ ##
# QA/QC: newColumns Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(newColumns)



## ------------------------------ ##
# QA/QC: notes Checks ####
## ------------------------------ ##
# Error wishlist:
## 1) 

# Examine data
head(notes)



# END ####

# Template heading lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##
