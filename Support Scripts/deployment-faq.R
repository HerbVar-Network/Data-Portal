## --------------------------------------------------------------------- ##
            # HerbVar Data Submission Portal - Deployment FAQ
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## Deploying an app uses the rsconnect library which has its own host of functions
  ## Easier to write them down rather than memorizing them
  ## See here for more info: shiny.rstudio.com/articles/shinyapps.html

# Load library
library(rsconnect)

## ----------------------------------------------- ##
              # (Re-)Deploy App ####
## ----------------------------------------------- ##
# Make the changes to "app.R" then run this line
rsconnect::deployApp('Data Portal Actual')

# NOTE
  ## Running that line asks you if you want to update the app "[Y/n]"
  ## You have to type "Y" in the console and hit enter to proceed

## ----------------------------------------------- ##
                  # Check Log ####
## ----------------------------------------------- ##
# Run this while an instance is up to get a running log of any issues
rsconnect::showLogs(appPath = 'Data Portal Actual/',
                    streaming = T)

## ----------------------------------------------- ##
           # Changing Configuration ####
## ----------------------------------------------- ##
# If the app needs to be changed to larger that can be done
#rsconnect::configureApp(appDir = 'Data Portal Actual/',
#                        size = 'medium',
#                        redeploy = T)
  ## This feels dangerous though so it's commented out




# END ####
