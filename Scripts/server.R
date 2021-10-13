## --------------------------------------------------------------------- ##
                 # HerbVar Data Submission Portal - Server
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## Practice harvesting simple information from users

# Call any needed libraries
library(shiny); library(stringr)

## --------------------------------------------------------------------- ##
             # Part 2: Define Server (Internal Workings) ####
## --------------------------------------------------------------------- ##
# Create the internal mechanism(s) of the app
server <- function(input, output, session) {

## ----------------------------------------------- ##
         # P2-1: File Name Output ####  
## ----------------------------------------------- ##
# Render the filename from the supplied information in the UI
  output$fileID <- renderPrint({
    paste(
      input$pi_name,
      input$genus,
      input$sp,
      str_sub(input$site,
              start = 1, end = 8),
      input$date,
      sep = '_')
  })
  
## ----------------------------------------------- ##
      # P2-2: Reactive Button 1 Response ####  
## ----------------------------------------------- ##
#  observeEvent(input$check_button, {})
  

  
## ----------------------------------------------- ##
          # P2-3: Finish Server Part ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

