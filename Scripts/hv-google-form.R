## --------------------------------------------------------------------- ##
             # HerbVar - Mimicking a Google Form in R Shiny
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## Practice harvesting simple information from users

# Clear environment
rm(list = ls())

# Set working directory (should end in project directory)
getwd()
myWD <- getwd()

# Call any needed libraries
library(shiny); library(stringr)

## --------------------------------------------------------------------- ##
                  # Part 1: Define User Interface ####
## --------------------------------------------------------------------- ##
gg.ui <- fluidPage(

## ----------------------------------------------- ##
       # P1-1: GUI Header (Above Layout) ####
## ----------------------------------------------- ##
  # Add an app title
  tags$h2("HerbVar Data Submission Portal - Phase 2"),
    ## For more info on tags object & HTML shortcuts:
    ## https://shiny.rstudio.com/articles/tag-glossary.html
  
  # Add information about the purpose of this portal
  tags$h4("This Shiny App handles data submission 
          for the Herbivory Variability Network.
          More information can be found",
          tags$a(href = "http://herbvar.org/",
                 "on the Network's website.")),

  # Add a line separating this from the rest of the app
  tags$hr(),
  
  # Make the app have a side bar and main panel
  sidebarLayout(position = 'left',
                fluid = T,
                 
## ----------------------------------------------- ##
           # P1-2: Sidebar Contents ####
## ----------------------------------------------- ##
# Create the sidebar
sidebarPanel(

# Give the sidebar a global title
tags$h2("Pre-Data Submission Information"),
  
## ------------------------------ ##
  # P1-2-1: File Name Bits ####
## ------------------------------ ##
# Add a subtitle & more information
  h3("Survey Identifying Information"),
  tags$h5("To ensure that the data you are about to submit is 
          correctly named, please enter the following 
          survey-level information as you submit data."),

# Add a warning about underscores
  tags$h5("Please", tags$strong("DO NOT"),
          "use underscores ('_') in your entries"),
  
  # We need the following to uniquely name the file users submit
    ## PI name
  textInput(
    inputId = "pi_name",
    label = "Last Name of the PI",
    placeholder = "Darwin"
  ),
  
    ## Genus of survey species
  textInput(
    inputId = "genus",
    label = "Genus of Surveyed Plant",
    placeholder = "Plantago"
  ),
  
    ## Specific epithet of survey species
  textInput(
    inputId = "sp",
    label = "Specific Epithet of Surveyed Plant",
    placeholder = "major"
  ),
  
    ## Site name
  textInput(
    inputId = "site",
    label = "Site Name",
    placeholder = "Site 1"
  ),
  
  # Note on site name
  tags$h5("Note: site name will be capped at 8 characters automatically"),

## ------------------------------ ##
   # P1-2-2: Data Checkboxes ####
## ------------------------------ ##
# Prompt user to identify what data they collected
tags$h3("What tabs of the Excel file should be uploaded"),
tags$h5("You", tags$strong("must"),
        "use the template file provided ",
        tags$a(href = "http://herbvar.org/protocols.html",
               "here."),
        "Check all that apply"),

# Actual checkbox creation
checkboxGroupInput(
  inputId = 'data_collected',
  label = "Excel Tabs with Data",
  choices = c("siteData", "densityData", "plantData", "reproData",
              "herbivoreData", "newColumns", "notes"),
  selected = c("siteData"),
  inline = T
),

# More notes on the checkboxes
tags$h5("Note: failure to check a box ensures that the corresponding sheet",
        tags$strong("will not be uploaded")),

## ------------------------------ ##
   # P1-2-3: Reactive Button ####
## ------------------------------ ##
  ## Button does nothing (for now, will update)
actionButton(inputId = "check_button",
             label = "Pre-Submission Check")

## ----------------------------------------------- ##
          # P1-3: Main Panel Contents ####
## ----------------------------------------------- ##
# Create main panel (& close out sidebar)
), mainPanel(

## ------------------------------ ##
  # P1-3-1: File Name Preview ####
## ------------------------------ ##
  # Output file name from user inputs
  tags$h3("File Name Preview"),
  verbatimTextOutput(outputId = "fileID",
                     placeholder = T),
    ## And explain that is what it's doing
  tags$h5("Does the above look correct as a file name for your survey?",
          tags$strong("If not,"), 
          "edit your inputs to the left")

## ----------------------------------------------- ##
            # P1-4: Finish UI Part ####
## ----------------------------------------------- ##
# Close out formatting parentheses that wrap all of the above
  ## fluidPage(...sidebarLayout(...mainPanel(...
)))

## --------------------------------------------------------------------- ##
             # Part 2: Define Server (Internal Workings) ####
## --------------------------------------------------------------------- ##
# Create the internal mechanism(s) of the app
gg.server <- function(input, output, session) {

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

## --------------------------------------------------------------------- ##
                  # Part 3: Create Shiny App ####
## --------------------------------------------------------------------- ##
# This is the simplest bit because you just call your UI and Server

shinyApp(ui = gg.ui, server = gg.server)



# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

