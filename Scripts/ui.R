## --------------------------------------------------------------------- ##
           # HerbVar Data Submission Portal - User Interface (UI)
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## Practice harvesting simple information from users

# Call any needed libraries
library(shiny); library(stringr); library(readxl)

# Define user interface object
ui <- fluidPage(

## ----------------------------------------------- ##
            # Header (Above Layout) ####
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
               # Sidebar Contents ####
## ----------------------------------------------- ##
# Create the sidebar
sidebarPanel(

# Give the sidebar a global title
tags$h2("Pre-Data Submission Information"),

## ------------------------------ ##
      # Data Checkboxes ####
## ------------------------------ ##
# Prompt user to identify what data they collected
tags$h3("What tabs of the Excel file should be uploaded?"),
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

# Add a line separating this from the rest of the app
tags$hr(),

## ------------------------------ ##
   # Gather File Name Parts ####
## ------------------------------ ##
# Add a subtitle & more information
  h3("Survey Identifying Information"),
  tags$h5("To ensure that the data you are about to submit is 
          correctly named, please enter the following 
          survey-level information as you submit data."),

# Add a warning about underscores
  tags$h5("Please", tags$strong("DO NOT"),
          "use underscores ('_') in your entries"),
  
  # We need the following to uniquely name the file users submit:
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

    ## Sampling date
  ### Gets around possible issue of Name_Species_Site not being unique / survey
  dateInput(inputId = "date",
            label = "Sampling Date (if sampling took >1 day, enter first day)",
            min = "2000-01-01",
            format = 'yyyy-mm-dd'),
  
  # Note on site name
  tags$h5("Note: site name will be capped at 8 characters automatically")

## ----------------------------------------------- ##
            # Main Panel Contents ####
## ----------------------------------------------- ##
# Create main panel (& close out sidebar)
), mainPanel(

## ------------------------------ ##
     # File Name Preview ####
## ------------------------------ ##

# Give it a title
  tags$h3("File Name Preview"),

# Output file name from user inputs
  verbatimTextOutput(outputId = "fileID",
                     placeholder = F),

# And explain that is what it's doing
  tags$h5("Does the above look correct as a file name for your survey?",
          tags$strong("If not,"), 
          "edit your inputs to the left"),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
          # File Input ####
## ------------------------------ ##
# Provide a place for Excel file uploading
fileInput(inputId = "file_upload",
          label = tags$h3("Attach Excel File Here"),
          accept = ".xlsx"),

## ------------------------------ ##
       # Reactive Button ####
## ------------------------------ ##
## Button does nothing (for now, will update)
actionButton(inputId = "upload_button",
             label = "Upload Attached Data"),

# After clicking the button, return the message created in the server
verbatimTextOutput("attach_message")

## ----------------------------------------------- ##
            # Close UI Parentheses ####
## ----------------------------------------------- ##
# Close out formatting parentheses that wrap all of the above
  ## fluidPage(...sidebarLayout(...mainPanel(...
)))

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

