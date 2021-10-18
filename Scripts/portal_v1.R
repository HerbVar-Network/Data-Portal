## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Version 1
## --------------------------------------------------------------------- ##
# Structure:
  ## Provides a drop down menu for users to pick which sheet to upload
  ## Note that it only works for one sheet at a time

# Clear environment
rm(list = ls())

# Call any needed libraries
library(shiny); library(stringr); library(readxl)
library(googlesheets4); library(googledrive)

## --------------------------------------------------------------------- ##
                          # User Interface (UI) ####
## --------------------------------------------------------------------- ##
# Define user interface object
ui.v1 <- fluidPage(
  
## ----------------------------------------------- ##
          # UI: Header (Above Layout) ####
## ----------------------------------------------- ##
# Add an app title
tags$h2("HerbVar Data Submission Portal - Phase 2"),
  ## For more info on tags object & HTML shortcuts:
  ## https://shiny.rstudio.com/articles/tag-glossary.html

# Add information about the purpose of this portal
tags$h4("This Shiny App handles data submission for the Herbivory Variability Network.
        More information can be found",
        tags$a(href = "http://herbvar.org/", "on the Network's website.")),

# Add a line separating this from above
tags$hr(),

# Remind users they must use the template datafile
tags$h4("You", tags$strong("must"),
        "use the template file provided ",
        tags$a(href = "http://herbvar.org/protocols.html",
               "here.")),

# Add a line separating this from the rest of the app
tags$hr(),
  
# Make the app have a side bar and main panel
sidebarLayout(position = 'left',
                fluid = T,
                
## ----------------------------------------------- ##
           # UI: Sidebar Contents ####
## ----------------------------------------------- ##
# Create the sidebar
sidebarPanel(

# Give the sidebar a global title
tags$h2("Pre-Data Submission"),

## ------------------------------ ##
  # UI: Gather File Name Parts ####
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
  label = tags$h4("Last Name of the PI"),
  placeholder = "von Humboldt"
  ),
          
## Genus of survey species
textInput(
  inputId = "genus",
  label = tags$h4("Genus of Surveyed Plant"),
  placeholder = "Plantago"
  ),

## Specific epithet of survey species
textInput(
  inputId = "sp",
  label = tags$h4("Specific Epithet of Surveyed Plant"),
  placeholder = "major"
  ),
          
## Site name
textInput(
  inputId = "site",
  label = tags$h4("Site Name"),
  placeholder = "Site 1"
  ),
          
# Note on site name
tags$h5("Note: site name will be capped at 8 characters automatically"),
          
## Sampling date
  ### Gets around possible issue of Name_Species_Site not being unique / survey
dateInput(inputId = "date",
          label = tags$h4("Sampling Date"),
          min = "1801-01-01",
          format = 'yyyy-mm-dd'),

# Note on date
tags$h5("Note: if sampling took >1 day, pick first day")

## ----------------------------------------------- ##
           # UI: Main Panel Contents ####
## ----------------------------------------------- ##
# Close out sidebar
), 

# Create main panel
mainPanel(
                  
# Give the main panel a title too
tags$h2("Data Submission Process"),
                  
## ------------------------------ ##
    # UI: File Name Preview ####
## ------------------------------ ##
# Give it a title
tags$h3("File Name Preview"),
                  
# Output file name from user inputs
verbatimTextOutput(outputId = "fileID",
                   placeholder = F),
                  
# And explain that is what it's doing
tags$h5("Does the above look correct as a file name for your survey?",
        tags$strong("If not,"), "edit your inputs to the left"),
                  
# Add a horizontal line
tags$hr(),

## ------------------------------ ##
     # UI: Data Dropdown ####
## ------------------------------ ##
# Prompt user to identify what data they collected
tags$h3("Which tab would you like to upload?"),

# Dropdown menu options
selectInput(
  inputId = 'data_collected',
  label = "Excel Tabs to Upload",
  choices = c("siteData", "densityData", "plantData", "reproData",
              "herbivoreData", "newColumns", "notes"),
  selected = c("siteData")
),

# More notes on the checkboxes
tags$h5("Note that you must run this app once per tab to upload.
        We know this is cumbersome and are working on an upgrade currently.
        Thanks for your patience!"),

# Add a line separating this from the rest of the app
tags$hr(),

## ------------------------------ ##
       # UI: File Input ####
## ------------------------------ ##
# Provide a place for Excel file uploading
fileInput(inputId = "file_upload",
          label = tags$h3("Attach Excel File Here"),
          accept = ".xlsx"),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
    # UI: Authorized Email ####
## ------------------------------ ##
# Note on email
tags$h5("This app uploads your data to GoogleDrive.
        Because of this,",
        tags$strong("you need to enter an email with access to",
                    tags$a(href = "https://drive.google.com/drive/folders/1YiQqcrQwlbDyxghVdQwxbTDv919oBuSY?usp=sharing",
                           "this GoogleDrive folder"))),

# Request email
textInput(
  inputId = "auth_email",
  label = tags$h4("GoogleDrive-Authorized Email"),
  placeholder = "me@gmail.com"
),

# Add a horizontal line
tags$hr(),
## ------------------------------ ##
      # UI: Upload Button ####
## ------------------------------ ##
## Button to upload data on click
actionButton(inputId = "upload_button",
             label = "Upload Attached Data"),
        
# After clicking the button, return the message created in the server
verbatimTextOutput("attach_message"),

# Add a line for some breathing room at the bottom
tags$hr(),

## ----------------------------------------------- ##
           # UI: Close UI Parentheses ####
## ----------------------------------------------- ##
# Close out formatting parentheses that wrap all of the above
  ## fluidPage(...sidebarLayout(...mainPanel(...
)))

## --------------------------------------------------------------------- ##
                      # Server Function (S) ####
## --------------------------------------------------------------------- ##
# Create the internal mechanism(s) of the app
server.v1 <- function(input, output, session) {

## ----------------------------------------------- ##
             # S: File Name Output ####  
## ----------------------------------------------- ##
# Render the filename from the supplied information in the UI
  ## Inside of a render*() function to update dynamically
output$fileID <- renderPrint({
  paste(
    input$pi_name,
    input$genus,
    input$sp,
    str_sub(input$site, start = 1, end = 8),
    input$date,
    sep = '_')
  })
  
## ----------------------------------------------- ##
        # S: Collect Checkbox Choices ####
## ----------------------------------------------- ##
# Make the chosen checkboxes a small table for the user to examine
output$chosen <- renderText(expr = input$data_collected)

## ----------------------------------------------- ##
          # S: 'Upload Data' Button ####  
## ----------------------------------------------- ##
# If the button is clicked, do the stuff in the {} brackets
observeEvent(input$upload_button, {
    
# Need to make the code actually wait for the data
req(input$file_upload)
    
# If the upload button is clicked but no data are attached:
if(is.null(input$file_upload))
  {
  # Generates pop-up warning
  showModal(modalDialog(
    title = "Error: Missing Data",
    paste0("No file selected to upload."),
    easyClose = T,
    footer = NULL
    ))
# Message when push upload button without attaching a data file
  my_text('Please attach a file')
  
    } else {

# If there is a file, retrieve it
upload <- input$file_upload
      
# If the file is NULL, return NULL
if (is.null(upload)) { return(NULL) }
      
# If a file is actually uploaded, read it in!
data_file <- as.data.frame(
  read_excel(upload$datapath,
             sheet = input$data_collected,
             col_types = 'text'
             ))
      
# Set the reactive value called fileData to the file inputs
fileData(data_file)
      
# Gather the name the users entered in the UI
surveyID <- paste(input$pi_name,
                  input$genus,
                  input$sp,
                  str_sub(input$site, start = 1, end = 8),
                  input$date,
                  sep = '_')

# Pre-emptively solve an issue with an HTTP2 error
httr::set_config(httr::config(http_version = 0))

# Authorize GoogleDrive and GoogleSheets with the provided email
googledrive::drive_auth(email = input$auth_email)
gs4_auth(email = input$auth_email)

# Upload data as a googlesheet
gs4_create(name = paste0(surveyID, "_",
                         input$data_collected),
           sheets = data_file)

# Move this to the correct folder
drive_mv(file = paste0(surveyID, "_",
                       input$data_collected),
         path = "HerbVar Phase II Data - All Uploads/App Test Area/")

# Writing out the same file - but under a different name:
#write.csv(x = data_file,
#          file = paste0(surveyID, "_",
#                        input$data_collected,
#                        ".csv"),
#          row.names = F)
      
# Successful upload message
my_text('Data uploaded. Thank you!')
}
  })
  
# Call final fileData and my_text information
fileData <- reactiveVal()
my_text <- reactiveVal()
  
# Produce a message following the button press
output$attach_message <- renderText({my_text()})
  
## ----------------------------------------------- ##
        # S: Close Server Parentheses ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}

## --------------------------------------------------------------------- ##
                              # Build App ####
## --------------------------------------------------------------------- ##
shinyApp(ui = ui.v1, server = server.v1)

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

