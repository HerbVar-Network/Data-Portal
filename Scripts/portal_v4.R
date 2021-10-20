## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Version 4
## --------------------------------------------------------------------- ##
# Structure:
  ## Retains the checkbox structure first instantiated in version 2
  ## Retains the data preview tabs first instantiated in version 3
  ## Also performs some preliminary QA/QC for some of the data tabs

# Clear environment
rm(list = ls())

# Call any needed libraries
library(shiny); library(stringr); library(readxl)
library(googlesheets4); library(googledrive); library(DT)

## --------------------------------------------------------------------- ##
                          # User Interface (UI) ####
## --------------------------------------------------------------------- ##
# Define user interface object
ui.v4 <- fluidPage(

# Add a title that appears in the browser tab
title = "HerbVar Data Portal",
## ----------------------------------------------- ##
          # UI: Header (Above Layout) ####
## ----------------------------------------------- ##
# Add an app title that can be read in the app
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
h3("Survey Identifying Information (REQUIRED)"),
tags$h5("The following information is required to give a unique name to your data file.
        Please fill out all of the following as best you can."),
          
# Add a warning about underscores
tags$h5("Please", tags$strong("DO NOT"),
        "use underscores ('_') in your entries"),

# We need the following to uniquely name the file users submit:
  ## PI last name
textInput(
  inputId = "pi_last",
  label = tags$h4("Last Name of the PI"),
  placeholder = "von Humboldt"
  ),

## PI first initial
textInput(
  inputId = "pi_first",
  label = tags$h4("First Initial of the PI"),
  placeholder = "A"
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
tags$h5("Note: if sampling took >1 day, pick first day"),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
   # UI: Other Survey Info ####
## ------------------------------ ##
# We want to harvest some completed surveys information too
# So we'll add another subheading explaining that
h3("Additional Information (Optional", tags$strong("though Encouraged!"), ")"),
tags$h5("The following information is useful site-level metadata that we expect
        may vary among scientists within a species.
        Please fill out as much as possible"),

# And collect some more information
## Plant Common Name
textInput(
  inputId = "common",
  label = tags$h4("Common Name of Surveyed Plant"),
  placeholder = "broadleaf plantain"
),

## Other Observers
textInput(
  inputId = "helpers",
  label = tags$h4("Other Data Collectors"),
  placeholder = "R Carson; C Darwin"
),

tags$h5("Please separate observers with a semicolon (';')"),

## Foliage type - simple
selectInput(
  inputId = "flgSimp",
  label = tags$h4("Foliage Type - Simple"),
  choices = c("-", "Deciduous", "Evergreen", "Annual")
),

## Foliage type - verbose
textInput(
  inputId = "flgVerbose",
  label = tags$h4("Foliage Type - Verbose"),
  placeholder = "Semi-deciduous: loses 50% leaves in winter"
),

## Native status
selectInput(
  inputId = "native",
  label = tags$h4("Native Status (at your site)"),
  choices = c("-", "Native", "Non-Native")
),

## Site type
selectInput(
  inputId = "siteType",
  label = tags$h4("What is the status of the population at this site?"),
  choices = c("-", "Natural", "Managed", "Cultivated")
),

## Single stage
radioButtons(
  inputId = "singleStage",
  label = tags$h4("Only Plants of One Lifestage Recorded?"),
  choices = c("-", 'yes', 'no')
),

## General notes
textInput(
  inputId = "miscNotes",
  label = tags$h4("Notes"),
  placeholder = "Any other global comments you feel are important."
),

## ------------------------------ ##
   # UI: End Sidebar Contents ####
## ------------------------------ ##

# Closing out notes
tags$h5("Thank you for entering this pre-submission information!"),
tags$h5("Please proceed to the main panel on the right")

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
     # UI: Data Checkboxes ####
## ------------------------------ ##
# Prompt user to identify what data they collected
tags$h3("What tabs of the Excel file should be uploaded?"),

# Actual checkbox creation
checkboxGroupInput(
  inputId = 'data_collected',
  label = "Excel Tabs with Data",
  choices = c("siteData", "densityData", "plantData", "reproData",
              "herbivoreData", "newColumns", "notes"),
  selected = c("siteData", "plantData"),
  inline = T
),

# More notes on the checkboxes
tags$h5("Note: only checked tabs will be uploaded"),

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
        # UI: Tab Panels ####
## ------------------------------ ##
# Give a title above this section
tags$h3("Preview Data"),

# Make tabs for each sheet of the data
tabsetPanel(
  id = "data_tabs",
  tabPanel(title = "siteData", DT::dataTableOutput("site_out")),
  tabPanel(title = "densityData", DT::dataTableOutput("dens_out")),
  tabPanel(title = "plantData", DT::dataTableOutput("plant_out")),
  tabPanel(title = "reproData", DT::dataTableOutput("repr_out")),
  tabPanel(title = "herbivoreData", DT::dataTableOutput("bug_out")),
  tabPanel(title = "newColumns", DT::dataTableOutput("new_out")),
  tabPanel(title = "notes", DT::dataTableOutput("notes_out")),
),

# Below the tab, print the message about each tab (if it's not attached)
#verbatimTextOutput("attach_msg"),

# End with a horizontal line
tags$hr(),

## ------------------------------ ##
    # UI: Test Outputs ####
## ------------------------------ ##
# Give it a title
tags$h3("Test Outputs"),

# Spit out a table of selected options in the checkboxes
tableOutput(outputId = "test_out1"),
verbatimTextOutput(outputId = 'test_out2'),
verbatimTextOutput(outputId = 'test_out3'),
verbatimTextOutput(outputId = 'test_out4'),

# Explain the output
tags$h5("This section is purely for diagnostic purposes;
        As each portal version is created it is helpful to have spaces
        to export inner workings for visualization"),

# End with a horizontal line
tags$hr(),

## ------------------------------ ##
    # UI: Authorized Email ####
## ------------------------------ ##
# Request email
textInput(
  inputId = "auth_email",
  label = tags$h3("GoogleDrive-Authorized Email"),
  placeholder = "me@gmail.com"
),

# Note on email
tags$h5("This app uploads your data to GoogleDrive.
        Because of this,",
        tags$strong("you need to enter an email with access to",
                    tags$a(href = "https://drive.google.com/drive/folders/1YiQqcrQwlbDyxghVdQwxbTDv919oBuSY?usp=sharing",
                           "this GoogleDrive folder"))),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
      # UI: Upload Button ####
## ------------------------------ ##
## Button to upload data on click
actionButton(inputId = "upload_button",
             label = "Upload Attached Data"),
        
# After clicking the button, return the message created in the server
verbatimTextOutput("upload_msg"),

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
server.v4 <- function(input, output, session) {

## ----------------------------------------------- ##
             # S: File Name Output ####  
## ----------------------------------------------- ##
# Render the filename from the supplied information in the UI
  ## Inside of a render*() function to update dynamically
output$fileID <- renderPrint({
  paste(
    input$pi_last,
    input$pi_first,
    input$genus,
    input$sp,
    str_sub(input$site, start = 1, end = 8),
    input$date,
    sep = '_')
  })
  
## ----------------------------------------------- ##
        # S: Collect Checkbox Choices ####
## ----------------------------------------------- ##
# Get an object of the selected checkboxes
chosen_tabs <- reactive({as.data.frame(as.matrix(
  (str_split(string = input$data_collected, pattern = "\\s+"))))})

## ----------------------------------------------- ##
         # S: Test Outputs Creation ####
## ----------------------------------------------- ##
# Make the chosen checkboxes a small table for the user to examine
output$test_out1 <- renderTable(expr = input$data_collected,
                             rownames = F,
                             colnames = F,
                             align = 'c')

# Print the checkbox output to be able to see it better
  ## Note these outputs are to help me diagnose issues in the app
  ## they will not be included in the final app
output$test_out2 <- renderPrint({input$data_collected})


# See if that works as intended
output$test_out3 <- renderPrint({
  chosen_tabs()
  })

# Test how bracket notation affects the reactive
output$test_out4 <- renderPrint({
  for (i in 1:nrow(chosen_tabs())) {
    print(as.character(chosen_tabs()[i, ]))
  }
})

## ----------------------------------------------- ##
           # S: Create Tabs for Data ####  
## ----------------------------------------------- ##
# Make generic pseudo-error messages for the tabs when:
  ## 1) The data haven't been attached
attach_error <- data.frame("Alert" = c("No data detected",
                                       "Have you attached your Excel file?"))
  ## 2) The data were attached but a given sheet's box wasn't selected
box_error <- data.frame("Alert" = c("Sheet not selected for upload",
                                     "Check the box above if you want to upload"))
# siteData tab
output$site_out <- DT::renderDataTable({
  # If no data are attached, return the attach error
  if(is.null(input$file_upload)){
    attach_error
    } else {
  # If they are attached but the sheet isn't selected in the chechboxes
  if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "siteData")) == 0){
    box_error
    } else {
  # If data are attached and checkbox is selected, preview the table
  DT::datatable(data = as.data.frame(
    readxl::read_xlsx(path = input$file_upload$datapath,
                      sheet = "siteData")),
    options = list(pageLength = 5),
    rownames = F) }
      }
    })

# densityData tab
  ## Note, the following do the same thing as the siteData tab
  ## So comments are excluded for brevity
output$dens_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "densityData")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "densityData")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# plantData tab
output$plant_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "plantData")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "plantData")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# reproData
output$repr_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "reproData")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "reproData")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# herbivoreData
output$bug_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "herbivoreData")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "herbivoreData")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})


# newColumns
output$new_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "newColumns")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "newColumns")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# Notes tab
output$notes_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(filter(chosen_tabs(), chosen_tabs()[1] == "notes")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "notes")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})


## ----------------------------------------------- ##
           # S: 'Upload Data' Button ####  
## ----------------------------------------------- ##
# If the button is clicked, do the stuff in the {} brackets
observeEvent(input$upload_button, {
    
# Need to make the code actually wait for the data
req(input$file_upload)

## ------------------------------ ##
  # S: Button Pushed w/o Data ####
## ------------------------------ ##
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
  upload_msg('Please attach a file')

## ------------------------------ ##
 # S: Button Pushed with Data ####
## ------------------------------ ##
  } else {    

# If there is a file, retrieve it
upload <- input$file_upload
      
# If the file is NULL, return NULL
if (is.null(upload)) { return(NULL) }

# Gather the name the users entered in the UI
surveyID <- paste(input$pi_last,
                  input$pi_first,
                  input$genus,
                  input$sp,
                  str_sub(input$site, start = 1, end = 8),
                  input$date,
                  sep = '_')

## ------------------------------ ##
  # S: Get all Checked Sheets ####
## ------------------------------ ##
# Create an empty list
data_files <- list()

# Run a for loop that:
for (i in 1:nrow(chosen_tabs())) {
  # 1) Imports data into list
  data_files[[i]] <- as.data.frame(
    readxl::read_xlsx(path = upload$datapath,
                      sheet = as.character(chosen_tabs()[i, ])))
  
  # 2) Names elements after contents
  names(data_files)[i] <- chosen_tabs()[i, ]
}

# Set reactive value
fileData(data_files)

## ------------------------------ ##
  # S: Save all Checked Sheets ####
## ------------------------------ ##
# Pre-emptively solve an issue with an HTTP2 error
httr::set_config(httr::config(http_version = 0))

# Authorize GoogleDrive and GoogleSheets with the provided email
googledrive::drive_auth(email = input$auth_email)
gs4_auth(email = input$auth_email)

# Loop to save the data from the list
for (i in 1:length(data_files)) {
  # Create a GoogleSheet of each datafile
  gs4_create(name = paste0(surveyID, "_",
                           names(data_files)[i]),
             sheets = data_files[i])
  
  # Move each one to the pre-specified correct folder
  drive_mv(file = paste0(surveyID, "_",
                         names(data_files)[i]),
           path = "HerbVar Phase II Data - All Uploads/App Test Area/")
  
}

# Successful upload message
upload_msg('Data uploaded. Thank you!')
}
  })
  
# Call any remaining reactive values
fileData <- reactiveVal()
upload_msg <- reactiveVal()
  
# Produce any needed messages
output$upload_msg <- renderText({upload_msg()})
output$attach_msg <- renderText({attach_text1()})

## ----------------------------------------------- ##
        # S: Close Server Parentheses ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}

## --------------------------------------------------------------------- ##
                              # Build App ####
## --------------------------------------------------------------------- ##
shinyApp(ui = ui.v4, server = server.v4)

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

