## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Version 7
## --------------------------------------------------------------------- ##
# Structure:
  ## Retains the checkbox structure (see ver. 2)
  ## Retains the data preview tabs (see ver. 3)
  ## Retains harvesting of survey-level metadata (see ver. 4)
  ## Retains preliminary QA/QC of attached data (see ver. 5)
  ## Retains reset button for most user inputs (see ver. 6)
  ## Want to change authentication method for file upload

# Clear environment
rm(list = ls())

# Call any needed libraries
library(shiny); library(stringr); library(readxl)
library(googlesheets4); library(googledrive); library(DT)

## --------------------------------------------------------------------- ##
                          # User Interface (UI) ####
## --------------------------------------------------------------------- ##
# Define user interface object
ui.v7 <- fluidPage(

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

# And instruct them on the use of the app
tags$h4("Please follow the numbered steps to upload your data successfully."),

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
h3("1. Survey Identifying Information (REQUIRED)"),
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
tags$h5("Note: site name will be capped at 8 characters automatically",
        tags$strong("for the file name,"),
        "but full name will also be retained elsewhere, so be as detailed as needed"),
          
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
h3("2. Additional Information (Optional", tags$strong("though Encouraged!"), ")"),
tags$h5("The following information is useful survey-level metadata that
        we expect may vary among scientists within a species.
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
  label = tags$h4("Species' Management Status (at your site)"),
  choices = c("-", "Natural", "Managed", "Cultivated")
),

## Single stage
radioButtons(
  inputId = "singleStage",
  label = tags$h4("Did you record",
                  tags$strong("only a single"),
                  "lifestage of plant"),
  choices = c("-", 'yes', 'no')
),

# Explanation
tags$h5("E.g., Only surveyed flowering plants, or only seedlings, etc."),

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
tags$h5(tags$strong("Thank you for entering this pre-submission information!")),
tags$h5("Please proceed to the main panel on the right"),

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
tags$h3("3. Check File Name"),
                  
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
# Checkboxes of the different tab options
checkboxGroupInput(
  inputId = 'data_collected',
  label = tags$h3("4. Select which Excel sheets you want to upload"),
  choices = c("siteData", "densityData", "plantData", "reproData",
              "herbivoreData", "newColumns", "notes"),
  selected = c("siteData", "plantData"),
  inline = T
),

# More notes on the checkboxes
tags$h5("Note that only checked tabs will be uploaded"),

# Add a line separating this from the rest of the app
tags$hr(),

## ------------------------------ ##
        # UI: File Input ####
## ------------------------------ ##
# Provide a place for Excel file uploading
fileInput(inputId = "file_upload",
          label = tags$h3("5. Attach Excel File"),
          accept = ".xlsx",
          width = '65%'),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
     # UI: Preview Panels ####
## ------------------------------ ##
# Give a title above this section
tags$h3("6. Preview Data"),

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

# End with a horizontal line
tags$hr(),

## ------------------------------ ##
      # UI: QA/QC Panels ####
## ------------------------------ ##
# Give a title above this section
tags$h3("7. Check errors identified by app"),

# Tell people what to do if there are errors
tags$h5("Please fix identified issues before uploading your data."),
tags$h5("To make errors go away: edit and save the data on your computer then re-attach it (step 5)."),

# Make tabs for each sheet of the data
tabsetPanel(
  id = "check_tabs",
  tabPanel(title = "siteData", tableOutput("site_chk")),
  tabPanel(title = "densityData", tableOutput("dens_chk")),
  tabPanel(title = "plantData", tableOutput("plant_chk")),
  tabPanel(title = "reproData", tableOutput("repr_chk")),
  tabPanel(title = "herbivoreData", tableOutput("bug_chk")),
  tabPanel(title = "newColumns", tableOutput("new_chk")),
  tabPanel(title = "notes", tableOutput("notes_chk")),
),

# End with a horizontal line
tags$hr(),

## ------------------------------ ##
    # UI: Authorized Email ####
## ------------------------------ ##
# Request email
textInput(
  inputId = "auth_email",
  label = tags$h3("8. Enter GoogleDrive-Authorized Email"),
  placeholder = "me@gmail.com",
  width = '50%'
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
# Provide heading for upload button
tags$h3("9. Upload data!"),

# Button to upload data on click
actionButton(inputId = "upload_button",
             label = "Upload Attached Data"),
        
# After clicking the button, return the message created in the server
verbatimTextOutput("upload_msg"),

# And have a warning on timing
tags$h5("Note that upload speed varies depending on internet speed & file size.
        A confirmation message will appear when upload is successful"),

# Add a line to divide this from below
tags$hr(),

## ------------------------------ ##
      # UI: Reset Button ####
## ------------------------------ ##
# Provide heading for upload button
tags$h3("10. Reset the App (if you're submitting >1 file)"),

# Give a warning about clicking it to early
tags$h5("WARNING: ", tags$strong("DO NOT"),
        "click this before the sucessful upload message prints below the upload button."),

# Button to upload data on click
actionButton(inputId = "reset_button",
             label = "Reset Inputs"),

# After clicking the button, return the message created in the server
verbatimTextOutput("reset_msg"),

# And have a warning on timing
tags$h5("After each file upload, click this button and repeat steps 1-9 for the next file"),

# And yet another warning that not everything will be reset
tags$h5("Note that PI name, authorization email, and checkboxes are not reset by this button"),

# Add a line for some breathing room at the bottom
tags$hr(),

## ------------------------------ ##
      # UI: Test Outputs ####
## ------------------------------ ##
# Give it a title
tags$h3("Test Outputs"),

# Explain the output
tags$h5("This section is purely for diagnostic purposes;
        As each portal version is created it is helpful to have spaces
        to export inner workings for visualization"),

# Spit out a table of selected options in the checkboxes
tags$h5("Test Out 1"),
tableOutput(outputId = "test_out1"),
tags$h5("Test Out 2"),
#DT::dataTableOutput(outputId = "test_out2"),
tableOutput(outputId = 'test_out2'),
tags$h5("Test Out 3"),
verbatimTextOutput(outputId = 'test_out3'),
tags$h5("Test Out 4"),
verbatimTextOutput(outputId = 'test_out4'),

# End with a horizontal line
tags$hr()

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
server.v7 <- function(input, output, session) {

## ----------------------------------------------- ##
             # S: File Name Output ####  
## ----------------------------------------------- ##
# Render the filename from the supplied information in the UI
  ## Inside of a render*() function to update dynamically
# Gather file name
surveyID <- reactive({
  paste(
    input$pi_last,
    input$pi_first,
    input$genus,
    input$sp,
    str_sub(input$site, start = 1, end = 8),
    input$date,
    sep = '_')
  })
  
# Call it as an output
output$fileID <- renderPrint({ surveyID() })

## ----------------------------------------------- ##
        # S: Collect Checkbox Choices ####
## ----------------------------------------------- ##
# Get an object of the selected checkboxes
chosen_tabs <- reactive({
  as.data.frame(
    as.matrix(
      str_split(string = input$data_collected, pattern = "\\s+")
      )
    )
  })

## ----------------------------------------------- ##
       # Collect Bonus Survey Metadata ####
## ----------------------------------------------- ##
# Collect all of the entered info in a reactive dataframe
meta <- reactive({
  data.frame(
   "fileName" = surveyID(),
   "portalUser" = input$auth_email,
   "PI_lastName" = input$pi_last,
   "PI_firstName" = input$pi_first,
   "plantGenus" = input$genus,
   "plantSpecies" = paste(input$genus, input$sp, sep = '_'),
   "plantCommon" = input$common,
   "siteSimp" = str_sub(input$site, start = 1, end = 8),
   "siteVerbose" = input$site,
   "surveyDate" = input$date,
   "otherObservers" = input$helpers,
   "foliageTypeSimp" = input$flgSimp,
   "foliageTypeVerbose" = input$flgVerbose,
   "nativeStatus" = input$native,
   "siteType" = input$siteType,
   "singleStage" = input$singleStage,
   "generalNotes" = input$miscNotes,
   "siteDataIncluded" = "siteData" %in% chosen_tabs()$V1,
   "densityDataIncluded" = "densityData" %in% chosen_tabs()$V1,
   "plantDataIncluded" = "plantData" %in% chosen_tabs()$V1,
   "reproDataIncluded" = "reproData" %in% chosen_tabs()$V1,
   "herbivoreDataIncluded" = "herbivoreData" %in% chosen_tabs()$V1,
   "newColumnsIncluded" = "newColumns" %in% chosen_tabs()$V1,
   "notesIncluded" = "notes" %in% chosen_tabs()$V1
  )
})

## ----------------------------------------------- ##
         # S: Test Outputs Creation ####
## ----------------------------------------------- ##
# Test output 1
output$test_out1 <- renderTable({
  chosen_tabs()
  })


# Test output 2
output$test_out2 <- renderTable({
  meta()
})

#output$test_out2 <- DT::renderDataTable({
#  DT::datatable(
#    data = meta(),
#    rownames = F)
#  })

# output #3
output$test_out3 <- renderPrint({
  chosen_tabs()
  })

# Output #4
output$test_out4 <- renderPrint({
  for (i in 1:nrow(chosen_tabs())) {
    print(as.character(chosen_tabs()[i, ]))
  }
})

## ----------------------------------------------- ##
        # S: Make all Sheets Reactive ####
## ----------------------------------------------- ##
# Each sheet needs to be made reactive for later reference
  ## For simplicity's sake, only the siteData reactive is fully commented

# siteData reactive
site_actual <- reactive({
  # If no file is attached, do nothing
  if(is.null(input$file_upload)) { return(NULL) } else {
  # If there is a file, make it reactive
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "siteData")) }
})

# densityData reactive
dens_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "densityData")) }
})

# plantData reactive
plant_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "plantData")) }
})

# reproData reactive
repr_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "reproData")) }
})

# herbivoreData reactive
bug_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "herbivoreData")) }
})

# newColumns reactive
new_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "newColumns")) }
})

# notes reactive
notes_actual <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    as.data.frame(readxl::read_xlsx(path = input$file_upload$datapath,
                                    sheet = "notes")) }
})

## ----------------------------------------------- ##
     # S: Create Informative Error Messages ####  
## ----------------------------------------------- ##
# These are called later as needed

# If data aren't attached
attach_error <- data.frame("ALERT" = c("No data detected",
                                       "Have you attached your Excel file?"))

# Data are attached but a given sheet's box wasn't selected
box_error <- data.frame("ALERT" = c("Sheet not selected for upload",
                                    "Check the box above if you want to upload"))

# siteData box unchecked
site_error <- data.frame("FATAL_ERROR" = c("*This sheet is REQUIRED*",
                                           "Please check the box above and",
                                           "ensure that this sheet is filled out"))

# Date is incorrectly formatted
error_msg.bad_date_format <- paste("Date is incorrectly formatted in at least one row.",
                                    "Please use yyyy.mm.dd format.",
                                    "Be careful to use periods instead of slashes,",
                                    "this avoids Excel date issues", sep = ' ')

# Any new columns created but missing from newColumns sheet
error_msg.missing_new_cols <- paste("New variables(s) added to sheet(s) but",
                                     "not defined in newColumns sheet.",
                                     "Please define all new columns.", sep = ' ')

# Missing surveyID, plantSpecies, date, or site
error_msg.missing_index <- paste("At least one row is missing surveyID, plantSpecies, date, or site")

# No data in sheet
error_msg.empty_sheet <- paste("You've chosen to upload this sheet but no entries were detected.",
                               "Please do not upload blank Excel sheets", sep = ' ')

# Message when no (other) errors are detected (in QA/QC portion)
green_light <- paste("No (other) errors detected; thank you for your diligence!")

## ----------------------------------------------- ##
      # S: Create Tabs for Data PREVIEWS ####  
## ----------------------------------------------- ##
# siteData tab - preview
output$site_out <- DT::renderDataTable({
  # If no data are attached, return the attach error
  if(is.null(input$file_upload)){
    attach_error
    } else {
  # If they are attached but the sheet isn't selected in the chechboxes
  if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "siteData")) == 0){
    site_error
    } else {
  # If data are attached and checkbox is selected, preview the table
  DT::datatable(data = site_actual(),
    options = list(pageLength = 5),
    rownames = F) }
      }
    })

# densityData tab - preview
  ## Note, the following do the same thing as the siteData tab
  ## So comments are excluded for brevity
output$dens_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "densityData")) == 0){ box_error }
    else {
      DT::datatable(data = dens_actual(),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# plantData tab - preview
output$plant_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "plantData")) == 0){ box_error }
    else {
      DT::datatable(data = plant_actual(),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# reproData - preview
output$repr_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "reproData")) == 0){ box_error }
    else {
      DT::datatable(data = repr_actual(),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# herbivoreData - preview
output$bug_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "herbivoreData")) == 0){ box_error }
    else {
      DT::datatable(data = bug_actual(),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# newColumns - preview
output$new_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "newColumns")) == 0){ box_error }
    else {
      DT::datatable(data = new_actual(),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

# Notes tab - preview
output$notes_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "notes")) == 0){ box_error }
    else {
      DT::datatable(data = as.data.frame(
        readxl::read_xlsx(path = input$file_upload$datapath,
                          sheet = "notes")),
        options = list(pageLength = 5),
        rownames = F) }
  }
})

## ----------------------------------------------- ##
        # S: Create Tabs for Data CHECKS ####  
## ----------------------------------------------- ##
## ------------------------------ ##
       # Check siteData ####
## ------------------------------ ##
output$site_chk <- renderTable({
  # Return NULL if either (1) no data are attached OR
  if(is.null(input$file_upload)){ return(NULL) } else {
    # (2) This tab isn't selected
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "siteData")) == 0){ site_error }
    else {
      # If data are attached and checkbox is selected, check for issues
        rbind(
        ## Any missing values
        data.frame("Errors" =
                     ifelse(is.na(site_actual()$datum)[1:18],
                            yes = paste0("No entry detected for '",
                                         site_actual()$variable, "'."),
                            no = NA))
        ) %>%
          # Remove any NAs (i.e., rows without issues identified by ifelse())
          dplyr::filter(!is.na(Errors)) %>%
          # Make that column a character
          dplyr::mutate(Errors = as.character(Errors)) %>%
          # Add a row saying there are no (more) errors
            ## This row will be the only if there are no errors
          dplyr::add_row(Errors = green_light) %>%
          # Remove duplicate rows (i.e., same issue across several rows)
          unique()
      }
    }
})

## Note, the following do the same thing as the siteData tab
## So comments are excluded for brevity

## ------------------------------ ##
     # Check densityData ####
## ------------------------------ ##
output$dens_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "densityData")) == 0){ box_error }
    else {
      rbind(
        # Any cells that would be coerced into numeric
        data.frame("Errors" = ifelse(is.na(as.numeric(dens_actual()$numPlantsPer_m2)),
                                     yes = paste0("This entry '",
                                                  dens_actual()$numPlantsPer_m2,
                                                  "' contains a letter or special character ",
                                                  "or is lacking any input"),
                                     no = NA))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        dplyr::add_row(Errors = green_light) %>%
        unique()
      }
  }
})

## ------------------------------ ##
      # Check plantData ####
## ------------------------------ ##
output$plant_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "plantData")) == 0){ box_error }
    else {
      rbind(
        # numLeaves/Herb missing data
        data.frame("Errors" = ifelse(
          test = (is.na(plant_actual()$numLeaves) | is.na(plant_actual()$numLeavesHerb)), 
          yes = paste0("Plant '", plant_actual()$plantID, 
                       "' is missing numLeaves or numLeavesHerb"),
          no = NA))
        # numLeavesHerb > numLeaves
        , data.frame("Errors" = ifelse(test = as.numeric(plant_actual()$numLeavesHerb) > as.numeric(plant_actual()$numLeaves),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' numLeavesHerb (# damaged leaves) ",
                                                    "is greater than numLeaves (total leaves)"),
                                       no = NA))
        # Percents >100
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$focalPlantCover) > 100 |
                                          as.numeric(plant_actual()$otherPlantCover) > 100 |
                                          as.numeric(plant_actual()$percHerbPlant) > 100),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has percHerbPlant, focal, or otherPlantCover > 100%."),
                                       no = NA))
        # Percents <0
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$focalPlantCover) < 0 |
                                          as.numeric(plant_actual()$otherPlantCover) < 0 |
                                          as.numeric(plant_actual()$percHerbPlant) < 0),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has percHerbPlant, focal, or otherPlantCover < 0%."),
                                       no = NA))
        # Percent between 0 and 1
        , data.frame("Errors" = ifelse(((as.numeric(plant_actual()$focalPlantCover) > 0 & as.numeric(plant_actual()$focalPlantCover) < 0.5) |
                                          (as.numeric(plant_actual()$otherPlantCover) > 0 & as.numeric(plant_actual()$otherPlantCover) < 0.5) |
                                          (as.numeric(plant_actual()$percHerbPlant) > 0 & as.numeric(plant_actual()$percHerbPlant) < 0.5)),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has a very small percHerbPlant, focal, or otherPlantCover (0 < x < 0.5).",
                                                    "Please check that you didn't enter a % in Excel (",
                                                    "this would auto-convert to a decimal upon uploading)"),
                                       no = NA))
        # Date incorrectly formatted
        , data.frame("Errors" = ifelse(test = (nchar(plant_actual()$date) != 9 & 
                                                 nchar(plant_actual()$date) != 10),
                                       yes = error_msg.bad_date_format,
                                       no = NA))
        # Index columns missing
        , data.frame("Errors" = ifelse(test = ( is.na(plant_actual()$surveyID) |
                                                  is.na(plant_actual()$plantSpecies) |
                                                  is.na(plant_actual()$date) |
                                                  is.na(plant_actual()$site) ),
                                       yes = error_msg.missing_index,
                                       no = NA))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        unique() %>%
        dplyr::arrange(Errors) %>%
        dplyr::add_row(Errors = green_light)
      }
  }
})

## ------------------------------ ##
       # Check reproData ####
## ------------------------------ ##
output$repr_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "reproData")) == 0){ box_error }
    else {
      rbind(
        # numRepro/Herb missing data
        data.frame("Errors" = ifelse(
          test = (is.na(repr_actual()$numRepro) | is.na(repr_actual()$numReproHerb)), 
          yes = paste0("Plant '", repr_actual()$plantID, 
                       "' is missing numRepro or numReproHerb"),
          no = NA))
        # numReproHerb > numRepro
        , data.frame("Errors" = ifelse(test = as.numeric(repr_actual()$numReproHerb) > as.numeric(repr_actual()$numRepro),
                                       yes = paste0("Plant '", repr_actual()$plantID, 
                                                    "' numReproHerb (# damaged repro units) ",
                                                    "is greater than numRepro (total repro units)"),
                                       no = NA))
        # Date incorrectly formatted
        , data.frame("Errors" = ifelse(test = (nchar(repr_actual()$date) != 9 & 
                                                 nchar(repr_actual()$date) != 10),
                                       yes = error_msg.bad_date_format,
                                       no = NA))
        # Index columns missing
        , data.frame("Errors" = ifelse(test = ( is.na(repr_actual()$surveyID) |
                                                  is.na(repr_actual()$plantSpecies) |
                                                  is.na(repr_actual()$date) |
                                                  is.na(repr_actual()$site) ),
                                       yes = error_msg.missing_index,
                                       no = NA))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        unique() %>%
        dplyr::arrange(Errors) %>%
        dplyr::add_row(Errors = green_light)
      }
  }
})

## ------------------------------ ##
     # Check herbivoreData ####
## ------------------------------ ##
output$bug_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "herbivoreData")) == 0){ box_error }
    else {
      rbind(
        # insectUnit missing
        data.frame("Errors" = ifelse(test = is.na(bug_actual()$insectUnit),
                                     yes = paste0("Plant '", bug_actual()$plantID,
                                                  "' is missing insectUnit (should be ",
                                                  "either 'count' or 'presence/absence')"),
                                     no = NA))
        # Date incorrectly formatted
        , data.frame("Errors" = ifelse(test = (nchar(bug_actual()$date) != 9 & 
                                                 nchar(bug_actual()$date) != 10),
                                       yes = error_msg.bad_date_format,
                                       no = NA))
        # Index columns missing
        , data.frame("Errors" = ifelse(test = ( is.na(bug_actual()$surveyID) |
                                                  is.na(bug_actual()$plantSpecies) |
                                                  is.na(bug_actual()$date) |
                                                  is.na(bug_actual()$site) ),
                                       yes = error_msg.missing_index,
                                       no = NA))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        unique() %>%
        dplyr::arrange(Errors) %>%
        dplyr::add_row(Errors = green_light)
      }
  }
})

## ------------------------------ ##
      # Check newColumns ####
## ------------------------------ ##
# Identify new columns in all sheets where columns can be added
  ## siteData new entries in "variable" column
site_new <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
  setdiff(site_actual()$variable,
          c(site_actual()$variable[1:18], "NOTES TO DATA ENTERER:")) }
  })
  ## plantData new columns
plant_new_cols <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
  setdiff(names(plant_actual()),
          names(dplyr::select(plant_actual(), surveyID:percLf30))) }
  })

  ## reproData new columns
repro_new_cols <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    setdiff(names(repr_actual()),
            names(dplyr::select(repr_actual(), surveyID:notes))) }
  })
  ## herbivoreData new columns
bug_new_cols <- reactive({
  if(is.null(input$file_upload)) { return(NULL) } else {
    setdiff(names(bug_actual()),
            names(dplyr::select(bug_actual(), surveyID:notes))) }
})

# Actual checks
output$new_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "newColumns")) == 0){ box_error }
    else {
      rbind(
        # Any new entries anywhere
        data.frame("Errors" = ifelse(test = (!is.na(setdiff(site_new(),
                                                            new_actual()$variable)) |
                                               !is.na(setdiff(plant_new_cols(),
                                                              new_actual()$variable)) |
                                               !is.na(setdiff(repro_new_cols(),
                                                              new_actual()$variable)) |
                                               !is.na(setdiff(bug_new_cols(),
                                                              new_actual()$variable))),
                                     yes = error_msg.missing_new_cols,
                                     no = NA))
        # New rows added to siteData tab
        , data.frame("Errors" = ifelse(test = !is.na(setdiff(site_new(),
                                                             new_actual()$variable)),
                                       yes = paste0("From siteData tab, ",
                                                    "Please define ",
                                                    site_new()),
                                       no = NA))
        # New columns added to plantData
        , data.frame("Errors" = ifelse(test = !is.na(setdiff(plant_new_cols(),
                                                             new_actual()$variable)),
                                       yes = paste0("From plantData tab, ",
                                                    "Please define ",
                                                    plant_new_cols()),
                                       no = NA))
        # New columns added to reproData
        , data.frame("Errors" = ifelse(test = !is.na(setdiff(repro_new_cols(),
                                                             new_actual()$variable)),
                                       yes = paste0("From reproData tab, ",
                                                    "Please define ",
                                                    repro_new_cols()),
                                       no = NA))
        # New columns added to herbivoreData
        , data.frame("Errors" = ifelse(test = !is.na(setdiff(bug_new_cols(),
                                                             new_actual()$variable)),
                                       yes = paste0("From herbivoreData tab, ",
                                                    "Please define ",
                                                    bug_new_cols()),
                                       no = NA))
        #, data.frame("Errors" = ifelse(test = , yes = , no = ))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        unique() %>%
        dplyr::arrange(Errors) %>%
        dplyr::add_row(Errors = green_light)
      }
  }
})

## ------------------------------ ##
   # Check notes (the sheet) ####
## ------------------------------ ##
output$notes_chk <- renderTable({
  if(is.null(input$file_upload)){ return(NULL) } else {
    if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "notes")) == 0){ box_error }
    else {
      rbind(
        data.frame("Errors" = ifelse(test = nrow(notes_actual()) == 0,
                                     yes = error_msg.empty_sheet,
                                     no = NA))
      ) %>%
        dplyr::filter(!is.na(Errors)) %>%
        dplyr::mutate(Errors = as.character(Errors)) %>%
        unique() %>%
        dplyr::arrange(Errors) %>%
        dplyr::add_row(Errors = green_light)
      }
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
  gs4_create(name = paste0(surveyID(), "_",
                           names(data_files)[i]),
             sheets = data_files[i])
  
  # Move each one to the pre-specified correct folder
  drive_mv(file = paste0(surveyID(), "_",
                         names(data_files)[i]),
           path = "HerbVar Phase II Data - All Uploads/App Test Area/")

}

## ------------------------------ ##
  # S: Save Entered Metadata ####
## ------------------------------ ##
# Add the survey metadata to the Completed Surveys file
sheet_append(ss = "https://docs.google.com/spreadsheets/d/1XFNI7KXeuo5NuHL-0miKhWYkt3MeWUFYu2LugHRHE6Q/edit?usp=sharing",
             data = meta(),
             sheet = "completedSurveys")

# Successful upload message
upload_msg('Data uploaded. Thank you!')
}
  })
  
# Call any remaining reactive values
fileData <- reactiveVal()
upload_msg <- reactiveVal()
  
# Produce any needed messages
output$upload_msg <- renderText({upload_msg()})

## ----------------------------------------------- ##
                # S: Reset Button ####  
## ----------------------------------------------- ##
# If the button is clicked, do the stuff in the {} brackets
observeEvent(input$reset_button, {
  
  # Reset all of the inputs up to this point
    ## Genus
  updateTextInput(inputId = "genus",
                  value = NA,
                  placeholder = "Plantago")
    ## Species
  updateTextInput(inputId = "sp",
                  value = NA,
                  placeholder = "major")
    ## Site
  updateTextInput(inputId = "site",
                  value = NA,
                  placeholder = "Site 1")
    ## Date
  updateDateInput(inputId = "date",
                  value = NULL)
    ## Common name
  updateTextInput(inputId = "common",
                  value = NA,
                  placeholder = "broadleaf plantain")
    ## Observers
  updateTextInput(inputId = "helpers",
                  value = NA,
                  placeholder = "who helped collect these data?")
    ## Foliage - simple
  updateSelectInput(inputId = "flgSimp",
                    choices = c("-", "Deciduous", "Evergreen", "Annual"),
                    selected = "-")
    ## Foliage - verbose
  updateTextInput(inputId = "flgVerbose",
                  value = NA,
                  placeholder = "Semi-deciduous: loses 50% leaves in winter")
    ## Native status
  updateSelectInput(inputId = "native",
                    choices = c("-", "Native", "Non-Native"),
                    selected = "-")
    ## Management status
  updateSelectInput(inputId = "siteType",
                    choices = c("-", "Natural", "Managed", "Cultivated"),
                    selected = "-")
    ## Single life stage
  updateRadioButtons(inputId = "singleStage",
                     choices = c("-", 'yes', 'no'),
                     selected = "-")
    ## Notes
  updateTextInput(inputId = "miscNotes",
                  value = NA,
                  placeholder = "Any file-wide additional notes you'd like to provide")

  # Also want to 'reset' (i.e., detach) the attached file
  #fileData <- reactiveVal(data_files = NULL)
  shinyjs::reset("file_upload")
  
  # Print successful reset message
  reset_msg('App reset. Awaiting next file')

  # Note that PI name (first and last), auhtorization email, and checkboxes are not reset
  # It seems likely that users of the app who want a reset button would:
    ## 1) be representing the same research team (so same PI file to file)
    ## 2) be the same person (so same email for authorization)
    ## 3) and have collected the same data (so checkboxes not reset)
    
})

# Call reactive mssage
reset_msg <- reactiveVal()

# And send it to UI
output$reset_msg <- renderText({reset_msg()})

## ----------------------------------------------- ##
        # S: Close Server Parentheses ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}

## --------------------------------------------------------------------- ##
                              # Build App ####
## --------------------------------------------------------------------- ##
shinyApp(ui = ui.v7, server = server.v7)

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

