## --------------------------------------------------------------------- ##
              # HerbVar Data Submission Portal - Actual Version
## --------------------------------------------------------------------- ##
# Structure:
  ## Uses checkbox structure to choose sheets to upload (see ver. 2)
  ## Includes data preview tabs (see ver. 3)
  ## Harvests survey-level metadata (see ver. 4)
  ## Performs preliminary QA/QC of attached data (see ver. 5)
  ## Includes reset button for user's convenience (s ee ver. 6)
  ## Adds harvesting of which sheets are attached (see ver. 7)
  ## Streamlines explanation formatting and adds 'authorize email' button

# Call any needed libraries
library(shiny); library(stringr); library(readxl)
library(googlesheets4); library(googledrive); library(DT)
library(gargle); library(googleAuthR)

## --------------------------------------------------------------------- ##
                          # User Interface (UI) ####
## --------------------------------------------------------------------- ##
# Define user interface object
ui <- fluidPage(

# Add a title that appears in the browser tab
title = "HerbVar Data Portal",
## ----------------------------------------------- ##
          # UI: Header (Above Layout) ####
## ----------------------------------------------- ##
# Add an app title that can be read in the app
tags$h2("HerbVar Data Submission Portal - Phase 2"),
  ## For more info on tags object & HTML shortcuts:
  ## https://shiny.rstudio.com/articles/tag-glossary.html

# Remind users they must use the template datafile
tags$h4(tags$strong("This portal only accepts data entered into",
                    tags$a(href = "http://herbvar.org/protocols/HerbVar%20Datasheet%20Template.xlsx",
                           "the template Excel file.",
                           target = "_blank"))),

# And instruct them on the use of the app
tags$h4("Please follow the numbered steps ",
        tags$strong("to step 9"),
        " to upload your data."),

# Add a line separating this from above
tags$hr(),

## ----------------------------------------------- ##
              # UI: Authorize App ####
## ----------------------------------------------- ##
# Add heading for this section
h3("1. Authorize App (Equivalent to Password Entry)"),

# Explain authorization
# Get the .json key
h4("i)"
   ,a(href = "https://drive.google.com/drive/u/3/folders/1zDl5qqLMLHeAgi7bqw6J59dJeM8Ex84U",
      "Download Phase 2 Password File (.json file)",
      target = "_blank"
   )),

# Offer information on path to membership
h5("Only members can use the above link and download the file."),
tags$h5("Not a member but want to become one in order to submit data?
        See guidelines for becoming a member",
        tags$a(href = "http://herbvar.org/participation.html",
               "here.",
               target = "_blank")),

# Provide a place for JSON key attachment
fileInput(inputId = "json_attach",
          label = tags$h4("ii) Attach .JSON File"),
          accept = '.json',
          width = '35%'),

# Give authorization button a heading
tags$h4("iii) Click Button to Authorize"),

# Button to authorize email on click
actionButton(inputId = "auth_button",
             label = "Authorize"),

# After clicking the button, return the message created in the server
verbatimTextOutput("auth_msg"),

# Add a horizontal line
tags$hr(),

## ----------------------------------------------- ##
       # UI Sidebar vs. Main Panel Format ####
## ----------------------------------------------- ##
# Make the app have a side bar and main panel
sidebarLayout(position = 'left',
                fluid = T,
                
## ----------------------------------------------- ##
           # UI: Sidebar Contents ####
## ----------------------------------------------- ##
# Create the sidebar
sidebarPanel(

# Add a subtitle & more information
h3("2. Enter Survey Information (REQUIRED)"),
tags$h5("The following information is needed to name your data file.
        Please fill out all fields in this step (and do not use underscores (' _ ')."),
  
## ------------------------------ ##
     # UI: Require Email ####
## ------------------------------ ##
# Request email
  textInput(
    inputId = "user_email",
    label = tags$h4("Contact Email Address"),
    placeholder = "me@gmail.com"
  ),
  
## ------------------------------ ##
  # UI: Gather File Name Parts ####
## ------------------------------ ##
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
  label = tags$h4("Species of Surveyed Plant"),
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
        "but full name will also be retained elsewhere,",
        "so be as detailed as needed."),
          
## Sampling date
  ### Gets around possible issue of Name_Species_Site not being unique / survey
dateInput(inputId = "date",
          label = tags$h4("Sampling Date"),
          min = "1801-01-01",
          format = 'yyyy-mm-dd'),

# Note on date
tags$h5("Note: if sampling took >1 day, select the first day"),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
   # UI: Other Survey Info ####
## ------------------------------ ##
# We want to harvest some completed surveys information too
# So we'll add another subheading explaining that
h3("3. Enter Additional Information (Optional", tags$strong("though Encouraged!"), ")"),
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

tags$h5("Please separate observers with a semicolon ( ; )."),

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
                  "lifestage of plant?"),
  choices = c("-", 'yes', 'no')
),

# Explanation
tags$h5("E.g., Only surveyed flowering plants, or only seedlings, etc."),

## Did you look for herbivores?
radioButtons(
  inputId = "bugsCounted",
  label = tags$h4("Did you look for herbivores?"),
  choices = c("-", 'yes', 'no')
),

# Explanation
h5("If you are not uploading herbivore data,",
   " this helps us to know whether you didn't survey herbivores or",
   "did survey them but didn't find any."),

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
tags$h5("Please proceed to the main panel on the right."),

## ----------------------------------------------- ##
           # UI: Main Panel Contents ####
## ----------------------------------------------- ##
# Close out sidebar
), 

# Create main panel
mainPanel(

## ------------------------------ ##
    # UI: File Name Preview ####
## ------------------------------ ##
# Give it a title
tags$h3("4. Check File Name"),
                  
# Output file name from user inputs
verbatimTextOutput(outputId = "fileID",
                   placeholder = F),
                  
# And explain that is what it's doing
tags$h5("Does the above look correct as a file name for your survey?",
        tags$strong("If not,"),
        "edit your inputs in step 2."),
                  
# Add a horizontal line
tags$hr(),

## ------------------------------ ##
     # UI: Data Checkboxes ####
## ------------------------------ ##
# Checkboxes of the different tab options
checkboxGroupInput(
  inputId = 'data_collected',
  label = tags$h3("5. Select which Excel sheets you want to upload"),
  choices = c("siteData", "densityData", "plantData", "reproData",
              "herbivoreData", "newColumns", "notes"),
  selected = c("siteData", "plantData"),
  inline = T
),

# More notes on the checkboxes
tags$h5("Note that only checked boxes will be uploaded."),

# Add a line separating this from the rest of the app
tags$hr(),

## ------------------------------ ##
        # UI: File Input ####
## ------------------------------ ##
# Provide a place for Excel file uploading
fileInput(inputId = "file_upload",
          label = tags$h3("6. Attach Excel File"),
          accept = ".xlsx",
          width = '65%'),

# Add a note about misleading "upload" bar
tags$h5("Note that the blue 'upload' progress bar is misleading!"),
tags$h5("Your data",
        strong("HAVE NOT been uploaded"),
        "(yet); they are uploaded at step 9."),
tags$h5("So, please preview data in step 7,",
        "and fix any errors identified in each sheet by step 8."),

# Add a horizontal line
tags$hr(),

## ------------------------------ ##
     # UI: Preview Panels ####
## ------------------------------ ##
# Give a title above this section
tags$h3("7. Preview Data"),

# Make tabs for each sheet of the data
tabsetPanel(
  id = "data_tabs",
  tabPanel(title = "plantData", DT::dataTableOutput("plant_out")),
  tabPanel(title = "siteData", DT::dataTableOutput("site_out")),
  tabPanel(title = "densityData", DT::dataTableOutput("dens_out")),
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
tags$h3("8. Fix Errors Identified by the App and Re-Attach Data"),

# Tell people what to do if there are errors
tags$h5("To make errors go away:
        edit and save the data on your computer then re-attach it to the app (step 6)."),

# Make tabs for each sheet of the data
tabsetPanel(
  id = "check_tabs",
  tabPanel(title = "plantData", tableOutput("plant_chk")),
  tabPanel(title = "siteData", tableOutput("site_chk")),
  tabPanel(title = "densityData", tableOutput("dens_chk")),
  tabPanel(title = "reproData", tableOutput("repr_chk")),
  tabPanel(title = "herbivoreData", tableOutput("bug_chk")),
  tabPanel(title = "newColumns", tableOutput("new_chk")),
  tabPanel(title = "notes", tableOutput("notes_chk")),
),

# End with a horizontal line
tags$hr(),

## ------------------------------ ##
      # UI: Upload Button ####
## ------------------------------ ##
# Provide heading for upload button
tags$h3("9. Upload data!"),

# Ask users to confirm they are ready
radioButtons(inputId = "upload_confirmation",
             label = h4("i) Please confirm that you are ready for upload."),
             choices = c("No"),
             width = '100%'),

# Explain button
h5("If above button only has 'No' as an option, then either no
   .json file was attached in step 1 OR the file was attached but
   the 'Authorize' button was not clicked in step 1."),
h5("In either case, please return to step 1 before proceeding."),

# Lot of text so insert a break
br(),

# Button to upload data on click
h4("ii) When ready, click the button below to upload your data!"),
actionButton(inputId = "upload_button",
             label = "Upload Attached Data"),

# After clicking the button, return the message created in the server
verbatimTextOutput("upload_msg"),

# And tell people about the file that contains the secondary check for upload
# Warn users about timing
tags$h5("This step takes ~5 seconds per selected sheet;",
        strong("a confirmation message will appear when upload is complete.")),
tags$h5(a(href = "https://docs.google.com/spreadsheets/d/1XFNI7KXeuo5NuHL-0miKhWYkt3MeWUFYu2LugHRHE6Q/edit#gid=0",
               "This file",
               target = "_blank"),
        "will include your entries from steps 2 and 3 when data upload is complete."),

# Add a line to divide this from below
tags$hr(),

## ------------------------------ ##
      # UI: Reset Button ####
## ------------------------------ ##
# Provide heading for upload button
tags$h3("10. Reset the App (Optional)"),

# Add explanation note
tags$h5("This button is an optional convenience to facilitate uploading multiple files."),

# Button to upload data on click
actionButton(inputId = "reset_button",
             label = "Reset Inputs"),

# After clicking the button, return the message created in the server
verbatimTextOutput("reset_msg"),

# And add some explanation
tags$h5("This button resets everything",
        tags$strong("except for"),
        "email, PI name, and checkboxes."),

# Add a line for some breathing room at the bottom
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
server <- function(input, output, session) {

## ----------------------------------------------- ##
          # S: 'Authorize App' Button ####  
## ----------------------------------------------- ##
# If the authorize button is clicked
observeEvent(input$auth_button, {

# If button pushed without required information
  if (is.null(input$json_attach)) {
    
    # Make a failure message
    output$auth_msg <- renderPrint({
      'No .json file detected. Please attach the correct file.'
      })
      
      # Otherwise:
    } else {
      # Pre-emptively solve an issue with an HTTP2 error
      httr::set_config(httr::config(http_version = 0))
      
      # Authorize library(googledrive)
      drive_auth(email = input$user_email,
                 path = input$json_attach$datapath)
      
      # Authorize library(googlesheets4)
      gs4_auth(email = input$user_email,
               path = input$json_attach$datapath)
      
      # And update the upload radiobuttons to allow users to submit their data
      updateRadioButtons(inputId = "upload_confirmation",
                         choices = c("No", "Yes"))
      
      # Print a success message
      output$auth_msg <- renderPrint({
        'Access granted. Please proceed to step 2.'
      })
      }
  })

## ----------------------------------------------- ##
             # S: File Name Output ####  
## ----------------------------------------------- ##
# Gather file namefrom the supplied information in the UI
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
  ## The complexity below is to read a string as a dataframe
chosen_tabs <- reactive({
  as.data.frame(
    as.matrix(
      str_split(string = input$data_collected, pattern = "\\s+")
      )
    )
  })

## ----------------------------------------------- ##
       # S: Collect Bonus Survey Metadata ####
## ----------------------------------------------- ##
# Collect all of the entered info in a reactive dataframe
meta <- reactive({
  data.frame(
   "fileName" = surveyID(),
   "portalUser" = input$user_email,
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
   "herbivoresLookedFor" = input$bugsCounted,
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
attach_error <- data.frame("ALERT" = c("No data detected. Have you attached your Excel file?"))

# Data are attached but a given sheet's box wasn't selected
box_error <- data.frame("ALERT" = c("Sheet not selected for upload. Check the box above if you want to upload."))

# siteData box unchecked
site_error <- data.frame("FATAL_ERROR" = c("This sheet is REQUIRED.",
                                           "Please check the box above and ensure that this sheet is filled out."))

# Date is incorrectly formatted
error_msg.bad_date_format <- paste("Date is incorrectly formatted in at least one row.",
                                    "Please use yyyy.mm.dd format and use periods instead of slashes (this avoids Excel date issues).", sep = ' ')

# Any new columns created but missing from newColumns sheet
error_msg.missing_new_cols <- paste("New variables(s) added to sheet(s) but not defined in newColumns sheet.",
                                     "Please provide definitions for all new columns.", sep = ' ')

# Missing surveyID, plantSpecies, date, or site
error_msg.missing_index <- paste("At least one row is missing surveyID, plantSpecies, date, or site.")

# No data in sheet
error_msg.empty_sheet <- paste("You've chosen to upload this sheet but no entries were detected.",
                               "Please do not upload blank Excel sheets.", sep = ' ')

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
    options = list(pageLength = 10),
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
        options = list(pageLength = 10),
        rownames = F) }
  }
})

# plantData tab - preview
output$plant_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "plantData")) == 0){ box_error }
    else {
      DT::datatable(data = plant_actual(),
        options = list(pageLength = 10),
        rownames = F) }
  }
})

# reproData - preview
output$repr_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "reproData")) == 0){ box_error }
    else {
      DT::datatable(data = repr_actual(),
        options = list(pageLength = 10),
        rownames = F) }
  }
})

# herbivoreData - preview
output$bug_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "herbivoreData")) == 0){ box_error }
    else {
      DT::datatable(data = bug_actual(),
        options = list(pageLength = 10),
        rownames = F) }
  }
})

# newColumns - preview
output$new_out <- DT::renderDataTable({
  if(is.null(input$file_upload)){ attach_error }
  else { if(nrow(dplyr::filter(chosen_tabs(), chosen_tabs()[1] == "newColumns")) == 0){ box_error }
    else {
      DT::datatable(data = new_actual(),
        options = list(pageLength = 10),
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
        options = list(pageLength = 10),
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
                                     yes = paste0("The entry for location ",
                                                  dens_actual()$randomLocationID,
                                                  " contains a letter or special character ",
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
        # Percents >100 - percHerbPlant
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$percHerbPlant) > 100),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has percHerbPlant > 100%."),
                                       no = NA))
        # Percents >100 - focal/otherPlantCover
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$focalPlantCover) > 100 |
                                          as.numeric(plant_actual()$otherPlantCover) > 100),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has focalPlantCover or otherPlantCover > 100%."),
                                       no = NA))
        # Percents <0 - percHerbPlant
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$percHerbPlant) < 0),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has percHerbPlant < 0%."),
                                       no = NA))
        # Percents <0 - focal/otherPlantCover
        , data.frame("Errors" = ifelse((as.numeric(plant_actual()$focalPlantCover) < 0 |
                                          as.numeric(plant_actual()$otherPlantCover) < 0),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has focalPlantCover or otherPlantCover < 0%."),
                                       no = NA))
        # Percent between 0 and 1 - percHerbPlant
        , data.frame("Errors" = ifelse(((as.numeric(plant_actual()$percHerbPlant) > 0 &
                                           as.numeric(plant_actual()$percHerbPlant) < 0.5)),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has a very small percHerbPlant (0 < x < 0.5). ",
                                                    "Please check that you didn't enter a % in Excel (",
                                                    "this would auto-convert to a decimal upon uploading)"),
                                       no = NA))
        # Percent between 0 and 1 - focal/otherPlantCover
        , data.frame("Errors" = ifelse(((as.numeric(plant_actual()$focalPlantCover) > 0 &
                                           as.numeric(plant_actual()$focalPlantCover) < 0.5) |
                                          (as.numeric(plant_actual()$otherPlantCover) > 0 &
                                             as.numeric(plant_actual()$otherPlantCover) < 0.5)),
                                       yes = paste0("Plant '", plant_actual()$plantID, 
                                                    "' has a very small focalPlantCover or otherPlantCover (0 < x < 0.5). ",
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
          #S: 'Upload Data' Button ####  
## ----------------------------------------------- ##
# If the button is clicked, do the stuff in the {} brackets
observeEvent(input$upload_button, {

## ------------------------------------ ##
       # S: Upload Not Ready ####
## ------------------------------------ ##
if(input$upload_confirmation == "No") { 

# Print error message but otherwise do nothing
output$upload_msg <- renderPrint({
    "'Yes' option NOT selected above (step 9). See note below buttons for troubleshooting."
  })
  

## ------------------------------------ ##
 # S: Upload Ready but File Missing ####
## ------------------------------------ ##
} else {

# If the upload button is clicked but no data are attached:
if(is.null(input$file_upload) & input$upload_confirmation == "Yes")
  {

# Message when push upload button without attaching a data file
output$upload_msg <- renderPrint({
  'No file detected. Please attach a file.'
  })

## ------------------------------------ ##
 # S: Upload Ready and File Attached ####
## ------------------------------------ ##
} else {

# Make the code actually wait for the data
req(input$file_upload)

# Create an empty list
data_files <- list()

# Run a for loop that:
for (i in 1:nrow(chosen_tabs())) {
  # 1) Imports data into list
  data_files[[i]] <- as.data.frame(
    readxl::read_xlsx(path = input$file_upload$datapath,
                      sheet = as.character(chosen_tabs()[i, ])))
  
  # 2) Names elements after contents
  names(data_files)[i] <- chosen_tabs()[i, ]
}

# Set reactive value
fileData(data_files)

# Pre-emptively solve an issue with an HTTP2 error
httr::set_config(httr::config(http_version = 0))

# Loop to save the data from the list
for (i in 1:length(data_files)) {
  # Create a GoogleSheet of each datafile
  googlesheets4::gs4_create(name = paste0(surveyID(), "_",
                           names(data_files)[i]),
             sheets = data_files[i])
  
  # Move each one to the pre-specified correct folder
  googledrive::drive_mv(file = paste0(surveyID(), "_",
                         names(data_files)[i]),
           path = as_id("1WMgV2n3GF1jKqbkCqWN1O7EnQ23hWu43"))
  }

# Add the survey metadata to the Completed Surveys file
googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1XFNI7KXeuo5NuHL-0miKhWYkt3MeWUFYu2LugHRHE6Q/edit?usp=sharing",
             data = meta(),
             sheet = "completedSurveys")

# Successful upload message
output$upload_msg <- renderPrint({
  'Data uploaded. Thank you!'
})

# Close out parentheses
}}})

# Call any remaining reactive values
fileData <- reactiveVal()

## ----------------------------------------------- ##
              # S: 'Reset' Button ####  
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
    ## Herbivores looked for
  updateRadioButtons(inputId = "bugsCounted",
                     choices = c("-", 'yes', 'no'),
                     selected = "-")
    ## Notes
  updateTextInput(inputId = "miscNotes",
                  value = NA,
                  placeholder = "Any file-wide additional notes you'd like to provide")

  # Also want to 'reset' (i.e., detach) the attached file
  #fileData <- reactiveVal(data_files = NULL)
  shinyjs::reset("file_upload")
  
  # Reset data upload & authorization messages
  output$upload_msg <- renderPrint({'Please attach next file.'})
  output$auth_msg <- renderPrint({'App still authorized. Proceed to step 2.'})
  
  # Create successful reset message
  output$reset_msg <- renderPrint({
    'App reset. Awaiting next file.'
    })
  
  # Note that PI name (first and last), auhtorization email, and checkboxes are not reset
  # It seems likely that users of the app who want a reset button would:
    ## 1) be representing the same research team (so same PI file to file)
    ## 2) be the same person (so same email for authorization)
    ## 3) and have collected the same data (so checkboxes not reset)
    
})

## ----------------------------------------------- ##
        # S: Close Server Parentheses ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}

## --------------------------------------------------------------------- ##
                              # Build App ####
## --------------------------------------------------------------------- ##
shinyApp(ui = ui, server = server)

# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

