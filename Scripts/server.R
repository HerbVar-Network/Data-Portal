## --------------------------------------------------------------------- ##
                 # HerbVar Data Submission Portal - Server
## --------------------------------------------------------------------- ##
# Written by Nick J Lyon

# PURPOSE ####
  ## Practice harvesting simple information from users

# Call any needed libraries
library(shiny); library(stringr); library(readxl)

# Create the internal mechanism(s) of the app
server <- function(input, output, session) {

## ----------------------------------------------- ##
              # File Name Output ####  
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
             # 'Upload Data' Button ####  
## ----------------------------------------------- ##
# Handling the "upload_button"
   observeEvent(input$upload_button, {
     
     # Need to make the code actually wait for the data
     req(input$file_upload)
     
     # If the upload button is clicked but no data are attached:
     if(is.null(input$file_upload))
     {
       # Generates pop-up warning
       showModal(modalDialog(
         title = "Error",
         paste0("No file selected to upload."),
         easyClose = TRUE,
         footer = NULL
       ))
       # If they don't upload data print the message:
       my_text('Please attach a file')
     }
     else
     {
       # Get the file from the app
       upload <- input$file_upload
       
       # If the file is NULL, return NULL
       if (is.null(upload)) {
         return(NULL)
       }
       
       # If a file is actually uploaded, read it in!
       data_file <- as.data.frame(
         read_excel(upload$datapath,
                    sheet = "siteData",
                    col_types = 'text'
         ))
       
       # Set the reactive value called fileData to the file inputs
       fileData(data_file) 
       
       # Gather the name the users entered in the UI
       surveyID <- paste(input$pi_name,
                            input$genus,
                            input$sp,
                            str_sub(input$site,
                                    start = 1, end = 8),
                            input$date,
                            sep = '_')
       
       # Writing out the same file - but under a different name:
       write.csv(x = data_file,
                 file = paste(surveyID, ".csv"),
                 row.names = F)
       my_text('Data uploaded. Thank you!')
     }
   })
   
   fileData <- reactiveVal()
   my_text <- reactiveVal()
   output$attach_message <- renderText({my_text()})
   
## ----------------------------------------------- ##
           # Close Server Parentheses ####
## ----------------------------------------------- ##
# Close out formatting curly braces that wrap server components
  ## server <- function(...) {...
}
   
# END ####


# Template border lengths
## --------------------------------------------------------------------- ##
## ----------------------------------------------- ##
## ------------------------------ ##

