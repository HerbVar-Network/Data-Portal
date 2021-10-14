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
          # Collect Checkbox Choices ####
## ----------------------------------------------- ##
# Make the chosen checkboxes a small table for the user to examine
   output$chosen <- renderTable(expr = input$data_collected,
                              rownames = F,
                              colnames = F,
                              align = 'c')

   # Print the checkbox output to be able to see it better
   output$chose_v2 <- renderPrint({input$data_collected})
   output$chose_v3 <- renderPrint({
     as.data.frame(as.matrix(
       (str_split(string = input$data_collected, pattern = "\\s+"))
       ))
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
       # Message when push upload button without attaching a data file
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
       
       # Make an empty list to receive the data
       all_data <- NULL
       
       # Get a more maleable list of selected checkboxes
       picked_sheets <- as.data.frame(as.matrix(
         (str_split(string = input$data_collected, pattern = "\\s+"))
         ))
       
       # If a file is uploaded
       # Put all sheets that are checked in the checkboxes in a list
       for (i in picked_sheets){
        # PROBLEM IS HERE ####
         
         # input$data_collected is not a vector, it is a single thing
         # somehow need to split the "entries" apart
         # so that they can be treated as a list
         
         all_data[[i]] <- as.data.frame(readxl::read_xlsx(
           path = upload$datapath, 
           sheet = all_data[[i]]
         ))
       }
       
       # THIS (BELOW) WORKS FOR ONE SHEET AT A TIME
       
       # If a file is actually uploaded, read it in!
      # data_file <- as.data.frame(
      #   read_excel(upload$datapath,
      #              sheet = input$data_collected,
      #              col_types = 'text'
      #   ))
       
       # Set the reactive value called fileData to the file inputs
       #fileData(data_file) 
       fileData(all_data)
       
       # Gather the name the users entered in the UI
       surveyID <- paste(input$pi_name,
                            input$genus,
                            input$sp,
                            str_sub(input$site,
                                    start = 1, end = 8),
                            input$date,
                            sep = '_')
       
       # Writing out the same file - but under a different name:
    #   write.csv(x = data_file,
    #             file = paste(surveyID, ".csv"),
    #             row.names = F)
       for (i in 1:length(input$data_collected)) {
         write.csv(x = all_data[[i]],
                   file = paste0(surveyID, "_",
                                 names(all_data)[i],
                                 ".csv"),
                   row.names = F)
       }
       
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

