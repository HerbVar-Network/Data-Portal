## ------------------------------ ##
      # UI: Test Outputs ####
## ------------------------------ ##
ui <- fluidPage(
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
)

## ----------------------------------------------- ##
        # S: Test Outputs Creation ####
## ----------------------------------------------- ##
server <- function(input, output, session){
  # Test output 1
  output$test_out1 <- renderTable({
    chosen_tabs()
    })
  
  # Test output 2
      output$test_out2 <- renderTable({
        meta()
      })
      
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
      
}

## ----------------------------------------------- ##
                  # Build App ####
## ----------------------------------------------- ##
shinyApp(ui = ui, server = server)

