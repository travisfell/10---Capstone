# server.r
# This is the server definition script for the 
# Capstone final project

library(shiny)
shinyServer(
    function(input,output) {
      output$prediction <- renderPrint({textpred(input$enteredtext)})
    }
  )