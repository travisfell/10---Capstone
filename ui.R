#ui.R
# This is the UI definition script for the 
#Capstone final project

library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Text Prediction and Word Cloud."),
    sidebarPanel(
      h3('Enter your text.'),
      p("This app predicts the next word based on the words you enter."),
      p("Type your text in the text box below. Then click Submit."),
      textInput('enteredtext', 'Enter your text here: '),
      submitButton('Submit')
    ),
    mainPanel(
        h3('Your Word Prediction'),
        verbatimTextOutput("prediction"),
        p("Word cloud will go here.")
    )
))