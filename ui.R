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
        h3('The top 7 words associated with your text are: '),
        verbatimTextOutput("prediction"),
        p("The word cloud below shows upto the top 25 words that match the entered text."),
        plotOutput("textcloudplot")
    )
))