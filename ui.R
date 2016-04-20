#ui.R
# This is the UI definition script for the 
#Capstone final project

library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Text Prediction Cloud"),
    sidebarPanel(
      h3('Enter your text.'),
      p("This app predicts the next word based on the words you enter."),
      p("Type your text in the text box below. Then click Submit."),
      p("Please allow 5-10 seconds for the app to return results."),
      textInput('enteredtext', 'Enter your text here: '),
      #submitButton('Submit')
      actionButton('Submit', "Submit")
    ),
    mainPanel(
        h3('The top 3 words associated with your text are: '),
        verbatimTextOutput("prediction"),
        p("The word cloud below shows upto the top 25 words predicted from the entered text."),
        plotOutput("textcloudplot")
    )
))