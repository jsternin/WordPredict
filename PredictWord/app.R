#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(dplyr)
library(data.table)
source("helperS.R") ##stupid backoff (also longest backoff)


## load dt[1-6] data tables for stupid backoff algorithm
dt1 <<- readRDS("data/dt1.rds")
dt2 <<- readRDS("data/dt2.rds")
dt3 <<- readRDS("data/dt3.rds")
dt4 <<- readRDS("data/dt4.rds")
dt5 <<- readRDS("data/dt5.rds")
dt6 <<- readRDS("data/dt6.rds")


s0 <<- "We offer 3 options of word prediction that could be chosen from navigation bar at the top"
s1 <<- c("1.Word prediction option based on sentence context",
          "Type your sentence and press Submit to predict next word.","Examples:",
         "how are you", "To be or not to be that is the", "I took the road less")
s2 <<- c("2. Word prediction with the list of possible words (quiz-like)",
          "Type your sentence and list of possible words and press Submit.","Example:",
         "Go on a romantic date at the", "mall beach grocery movies")
s3 <<- c("3.Character by character continuous prediction option",
          "Type your sentence first, press Submit. Add characters one-by-one pressing Submit.",
         "Example","Help me if you can I'm","f")
# Define ui 
ui<-shinyUI(navbarPage("JHU capstone project by jsternin",
   tabPanel("Predict Word",
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          h5(s0),h5(s1[1]),h5(s1[2]),h5(s1[3]),h5(s1[4]),h5(s1[5]),h5(s1[6]),
          textInput("textInput1","Enter your sentence:",width='400px'),
          radioButtons("radio1", label = h5("Algorithm:"),
               choices = list("Longest Backoff" = 1, "Stupid Backoff" = 2),selected = 1),
          actionButton("go1", "Submit")
        ),
        mainPanel(
          h4("Word prediction."),
          verbatimTextOutput("textOutput1"),
          h4("Matching words"),
          tableOutput("table1")
        )
      )
   ), ## endof predict word
   ##------------------ 2 Q U I Z--------------------------------
   tabPanel("Best from list",
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          h5(s0),h5(s2[1]),h5(s2[2]),h5(s2[3]),h5(s2[4]),h5(s2[5]),
          textInput("textInput2","Enter your sentence:"), 
          textInput("textInput2a","Possible answers (separated by spaces):"), #,width='1000px'),
          radioButtons("radio2", label = h5("Algorithm:"),
               choices = list("Longest Backoff" = 1, "Stupid Backoff" = 2),selected = 1),
          actionButton("go2", "Submit")
        ),
        mainPanel(
          h4("Word prediction."),
          verbatimTextOutput("textOutput2"),
          h4("Matching words"),
          tableOutput("table2")
        )
      )
   ), ## endof quiz
   ##-------------3. CHAR_BY_CHAR--------------------------------
   tabPanel("Predict char by char",
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          h5(s0),h5(s3[1]),h5(s3[2]),h5(s3[3]),h5(s3[4]),h5(s3[5]),
          textInput("textInput3","Enter your sentence:",width='400px'),
          textInput("textInput3a","Add character(s) to your sentence:",width='400px'),
          radioButtons("radio3", label = h5("Algorithm:"),
              choices = list("Longest Backoff" = 1, "Stupid Backoff" = 2),selected = 1),
          actionButton("go3", "Submit")
          
        ),
        mainPanel(
          h4("Word prediction."),
          verbatimTextOutput("textOutput3"),
          h4("Matching words"),
          tableOutput("table3")
        )
      )
   ) ##
))


# Define server 
server <- function(input, output) {
  ##----------------P R E D I C T    W O R D-----------------------------
  ## 1. Predict word
  resdfVals1 <- eventReactive(input$go1, {
    if (input$radio1==1)
      dtx <- stupid_predict_word(input$textInput1,FALSE) 
    else
      dtx <-stupid_predict_word(input$textInput1,TRUE) 
  })
  output$table1 <- renderTable({
    resdfVals1() 
  },digits=4)
  output$textOutput1 <- renderText({
    if (nrow(resdfVals1()) == 0)
      s <- "**NO MATCHES**"
    else {
      if (nchar(input$textInput1)==0)
        s <-""  
      else
        s <- paste(input$textInput1,resdfVals1()$word[1])
    }
    
  })
  ##----------------Q U I Z-----------------------------
  ## 2. Quiz
  resdfVals2 <- eventReactive(input$go2, {
    if (input$radio2==1) ###dataInput2(),dataInput2a())
      dtx <- stupid_predict_quiz(input$textInput2,input$textInput2a,FALSE) 
    else
      dtx <- stupid_predict_quiz(input$textInput2,input$textInput2a,TRUE)
  })
  output$table2 <- renderTable({ 
    resdfVals2() 
  },digits=4)
  output$textOutput2 <- renderText({
    if (nrow(resdfVals2()) == 0)
      s <- "**NO MATCHES**"
    else
      s <- paste(input$textInput2,resdfVals2()$word[1])
    
  })
  
  ##--------------CHAR BY CHAR----------------------------
  ## 3. Predict word char-by-char
  resdfVals3 <- eventReactive(input$go3, {
    if (input$radio3==1)
      dtx <- stupid_char_by_char(input$textInput3,input$textInput3a,FALSE) 
    else
      dtx <- stupid_char_by_char(input$textInput3,input$textInput3a,TRUE)
  })
  #dataInput3 <- reactive({input$textInput3})
  output$table3 <- renderTable({ 
    resdfVals3() 
  },digits=4)
  output$textOutput3 <- renderText({
    if (nrow(resdfVals3()) == 0)
      s <- "**NO MATCHES**"
    else
      s <- paste(input$textInput3,resdfVals3()$word[1])
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

