# add libraries, download data
library("shiny")
library("shinythemes")
library("ngram")
library("dplyr")
library("ggplot2")
library("markdown")
library("rmarkdown")
library("tm")

unzip("samples.zip")
   
source("capstonefinal.R")

# Server.R
server<-shinyServer(function(input, output, session) {
  result<-reactive({tp(input$intext)})
    output$result<-result
    
    nextword1<-reactive({result()[1]})
    output$nextword1<-nextword1
    observeEvent(input$add1, 
              {updateTextInput(session, "intext",
                      value=paste(input$intext,nextword1()))})
  
    nextword2<-reactive({result()[2]})
    output$nextword2<-nextword2
    observeEvent(input$add2, 
              {updateTextInput(session, "intext",
                      value=paste(input$intext,nextword2()))})
    
    nextword3<-reactive({result()[3]})
    output$nextword3<-nextword3
    observeEvent(input$add3, 
              {updateTextInput(session, "intext",
                      value=paste(input$intext,nextword3()))})
    
    nextword4<-reactive({result()[4]})
    output$nextword4<-nextword4
    observeEvent(input$add4, 
              {updateTextInput(session, "intext",
                      value=paste(input$intext,nextword4()))})
    
    nextword5<-reactive({result()[5]})
    output$nextword5<-nextword5
    observeEvent(input$add5, 
              {updateTextInput(session, "intext",
                      value=paste(input$intext,nextword5()))})


    wordplot<-reactive({tpwordplot(input$intext)})
    output$wordplot<-renderPlot({wordplot()})
    
# clear sentence
    observeEvent(input$clear,
                 {updateTextInput(session, "intext", value="")})
   })


# ui.R
ui<-shinyUI(fluidPage(
    theme=shinytheme("sandstone"),

    # Application title
    titlePanel("JHU Data Science Capstone Project: 
                      Text Prediction Algorithm"),
        br(),
    # Sidebar with textbox for sentence to predict next word from
    sidebarLayout(
        sidebarPanel(width=4,
                h5(tags$b("TEXT INPUT")), br(),
            p("Enter your text here:"),
            textInput("intext",label=NULL, width="100%"),
            p("Please be patient..."),
            actionButton("clear",label="Clear", style="background-color:purple")
            ),
        
        # Show a list of the 5 possible next words
        mainPanel(width=8, tabsetPanel(
            tabPanel(h5(tags$b("Prediction")), p(),
                     (p("Possible next words:", align="center")), p(),
            fluidRow(column(width=12,align="center",
            actionButton("add1", label=textOutput("nextword1"),
                         style="background-color:red"),
            actionButton("add2", label=textOutput("nextword2"),
                         style="background-color:orange"),
            actionButton("add3", label=textOutput("nextword3"),
                         style="background-color:gold"),
            actionButton("add4", label=textOutput("nextword4"),
                         style="background-color:green"),
            actionButton("add5", label=textOutput("nextword5"),
                         style="background-color:blue")
            ))),
           tabPanel(h5(tags$b("Word Plot")), p(),
               plotOutput("wordplot")),
            
          tabPanel(h5(tags$b("Presentation")),p(),
         includeMarkdown("README.md")),
           
           tabPanel(h5(tags$b("Code")), p() 
          , includeMarkdown("capstonefinal.Rmd"))  
            ))

        )
        ))
        

shinyApp(ui,server)
