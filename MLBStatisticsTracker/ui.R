library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #html styles
  tags$head(
    tags$style(HTML("
      h3 {
        margin-top: 0px;
        margin-bottom: 10px;
      }
      
      dt { float: left; clear:both; margin-bottom: 5px; font-weight: normal;}
      dd { float: right; }
      dl { overflow: hidden; }
    "))
  ),
  # Application title
  titlePanel("MLB Statistics Tracker"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      tags$h3(textOutput("role")),
      tags$hr(),
      tags$dl(
        tags$dt("Mean X:"),
        tags$dd(textOutput("mean_x")),
        tags$dt("Mean Y:"),
        tags$dd(textOutput("mean_y")),
        tags$dt("Correlation Coefficient:"),
        tags$dd(textOutput("cc"))
      ),
      helpText("Discover MLB trends throughout the years."),
      selectInput("role", 
                  label = "Choose the type of statistics to study:",
                  choices = list("Hitting", 
                                 "Pitching"),
                  selected = "Hitting"),
      selectInput(
        inputId =  "year", 
        label = "Choose a year to display trends from:", 
        choices = 1876:2100
      ),
      selectInput(
          inputId =  "x", 
          label = "Choose a statistic for x to represent:", 
          choices = list("")
      ),
      selectInput(
          inputId =  "y", 
          label = "Choose a statistic for y to represent:", 
          choices = list("")
      )
    ),
    mainPanel(
      plotOutput("plot",height = '450px',width = '550px'),
      fluidRow(column(DT::dataTableOutput("table"),width = 12)),
      textOutput("test")
    )
  )
)