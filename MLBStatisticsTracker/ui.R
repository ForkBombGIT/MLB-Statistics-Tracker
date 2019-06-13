library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML("
  h3 {
    margin-top: 0px;
    margin-bottom: 10px;
  }
  ")),
  # Application title
  titlePanel("MLB Statistics Tracker"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      tags$h3(textOutput("role")),
      tags$hr(),
      helpText("Discover MLB trends throughout the years."),
      selectInput("role", 
                  label = "Choose the type of statistics to study:",
                  choices = list("Batting", 
                                 "Pitching"),
                  selected = "Batting"),
      selectInput(
        inputId =  "year", 
        label = "Choose a year to display trends from:", 
        choices = 1900:2100
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
      fluidRow(column(DT::dataTableOutput("table"),width = 12))
    )
  )
)