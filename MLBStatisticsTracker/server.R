library(rvest) 
library(RSelenium)
library(ggplot2)
library(hash)

#MLB Statistics Sheet
mlb_url <- "http://mlb.mlb.com/stats/sortable.jsp#elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+ROLE&game_type='R'&season=YEAR&season_type=ANY&league_code='MLB'&sectionType=sp&statType=ROLE&page=1&ts=1560404713148"

#possible hitting input choices
hitting_choices <- list("G (Games Played)",
                        "AB (At Bats)",
                        "R (Runs)",
                        "H (Hits)",
                        "2B (Doubles)",
                        "3B (Triples)",
                        "HR (Homeruns)",
                        "RBI (Runs Batted In)",
                        "BB (Base on Balls)",
                        "SO (Strikeouts)",
                        "SB (Stolen Bases)",
                        "CS (Caught Stealing)",
                        "AVG (Batting Average)",
                        "OBP (On Base Percentage)",
                        "SLG (Slugging Percentage)",
                        "OPS (OBS + SLG)")

#possible pitching input choices
pitching_choices <- list("W (Wins)",
                         "L (Losses)",
                         "ERA (Earned Runs Average)",
                         "G (Game Appearances)",
                         "GS (Games Started)",
                         "SV (Saves)",
                         "SV (Save Appearances)",
                         "IP (Innings Pitches)",
                         "H (Hits Allowed)",
                         "R (Runs Allowed)",
                         "ER (Earned Runs Allowed)",
                         "HR (Homeruns Allowed)",
                         "BB (Walks Allowed)",
                         "SO (Strikeouts)",
                         "AVG (H/AB)",
                         "WHIP (AVG of Walks + Hits Per IP)")
#hitting html classes
hitting_stats <- list("td.dg-g",
                      "td.dg-ab",
                      "td.dg-r",
                      "td.dg-h",
                      "td.dg-d",
                      "td.dg-t",
                      "td.dg-hr",
                      "td.dg-rbi",
                      "td.dg-bb",
                      "td.dg-so",
                      "td.dg-sb",
                      "td.dg-cs",
                      "td.dg-avg",
                      "td.dg-obp",
                      "td.dg-slg",
                      "td.dg-ops")
#pitching html classes
pitching_stats <- list("td.dg-w",
                       "td.dg-l",
                       "td.dg-era",
                       "td.dg-g",
                       "td.dg-gs",
                       "td.dg-sv",
                       "td.dg-svo",
                       "td.dg-ip",
                       "td.dg-h",
                       "td.dg-r",
                       "td.dg-er",
                       "td.dg-hr",
                       "td.dg-bb",
                       "td.dg-so",
                       "td.dg-avg",
                       "td.dg-whip")

#hash to convert input to html tags
input_hash <- hash(c(hitting_choices,pitching_choices),
                   c(hitting_stats,pitching_stats))

#retieves data from mlb website
get.data <- function(input) {
  #Convert url
  url <- mlb_url %>% gsub("YEAR",input$year,.) %>% gsub("ROLE",tolower(input$role),.)
  
  # Start the Server
  rD <- rsDriver(browser = "firefox")
  remDr <- rD$client
  remDr$navigate(url)
  source <- read_html(remDr$getPageSource()[[1]])
  remDr$close()
  rD[["server"]]$stop()
  
  #grab title data, with post processing
  player_names <- source %>% 
                  html_nodes('.dg-name_display_last_init a') %>% 
                  html_text() %>%  
                  gsub("[\r\n]","",.) %>%
                  factor()
  
  #iterate through possible options, name the columns after the html class
  #add to dataframe
  #initialize data frame with player names
  df <- data.frame("names" = player_names)
  list <- if (input$role == "Hitting") hitting_stats else pitching_stats
  for (i in 1:length(list)) {
    col <- source %>% 
           html_nodes(list[[i]]) %>% 
           html_text() %>%  
           gsub("[\r\n]","",.) %>%
           as.numeric()
    
    df[gsub("td.dg-","",list[[i]])] <- col
  }
  df         
}

# Define server logic 
server <- function(input, output,session) {
    reactive_values <- reactiveValues(df_data = NULL)
    
    observeEvent(c(input$role,
                  input$year
    ), { reactive_values$df_data <- get.data(input)})
    
    observe({
      updateSelectInput(session,"x",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
      updateSelectInput(session,"y",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
    })
  
    #output role
    output$role <- renderText({
        paste(input$role)
    })
    
    #renders mean of data
    output$mean_x <- renderText({
      paste(round(mean(reactive_values$df_data[[gsub("td.dg-","",input_hash[[input$x]])]],rm.na = TRUE),digits = 3))
    })
    
    #renders mean of data
    output$mean_y <- renderText({
      paste(round(mean(reactive_values$df_data[[gsub("td.dg-","",input_hash[[input$y]])]],rm.na = TRUE),digits = 3))
    })
    
    #renders mean of data
    output$cc <- renderText({
      paste(round(cor(reactive_values$df_data[[gsub("td.dg-","",input_hash[[input$x]])]],
                      reactive_values$df_data[[gsub("td.dg-","",input_hash[[input$y]])]]),
                  digits = 3))
    })

    
    #display graph comparing x and y variables
    output$plot <- renderPlot({ 
      ggplot(reactive_values$df_data, aes_string(x=gsub("td.dg-","",input_hash[[input$x]]),y=gsub("td.dg-","",input_hash[[input$y]]))) + 
        geom_point() +
        ggtitle(paste(toString(input$x),"v", toString(input$y),"in the year", toString(input$date))) +
        labs(x = toString(input$x), y = toString(input$y))
    })
    
    #renders the list of players
    output$table <- DT::renderDataTable(reactive_values$df_data, options = list(scrollX = TRUE))
    
}
