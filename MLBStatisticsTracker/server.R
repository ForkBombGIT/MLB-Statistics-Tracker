library(rvest) 
library(RSelenium)
library(ggplot2)
library(hash)
library(plyr)

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
pitching_stats <- list(".dg-w",
                       ".dg-l",
                       ".dg-era",
                       ".dg-g",
                       ".dg-gs",
                       ".dg-sv",
                       ".dg-svo",
                       ".dg-ip",
                       ".dg-h",
                       ".dg-r",
                       ".dg-er",
                       ".dg-hr",
                       ".dg-bb",
                       ".dg-so",
                       ".dg-avg",
                       ".dg-whip")

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
  df <- data.frame("names" = player_names)
  list <- if (input$role == "Hitting") hitting_stats else pitching_stat
  for (i in 1:length(list)) {
    col <- source %>% 
           html_nodes(list[[i]]) %>% 
           html_text() %>%  
           gsub("[\r\n]","",.)
    
    df[gsub("td.dg-","",list[[i]])] <- col
  }
  df         
}

# Define server logic 
server <- function(input, output,session) {
    observe({
      updateSelectInput(session,"x",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
      updateSelectInput(session,"y",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
    })
  
    #output role
    output$role <- renderText({
        paste(input$role)
    })
    
    #display graph comparing x and y variables
    #x and y will be retrieved from the scraped data by indexing: df[["x"]] and df[["y"]]
    #"x" and "y" hold the converted input values
    
    #renders the list of movies
    output$table <- DT::renderDataTable(get.data(input), options = list(scrollX = TRUE))
    
}
