library(rvest) 
library(RSelenium)
library(ggplot2)
library(hash)

#MLB Statistics Sheet
mlb_url <- "http://mlb.mlb.com/stats/sortable.jsp#elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+ROLE&game_type='R'&season=YEAR&season_type=ANY&league_code='MLB'&sectionType=sp&statType=ROLE&page=1&ts=1560404713148"

#hash to convert input to html tags
input.to.tags <- hash(c("G (Games Played)",
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
                      "OPS (OBS + SLG)",
                      "W (Wins)",
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
                      "WHIP (AVG of Walks + Hits Per IP)"),
                    c("dg-g",
                      "dg-ab",
                      "dg-r",
                      "dg-h",
                      "dg-d",
                      "dg-t",
                      "dg-hr",
                      "dg-rbi",
                      "dg-bb",
                      "dg-so",
                      "dg-sb",
                      "dg-cs",
                      "dg-avg",
                      "dg-obp",
                      "dg-slg",
                      "dg-ops",
                      "dg-w",
                      "dg-l",
                      "dg-era",
                      "dg-g",
                      "dg-gs",
                      "dg-sv",
                      "dg-svo",
                      "dg-ip",
                      "dg-h",
                      "dg-r",
                      "dg-er",
                      "dg-hr",
                      "dg-bb",
                      "dg-so",
                      "dg-avg",
                      "dg-whip"))
                      

#retieves data from mlb website
get.data <- function(input) {
  #Convert url
  url <- mlb_url %>% gsub("YEAR",input$year,.) %>% gsub("ROLE",tolower(input$role),.)
  hitting_stats <- list(".dg-g",
                        ".dg-ab",
                        ".dg-r",
                        ".dg-h",
                        ".dg-d",
                        ".dg-t",
                        ".dg-hr",
                        ".dg-rbi",
                        ".dg-bb",
                        ".dg-so",
                        ".dg-sb",
                        ".dg-cs",
                        ".dg-avg",
                        ".dg-obp",
                        ".dg-slg")
 
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
  
  df <- data.frame("names" = player_names)
  
  #iterate through possible options, name the columns after the html class
  list <- if (input$role == "Hitting") hitting_stats else pitching_stats
  datalist <- list()
  for (i in 1:length(list)) {
    col <- source %>% 
           html_nodes(list[[i]]) %>% 
           html_text() %>%  
           gsub("[\r\n]","",.)
    
    col_name <- list[[i]]
    datalist[[i]] <- data.frame(col_name = col)
  }
   
  datalist         
}

# Define server logic 
server <- function(input, output,session) {
    observe({
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
      
      updateSelectInput(session,"x",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
      updateSelectInput(session,"y",choices = if (input$role == "Hitting") hitting_choices else pitching_choices)
    })
  
    #output role
    output$role <- renderText({
        paste(input$role,get.data(input))
    })
    
    #display graph comparing x and y variables
    #x and y will be retrieved from the scraped data by indexing: df[["x"]] and df[["y"]]
    #"x" and "y" hold the converted input values
    
    #renders the list of movies
    #output$table <- DT::renderDataTable(get.data(input), options = list(scrollX = TRUE))
    
}
