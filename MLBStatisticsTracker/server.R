library(rvest) 
library(ggplot2)

#MLB Statistics Sheet
mlb_url <- "http://mlb.mlb.com/stats/sortable.jsp#elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+ROLE&game_type='R'&season=YEAR&season_type=ANY&league_code='MLB'&sectionType=sp&statType=ROLE&page=1&ts=1560404713148"

get.data <- function(input) {
  url <- mlb_url %>% gsub("YEAR",input$year,.) %>% gsub("ROLE",input$role,.)
  webpage <- read_html(url)
}

# Define server logic 
server <- function(input, output,session) {
    observe({
      battingChoices <- list("G (Games Played)",
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
      pitchingChoices <- list("W (Wins)",
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
      
      updateSelectInput(session,"x",choices = if (input$role == "Batting") battingChoices else pitchingChoices)
      updateSelectInput(session,"y",choices = if (input$role == "Batting") battingChoices else pitchingChoices)
    })
    output$role <- renderText({
        paste(input$role)
    })
}
