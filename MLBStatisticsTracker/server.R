library(rvest) 
library(ggplot2)

#MLB Statistics Sheet
mlb_url <- "http://mlb.mlb.com/stats/sortable.jsp#elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+hitting&game_type='R'&season=2019&season_type=ANY&league_code='MLB'&sectionType=sp&statType=hitting&page=1&ts=1560386674503&playerType=QUALIFIER&sportCode='mlb'&split=&team_id=&active_sw=&position=&page_type=SortablePlayer&sortOrder='desc'&sortColumn=avg&results=&perPage=50&timeframe=&last_x_days=&extended=0"

get.data <- function() {
}

# Define server logic 
server <- function(input, output) { 
    output$role <- renderText({
        paste(input$role)
    })
}
