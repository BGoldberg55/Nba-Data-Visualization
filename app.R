#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(rstatix)
dummy <- "all"
datafiles <- list.files("./data", pattern = 'csv')
#Real wd "C:/Users/bengo/OneDrive/Documents"
getwd()
setwd("./data")
nbafiles <- list()
for (x in 1:length(datafiles)) {
  nbafiles[[x]] <- read.csv(datafiles[x], encoding="UTF-8")
}
#setwd("C:/Users/bengo/OneDrive/Documents")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
      conditionalPanel(condition="input.tabselected==1",
          radioButtons("filter_award", "Filter by award:",
                       c("No Filter" = "all",
                         "DPOY" = "dpoy",
                         "MIP" = "mip",
                         "MVP" = "nba mvp",
                         "6MOY" = "smoy",
                         "ABA MVP" = "aba mvp",
                         "ABA ROY" = "aba roy"
                       )),
            radioButtons(
              "filter_pos", "Filter by",
              c("Position" = "pos",
                "Player" = "player"
              )
            )
          ),
      conditionalPanel(condition="input.tabselected==2",
          selectInput("chose_stat", "Choose your Statistic:",
                      c("VORP" = "vorp",
                        "BPM" = "bpm",
                        "WS" = "ws")),
          textInput("player_1", "Choose the First Player:", "LeBron James"),
          textInput("player_2", "Choose the Second Player:", "Michael Jordan")
        )
      ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel( type = "tabs", id = "tabselected",
                       tabPanel(h4("Award Analysis"), value = 1, plotOutput("test")),
                       tabPanel(h4("Player Comparison"), value = 2, tableOutput("comp"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$test <- renderPlot({
      nbafiles[[10]] <- nbafiles[[10]] %>%
        filter(award == input$filter_award | dummy == input$filter_award)
      if(input$filter_pos == "pos"){
      inter_data <- nbafiles[[10]] %>% filter(winner == TRUE) %>%
       group_by(player_id, player, season) %>% summarise(n = n()) %>% arrange(desc(n))
      right_join(nbafiles[[1]], inter_data, by = 'player_id') %>%
        filter(season.x == season.y) %>% filter(!is.na(n)) %>%
        group_by(pos) %>% summarise(Total_Awards = sum(n)) %>% ggplot() +
        geom_bar(aes(x = fct_reorder(pos, Total_Awards, .desc = TRUE),
                     y = Total_Awards, fill = pos), stat = "identity") +
        labs(title = "NBA Award Analysis", x = "Position",
             y = "Number of Awards", fill = "Position") +
        theme(legend.key.size = unit(1.2, 'cm'), legend.title = element_text(size=24),
              legend.text = element_text(size=10),
              plot.title = element_text(size = 40, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text.x= element_text(size = 12),
              axis.text.y= element_text(size = 12))
      }
      else {
        nbafiles[[10]] %>% filter(winner == TRUE) %>%
          group_by(player_id, player) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
          head(.,10) %>% ggplot() +
          geom_bar(aes(x = fct_reorder(player, n, .desc = TRUE),
                       y = n, fill = player), stat = "identity") +
          labs(title = "NBA Award Analysis", x = "Player",
               y = "Number of Awards", fill = "Player") +
          scale_x_discrete(guide = guide_axis(n.dodge=2)) +
          theme(legend.key.size = unit(1.2, 'cm'), legend.title = element_text(size=24),
                legend.text = element_text(size=10),
                plot.title = element_text(size = 40, hjust = 0.5),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                axis.text.x= element_text(size = 12),
                axis.text.y= element_text(size = 12))
      }
    })
    output$comp <- renderTable ({
      #All of in the selection function allows it to handle reactives
      nba_filt <- nbafiles[[1]] %>% 
        filter(player == input$player_1 | player == input$player_2) %>%
        select(all_of(input$chose_stat), player)
      #Col needs to be renamed here so it creates a column that is always in the dataset
      #Since t_test cant handle reactives.
      names(nba_filt)[1] <- "stat"
       t_test(stat ~ player, data = nba_filt)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
