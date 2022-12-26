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
library(gghighlight)
dummy <- "all"
datafiles <- list.files("./data", pattern = 'csv')
#Real wd "C:/Users/bengo/OneDrive/Documents"
#Need to the WD here so we can get the files easily
setwd("./data")
nbafiles <- list()
#For loop puts all the files in this list. It goes down the list of files and assigns them a number. Ex: nbafiles[[4]]
#Order of the excel files does matter. In the github, the order of the excel files is shown
for (x in 1:length(datafiles)) {
  nbafiles[[x]] <- read.csv(datafiles[x], encoding="UTF-8")
}
# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("NBA Analysis"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
#Panels here are condtional, so when the user changes the main panel tile, the sidebar changes
#Panel has two sets of buttons for filtering by award and either shwoing positions or players
      conditionalPanel(condition="input.tabselected==1",
          radioButtons("filter_award", "Filter by award:",
                       c("No Filter" = "all",
                         "DPOY" = "dpoy",
                         "MIP" = "mip",
                         "MVP" = "nba mvp",
                         "6MOY" = "smoy",
                         "ROY" = "nba roy",
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
#Panel gives drop down menu and two text inputs to put player's name into to compare
      conditionalPanel(condition="input.tabselected==2",
          selectInput("chose_stat", "Choose your Statistic:",
                      c("VORP" = "vorp",
                        "BPM" = "bpm",
                        "WS" = "ws",
                        "TS" = "ts_percent",
                        "OWS" = "ows",
                        "DWS" = "dws",
                        "PER" = "per",
                        "OBPM" = "obpm",
                        "DBPM" = "dbpm",
                        "PPG" = "pts_per_game",
                        "MPG" = "mp_per_game",
                        "FG Per Game" ="fg_per_game",
                        "FGA Per Game" =  "fga_per_game",
                        "FG %" = "fg_percent",
                        "EFG" = "e_fg_percent",
                        "FT Made Per Game" = "ft_per_game",
                        "FTA Per Game"  = "fta_per_game",
                        "FT %" = "ft_percent",
                        "ORG Per Game" = "orb_per_game",
                        "DRB Per Game" =  "drb_per_game",
                        "Total RB Per Game" =  "trb_per_game",
                        "Assists Per Game"  = "ast_per_game",
                        "Steals Per Game" =  "stl_per_game",
                        "Blocks Per Game" = "blk_per_game",
                        "Turnovers Per Game" =  "tov_per_game",
                         "Fouls Per Game" =  "pf_per_game" )),
          textInput("player_1", "Choose the First Player:", "LeBron James"),
          textInput("player_2", "Choose the Second Player:", "Michael Jordan")
        ),
#Panel gives a drop down menu for selecting the stat and a text input to pick a player
#Also, it gives a button to the user can select if they want the mean, median or max of the stat to be compared
      conditionalPanel(condition="input.tabselected==3",
                       selectInput("chose_stat_all", "Choose your Statistic:",
                                   c("VORP" = "vorp",
                                     "BPM" = "bpm",
                                     "WS" = "ws",
                                     "TS" = "ts_percent",
                                     "OWS" = "ows",
                                     "DWS" = "dws",
                                     "PER" = "per",
                                     "OBPM" = "obpm",
                                     "DBPM" = "dbpm",
                                     "PPG" = "pts_per_game",
                                     "MPG" = "mp_per_game",
                                     "FG Per Game" ="fg_per_game",
                                     "FGA Per Game" =  "fga_per_game",
                                     "FG %" = "fg_percent",
                                     "EFG" = "e_fg_percent",
                                     "FT Made Per Game" = "ft_per_game",
                                     "FTA Per Game"  = "fta_per_game",
                                     "FT %" = "ft_percent",
                                     "ORG Per Game" = "orb_per_game",
                                     "DRB Per Game" =  "drb_per_game",
                                     "Total RB Per Game" =  "trb_per_game",
                                     "Assists Per Game"  = "ast_per_game",
                                     "Steals Per Game" =  "stl_per_game",
                                     "Blocks Per Game" = "blk_per_game",
                                     "Turnovers Per Game" =  "tov_per_game",
                                     "Fouls Per Game" =  "pf_per_game" )),
      textInput("player_1_all", "Choose the First Player:", "LeBron James"),
      radioButtons(
        "stat_filter", "How Should the Statistic be Manipulated?",
        c("Median (Robust Average)" = 2,
          "Mean (Average)" = 3,
          "Maximum" = 4
        )
      )
      )
      ),

  # Show the tabs. Br(), creates breaks, so the text and plot aren't on top of each other
        mainPanel(width = 500, height = 200,
          tabsetPanel( type = "tabs", id = "tabselected",
                       tabPanel(h4("Award Analysis"), value = 1, plotOutput("test")),
                       tabPanel(h4("Player Comparison"), value = 2, htmlOutput("comp"),
                                br(), br(), plotOutput("stat_graph")),
                       tabPanel(h4("Player Comparison Versus Everyone"), value = 3,
                                plotOutput("comp_all"))
          )
        )
    )
)

# Define server logic
server <- function(input, output) {
  #Create the plot for award analysis
    output$test <- renderPlot({
#Filter out the right dataset, so only the awards selected are there. Dummy is for no filter
      nbafiles[[10]] <- nbafiles[[10]] %>%
        filter(award == input$filter_award | dummy == input$filter_award)
      #Only show the positions on x Axis and not player names
      if(input$filter_pos == "pos"){
  #Create a dataframe that has the player with the number of awards they have won
      inter_data <- nbafiles[[10]] %>% filter(winner == TRUE) %>%
       group_by(player_id, player, season) %>% summarise(n = n()) %>% arrange(desc(n))
      #Join with another dataset to get the players position
      #Joining by season removes duplicate rows that have the same player but different season
      right_join(nbafiles[[1]], inter_data, by = c("player_id", "season")) %>%
        #Get the total number of awards for each position and make it into a dataframe
        filter(!is.na(n)) %>% group_by(pos) %>%
        summarise(Total_Awards = sum(n)) %>% ggplot() +
        #Graph it with x Axis in descending order based on number of awards won
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
      #Make the graph but instead of grouping by position we keep it with players
      #The head(.,10) gives us only the top 10 players with the most awards won in our dataframe
      #For graph clarity reasons
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
    #Create the text for player comparisons
    output$comp <- renderText({
      #All of in the selection function allows it to handle reactives
      #Join the datasets so all the stats are together in one dataset
      nba_filt <- full_join(nbafiles[[1]], nbafiles[[12]], by = c("season", "player", "g"))
      #Create a dataset with only the two players selected in it and the stat and the games played that season
      nba_filt <- nba_filt %>% 
        filter(player == input$player_1 | player == input$player_2) %>%
        select(all_of(input$chose_stat), player, g)
      #Col needs to be renamed here so it creates a column that is always in the dataset
      #Since t_test cant handle reactives.
      names(nba_filt)[1] <- "stat"
      #Weight the stat by games played that season
      nba_filt <- nba_filt %>% mutate(weight  = stat * g)
      #T test for the two players
      stat_test <- t_test(weight ~ player, data = nba_filt)
      #Table with both max and average of the chosen stat
      max_stat <- nba_filt %>% group_by(player) %>% summarize(maxstat = max(stat),
              avgstat = median(stat))
      #Text if p value > .05 (No significance)
      if(stat_test$p > 0.05){
        #Reactive texts using paste function to put it all together
      str1 <-  paste(input$player_1, "has an average", input$chose_stat, "of",
                     max_stat[max_stat$player == input$player_1 ,3],
              input$player_2, "has an average", input$chose_stat, "of",
              max_stat[max_stat$player == input$player_2 ,3])
      str2 <- paste(input$player_1, "does not have a significantly different", 
                    input$chose_stat,"from", input$player_2)
        
      }
      #Text if the p value < 0.05 (Significant)
      else {
        #Reactive texts using paste function to put it all together
       str1 <- paste(input$player_1, "has an average", input$chose_stat, "of",
                     max_stat[max_stat$player == input$player_1 ,3],
              input$player_2, "has an average", input$chose_stat, "of",
              max_stat[max_stat$player == input$player_2 ,3])
        str2 <- paste(input$player_1, "does have a significantly different", 
                      input$chose_stat,"from", input$player_2)
      }
      #Create the HTML output, so the text lines can be seperated by line breaks
      HTML(paste(h3(str1), h3(str2), sep = '<br/>'))
      
    })
    #Create the plot for player comparison
    output$stat_graph <- renderPlot({
      #All of in the selection function allows it to handle reactives
      #Dataset with all the stats in one place
      nba_filt <- full_join(nbafiles[[1]], nbafiles[[12]], by = c("season", "player", "g"))
      #Create a dataset with only the two players selected in it and the stat and the games played that season
      nba_filt <- nba_filt %>% 
        filter(player == input$player_1 | player == input$player_2) %>%
        select(all_of(input$chose_stat), player, g)
      #Col needs to be renamed here so it creates a column that is always in the dataset
      #Since t_test cant handle reactives.
      names(nba_filt)[1] <- "stat"
      nba_filt <- nba_filt %>% mutate(weight  = stat * g)
      #Box and Whisker plot of the chosen stat for each player
      ggplot(data = nba_filt, aes(x = player, y = stat)) + geom_boxplot() +
        geom_jitter(height = 0) + 
        labs(title = paste("Box Plot of Player", input$chose_stat, "Comparison"), 
        x = "Player", y = input$chose_stat) +
        theme(plot.title = element_text(size = 40, hjust = 0.5),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x= element_text(size = 12),
      axis.text.y= element_text(size = 12))
    })
    #Plot for player comparison versus everyone
    output$comp_all<- renderPlot({
      #Dataset with all the stats together
      nba_filt <- full_join(nbafiles[[1]], nbafiles[[12]], by = c("season", "player", "g"))
      #Create a datset with players that have 246+ games played and only the chosen stat and the player
      nba_filt <- nba_filt %>% group_by(player) %>% mutate(total_g = sum(g)) %>%
        filter(total_g >= 246) %>% select(all_of(input$chose_stat_all), player)
      #Rename the column so the reactives can be used same as above
      names(nba_filt)[1] <- "stat"
      #Create a table with the player and their median, mean, and maximum of the chosen stat
      nba_filt <- nba_filt %>% group_by(player) %>% summarize(avg_stat = median(stat), 
      mean_stat = mean(stat), max_stat = max(stat), sum_stat = sum(stat)) %>% 
        filter(!is.na(avg_stat))
      #Change the name of column with the manipulation of the stat chosen by the user
      #Ex: Mean, Median or Maximum
      #Similar to above, this allows the ggplot function to be able to work with reactives
      names(nba_filt)[as.numeric(input$stat_filter)] <- "stat"
      #Plot it
      ggplot(data = nba_filt) +
        geom_point(aes(x = player, y = stat) , col = "red", size = 3) +
        #GGhighlight creates the player who is selected's point on the graph to be big and red with a label with their name on it
        gghighlight(player == input$player_1_all,
                    unhighlighted_params = list(linewidth = 1, colour = alpha("black",0.4), size = 1.5)) +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(size = 30, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text.y= element_text(size = 12)) +
        labs(title = paste("Scatter Plot of", input$player_1_all , input$chose_stat_all, "Versus All Other Players"), 
             x = "Player", y = input$chose_stat_all)
      
       
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
