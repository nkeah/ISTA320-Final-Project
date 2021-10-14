# load libraries
library(tidyverse)
library(knitr)
library(shiny)
library(lubridate)

# read data in
lfc_managers <- read.csv("data/lfc_managers.csv")
lfc_man_games <- read.csv("data/lfc_manager_games.csv")
lfc_ratio <- read.csv("data/lfc_win_ratios.csv")

year_options <- lfc_man_games %>%
  distinct(Season) %>%
  arrange(Season)
team_options <- lfc_man_games %>%
  distinct(home) %>%
  arrange(home)

# define UI for application
ui <- fluidPage (
  titlePanel("Liverpool Football Club Manager Dashboard"),
  
  # sidebar info
  sidebarLayout(
    sidebarPanel(
      h3("Liverpool Football Club Statistics"),
      p("Click on tabs to see the different visualizations regarding the LFC"),
      HTML('Data from the <a href="https://data.world/ryanes/liverpool-english-league-matches">LFC Data</a>')
    ),
    
    # different plots
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Managers",
                                   selectInput("plotType",
                                               "Select visualization to display:",
                                               choices = c("win_rate",
                                                           "total_games_played",
                                                           "total_goals")),
                                   plotOutput("manager_plot")),
                          tabPanel("Goals Scored Per Season",
                                   selectInput("season_select",
                                               "Select Season to display:",
                                               choices = year_options),
                                   plotOutput("seasonal_plot")),
                          tabPanel("Comparison Against Teams",
                                   selectInput("team_select",
                                               "Select team to compare:",
                                               choices = team_options),
                                   plotOutput("team_plot")))
    )
  )
)
# server logic
server <- function(input, output) {
  # color blind color palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # if choice is win rate
  output$manager_plot <- renderPlot({
    if(input$plotType == "win_rate") {
      lfc_managers %>%
        ggplot(aes(x = Name, y = win_perc, fill = Nationality)) +
        geom_bar(stat = "identity") +
        coord_flip() + 
        labs(y = "Win Percentage(%)", x = "Manager's Name", 
             title = "Manager's Win Rate",
             subtitle = "Liverpool Matches From 1893-2017") +
        scale_fill_manual(values=cbPalette) +
        theme_classic()
    }
    # if choice is total_games_played
    else if(input$plotType == "total_games_played") {
      lfc_managers %>%
        ggplot(aes(x = Name, y = P, fill = Nationality)) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        scale_fill_manual(values=cbPalette) +
        labs(y = "Total Games Played", x = "Manager's Name",
             title = "Manager's Total Games Played",
             subtitle = "Liverpool Matches From 1893-2017") +
        theme_classic()
    }
    # if choice is total_goals
    else {
      data_to_plot <- lfc_man_games %>%
        group_by(mngr) %>%
        summarize(sum(lfc_goals)) %>%
        rename(Name = mngr)
      data_to_plot <- data_to_plot[!(data_to_plot$Name == "character(0)"),]
      merged_data <- merge(lfc_managers, data_to_plot, by = "Name")
      merged_data %>%
        ggplot(aes(x = Name, y = `sum(lfc_goals)`, fill = Nationality)) +
        geom_bar(stat = "identity") + 
        coord_flip() +
        scale_fill_manual(values=cbPalette) +
        labs(y = "Total Goals Scored", x = "Manager's Name",
             title = "Manager's Total Goals Scored",
             subtitle = "Liverpool Matches From 1893-2017") +
        theme_classic()
    }
  })
  output$seasonal_plot <- renderPlot({
    seasonal_data <- lfc_man_games %>%
      filter(Season == input$season_select)
    seasonal_data %>%
      ggplot(aes(x = as.Date(Date), y = lfc_goals, color = mngr)) + 
      geom_line(size = 1) + 
      geom_line(aes(x = as.Date(Date), y = totgoal, color = "Total Goals"),
                linetype = "dashed",
                size = 1) +
      geom_point() +
      scale_color_manual(values=cbPalette) +
      labs(x = "Date", y = "Goals Scored",
           title = "Liverpool Goals Throughout the Season",
           subtitle = "Liverpool Matches From 1893-2016") +
      theme_classic() +
      guides(fill=guide_legend(title="Line Style"))
  })
  output$team_plot <- renderPlot({
    team_data <- lfc_man_games %>%
      filter(visitor == input$team_select)
    team_data %>%
      ggplot(aes(x = mngr, y = hgoal, fill = result)) +
      geom_bar(stat = "identity") + 
      coord_flip() + 
      scale_fill_manual(values=cbPalette) +
      labs(y = "Total Liverpool Goals", x = "Manager",
           title = "Manager's Defending Anfield(home field)",
           subtitle = "Liverpool Matches From 1893-2017",
           caption = "A = Away Win\n D = Draw\n H = Home Win") +
      theme_classic()
      
  })
}

shinyApp(ui = ui, server = server)
