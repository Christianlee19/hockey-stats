#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(nhlapi)
# library(highcharter)
library(dplyr)
library(data.table)
library(plotly)
library(RColorBrewer)


## ------------------------------------------------------------------------------------------------------------------------------
## Data
## ------------------------------------------------------------------------------------------------------------------------------
# basic team info
active_team_roster_team_info = nhl_teams_rosters() %>%
  select(name, abbreviation, teamName, division.name, conference.name)
#dim(active_team_roster_team_info)
#[1] 32  5


# team rosters
active_team_rosters_list = nhl_teams_rosters() %>%
  select(roster.roster) %>%
  as.list() %>%
  do.call(c,.) %>%
  setNames(active_team_roster_team_info$name)


#### team data ####
team_data = nhl_standings(
  seasons = 2021:2022) %>%
  select(1:3,7:8,13)


## **** using more years will require function to combine b/c different column names
# team_data = nhl_standings(
#   seasons = 2018:2022)

team_stats = do.call(rbind, lapply(team_data$teamRecords, "[", 1:ncol(team_data$teamRecords[[1]]))) %>%
  select(team.name, regulationWins, goalsAgainst, goalsScored, points, divisionRank, divisionL10Rank, leagueRank, gamesPlayed, pointsPercentage)

team_stats$division.name = active_team_roster_team_info$division.name[match(team_stats$team.name, active_team_roster_team_info$name)]

# basic player info [data frame of player name, id, and position]
active_player_info = rbindlist(active_team_rosters_list)

# player season stats
# active_player_data = nhl_players_seasons(playerIds = active_player_info$person.id,
#                                          season = "20212022")



df = team_stats
team_vars = sort(unique(colnames(df)))
team_vars = team_vars[!(team_vars %in% c("division.name","team.name"))]



## ------------------------------------------------------------------------------------------------------------------------------
## Server
## ------------------------------------------------------------------------------------------------------------------------------
server = function(input, output) {
  
  team_user_selected = reactive({input$team_user_selected})

  # Return the formula text for printing as a caption ----
  output$user_selected_team = renderText({
    team_user_selected()
  })
  
  ## subset data by team
  # df_team_specific = df[df$Tm == as.character(input$variable),]
  df_team_specific = reactive({
    x = subset(df, team.name == team_user_selected())
    return(x)
  })
  
  
  
  #### Total team stats ####
  tts_x_us = reactive({input$total_team_stats_x_user_selected})
  tts_y_us = reactive({input$total_team_stats_y_user_selected})

  output$tts_x_us = renderText({
    tts_x_us()
  })
  
    output$tts_y_us = renderText({
    tts_y_us()
  })
    


  #### Team plots ####
  get_cleaned_titles = function(i){return(tolower(gsub('([[:upper:]])', ' \\1', i)))}


    output$teamPlot1 <- renderPlotly({
    
    ## get vars and cleaned titles
    testx = sym(input$total_team_stats_x_user_selected)
    testy = sym(input$total_team_stats_y_user_selected)
    charx = get_cleaned_titles(as.character(input$total_team_stats_x_user_selected))
    chary = get_cleaned_titles(as.character(input$total_team_stats_y_user_selected))
    
    ggplotly(
      ggplot(data=df, aes(x=!!testx,
                          y=!!testy,
                          group=team.name)) +
        geom_point(aes(fill=divisionRank), color="black", size=4, pch=21) +
        scale_fill_manual(values=brewer.pal(8, "Blues")) +
        geom_abline(intercept = 0, slope = 1, alpha=.35, linetype="dashed", lwd = .5) +
        theme_bw() +
        labs(x = charx, y=chary, fill="Division Rank") +
        theme(title = element_text(size=14),
              axis.text = element_text(size=11),
              axis.title = element_text(size=13),
              legend.position="right")
    )
    }
  )


  output$teamPlot2 <- renderPlotly({
  ggplotly(
    ggplot(data=df, aes(x=gamesPlayed, y=points, group=team.name,
          text=paste0(team.name, "\nGoals for = ", gamesPlayed, "\nGoals against = ", points))) +
    geom_point(aes(color=pointsPercentage), size=3) +
    scale_color_continuous(low="darkred", high="red", guide="legend") +
    geom_abline(intercept = 0, slope = 1, alpha=.35, linetype="dashed", lwd = .5) +
    theme_bw() +
    labs(x = "Games Played", y="Points") +
    theme(title = element_text(size=14),
          axis.text = element_text(size=11),
          axis.title = element_text(size=13)),
  tooltip = "text")
  }
)
 
# output$teamPlot = renderHighchart({
#   hchart(df, "scatter", hcaes(x=goalsScored, y=goalsAgainst, group=team.name)),
#   hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
#               name = "Fit", id = "fit")
# })

}




## ------------------------------------------------------------------------------------------------------------------------------
## ui
## ------------------------------------------------------------------------------------------------------------------------------
ui = fluidPage(
  navbarPage("Hockey Stats",
             tabPanel("Player Stats",
                      titlePanel(h3("placeholder", align = "center"))),
             
              navbarMenu("Team Stats",
                         tabPanel("Season Totals",
                          fluidRow(
                            titlePanel(h1("Season Totals", align = "center")),
                            titlePanel(h3("2020-2021", align = "center")),
                            column(3, 
                              selectInput("total_team_stats_x_user_selected", "X-axis:", choices=team_vars,
                                          selected="goalsScored"),
                              selectInput("total_team_stats_y_user_selected", "Y-axis:", choices=sort(unique(colnames(df))),
                                          selected="goalsAgainst")),
                            column(7, plotlyOutput("teamPlot1")),
                            column(2, titlePanel(h3("placeholder", align = "center"))))),
                         tabPanel("Per game")),
               navbarMenu("More",
               tabPanel("Sub-Component A"),
               tabPanel("Sub-Component B")))
)
  










# ui <- fluidPage(
# 
#   # App title ----
#   titlePanel("Hockey Stats"),
# 
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
# 
#     # Sidebar panel for inputs ----
#     sidebarPanel(
# 
#       # Input: Selector for variable to plot against mpg ----
#       # selectInput("variable", "Team:",
#       #             c("Cylinders" = "cyl",
#       #               "Transmission" = "am",
#       #               "Gears" = "gear")),
#       selectInput("team_user_selected", "Team:", choices=sort(active_team_roster_team_info$name)),
# 
#       # Input: Checkbox for whether outliers should be included ----
#       checkboxInput("outliers", "Show outliers", TRUE)
# 
#     ),
# 
#     # Main panel for displaying outputs ----
#     mainPanel(
# 
#       # Output: Formatted text for caption ----
#       h3(textOutput("caption")),
#       # h4(textOutput("caption2")),
# 
# 
#       # plot
#       plotlyOutput("teamPlot")
#       # plotOutput("teamPlot2")
#       #highchartOutput("teamPlot")
# 
# 
#     )
#   )
# )

















## test
# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
# server <- function(input, output) {
#   
#   # Compute the formula text ----
#   # This is in a reactive expression since it is shared by the
#   # output$caption and output$mpgPlot functions
#   formulaText <- reactive({
#     paste("mpg ~", input$variable)
#   })
#   
#   # Return the formula text for printing as a caption ----
#   output$caption <- renderText({
#     formulaText()
#   })
#   
#   # Generate a plot of the requested variable against mpg ----
#   # and only exclude outliers if requested
#   output$mpgPlot <- renderPlot({
#     boxplot(as.formula(formulaText()),
#             data = mpgData,
#             outline = input$outliers,
#             col = "#75AADB", pch = 19)
#   })
#   
# }

# Define UI for miles per gallon app ----
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Miles Per Gallon"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Selector for variable to plot against mpg ----
#       selectInput("variable", "Variable:",
#                   c("Cylinders" = "cyl",
#                     "Transmission" = "am",
#                     "Gears" = "gear")),
#       
#       # Input: Checkbox for whether outliers should be included ----
#       checkboxInput("outliers", "Show outliers", TRUE)
#       
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Formatted text for caption ----
#       h3(textOutput("caption")),
#       
#       # Output: Plot of the requested variable against mpg ----
#       plotOutput("mpgPlot")
#       
#     )
#   )
# )
## end test






# Run the application 
shinyApp(ui = ui, server = server)

