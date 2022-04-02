library(shiny)
library(ggplot2)
library(nhlapi)
library(scales)
# library(fmsb)
library(ggradar)
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

get_active_player_data = function(team_name, list_of_df){
  temp_df = list_of_df[[team_name]]
  temp_df$team_name = team_name
  temp_df = temp_df[,c("person.fullName", "person.id", "position.abbreviation", "position.type", "team_name")]
  return(temp_df)
}

full_active_player_list = do.call(rbind, lapply(names(active_team_rosters_list), get_active_player_data, list_of_df=active_team_rosters_list))
#dim(full_active_player_list)
#[1] 832   5


## ** DO NOT DELETE ** ##
#### player data ####
# active_player_stats = nhl_players_seasons(playerIds=full_active_player_list$person.id[1:nrow(full_active_player_list)], seasons = "20212022")
#
# ## get name by id
# active_player_stats = merge(active_player_stats, full_active_player_list, by.x="playerId", by.y="person.id")
#
# ## only keep players with n games
# active_player_stats = active_player_stats[active_player_stats$stat.games > 10,,drop=F]
#
# ## get top n players by position
# active_skater_stats = setDT(active_player_stats[active_player_stats$position.type != "Goalie",,drop=F])[order(-rank(stat.points))]
# #test_goalie = setDT(test[test$position.type == "Goalie",,drop=F])[order(-rank(stat.savePercentage))]
#
# # skaters_to_select = unlist(strsplit(active_skater_stats_select$person.fullName, "' '"))
# skaters_to_select = active_skater_stats[,.SD[1:20], by="position.type"]$person.fullName
#saveRDS(skaters_to_select, file="~/Documents/hockey-stats/bin/hockeystatsapp/data/skaters_to_select.rds")

skaters_to_select = readRDS("data/skaters_to_select.rds")

## filter player data
player_data = nhl_players_seasons(playerNames = skaters_to_select, seasons = "20212022") ## maybe replace this with ids
rownames(player_data) = skaters_to_select

# clean column names
colnames(player_data) = gsub("stat.", "", colnames(player_data))

#### team data ####
team_data = nhl_standings(
  seasons = 2021:2022) %>%
  select(1:3, 7:8, 13)


## **** using more years will require function to combine b/c different column names
# team_data = nhl_standings(
#   seasons = 2018:2022)

team_stats = do.call(rbind, lapply(team_data$teamRecords, "[", 1:ncol(team_data$teamRecords[[1]]))) %>%
  select(team.name, regulationWins, goalsAgainst, goalsScored, points, divisionRank, divisionL10Rank, leagueRank, gamesPlayed, pointsPercentage)

team_stats$division.name = active_team_roster_team_info$division.name[match(team_stats$team.name, active_team_roster_team_info$name)]

# basic player info [data frame of player name, id, and position]
active_player_info = rbindlist(active_team_rosters_list)



## set final variables
df = team_stats
team_vars = sort(unique(colnames(df)))
team_vars = team_vars[!(team_vars %in% c("division.name","team.name"))]




## ------------------------------------------------------------------------------------------------------------------------------
## Server
## ------------------------------------------------------------------------------------------------------------------------------
server = function(input, output) {

  # team_user_selected = reactive({input$team_user_selected})

  # Return the formula text for printing as a caption ----
  # output$user_selected_team = renderText({
  #   team_user_selected()
  # })

  ## subset data by team
  # df_team_specific = df[df$Tm == as.character(input$variable),]
  # df_team_specific = reactive({
  #   x = subset(df, team.name == team_user_selected())
  #   return(x)
  # })



  #### Total player stats ####
  # sktr_x_us = reactive({input$sktr_x_user_selected})
  # sktr_y_us = reactive({input$sktr_y_user_selected})

  # output$sktr_x_us = renderText({
  #   sktr_x_us()
  # })
  #
  # output$tts_y_us = renderText({
  #   sktr_y_us()
  # })


  # get user selected data
  # player_data_filtered = reactive({
  #   sktr_x = sym(input$sktr_x_user_selected)
  #   sktr_y = sym(input$sktr_y_user_selected)
  #   player_data[c(sktr_x, sktr_y), c("stat.points", "stat.goals", "stat.assists",
  #                                           "stat.powerPlayPoints", "stat.powerPlayGoals",
  #                                           "stat.shots", "stat.hits")]
  #   return(iris)
  # })
  
  
  ### Total team stats ####
  # output$sktr_x = renderText({input$sktr_x_user_selected})
  # output$sktr_y = renderText({input$sktr_y_user_selected})
  output$sktr_comparison = renderText({
    x = input$sktr_x_user_selected
    y = input$sktr_y_user_selected
    paste0(x, " vs ", y)
  })

  player_data_filtered = reactive({
    sktr_x = as.character(input$sktr_x_user_selected)
    sktr_y = as.character(input$sktr_y_user_selected)
    
    data = player_data[,c("points", "goals", "assists", "powerPlayPoints",
                            "powerPlayGoals", "shortHandedPoints",
                            "shots", "shotPct", "hits", "blocked",
                            "plusMinus", "timeOnIce", "games")]
    
    # get max value
    max_values = as.numeric(apply(data, 2, max))

    # filter for user selected players
    data = rbind("Leader Totals" = max_values,
                 data[rownames(data) %in% c(sktr_x),],
                 data[rownames(data) %in% c(sktr_y),]) #allows for duplicate selections
    
    # clean column names
    colnames(data) = tolower(gsub("powerPlay", "pp ", colnames(data)))
    colnames(data) = tolower(gsub("shorthanded", "sh ", colnames(data)))
    data = cbind("selection" = c("League Leader", "Player A", "Player B"), data)
    
    return(data)
  })

  
  # output table
  # output$table = renderTable({
  #   
  #   data = player_data_filtered()
  #   char_skater_table_norm = as.character(input$skater_table_norm)
  #   
  #   if(char_skater_table_norm == "Per Game"){
  #     data[,-1] = data[,-1] / data$games
  #     data
  #   } else if(char_skater_table_norm == "Per 60"){
  #     data[,-1] = data[,-1] / data$timeonice * 60
  #     data
  #   }
  #   
  #   return(data[-1,-1])
  # }, bordered = T, spacing = "s", rownames=T, striped=T, digits=1
  # )

  
  #### lollipop plot ####
  output$skater_lollipop_plot = renderPlot({
    
    data = player_data_filtered()
    char_skater_table_norm = as.character(input$skater_table_norm)
    
    if(char_skater_table_norm == "Per Game"){
      data[,-1] = data[,-1] / data$games
      data
    } else if(char_skater_table_norm == "Per 60"){
      data[,-1] = data[,-1] / data$timeonice * 60
      data
    }
    
    # remove rows that should not be included in plot
    # data = data[-1,-1]
    data = data[-1, !colnames(data) %in% c("selection","timeonice")]
    
    # transpose
    data = data.frame(t(data), stringsAsFactors = F)
    data = cbind(rownames(data), data)
    colnames(data) = c("var", "Player A", "Player B")
    
    # reorder
    data = data %>%
      rowwise() %>% 
      mutate(avg_value = mean(c(`Player A`, `Player B`) )) %>% 
      arrange(avg_value) %>%
      mutate(var=factor(var, var)) #set order
    

    # plot
    ggplot(data) +
      geom_point(aes(x=var, y=`Player A`, color="Player A"), alpha=.7, size=5) +
      geom_point(aes(x=var, y=`Player B`, color="Player B"), alpha=.7, size=5) +
      geom_segment(
        aes(x=var, xend=var, y=`Player A`, yend=`Player B`), color="gray40", size=1) +
      theme_bw() +
      labs(y = "Value", x="Category") +
      coord_flip() +
      scale_colour_manual(name=NULL, values=c(`Player A`="#d41102", `Player B`="#08519C")) +
      theme(title = element_text(size=11),
            axis.text = element_text(size=14),
            axis.title = element_text(size=15),
            legend.text = element_text(size=13),
            legend.position = "bottom")
  })
  
  ggplot(df, aes(x = instance, y = total_hits)) +
    geom_point(size = 1) + 
    geom_line()+
    geom_line(aes(x=instance, y = line1, colour="myline1")) +
    geom_vline(xintercept=805) + 
    geom_line(aes(x=instance, y = line2, colour="myline2"))+
    geom_line(aes(x=instance, y = line3, colour="myline3")) +
    scale_colour_manual(name="Line Color",
                        values=c(myline1="red", myline2="blue", myline3="purple"))
  
  #### new radar plot ####
  output$skaterPlot1 = renderPlot({

    data = player_data_filtered()
    data = data[,!colnames(data) %in% c("plusminus", "timeonice", "games")]
    max_values = as.numeric(data[1,-1])
    
    # make fraction of max
    data[,-1] = apply(data[,-1], 2, function(x){x/max(x)}) %>%
      as_tibble()

    # get labels
    axis_labels = paste0(colnames(data[,-1]), "\n(", max_values, ")")

    ggradar(data,
            values.radar = c("0%", "50%", "100%"),
            axis.labels = axis_labels,
            fill = T, fill.alpha = .25,
            grid.line.width = 0.2,
            label.gridline.max = F,
            gridline.min.linetype = "solid", gridline.mid.linetype = "longdash", gridline.max.linetype = "longdash",
            gridline.min.colour = "gray50", gridline.mid.colour = "gray50", gridline.max.colour = "gray50",
            grid.label.size = 5,
            axis.label.offset = 1.175, axis.label.size = 5,
            axis.line.colour = "grey", group.line.width = 1.25,
            group.point.size = 2.5,
            group.colours = c("#e3e3e3", "#d41102", "#08519C"),
            background.circle.colour = "#D7D6D1", background.circle.transparency = 0.1,
            legend.title = "",
            plot.title = "",
            legend.text.size = 14,
            legend.position = "bottom")
  })





  #### Total team stats ####
  # tts_x_us = reactive({input$total_team_stats_x_user_selected})
  # tts_y_us = reactive({input$total_team_stats_y_user_selected})
  # 
  # output$tts_x_us = renderText({
  #   tts_x_us()
  # })
  # 
  #   output$tts_y_us = renderText({
  #   tts_y_us()
  # })



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
        labs(x = charx, y=chary, fill = "Division Rank", title = "Hover Over Points!") +
        theme(title = element_text(size=11),
              axis.text = element_text(size=11),
              axis.title = element_text(size=13),
              legend.title = element_text(size=11), #change legend title font size
              legend.text = element_text(size=10))) %>% 
      config(displayModeBar = F) %>%
      layout(legend = list(
        orientation = "h", y=-.3)
      )
    }
  )



}




## ------------------------------------------------------------------------------------------------------------------------------
## ui
## ------------------------------------------------------------------------------------------------------------------------------
ui = fluidPage(
  navbarPage("Hockey Stats",
    navbarMenu("Player Stats",
      tabPanel("Season Totals",
        fluidRow(
               titlePanel(h1("Live Totals", align = "center")),
               # titlePanel(h3("2021-2022", align = "center")),
               h4(textOutput("sktr_comparison"), align="center"),
               column(3, style='padding-top:1em;',
                      selectInput("sktr_x_user_selected", "Player A:", choices=sort(skaters_to_select),
                                  selected="Connor McDavid"),
                      selectInput("sktr_y_user_selected", "Player B:", choices=sort(skaters_to_select),
                                  selected="Auston Matthews"),
                      h6("*Leauge leader corresponds to the max value from the above list of players"),
                      h6("pp = power play"),
                      h6("sh = short handed")),
               column(6, plotOutput("skaterPlot1")),
               column(3)),
        br(),
        fluidRow(style='padding-bottom:1.2em;',
          column(2, radioButtons("skater_table_norm", "Table Value Type", c("Season Total", "Per Game", "Per 60"))),
          column(9, plotOutput("skater_lollipop_plot")),
          column(1))
          #div(tableOutput("table"), align="center", style='padding-below:1em;'))
        ),
      
      tabPanel("League Leaders",
               "Coming soon."),
    
      tabPanel("Per game",
                "Coming soon.")),

    ## -------------------------------------------------------------------------
    navbarMenu("Team Stats",
      tabPanel("Season Totals",
      fluidRow(
        titlePanel(h1("Live Totals", align = "center")),
        titlePanel(h3("2021-2022", align = "center")),
        br(),
        column(3,
          selectInput("total_team_stats_x_user_selected", "X-axis:", choices=team_vars,
                      selected="goalsScored"),
          selectInput("total_team_stats_y_user_selected", "Y-axis:", choices=sort(unique(colnames(df))),
                      selected="goalsAgainst")),
        column(8, plotlyOutput("teamPlot1")),
        column(1))),
      tabPanel("Per game",
          "Coming soon.")),

      tabPanel("Blog",
                fluidRow(
                  titlePanel(h1("Gallery", align = "center")),
                  titlePanel(h4(tags$a(href="https://medium.com/hockey-stats", "Making sense of the game."), align = "center")),
                  br(),
                  column(3, style='padding:1.2em;',
                         tags$a(img(src = "040122_hc.png", height = "100%", width = "100%"),
                                href="https://medium.com/hockey-stats/comparing-current-nhl-superstars-with-nhl-all-time-greats-650af0ba0f87"),
                         tags$a(h5("Comparing Current NHL Superstars with NHL All-Time Greats", align = "center", style="color:black"),
                                href="https://medium.com/hockey-stats/comparing-current-nhl-superstars-with-nhl-all-time-greats-650af0ba0f87")),
                  column(3, style='padding:1.2em;',
                         tags$a(img(src = "040122_gf_gp_over_time.png", height = "100%", width = "100%"),
                                href="https://medium.com/hockey-stats/how-has-regular-season-nhl-goal-scoring-changed-over-time-733c6b527c8d"),
                         tags$a(h5("How has regular season NHL goal scoring changed over time?", align = "center", style="color:black"),
                                href="https://medium.com/hockey-stats/how-has-regular-season-nhl-goal-scoring-changed-over-time-733c6b527c8d")),
                  column(3, style='padding:1.2em;',
                         tags$a(img(src = "040122_shap.png", height = "100%", width = "100%"),
                                href="https://medium.com/hockey-stats/identifying-the-best-predictors-of-nhl-game-outcomes-using-random-forest-b4c11f46bc97"),
                         tags$a(h5("Identifying the Best Predictors of NHL Game Outcomes Using Random Forest", align = "center", style="color:black"),
                                href="https://medium.com/hockey-stats/identifying-the-best-predictors-of-nhl-game-outcomes-using-random-forest-b4c11f46bc97")),                  
                  column(3, style='padding:1.2em;',
                         tags$a(img(src = "040122_cap_hit.png", height = "100%", width = "100%"),
                                href="https://medium.com/hockey-stats/the-best-and-worst-value-nhl-skaters-will-mitch-marner-top-a-list-5d0667f5a53c"),
                         tags$a(h5("The Best and Worst Value NHL Skaters - Will Mitch Marner Top a List?", align = "center", style="color:black"),
                                href="https://medium.com/hockey-stats/the-best-and-worst-value-nhl-skaters-will-mitch-marner-top-a-list-5d0667f5a53c"))),

              fluidRow(
                column(3, style='padding:1.2em;',
                         tags$a(img(src = "code_example_cropped.png", height = "100%", width = "100%"),
                                href="https://medium.com/hockey-stats/two-approaches-to-scrape-data-from-capfriendly-using-rselenium-and-rvest-ab29c08e314f"),
                         tags$a(h5("Two Approaches to Scrape Data From CapFriendly Using RSelenium and rvest", align = "center", style="color:black"),
                                href="https://medium.com/hockey-stats/two-approaches-to-scrape-data-from-capfriendly-using-rselenium-and-rvest-ab29c08e314f")),
                column(3, style='padding:1.2em;',
                       tags$a(img(src = "code_example_cropped.png", height = "100%", width = "100%"),
                              href="https://medium.com/hockey-stats/how-to-scrape-nhl-com-dynamic-data-in-r-using-rvest-and-rselenium-ba3b5d87c728"),
                       tags$a(h5("How to Scrape (NHL.com) Dynamic Data in R Using rvest and RSelenium", align = "center", style="color:black"),
                              href="https://medium.com/hockey-stats/how-to-scrape-nhl-com-dynamic-data-in-r-using-rvest-and-rselenium-ba3b5d87c728")),
                column(3, style='padding:1.2em;',
                       tags$a(img(src = "040122_sog_cf.png", height = "100%", width = "100%"),
                              href="https://medium.com/hockey-stats/are-shot-attempts-and-shots-on-goal-meaningful-predictors-of-nhl-game-outcomes-not-really-f8f8d16811bf"),
                       tags$a(h5("Are Shot Attempts and Shots on Goal Alone Meaningful Predictors of NHL Game Outcomes? Not Really.", align = "center", style="color:black"),
                              href="https://medium.com/hockey-stats/are-shot-attempts-and-shots-on-goal-meaningful-predictors-of-nhl-game-outcomes-not-really-f8f8d16811bf")),
                column(3, style='padding:1.2em;',
                       tags$a(img(src = "040122_cf_rel_bars.png", height = "100%", width = "100%"),
                              href="https://medium.com/hockey-stats/advanced-hockey-stats-101-corsi-part-1-of-4-29d0a9fb1f95"),
                       tags$a(h5("Advanced Hockey Stats 101: Corsi (Part 1 of 4)", align = "center", style="color:black"),
                              href="https://medium.com/hockey-stats/advanced-hockey-stats-101-corsi-part-1-of-4-29d0a9fb1f95"))
                    ),
              br()
              
              )
        )
   )








#### Run the application #####
shinyApp(ui = ui, server = server)
