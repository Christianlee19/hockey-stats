library(shiny)
library(ggplot2)
library(nhlapi)
library(scales)
library(ggradar)
library(dplyr)
library(data.table)
library(plotly)
library(RColorBrewer)
library(formattable)
library(ggpubr)
# library(shinybrowser)


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
#[1] 832   5 (2022)
#[1] 803   5 (23.02.19)


# load list of skaters (created below) 
skaters_to_select = readRDS("data/skaters_to_select.rds") # doing this b/c it takes to long to load all players

# create season variable (eventually this will be replaced with a user-selected variable)
season_end_to_use = as.numeric(gsub("(.+)-(.+)-(.+)", "\\1", Sys.Date()))
title_season_to_use = as.character(paste(season_end_to_use-1, season_end_to_use, sep="-"))
season_to_use = as.character(paste0(season_end_to_use-1, season_end_to_use))
seasons_to_select_from = (season_end_to_use - 1):season_end_to_use


## ** DO NOT DELETE ** ##
## use this to generate a list of top scorers and avoid slow steps
#### player data ####
# active_player_stats = nhl_players_seasons(playerIds=full_active_player_list$person.id[1:nrow(full_active_player_list)], seasons = season_to_use) #very slow
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
# top50_forwards = active_skater_stats[active_skater_stats$position.type == "Forward",] %>% slice(1:50)
# top25_defensemen = active_skater_stats[active_skater_stats$position.type == "Defenseman",] %>% slice(1:25)
# skaters_to_select = rbind(top50_forwards, top25_defensemen) %>% select(playerId, person.fullName, team_name)
#                       
# # save  
# saveRDS(skaters_to_select, file="~/Documents/hockey-stats/bin/hockeystatsapp/data/skaters_to_select.rds")
# rm(top50_forwards, top25_defensemen, skaters_to_select)


## filter player data
player_data = nhl_players_seasons(playerIds = skaters_to_select$playerId, seasons = season_to_use)
rownames(player_data) = skaters_to_select$person.fullName
player_data$person.fullName = skaters_to_select$person.fullName

# clean column names
colnames(player_data) = gsub("stat.", "", colnames(player_data))





#### team data ####
team_data = nhl_standings(
  seasons = season_to_use) %>%
  select(1:3, 7:8, 13)


# team_stats = do.call(rbind, lapply(team_data$teamRecords, "[", 1:ncol(team_data$teamRecords[[1]]))) %>%
#   select(team.name, regulationWins, goalsAgainst, goalsScored, points, divisionRank, divisionL10Rank, leagueRank, gamesPlayed, pointsPercentage)

get_combined_team_stats = function(i, df){
  df = df$teamRecords[[i]] %>% 
    select(team.name, regulationWins, goalsAgainst, goalsScored, points, divisionRank, divisionL10Rank, leagueRank, gamesPlayed, pointsPercentage)
  return(df)
}

team_stats = do.call(rbind, lapply(1:length(team_data$teamRecords), get_combined_team_stats, team_data))
# dim(team_stats)
# [1]  32 10
team_stats$divisionL10Rank = as.numeric(team_stats$divisionL10Rank)
team_stats$divisionRank= as.character(team_stats$divisionRank) #leave this as character for team plot
team_stats$leagueRank = as.numeric(team_stats$leagueRank)

team_stats$division.name = active_team_roster_team_info$division.name[match(team_stats$team.name, active_team_roster_team_info$name)]
rm(team_data)




# basic player info [data frame of player name, id, and position]
active_player_info = rbindlist(active_team_rosters_list)


## set final variables
team_vars = sort(unique(colnames(team_stats)))
team_vars = team_vars[!(team_vars %in% c("division.name","team.name"))]


# today_schedule = nhl_schedule_today()[[1]]
# today_games = today_schedule[[8]]$games[[1]] %>%
#   select(c(gamePk, teams.home.team.name, teams.away.team.name, teams.home.score, teams.away.score, gameDate, status.abstractGameState)) %>%
#   mutate(gameDate = as.numeric(gsub("(.+)T([0-9][0-9]):([0-9][0-9]):(.+)", "\\2\\3", gameDate))) %>%
#   mutate(gameDate = ifelse(gameDate >= 1800, paste0(gameDate - 1700, "pm"), 
#                            ifelse(gameDate < 1700, paste0(gameDate - 500, "am"), paste0(gameDate - 500, "pm")))) %>%
#   mutate(gameDate = gsub("(.+)([0-9][0-9].+)", "\\1:\\2", gameDate))






#### game data ####
#"https://statsapi.web.nhl.com/api/v1/schedule"
# add "see yesterday's stats"
today_schedule = nhl_schedule_today()[[1]]

#******** (test) today_schedule = nhl_schedule_date_range(startDate = "2022-05-18", endDate = "2022-05-18")[[1]]

if(today_schedule$totalGames >= 1){
  today_games = today_schedule[[8]]$games[[1]] %>%
    select(c(gamePk, teams.home.team.name, teams.away.team.name, teams.home.score, teams.away.score, gameDate, status.abstractGameState)) %>%
    mutate(gameDate = gsub("(.+)T([0-9][0-9]):([0-9][0-9]):(.+)", "\\2:\\3", gameDate)) %>%
    mutate(gameDate = as.POSIXct(gameDate, tz = "UTC", format = "%H:%M")) %>%
    mutate(gameDate = format(gameDate, tz="America/Chicago", usetz=F)) %>%
    mutate(gameDate = gsub("(.+) (.+)", "\\2", gameDate)) %>%
    mutate(gameDate = format(strptime(gameDate, "%H:%M:%S"), "%I:%M%p"))
  colnames(today_games)[-1] = c("Home", "Away", "Home Goals",	"Away Goals", "Status (cst)", "Time")
  
} else{
  ## get nearest date that is not current date
  all_game_dates = as.Date(nhl_schedule_seasons((season_end_to_use-1):season_end_to_use)[[1]]$dates[[1]])
  all_game_dates = all_game_dates[all_game_dates != Sys.Date()]
  date_to_use = all_game_dates[which.min(abs(all_game_dates - Sys.Date()))]

  ## get schedule as usual
  today_schedule = nhl_schedule(startDate = date_to_use, endDate = date_to_use)[[1]]
  today_games = today_schedule[[8]]$games[[1]] %>%
    select(c(gamePk, teams.home.team.name, teams.away.team.name, teams.home.score, teams.away.score, gameDate, status.abstractGameState)) %>%
    mutate(gameDate = gsub("(.+)T([0-9][0-9]):([0-9][0-9]):(.+)", "\\2:\\3", gameDate)) %>%
    mutate(gameDate = as.POSIXct(gameDate, tz = "UTC", format = "%H:%M")) %>%
    mutate(gameDate = format(gameDate, tz="America/Chicago", usetz=F)) %>%
    mutate(gameDate = gsub("(.+) (.+)", "\\2", gameDate)) %>%
    mutate(gameDate = format(strptime(gameDate, "%H:%M:%S"), "%I:%M%p"))
  
  #today_games = expand.grid(NA, NA, NA, NA, NA, NA, NA, stringsAsFactors=F)
  colnames(today_games) = c("gamePk", "Home", "Away", "Home Goals",	"Away Goals", "Status (cst)", "Time")
}

today_games$`Status (cst)` = ifelse(today_games$`Status (cst)` == "Preview", today_games$Time, today_games$`Status (cst)`)
today_games$Time = NULL
today_games$Home = active_team_roster_team_info$teamName[match(today_games$Home, active_team_roster_team_info$name)]
today_games$Away = active_team_roster_team_info$teamName[match(today_games$Away, active_team_roster_team_info$name)]

## update today_games in case it is wrong
# names(nhl_games_linescore(today_games$gamePk))









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
            axis.text = element_text(size=14, color="gray10"),
            axis.title = element_text(size=15),
            legend.text = element_text(size=13),
            legend.position = "bottom")
  })



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

  
  
  #### bar plot ####
  output$skater_bar_plot = renderPlot({
    
    data = player_data[1:15, c("points", "goals", "assists", "powerPlayPoints",
                               "powerPlayGoals", "shortHandedPoints",
                               "shots", "shotPct", "hits", "blocked",
                               "plusMinus", "person.fullName")]
    
    metric_to_use = sym(input$metric_user_selected)
    char_metric_to_use = as.character(input$metric_user_selected)
    # upper_char_metric_to_use = paste0(toupper(substr(char_metric_to_use, 1, 1)), substr(char_metric_to_use, 2, nchar(char_metric_to_use)))
    
    data = data[order(data[[char_metric_to_use]], decreasing = T),]
    data$person.fullName = factor(data$person.fullName, levels=rev(unique(data$person.fullName)))
    
    ggplot(data, aes(y = person.fullName, x = !!metric_to_use)) + 
      geom_bar(stat = "identity", color = "#2f47bd", fill = "#D5DAF1", size=.65, width=.85) +
      theme_bw() +
      labs(x = char_metric_to_use, y = "skater") +
      theme(title = element_text(size=10),
            axis.text = element_text(size=14, color="gray10"),
            axis.title = element_text(size=14))
    
  })

  
  #### small bar plots ####
  output$skater_bar_plot_goals = renderPlot({
    
    data = player_data[1:5,]
    data = data[order(data$goals, decreasing = T),]
    data$person.fullName = factor(data$person.fullName, levels=unique(data$person.fullName))
    
    ggplot(data, aes(x = person.fullName, y = goals)) + 
      geom_bar(stat = "identity", color = "#2f47bd", fill = "#D5DAF1", size=.65, width=.85) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      theme_bw() +
      labs(y = "goals", x = "skater") +
      theme(title = element_text(size=10),
            axis.text = element_text(size=14, color="gray10"),
            axis.title.x = element_blank(),
            axis.title = element_text(size=14))
    
  })
  
  output$skater_bar_plot_assists = renderPlot({
    
    data = player_data[1:5,]
    data = data[order(data$assists, decreasing = T),]
    data$person.fullName = factor(data$person.fullName, levels=unique(data$person.fullName))
    
    ggplot(data, aes(x = person.fullName, y = assists)) + 
      geom_bar(stat = "identity", color = "#2f47bd", fill = "#D5DAF1", size=.65, width=.85) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      theme_bw() +
      labs(y = "assists", x = "skater") +
      theme(title = element_text(size=10),
            axis.text = element_text(size=14, color="gray10"),
            axis.title.x = element_blank(),
            axis.title = element_text(size=14))
    
  })
  
  output$skater_bar_plot_powerPlayPoints = renderPlot({
    
    data = player_data[1:5,]
    data = data[order(data$powerPlayPoints, decreasing = T),]
    data$person.fullName = factor(data$person.fullName, levels=unique(data$person.fullName))
    
    ggplot(data, aes(x = person.fullName, y = powerPlayPoints)) + 
      geom_bar(stat = "identity", color = "#2f47bd", fill = "#D5DAF1", size=.65, width=.85) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      theme_bw() +
      labs(y = "power play points", x = "skater") +
      theme(title = element_text(size=10),
            axis.text = element_text(size=14, color="gray10"),
            axis.title.x = element_blank(),
            axis.title = element_text(size=14))
    
  })


  ## -----------------------------------------------------------------------------------
  #### Games stats ####

  ## table
  output$live_game_table = renderFormattable({
    
  customRange = c(0, max(suppressWarnings(max(today_games$`Away Goals`)), suppressWarnings(max(today_games$`Home Goals`)))) # custom min / max values
  colors = csscolor(gradient(as.numeric(c(customRange, today_games$`Home Goals`)), "white", "#f2e200"))
  colors = colors[-(1:2)] ## remove colors for min/max
  
  colors2 = csscolor(gradient(as.numeric(c(customRange, today_games$`Away Goals`)), "white", "#5a9157"))
  colors2 = colors2[-(1:2)] ## remove colors for min/max
  
  
  fmt = formatter("span", 
                  style = function(x){
                    style(display = "block",
                          padding = "0 4px",
                          `border-radius` = "4px",
                          `background-color` = colors)})
  
  fmt2 = formatter("span", 
                   style = function(x){
                     style(display = "block",
                           padding = "0 4px",
                           `border-radius` = "4px",
                           `background-color` = colors2)})
  
  formattable(today_games[,-1], list(
    `Home Goals` = fmt,
    `Away Goals` = fmt2))
  })


  output$live_game_plot = renderPlot({
    
    if(as.character(input$game_user_selected) != "NA vs NA"){
      
      data = today_games %>%
        mutate(user_selection_key = paste0(today_games$Home, " vs ", today_games$Away))
      data = data[data$user_selection_key == as.character(input$game_user_selected),,drop=F]
  
      ## get liver player stats
      get_live_player_stats = function(i, df_box){
        df_box_i = df_box[[3]][[i]]
        if(is.null(df_box_i[3][[1]]$type)){
        
        } else if(!is.null(df_box_i[3][[1]]$type) & df_box_i[3][[1]]$type %in% c("Forward","Defenseman")){
          df_box_stats = data.frame(t(unlist(df_box_i[4][[1]]$skaterStats)), stringsAsFactors = F)
          df_box_stats = df_box_stats[,colnames(df_box_stats) %in% c("goals","assists","powerPlayGoals","powerPlayAssists","shots","hits","blocked","plusMinus")]
          df_box_stats$player_name = df_box_i[[1]]$fullName
          df_box_stats$type = df_box_i[3][[1]]$type
          df_box_stats$team_name = df_box_i[[1]]$currentTeam$name
          return(df_box_stats)
          
        } else if(df_box_i[3][[1]]$type == "Goalie") {
          # df_box_stats = data.frame(t(unlist(df_box_i[4][[1]]$goalieStats)), stringsAsFactors = F)
          # df_box_stats = df_box_stats[,colnames(df_box_stats) %in% c("shots","saves","savePercentage")]
          # df_box_stats$player_name = df_box_i[[1]]$fullName
          # df_box_stats$type = df_box_i[3][[1]]$type
          # df_box_stats$team_name = df_box_i[[1]]$currentTeam$name
          # return(df_box_stats)
          
        }else{ #sometimes there are "Unknown" ... probably when player is moved on or off line up
          # print(paste(i, " --> unknown"))
        }
      } 
      
      get_home_away_pstats = function(n, game_id){
        home_or_away_boxscore = nhl_games(game_id, "boxscore")[[1]][[2]][[n]] # 2 = home, 1 = away
        
        if(length(home_or_away_boxscore[[3]]) > 0){
          home_or_away_player_stats = do.call(rbind, lapply(1:length(home_or_away_boxscore[[3]]), get_live_player_stats, home_or_away_boxscore))
          home_or_away_player_stats$home_or_away = ifelse(n == 1, "Away", "Home")
          home_or_away_player_stats$team_name = ifelse(n == 1, data$Away, data$Home)
          return(home_or_away_player_stats)
        } 
        else{
          output$live_game_message = renderText({
            paste("Waiting on the API to update.",
                  "Please come back later to see the plots.", sep="\n")})
          return(NULL)
        }
      }
      
      combined_game_stats = do.call(rbind, lapply(1:2, get_home_away_pstats, game_id = data$gamePk))
      
      
      if(!is.null(combined_game_stats)){
        
        combined_game_stats[,1:8] = sapply(combined_game_stats[,1:8], as.numeric)
        combined_game_stats = combined_game_stats %>%
          mutate(points = goals + assists) %>%
          arrange(points) %>%
          mutate(player_name = gsub("(^[a-zA-Z])(.+) (.+)", "\\3.\\1", player_name)) %>%
          mutate(player_name = factor(player_name, player_name))
        
        ## rename with short form
        #combined_game_stats$team_name = active_team_roster_team_info$teamName[match(combined_game_stats$team_name, active_team_roster_team_info$name)]
        
        ## add Home and Away labels
        #combined_game_stats$home_or_away = ifelse(combined_game_stats$team_name %in% data$Home, "Home", "Away")
        
        ## make data long to plot
        melt_combined_game_stats = reshape2::melt(setDT(combined_game_stats), measure.vars=c("points", "goals", "assists", "shots", "hits", "blocked", "plusMinus"), 
                    id.vars=c("player_name","type", "team_name", "home_or_away"))
        # melt_combined_game_stats$home_or_away = factor(melt_combined_game_stats$home_or_away, levels=c("Home", "Away"))
        
  
        home = melt_combined_game_stats[melt_combined_game_stats$home_or_away == "Home",]
        away = melt_combined_game_stats[melt_combined_game_stats$home_or_away == "Away",]
        
        home = ggplot(home, aes(x=variable, y=player_name, fill=value, label=value)) +
          geom_tile(color = "gray80", lwd = .25, linetype = 1, alpha = .9) +
          scale_fill_gradient2(low = "#075AFF", mid = "#FFFFFF", high = "#FF0000") +
          geom_text(data = home[home$value != 0,], color = "gray15") + 
          facet_wrap(~ team_name, strip.position=c("right")) +
          labs(y="Player", x="Statistic", fill="Count") +
          theme(strip.background = element_rect(fill="gray95", color="gray75"),
                strip.text = element_text(size=13, color="black", face="bold"), 
                panel.background = element_rect(fill = "white", color="gray75"),
                axis.text = element_text(size=12, color="gray10"),
                axis.text.x = element_text(angle=45, hjust=1, size=13),
                axis.title = element_text(size=14),
                axis.title.x = element_blank(),
                legend.position = "none")
          
        away = ggplot(away, aes(x=variable, y=player_name, fill=value, label=value)) +
          geom_tile(color = "gray80", lwd = .25, linetype = 1, alpha = .9) +
          scale_fill_gradient2(low = "#075AFF", mid = "#FFFFFF", high = "#FF0000") +
          geom_text(data = away[away$value != 0,], color = "gray15") + 
          facet_wrap(~ team_name, strip.position=c("right")) +
          labs(y="Player", x="Statistic", fill="Count") +
          theme(strip.background = element_rect(fill="gray95", color="gray75"),
                strip.text = element_text(size=13, color="black", face="bold"), 
                panel.background = element_rect(fill = "white", color="gray75"),
                axis.text = element_text(size=12, color="gray10"),
                axis.text.x = element_text(angle=45, hjust=1, size=13),
                axis.title = element_text(size=14),
                legend.position = "none")
          
        ggarrange(home, away, ncol=1, common.legend = TRUE, legend="right")
      }
    } 
  })

  # output$live_game_message = renderText({
  #   ifelse(is.null(output$live_game_plot), "API is not yet updated. Come back later.", NULL)
  # })



  ## -----------------------------------------------------------------------------------
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

  output$teamPlot1 = renderPlotly({

    ## get vars and cleaned titles
    testx = sym(input$total_team_stats_x_user_selected)
    testy = sym(input$total_team_stats_y_user_selected)
    charx = get_cleaned_titles(as.character(input$total_team_stats_x_user_selected))
    chary = get_cleaned_titles(as.character(input$total_team_stats_y_user_selected))

    ggplotly(
      ggplot(data=team_stats, aes(x=!!testx,
                          y=!!testy,
                          group=team.name)) +
        geom_point(aes(fill=divisionRank), color="black", size=4, pch=21) +
        scale_fill_manual(values=brewer.pal(8, "Blues")) +
        geom_abline(intercept = 0, slope = 1, alpha=.35, linetype="dashed", lwd = .5) +
        theme_bw() +
        labs(x = charx, y=chary, fill = "Division Rank", title = "Hover Over Points!") +
        theme(title = element_text(size=10),
              axis.text = element_text(size=11, color="gray10"),
              axis.title = element_text(size=13),
              legend.title = element_text(size=11), #change legend title font size
              legend.text = element_text(size=10))) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(
        orientation = "h", y=-.3)
      )
    }
  )
  
  
  ## team tile plot
  ## TODO: create a scale for each variable for the alpha
  
  output$team_plot_tile = renderPlot({
    
    team_stats_tile = team_stats
    team_stats_tile$leagueRank = as.numeric(team_stats_tile$leagueRank)
    team_stats_tile = team_stats_tile[order(team_stats_tile$divisionRank, team_stats_tile$leagueRank),]
    team_levels = unique(team_stats_tile$team.name)
    
    team_stats_melt = reshape2::melt(team_stats, id.vars = c("team.name","division.name"), measure.vars = c("divisionRank","leagueRank","gamesPlayed","points","regulationWins"))
    team_stats_melt$team.name = factor(team_stats_melt$team.name, levels = rev(team_levels))
    
    team_stats_melt$value = as.numeric(team_stats_melt$value)
    
    ggplot(team_stats_melt, aes(y = team.name, x = variable, label = value, fill = variable)) +
      geom_tile(color="white", size=0.35, alpha=.25) +
      geom_text(size=4.25) +
      theme_bw() +
      labs(x = "Metric", y = "Team") +
      # coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      theme(
        # strip.background = element_rect(fill="gray95", color="gray75"),
        # strip.text = element_text(size=12, color="black"),
        # panel.background = element_rect(fill = "white", color="gray75"),
        axis.text = element_text(size=13, color="gray10"),
        axis.text.x = element_text(angle=45, hjust=1, size=12),
        axis.title = element_text(size=14),
        legend.position = "none")
  
      
      ## 02/19/2023 ** tried adding get_width but no plot was showing...
      # ggplot(team_stats_melt, aes(y = team.name, x = variable, label = value, fill = variable)) +
      #   geom_tile(color="white", size=0.35, alpha=.25) +
      #   geom_text(size=4.25) +
      #   theme_bw() +
      #   labs(x = "Metric", y = "Team") +
      #   coord_flip() +
      #   scale_fill_brewer(palette = "Set1") +
      #   # facet_wrap(~division.name, scale = "free") +
      #   theme(
      #     # strip.background = element_rect(fill="gray95", color="gray75"),
      #     # strip.text = element_text(size=12, color="black"),
      #     # panel.background = element_rect(fill = "white", color="gray75"),
      #     axis.text = element_text(size=13, color="gray10"),
      #     axis.text.x = element_text(angle=45, hjust=1, size=12),
      #     axis.title = element_text(size=14),
      #     legend.position = "none")
  }, height=600) 
}


## ------------------------------------------------------------------------------------------------------------------------------
## ui
## ------------------------------------------------------------------------------------------------------------------------------
ui = fluidPage(
  navbarPage("Hockey Stats",
             
## -------------------------------------------------------------------------        
    navbarMenu("Player Stats",
      tabPanel("Live Totals",
        fluidRow(
               titlePanel(h1("Live Totals", align = "center")),
               h4(textOutput("sktr_comparison"), align="center"),
               div(style='padding-top:1.25em;',
               column(3,
                      selectInput("sktr_x_user_selected", "Player A:", choices=sort(skaters_to_select$person.fullName),
                                  selected="Connor McDavid"),
                      selectInput("sktr_y_user_selected", "Player B:", choices=sort(skaters_to_select$person.fullName),
                                  selected="Auston Matthews"),
                      # selectInput("season_user_selected", "Season", choices = paste0(seasons_to_select_from - 1, seasons_to_select_from)),
                      div(style="font-size:1.1rem",
                      "*Leauge leader corresponds to the max value from the above list of players.",
                      br(),
                      "pp = power play, sh = short handed.")),
               column(6, plotOutput("skaterPlot1")),
               column(3))),
        
        br(),
        fluidRow(style='padding:1em 0 2em 0',
          column(2, radioButtons("skater_table_norm", "Table Value Type:", c("Season Total", "Per Game", "Per 60"))),
          column(9, plotOutput("skater_lollipop_plot")),
          column(1))
        ),


      tabPanel("League Leaders",
               fluidRow(style='padding:1em 0 2em 0',
                        titlePanel(h1("League Leaders", align = "center")),
                        h4("Top 15 Skaters By Metric", align="center"),
                        div(style='padding-top:.75em;',
                        column(2, selectInput("metric_user_selected", "Metric:", choices=c("points", "goals", "assists", "powerPlayPoints", "shortHandedPoints"),
                                              selected="points")),
                        column(9, plotOutput("skater_bar_plot")),
                        column(1))),
               fluidRow(
                 titlePanel(h2("Overview", align = "center")),
                 h4("Top 5 Skaters", align = "center"),
                 column(4, style='padding:1.2em;',
                        plotOutput("skater_bar_plot_goals")),
                 column(4, style='padding:1.2em;',
                        plotOutput("skater_bar_plot_assists")),
                 column(4, style='padding:1.2em;',
                        plotOutput("skater_bar_plot_powerPlayPoints"))))),
               

    ## -------------------------------------------------------------------------
    navbarMenu("Team Stats",
      tabPanel("Season Totals",
      fluidRow(
        titlePanel(h1("Live Totals", align = "center")),
        titlePanel(h3(title_season_to_use, align = "center")),
        br(),
        column(3,
          # selectInput("total_team_stats_season_user_selected", "Year:", choices=team_vars,
          #             selected="goalsScored")
          selectInput("total_team_stats_x_user_selected", "X-axis:", choices=team_vars[team_vars != "divisionRank"],
                      selected="goalsScored"),
          selectInput("total_team_stats_y_user_selected", "Y-axis:", choices=team_vars[team_vars != "divisionRank"],
                      selected="goalsAgainst")),
        column(8, plotlyOutput("teamPlot1")),
        column(1)),
      
      fluidRow(
        div(style='padding-top:1.25em;',
        titlePanel(h2("Rankings", align = "center")),
        column(1),
        column(10, plotOutput("team_plot_tile")),
        column(1))))),

    ## -------------------------------------------------------------------------
    tabPanel("Live Games",
             fluidRow(
               column(8, offset=2,
                      h1("Overview", style='padding-bottom:.5em'),
                      formattableOutput("live_game_table", width = "100%"),
                      align="center", style='padding:1em;')),
             fluidRow(
               column(4,
                      selectInput("game_user_selected", "Select Game:", 
                                  choices=paste0(today_games$Home, " vs ", today_games$Away)))),
             fluidRow(style='padding-bottom: 1em',
                      column(8, offset=2, style='padding:0 1em', align="center",
                             verbatimTextOutput("live_game_message"),
                             plotOutput("live_game_plot", height = "800px"))),
             
    ),

    
    ## -------------------------------------------------------------------------
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
