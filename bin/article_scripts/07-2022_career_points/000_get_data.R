library(ggplot2)
library(nhlapi)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(ggpubr)

setwd("~/Documents/hockey-stats/data/")


## get full list of players
df_draft = nhl_drafts(2000:2009)[[2]]

combine_df = function(i){
  df_draft_combined = df_draft[[i]]$picks
  df_class_combined = do.call(rbind, df_draft_combined)
  return(df_class_combined)
}

df_players = do.call(rbind, lapply(1:length(df_draft), combine_df))


## map players to stats
player_ids = data.frame(nhlapi:::util_prepare_player_ids(unique(df_players$prospect.fullName)), stringsAsFactors = F)
colnames(player_ids) = "player_id"
#player_ids$player_name = rownames(player_ids)

df = nhl_players_allseasons(playerIds = player_ids$player_id) %>%
  select(playerId, season, stat.assists, stat.goals, stat.overTimeGoals, stat.points, stat.shots, stat.games, league.name, team.name, seasonStart) %>%
  filter(league.name == "National Hockey League")


## get basic player info
df_player_info = nhl_players(playerIds = unique(df$playerId))
df_player_info = df_player_info[,c("fullName","id","birthDate","height","weight","primaryPosition.name","primaryPosition.abbreviation")]
df_stats_info = merge(df, df_player_info, by.x = "playerId", by.y = "id")


## remove players outside of date range (they were included if they had duplicated names)
df_stats_info = df_stats_info[df_stats_info$seasonStart >= 2000,]


## get birth year
df_stats_info$birth_year = as.numeric(gsub("(.+)-(.+)-(.+)", "\\1", df_stats_info$birthDate))
df_stats_info = df_stats_info[!is.na(df_stats_info$seasonStart),]


## extra filtering
min_by = 2000 - 20
max_by = 2009 - 20
df_stats_info = df_stats_info[df_stats_info$birth_year >= min_by & df_stats_info$birth_year <= max_by,]


## get age
df_stats_info = setDT(df_stats_info)[, c("num_seasons", "age") :=  list(.N, seasonStart - birth_year), by="playerId"]



## save
save(df_stats_info, file = "07-2022_df_stats_info.rsav")




## filter
df2 = df_stats_info[df_stats_info$primaryPosition.abbreviation != "G",]
dim(df2)


summary(df2$num_seasons)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   12.00   11.32   15.00   21.00 



##
df3 = setDT(df2)[, .SD[which.max(stat.points)], by="playerId"]
df3$cut_range = cut_number(df3$num_seasons, 5)
df3$cut = as.numeric(df3$cut_range)

df3$cut = ifelse(df3$cut == 1, "0-20%",
                 ifelse(df3$cut == 2, "20-40%",
                 ifelse(df3$cut == 3, "40-60%",
                 ifelse(df3$cut == 4, "60-80%", "80-100%"))))


gg_p1 = ggplot(df3, aes(x=age, color=cut, fill=cut, group=cut)) +
  geom_density(alpha=.25, size=.75) +
  theme_bw() + 
  labs(x = "Age at Career-High Points", y = "Density", color = "Career length\npercentile", fill = "Career length\npercentile",
       title = "Distribution of Career-High Points Across NHL Skaters",
       subtitle = "2000-2009 Draft Years\nn = 804") +
  scale_x_continuous(breaks = seq(20, 40, 2)) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        title = element_text(size = 15),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))



df3[, num_in_group := .N, by="cut"]


head(df3[order(-rank(age))])
head(df3[order(-rank(num_seasons))])



png("figures/07-2022_dist_career_high_points.png", width = 800, height = 600)
  gg_p1
dev.off()







## el fin