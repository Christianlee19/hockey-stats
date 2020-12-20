## Corsi over the course of a season
library(ggplot2)
library(data.table)

setwd("Documents/hockey-stats/data")

## get SAT data sorted by game (home team only)
print(load("1215_nhl_home_sat_stats_2018-2019.rsav"))
sat_df = df_cleaned[,c("team", "game", "shots", "sat_for", "sat_against","sat")]


## get basic game stats also sorted game
print(load("1215_nhl_home_summary_stats_2018-2019.rsav"))
basic_df = df_cleaned[,c("game", "wins", "losses", "points", "goal_for", "goal_against")]


## combine and add CF% (cfp)
df = merge(sat_df, basic_df, by="game")
df[,3:11] = sapply(df[,3:11], as.numeric)
df$cfp = df$sat_for / (df$sat_for + df$sat_against)


## can we reframe this as basic binary classification problem?
## logistic regression


## add CF% 
df_cleaned[4:12] = sapply(df_cleaned[4:12],as.numeric)
df_cleaned$corsi_for = df_cleaned$sat_for / (df_cleaned$sat_for + df_cleaned$sat_against)


## Plot 1
#y-axis = CF%
#x-axis = game
#color = win/loss or points
df$wins = ifelse(df$wins == 1, "Win", "Loss")

ggplot(df, aes(x = game, y = cfp, color = wins)) +
  geom_point() +
  scale_color_manual(values=c("Win"="blue", "Loss"="gray70")) + 
  labs(title="NHL team wins against CF%",
       subtitle="2018-2019 season\nFull 82 game season",
       x="CF%",y="Wins") +
  theme(axis.text=element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank())


ggplot(df, aes(x = points, y = cfp, group = points)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25, height = 0, shape = 1, alpha = .5) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "red", alpha = .5) +
  labs(title = "CF% versus points", subtitle = "2019-2020 full regular season",
       x="Points", y="CF%") +
  theme_bw()
  
  
  
  


#