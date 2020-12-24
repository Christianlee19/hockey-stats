## Corsi over the course of a season
library(ggplot2)
library(data.table)
library(ggpubr)

setwd("~/Documents/hockey-stats/data")

## get SAT data sorted by game (home team only)
print(load("1215_nhl_home_sat_stats_2018-2019.rsav"))
sat_df = df_cleaned[,c("team", "game", "shots", "sat_for", "sat_against","sat")]


## get basic game stats also sorted game
print(load("1215_nhl_home_summary_stats_2018-2019.rsav"))
basic_df = df_cleaned[,c("game", "wins", "losses", "points", "goal_for", "goal_against", "shots_over_gp", "sa_over_gp")]


## combine and add CF% (cfp)
df = merge(sat_df, basic_df, by="game")
df[,3:13] = sapply(df[,3:13], as.numeric)
df$points = as.character(df$points)
df$wins = as.character(df$wins)
df$cfp = df$sat_for / (df$sat_for + df$sat_against)
df$sp = df$shots_over_gp / (df$shots_over_gp + df$sa_over_gp)


## scatter plot of SOG% and SAT%
df_long = melt(df, measure.vars=c("cfp", "sp"))
df_long$variable = ifelse(df_long$variable == "cfp", "CF%", "SOG%")
dodge = position_dodge(width = .89)

## get df of counts
res_counts = data.frame(table(df$wins), stringsAsFactors = F)
df_long$ss = res_counts$Freq[match(df_long$wins, res_counts$Var1)]
df_long$wins = ifelse(df_long$wins == "1", "Win", "Loss")
df_long$result = paste0(df_long$wins,"\nn=",df_long$ss)

ggplot(df_long, aes(x = result, y = value, fill = variable, color = variable)) +
  geom_violin(position = dodge, alpha=.65) +
  geom_boxplot(position = dodge, alpha=.9, fill="white", outlier.shape = NA, width=.5) + 
  geom_hline(yintercept = .5, linetype = "dashed", color = "red", alpha = .5, size=1) +
  scale_fill_manual(values=c("CF%" = "#185d9e", "SOG%" = "#6a9ccc")) + 
  scale_color_manual(values=c("CF%" = "#185d9e", "SOG%" = "#6a9ccc")) + 
  labs(title = "Corsi and SOGs versus points", subtitle = "2019-2020 full regular season\nn = 1271 home games",
       x="Result", y="Fraction of SA or SOG",
       fill="Statistic",color="Statistic") +
  #stat_compare_means() +
  theme_bw() +
  theme(axis.text = element_text(size=11))

  
## plot correlation between SOG and CF
ggplot(df, aes(cfp, sp)) +
  geom_point() +
  theme_bw() +
  labs(title = "Correlation between SOG% and CF%", 
       subtitle = "2019-2020 full regular season\nn = 1271 home games",
       x="CF%", y="SOG%",
       fill="Statistic",color="Statistic") +
  stat_cor(method="spearman", show.legend  = F,alternative = "greater", size=4.5) +
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed", size=1, alpha=.65)
  
ggplot(df, aes(cfp, sp)) +
  #geom_hex(bins=30) +
  geom_point(color="gray52", alpha=.7) + 
  theme_bw() +
  labs(title = "Correlation between SOG% and CF%", 
       subtitle = "2019-2020 full regular season\nn = 1271 home games",
       x="CF%", y="SOG%",
       fill="Count", color="Count") +
  stat_cor(method="spearman", show.legend  = F,alternative = "greater", size=4.5) +
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed", size=1.25, alpha=.75) +
  geom_hline(yintercept = .5, color="gray45", alpha=.75, linetype="dashed") +
  geom_vline(xintercept = .5, color="gray45", alpha=.75, linetype="dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(fill="dodgerblue")





## ---- logistic regression ---- ##
## multiply fractions by 100 for easier interpretation
df$sp = df$sp * 100
df$cfp = df$cfp * 100
df$wins = factor(df$wins)

## split 80 20
sample_size = floor((nrow(df) * .80))
training_indx = sample(nrow(df), sample_size)
df_train = df[training_indx,]
df_test = df[-training_indx,]

## univariate logistic regression
summary(glm(wins ~ cfp , data = df_train, family = "binomial"))$coefficients
summary(glm(wins ~ sp , data = df_train, family = "binomial"))$coefficients

## test set
mod = glm(wins ~ cfp, data = df_train, family = "binomial")
df_pred = data.frame(predict(mod, df_test, type = "response"), stringsAsFactors = F)
df_pred$prediction = factor(ifelse(df_pred < .5, 0, 1))
confusionMatrix(df_pred$prediction,df_test$wins)


#end