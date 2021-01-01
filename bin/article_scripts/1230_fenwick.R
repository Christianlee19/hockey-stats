## Corsi over the course of a season
library(ggplot2)
library(data.table)
library(ggpubr)
library(ggrepel)

setwd("~/Documents/hockey-stats/data")


## load adv stats from: https://www.hockey-reference.com/leagues/NHL_2020_skaters-advanced.html
stats = read.csv("adv_hockey_stats.csv", header=T, skip=1, stringsAsFactors=F)
stats = stats[rowSums(is.na(stats)) == 0,c(2:14)]
colnames(stats)[1] = "name"


## combine subset of basic stats
bstats = read.csv("basic_stats_2020.csv", header=T, skip=1, stringsAsFactors=F)
bstats = bstats[,c(2,7:10,22)]
colnames(bstats) = c("name","goals","assists", "points","pm","toi")
df = merge(stats, bstats, by="name")


## keep unique and clean
df = df[!duplicated(df$name),]
colnames(df)[1:13] = c("name","age","team","pos","gp","cf","ca","cp","cprel","ff","fa","fp","ffrel")
df$name = gsub("\\\\.*","",df$name)

## keep players with more than 20 games
df = df[df$gp >= 20,]

## order by fp
df = df[order(df$fp, decreasing=T),]

## points per 60
df$pp60 = df$points / df$toi * 60
df$ppg = df$points / df$gp



## ---- ff% and cf% correlation ---- ##
df2 = df[order(df$fp, decreasing = T),]
df2$label = NA
df2$label[1:5] = df2$name[1:5]
df2$label[787:791] = df2$name[787:791]
  
ggplot(df2, aes(fp, cp, label=label)) +
  # geom_hex(bins=30) +
  geom_point(color="gray25", alpha=.85, shape=1, size=1.5) +
  theme_bw() +
  labs(title = "Correlation between FF% and CF%", 
       subtitle = "2019-2020 full regular season\nn = 791",
       x="Fenwick For %", y="Corsi For %",
       fill="Count",color="Count") +
  
  geom_label_repel(data=df2[1:5,],
                   size=4, alpha=.85, force = 10,
                   nudge_y = 10,
                   ylim=c(65,NA),
                   segment.size  = 0.2,
                   show.legend  = F) +
  
  geom_label_repel(data=df2[787:791,],
                   size=4, alpha=.85, force = 10,
                   nudge_y = -10,
                   ylim=c(NA,35),
                   xlim=c(NA,60),
                   segment.size  = 0.2,
                   show.legend  = F) +
  
  geom_hline(yintercept = 50, color="gray45", alpha=.65, linetype="dashed") +
  geom_vline(xintercept = 50, color="gray45", alpha=.65, linetype="dashed") +
  scale_x_continuous(limits=c(25,75)) +
  scale_y_continuous(limits=c(25,75)) +
  stat_cor(method="pearson", show.legend  = F,alternative = "greater", size=5) +
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed", size=1, alpha=.65)




## ---- team data ---- ##
df = read.csv("2018_corsi_team.csv", header=T, stringsAsFactors = F)
colnames(df)[c(3,9,12)] = c("wins","CF%","FF%")



## ---- regression ---- ##
## multiply fractions by 100 for easier interpretation
df$wins = factor(df$wins)

## split 80 20
# sample_size = floor((nrow(df) * .80))
# training_indx = sample(nrow(df), sample_size)
# df_train = df[training_indx,]
# df_test = df[-training_indx,]

## univariate logistic regression
summary(lm(Points ~ `CF%`, data=df))
summary(lm(Points ~ `FF%`, data=df))


df_melt = melt(df, measure.vars=c("CF%","FF%"))
df_melt = df_melt[order(df_melt$Points, decreasing = T),]
df_melt$label = NA
df_melt$label[1:6] = df_melt$Team[1:6]
df_melt$label[57:62] = df_melt$Team[57:62]


ggplot(df_melt, aes(x=value, y=Points, color=variable, label=label)) +
  geom_point(size=1.65, alpha=.95) +
  geom_smooth(se=F) +
  labs(title = "Points versus FF% and CF%", 
       subtitle = "2018-2019 full regular season\nn = 31",
       x="Advanced stat %", y="Points",
       color="Statistic") +
  geom_label_repel(data=df_melt[1:6,],
                   size=3.5, alpha=.85, force = 10,
                   nudge_y = 15,
                   ylim=c(110,NA),
                   segment.size  = 0.2,
                   show.legend  = F) +
  
  geom_label_repel(data=df_melt[57:62,],
                   size=3.5, alpha=.85, force = 10,
                   nudge_y = -17,
                   ylim=c(NA,70),
                   segment.size  = 0.2,
                   show.legend  = F) +
  
  geom_hline(yintercept = median(df_melt$Points), color="gray45", alpha=.65, linetype="dashed") +
  geom_vline(xintercept = 50, color="gray45", alpha=.65, linetype="dashed") +
  scale_color_manual(values=c("CF%"="Red", "FF%"="dodgerblue3")) +
  scale_x_continuous(breaks=seq(45, 55, 2.5), limits=c(45,55)) +
  scale_y_continuous(limits=c(40, 140)) +
  stat_cor(method="spearman", show.legend  = F,alternative = "greater", size=4) +
  theme_bw() +
  theme(axis.text = element_text(size=10))





#
