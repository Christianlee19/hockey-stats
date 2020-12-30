library(ggplot2)
library(ggrepel)
library(lubridate)
library(cowplot)
library(ggpubr)
library(formattable)

setwd("~/Documents/hockey-stats/data/")

## load data
print(load("1225_nhl_2019-2020_salary_cap.rsav"))
df = df[,c(1:12,19:20)]
df[,c(4:11)] = sapply(df[,c(4:11)], as.numeric)
head(df)


## remove empty data
min_games = floor(max(df$GP) / 10)
df_cleaned = df[df$GP > min_games,]
df_cleaned = df_cleaned[nchar(df_cleaned$TOI) > 2,]


## fix columns
colnames(df_cleaned)[9] = "plus_minus"
df_cleaned$name = gsub("([0-9]*)\\. (.+)", "\\2", df_cleaned$PLAYER)
df_cleaned$cap_hits = as.numeric(gsub('[$,]', '', df_cleaned$CAP.HIT))
df_cleaned$salary = as.numeric(gsub('[$,]', '', df_cleaned$SALARY))


## remove players with duplicate entries
df_cleaned = df_cleaned[order(df_cleaned$cap_hits, decreasing = T),]
df_cleaned$id = paste(df_cleaned$name, df_cleaned$TEAM, df_cleaned$POS)
df_cleaned = df_cleaned[!duplicated(df_cleaned$id),]


## get variables
df_cleaned$points_per_game = as.numeric(df_cleaned$P.GP)
df_cleaned$avg_toi_min = period_to_seconds(ms(df_cleaned$TOI)) / 60
df_cleaned$total_min = df_cleaned$avg_toi_min * df_cleaned$GP
df_cleaned$points_per_60 = as.numeric(df_cleaned$P) / as.numeric(df_cleaned$total_min) * 60


## add labels
df_cleaned$rel = df_cleaned$cap_hits / df_cleaned$points_per_60
df_cleaned$rel = ifelse(df_cleaned$rel == Inf, 5e6, df_cleaned$rel)
df_cleaned = df_cleaned[order(df_cleaned$rel, decreasing = F), ]
df_cleaned$label = NA
df_cleaned$label[1:5] = df_cleaned$name[1:5]
df_cleaned$label = ifelse(df_cleaned$cap_hits > 11000000, df_cleaned$name, df_cleaned$label)


## plot
p1 = ggplot(df_cleaned, aes(x=points_per_60, y=cap_hits)) +
      geom_point(size=1, color="gray30", alpha=.7) +
      geom_smooth() +
      geom_label_repel(data=df_cleaned[1:5,],aes(x=points_per_60, y=cap_hits, label=label), 
                       size=4.5, alpha=1, force = 100,
                       #nudge_y = 1e3,
                       ylim=c(NA,5e5),
                       xlim=c(1.55,NA),
                       segment.size  = 0.2,
                       min.segment.length = 0,
                       show.legend  = F) +
    geom_label_repel(data=df_cleaned[df_cleaned$cap_hits > 11000000,],aes(x=points_per_60, y=cap_hits, label=label), 
                     size=4.5, alpha=1, force = 50,
                     nudge_y = 1e3,
                     ylim=c(12e6,15e6),
                     xlim=c(1,NA),
                     segment.size  = 0.2,
                     min.segment.length = 0,
                     show.legend  = F) +
      geom_hline(yintercept = median(df_cleaned$cap_hits), 
                 linetype="dashed", color="darkorange3", size=.75, alpha=.75) +
      geom_vline(xintercept = median(df_cleaned$points_per_60), 
                 linetype="dashed", color="darkorange3", size=.75, alpha=.75) + 
      scale_y_continuous(breaks=seq(0,15000000, 2500000), limits=c(-1500000,15000000)) +
      stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
      labs(x="Points per 60min",y="Cap hit") +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            title=element_text(size=13),
            legend.text=element_text(size=11))

p1



## ---------------
## points per game
df_cleaned$rel = df_cleaned$cap_hits / df_cleaned$points_per_game
df_cleaned$rel = ifelse(df_cleaned$rel == Inf, 5e6, df_cleaned$rel)
df_cleaned = df_cleaned[order(df_cleaned$rel, decreasing = F), ]
df_cleaned$label2 = NA
df_cleaned$label2[1:5] = df_cleaned$name[1:5]
df_cleaned$label2 = ifelse(df_cleaned$cap_hits > 11000000, df_cleaned$name, df_cleaned$label2)

p2 = ggplot(df_cleaned, aes(x=points_per_game, y=cap_hits)) +
      geom_point(size=1, color="gray30", alpha=.7) +
      geom_smooth() +
      geom_label_repel(data=df_cleaned[1:5,],aes(x=points_per_game, y=cap_hits, label=label2),
                       size=4.5, alpha=1, force = 100,
                       #nudge_y = 1e3,
                       ylim=c(NA,5e5),
                       xlim=c(0.35,1.5),
                       segment.size  = 0.2,
                       min.segment.length = 0,
                       show.legend  = F) +
      geom_label_repel(data=df_cleaned[df_cleaned$cap_hits > 11000000,],aes(x=points_per_game, y=cap_hits, label=label), 
                       size=4.5, alpha=1, force = 50,
                       nudge_y = 1e3,
                       ylim=c(12e6,15e6),
                       xlim=c(0.35,1.55),
                       segment.size  = 0.2,
                       min.segment.length = 0,
                       show.legend  = F) +
      geom_hline(yintercept = median(df_cleaned$cap_hits), 
                 linetype="dashed", color="red", size=.75, alpha=.75) +
      geom_vline(xintercept = median(df_cleaned$points_per_game), 
                 linetype="dashed", color="red", size=.75, alpha=.75) + 
      #geom_hline(yintercept = 50, linetype="dashed", color="red", size=1) +
      stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
      scale_y_continuous(breaks=seq(0,15000000, 2500000), limits=c(-1500000,15000000)) +
      labs(x="Points per game",y="Cap hit") +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            title=element_text(size=13),
            legend.text=element_text(size=11))

p2
## combine plots
plot_grid(p1, p2, ncol=1)
          





## -----------
## tables
df_cleaned_for_tab = df_cleaned[,c("name","points_per_60","points_per_game","CAP.HIT","SALARY")]
colnames(df_cleaned_for_tab) = c("name", "points/60", "points/game", "cap_hit", "salary")
rownames(df_cleaned_for_tab) = NULL
formattable(df_cleaned_for_tab[c(1:10, 737:746),],
            list(area(row=c(11:20), col="name") ~ color_tile("#a0c5fa","#c96b6b"),
                 area(row=c(1:10), col="name") ~ color_tile("#c96b6b","#a0c5fa")))


## best value for each team
df_tab = df_cleaned[!duplicated(df_cleaned$TEAM),c("name","TEAM","POS","points_per_60","points_per_game","CAP.HIT","SALARY")]
colnames(df_tab) = c("name", "team", "position", "points/60", "points/game", "cap_hit", "salary")
df_tab = df_tab[nchar(df_tab$team) > 1,]
formattable(df_tab)


## worst value for each team
df_tab = df_cleaned[order(df_cleaned$rel, decreasing=T),]
df_tab = df_tab[!duplicated(df_tab$TEAM),c("name","TEAM","POS","points_per_60","points_per_game","CAP.HIT","SALARY")]
colnames(df_tab) = c("name", "team", "position", "points/60", "points/game", "cap_hit", "salary")
df_tab = df_tab[nchar(df_tab$team) > 1,]
formattable(df_tab)




## --------------------
## high profile players
x = summary(df_cleaned$cap_hits)
df_high = df_cleaned[df_cleaned$cap_hits > x[[4]],]
dim(df_high)


df_high$rel = df_high$cap_hits / df_high$points_per_game
df_high$rel = ifelse(df_high$rel == Inf, 5e6, df_high$rel)
df_high = df_high[order(df_high$rel, decreasing = F), ]
df_high$label2 = NA
df_high$label2[1:10] = df_high$name[1:10]
df_high$label2[298:307] = df_high$name[298:307]


p3 = ggplot(df_high, aes(x=points_per_game, y=cap_hits)) +
  geom_point(size=1, color="gray30", alpha=.7) +
  geom_smooth() +
  geom_label_repel(data=df_high[1:10,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 15,
                   ylim=c(NA,2e6),
                   xlim=c(0.15,1.55),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_label_repel(data=df_high[298:307,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 10,
                   nudge_y = 1e3,
                   ylim=c(NA,12e6),
                   xlim=c(-0.25,0.05),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_hline(yintercept = median(df_high$cap_hits), 
             linetype="dashed", color="red", size=.75, alpha=.75) +
  geom_vline(xintercept = median(df_high$points_per_game), 
             linetype="dashed", color="red", size=.75, alpha=.75) + 
  labs(title="Skaters with cap hit above league average ($2.9 million)",
        subtitle=paste0("n = ", nrow(df_high))) +
  stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
  scale_y_continuous(breaks=seq(0,15000000, 2500000), limits=c(-1500000,15000000)) +
  scale_x_continuous(limits=c(-.25,1.55)) +
  labs(x="Points per game",y="Cap hit") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=13),
        legend.text=element_text(size=11))

p3





## --------------------
## top 10% forwards
n=10
df10f = df_cleaned[grepl("C|FW|LW",df_cleaned$POS),]
df10f = df10f[df10f$cap_hits > quantile(df10f$cap_hits,prob=1-n/100),]


df10f$rel = df10f$cap_hits / df10f$points_per_game
df10f$rel = ifelse(df10f$rel == Inf, 5e6, df10f$rel)
df10f = df10f[order(df10f$rel, decreasing = F), ]
df10f$label2 = NA
df10f$label2[1:10] = df10f$name[1:10]
df10f$label2[35:44] = df10f$name[35:44]


p4 = ggplot(df10f, aes(x=points_per_game, y=cap_hits)) +
  geom_point(size=1.35, color="gray25", alpha=.8) +
  geom_smooth() +
  geom_label_repel(data=df10f[1:10,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 25,
                   ylim=c(NA,5e6),
                   xlim=c(0.5,1.55),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_label_repel(data=df10f[35:44,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 10,
                   nudge_y = 1e3,
                   ylim=c(NA,12e6),
                   xlim=c(-0.25,0.3),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_hline(yintercept = median(df10f$cap_hits), 
             linetype="dashed", color="red", size=.75, alpha=.75) +
  geom_vline(xintercept = median(df10f$points_per_game), 
             linetype="dashed", color="red", size=.75, alpha=.75) + 
  labs(title="Top 10% of forwards by cap hit",
       subtitle=paste0("n = ", nrow(df10f))) +
  stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
  scale_y_continuous(breaks=seq(0,15000000, 2500000), limits=c(-1500000,15000000)) +
  scale_x_continuous(limits=c(-.1,1.55)) +
  labs(x="Points per game",y="Cap hit") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=13),
        legend.text=element_text(size=11))

p4





## --------------------
## top 10% defensemen
n=10
df10d = df_cleaned[grepl("D",df_cleaned$POS),]
df10d = df10d[df10d$cap_hits > quantile(df10d$cap_hits,prob=1-n/100),]


df10d$rel = df10d$cap_hits / df10d$points_per_game
df10d$rel = ifelse(df10d$rel == Inf, 5e6, df10d$rel)
df10d = df10d[order(df10d$rel, decreasing = F), ]
df10d$label2 = NA
df10d$label2[1:10] = df10d$name[1:10]
df10d$label2[13:22] = df10d$name[13:22]


p5 = ggplot(df10d, aes(x=points_per_game, y=cap_hits)) +
  geom_point(size=1.35, color="gray25", alpha=.85) +
  geom_smooth() +
  geom_label_repel(data=df10d[1:10,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 15,
                   #nudge_y = 1e3,
                   ylim=c(NA,5e6),
                   xlim=c(0.5,1.5),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_label_repel(data=df10d[13:22,],aes(x=points_per_game, y=cap_hits, label=label2),
                   size=3.5, alpha=1, force = 10,
                   nudge_y = 1e3,
                   ylim=c(NA,12e6),
                   xlim=c(-0.15,0.135),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +
  geom_hline(yintercept = median(df10d$cap_hits), 
             linetype="dashed", color="red", size=.75, alpha=.75) +
  geom_vline(xintercept = median(df10d$points_per_game), 
             linetype="dashed", color="red", size=.75, alpha=.75) + 
  labs(title="Top 10% of defensemen by cap hit",
       subtitle=paste0("n = ", nrow(df10d))) +
  stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
  scale_y_continuous(breaks=seq(0,15000000, 2500000), limits=c(-1500000,15000000)) +
  scale_x_continuous(limits=c(-.1,1.55)) +
  labs(x="Points per game",y="Cap hit") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=13),
        legend.text=element_text(size=11))

p5









#