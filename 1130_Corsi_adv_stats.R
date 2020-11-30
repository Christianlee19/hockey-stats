## 11.29.2020
## adv hockey stats: Corsi
library(ggplot2)
library(ggrepel)
library(ggpubr)

setwd("Documents/hockey_stats/")
list.files()

## load adv stats from: https://www.hockey-reference.com/leagues/NHL_2020_skaters-advanced.html
stats = read.csv("adv_hockey_stats.csv", header=T, skip=3, stringsAsFactors=F)
stats = stats[rowSums(is.na(stats)) == 0,c(2:10)]
colnames(stats)[1] = "name"

## combine subset of basic stats
bstats = read.csv("basic_stats_2020.csv", header=T, skip=1, stringsAsFactors=F)
bstats = bstats[,c(2,7:10,22)]
colnames(bstats) = c("name","goals","assists", "points","pm","toi")
df = merge(stats, bstats, by="name")

## keep unique and clean
df = df[order(df$cprel, decreasing = T),]
df = df[!duplicated(df$name),]
colnames(df)[1:9] = c("name","age","team","pos","gp","cf","ca","cp","cprel")
df$name = gsub("\\\\.*","",df$name)

## keep players with more than 20 games
df = df[df$gp >= 20,]

## order
df = df[order(df$cprel, decreasing=T),]

## points per 60
df$pp60 = df$points / df$toi * 60

## add labels for top skaters
df$label = NA
df$label[1:5] = df$name[1:5]
df$label[381:385] = df$name[381:385]
df$label = ifelse(df$pp60 >= 3.3, df$name, df$label)

df$pm_group = ifelse(df$pm > 0, "+", ifelse(df$pm < 0, "-", "0"))



## ---- visualize ---- ##
p = ggplot(df, aes(pp60, cprel, label=label)) +
      geom_point(size=2) +
      geom_label_repel(data=df[df$cprel > 0,], 
                       size=3.5, alpha=.85, force = 10,
                       nudge_y = 7.5,
                       ylim=c(10,22),
                       segment.size  = 0.2,
                       min.segment.length = 0,
                       show.legend  = F) +
  
    geom_label_repel(data=df[df$cprel < 0,], 
                     size=3.5, alpha=.85, force = 10,
                     nudge_y = -3,
                     ylim=c(-10,-22),
                     segment.size  = 0.2,
                     #min.segment.length = 0,
                     show.legend  = F) +
  
      scale_y_continuous(limits=c(-25,25)) +
      stat_cor(method="spearman") + 
      labs(title="NHL skater CF% rel against points per 60",
            subtitle="2019-2020 season\nn = 385 (at least 20 games)",
            color="+/-",
            x="Points per 60min",y="CF% rel") +
      theme_bw()
p


## plus minus plot
df$label = NA
df$label[1:5] = df$name[1:5]
df$label[381:385] = df$name[381:385]
df2 = df

df2$label = ifelse(df2$pm >= 26, df2$name, df2$label)
p2 = ggplot(df2, aes(pm, cprel, label=label, color=pm_group)) +
  geom_point(size=2) +
  geom_label_repel(data=df2[df2$cprel > 0,],
                   size=3.5, alpha=.85, force = 5, 
                   segment.size  = 0.2, nudge_y = 5,
                   arrow = arrow(length = unit(0.015, "npc")),
                   show.legend  = F) +
  
  geom_label_repel(data=df2[df2$cprel < 0,],
                   size=3.5, alpha=.85, force = 5, 
                   segment.size  = 0.2, nudge_y = -5,
                   arrow = arrow(length = unit(0.015, "npc")),
                   show.legend  = F) +
  
  scale_y_continuous(limits=c(-25,25)) +
  scale_color_manual(values=c("+"="#ed4a2d", "-"="#2a6fde", "0"="gray75")) +
  stat_cor(method="spearman", show.legend  = F) + 
  labs(title="NHL skater CF% rel against plus/minus",
       subtitle="2019-2020 season\nn = 385 (at least 20 games)",
       x="+/-",y="CF% rel",
       color="+/- group") +
  theme_bw()

p2  





##
