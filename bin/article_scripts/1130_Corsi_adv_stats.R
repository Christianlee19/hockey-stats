## 11.29.2020
## adv hockey stats: Corsi
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(data.table)

setwd("~/Documents/hockey-stats/data")
list.files()

## load adv stats from: https://www.hockey-reference.com/leagues/NHL_2020_skaters-advanced.html
stats = read.csv("adv_hockey_stats.csv", header=T, skip=1, stringsAsFactors=F)
stats = stats[rowSums(is.na(stats)) == 0,c(2:10)]
colnames(stats)[1] = "name"

## combine subset of basic stats
bstats = read.csv("basic_stats_2020.csv", header=T, skip=1, stringsAsFactors=F)
bstats = bstats[,c(2,7:10,22)]
colnames(bstats) = c("name","goals","assists", "points","pm","toi")
df = merge(stats, bstats, by="name")

## keep unique and clean
df = df[!duplicated(df$name),]
colnames(df)[1:9] = c("name","age","team","pos","gp","cf","ca","cp","cprel")
df$name = gsub("\\\\.*","",df$name)

## keep players with more than 20 games
df = df[df$gp >= 20,]

## order by cp
df = df[order(df$cp, decreasing=T),]

## points per 60
df$pp60 = df$points / df$toi * 60

## add labels for top skaters
df$label = NA
df$label[1:5] = df$name[1:5]
df$label[666:670] = df$name[666:670]
df$label = ifelse(df$pp60 >= 4, df$name, df$label)

df$pm_group = ifelse(df$pm > 50, ">50+", ifelse(df$pm < 50, "<50", "0"))

## add specific colors
df$point_color = "Rest"
df$point_color[1:5] = "Top5 CF%"
df$point_color[666:670] = "Bot5 CF%"
df$point_color = ifelse(df$pp60 >= 4, "Top5 PP60", df$point_color)

## ---- visualize ---- ##
ggplot(df, aes(pp60, cp, label=label)) +
      geom_point(aes(color=point_color),size=2) +
      geom_label_repel(data=df[df$cp > 50,], 
                       size=4, alpha=.85, force = 10,
                       nudge_y = 7.5,
                       #ylim=c(10,22),
                       segment.size  = 0.2,
                       min.segment.length = 0,
                       show.legend  = F) +
  
      geom_label_repel(data=df[df$cp < 50 & df$point_color == "Top5 PP60",], 
                     size=4, alpha=.85, force = 10,
                     nudge_y = -3,
                     segment.size  = 0.2,
                     #min.segment.length = 0,
                     show.legend  = F)+
    
      geom_label_repel(data=df[df$point_color == "Bot5 CF%",], 
                       size=4, alpha=.85, force = 10,
                       nudge_y = -3,
                       ylim=c(20,38),
                       segment.size  = 0.2,
                       #min.segment.length = 0,
                       show.legend  = F) +
  
      scale_color_manual(values=c("Rest"="gray60", "Top5 CF%"="red", "Bot5 CF%"="blue", "Top5 PP60"="slateblue4")) + 
      geom_hline(yintercept = 50, linetype="dashed", color="red", size=1) +
      scale_y_continuous(limits=c(25,75)) +
      stat_cor(method="spearman", alternative = "greater", size=5, show.legend = F) + 
      labs(title="NHL skater CF% versus points per 60",
            subtitle="2019-2020 season\nn = 670 (at least 20 games)",
            color="Subset",
            x="Points per 60min",y="CF%") +
      theme_bw() +
      theme(axis.text=element_text(size=11),
            title=element_text(size=14),
            legend.text=element_text(size=12))




## --------------
## plus minus plot
df$label = NA
df$label[1:5] = df$name[1:5]
df$label[666:670] = df$name[666:670]
df2 = df

df2$label = ifelse(abs(df2$pm) >= 28, df2$name, df2$label)
df2$pm_group = ifelse(df2$pm > 0, "+", ifelse(df2$pm < 0, "-", "0"))

ggplot(df2, aes(pm, cp, label=label)) +
  geom_point(aes(color=pm_group),size=2) +
  geom_label_repel(data=df2[df2$cp > 50,],
                   size=4, alpha=.85, force = 5, 
                   segment.size  = 0.2, nudge_y = 6,
                   show.legend  = F) +
  
  geom_label_repel(data=df2[df2$cp < 50,],
                   size=4, alpha=.85, force = 5, 
                   segment.size  = 0.2, nudge_y = -5,
                   show.legend  = F) +
  
  scale_y_continuous(limits=c(25,75)) +
  scale_color_manual(values=c("+"="#ed4a2d", "-"="#2a6fde", "0"="gray75")) +
  stat_cor(method="spearman", show.legend  = F,alternative = "greater", size=5) + 
  labs(title="NHL skater CF% versus plus/minus",
       subtitle="2019-2020 season\nn = 670 (at least 20 games)",
       x="+/-",y="CF%",
       color="+/- group") +
  geom_hline(yintercept = 50, linetype="dashed", color="gray50", size=1) +
  
  theme_bw() +
  theme(axis.text=element_text(size=11),
        title=element_text(size=14),
        legend.text=element_text(size=12))







## -------------
## Tampa player stats

#plot highest and lowest cprel for each team
df = setDT(df)
df = df[order(df$cprel, decreasing = T)]
low = df[df[, .I[cprel == min(cprel)], by=team]$V1]
low = low[!duplicated(low$team),]

high = df[df[, .I[cprel == max(cprel)], by=team]$V1]
high = high[!duplicated(high$team),]

combined = data.frame(rbind(high,low),stringsAsFactors = F)
combined = combined[order(combined$team),]
combined$team = factor(combined$team, levels=rev(unique(combined$team)))


## plot
p3 = ggplot(combined, aes(y=cprel, x=team, label=name)) +
  geom_bar(stat="identity",alpha=.3, fill="dodgerblue4") +
  coord_flip() +
  geom_text(fontface="bold") +
  scale_y_continuous(limits=c(-23,15)) +
  geom_hline(yintercept = 0, linetype="dashed", color="red", size=.75, alpha=0.85) +
  labs(title="Highest and lowest CF% rel skater by NHL team",
       subtitle="2019-2020 season",
       y="CF% rel",x="Team") +
  theme_bw() +
  theme(axis.text=element_text(size=11),
        title=element_text(size=14),
        legend.text=element_text(size=12))
  



##
