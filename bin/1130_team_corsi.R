## corsi team
library(ggplot2)
library(ggrepel)
library(ggpubr)

setwd("~/Documents/stats//Hockey_stats/")
df = read.csv("2018_corsi_team.csv", header=T, stringsAsFactors = F)
colnames(df)[c(3,9)] = c("wins","cfp")

## add label
df$label = ifelse(df$cfp > 51.6 | df$cfp < 48.43,df$Team,NA)
df$label = ifelse(df$Team == "Tampa Bay Lightning",df$Team, df$label)

## 
p2 = ggplot(df, aes(cfp, wins, label=label, color=Points)) +
  geom_point(size=3) +
  geom_vline(xintercept = 50, linetype="dashed") +
  geom_label_repel(data=df[df$cfp > 50,],
                   size=4, alpha=1, force = 15, 
                   segment.size  = 0.2, nudge_y=7,
                   ylim=c(50,69),
                   min.segment.length = 0.01, show.legend  = F) +
  geom_label_repel(data=df[df$cfp < 50,],
                   size=4, alpha=1, force = 25, 
                   segment.size  = 0.2, nudge_y=-7,
                   ylim=c(27,15),
                   min.segment.length = 0.01, show.legend  = F) +
  scale_y_continuous(limits=c(15,70)) +
  scale_color_gradient2(low = "#0f5cd9",mid ="gray75",high="#cf0000", midpoint=90) +
  stat_cor(method="spearman", show.legend  = F) + 
  labs(title="NHL team wins against CF%",
       subtitle="2018-2019 season\nFull 82 game season",
       x="CF%",y="Wins") +
  theme_bw() +
  theme(axis.text=element_text(size=10))

p2  
