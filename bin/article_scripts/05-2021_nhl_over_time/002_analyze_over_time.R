library(ggplot2)
library(data.table)
library(RColorBrewer)
library(cowplot)
library(ggrepel)

setwd("~/Documents/hockey-stats/data/")


## load data
print(load("05012021_summary_data_from_1917.rsav"))


####
## taking a look at the data
####
length(unique(df$Team))
#[1] 58


## make columns of interest numeric
df[3:ncol(df)] = sapply(df[3:ncol(df)], as.numeric)
str(df)

## remove any rows missing date
df = df[nchar(df$Season) > 3,]


## assign numeric index for x-axis
setDT(df)[,season_num:= .GRP,by="Season"]
df$season_num = as.numeric(df$season_num)


## assign colours to years by first three digits
df$season_3_digits = gsub("([0-9]{3})[0-9]-[0-9]{2}", "\\1", df$Season)

season_colours = brewer.pal(n = 12, "Paired")
season_colours[11:12] = c("#f0cf2e","#bd9e04")
colour_df = data.frame(cbind("digits"=unique(df$season_3_digits), colours=season_colours), stringsAsFactors =F)
df$season_colour = colour_df$colours[match(df$season_3_digits, colour_df$digits)]



####
## plots
####

## ---- GF/GP and GA/GP
df$Season = factor(df$Season, levels=unique(df$Season))
xbreaks = seq(1, length(unique(df$Season)), 2)
xlabels = unique(df$Season)[c(TRUE, FALSE)]
 
p1 = ggplot(df, aes(x=season_num, y=GF/GP)) +
      geom_point(aes(colour=season_colour), size=1.75, alpha=.9) +
      geom_point(data=df2[df2$Team == "Edmonton Oilers",], color="black", size=2) +
      geom_smooth(alpha=.6, se=T) +
      labs(title = "Goals For / Games Played Across NHL Seasons",
           x = "Season", y="GF/GP") +
      scale_colour_identity() +
      scale_x_continuous(breaks = xbreaks,
                         labels = xlabels,
                         expand = c(.01,0)) +
      theme_bw() +
      theme(title = element_text(size=16),
            axis.text.y = element_text(size=14),
            axis.text.x = element_text(size=13, angle=90, hjust=.5, vjust=.5),
            legend.position="none")

p1
png("figures/0504_gf_over_gp.png", width=900, height=425)
  p1
dev.off()



## ---- Games Played (1917)
p2 = ggplot(df, aes(x=season_num, y=GP)) +
      geom_line() +
      geom_point(aes(colour=season_colour), size=3) +

      #geom_jitter(aes(colour=season_colour), size=1.75) +
      #geom_smooth(alpha=.55, se=F) +
      labs(title = "Games Played Across NHL Seasons",
           x = "Season", y="Games Played") +
      scale_colour_identity() +
      scale_x_continuous(breaks = xbreaks,
                         labels = xlabels,
                         expand = c(.01,0)) +
      theme_bw() +
      theme(title = element_text(size=16),
            axis.text.y = element_text(size=14),
            axis.text.x = element_text(size=13, angle=90, hjust=.5, vjust=.5),
            legend.position="none")

png("figures/0504_gp.png", width=900, height=425)
  p2
dev.off()





## ---- Shots/GP (1959)
## Note, SA is shots against (not shot attempts)
df2 = df[!is.na(df$`Shots/GP`),,drop=F]
p3 = ggplot(df2, aes(x=season_num, y=`Shots/GP`)) +
      geom_point(aes(colour=season_colour), size=1.75) +
      geom_smooth(alpha=.6, se=T) +
      labs(title = "Shots on Goal Across NHL Seasons",
           x = "Season", y="SOG") +
      scale_colour_identity() +
      scale_x_continuous(breaks = xbreaks,
                         labels = xlabels,
                         expand = c(.01,0)) +
      theme_bw() +
      theme(title = element_text(size=16),
            axis.text.y = element_text(size=14),
            axis.text.x = element_text(size=13, angle=90, hjust=.5, vjust=.5),
            legend.position="none")


png("figures/0504_shots_over_gp.png", width=900, height=425)
  p3
dev.off()




## ---- Save percentage (1959)
df2 = df[!is.na(df$`Shots/GP`),,drop=F]
df2$save_percentage = (1 - (df2$`GF/GP` / df2$`Shots/GP`)) * 100 
  
p4 = ggplot(df2, aes(x=season_num, y=save_percentage)) +
      geom_point(aes(colour=season_colour), size=1.75) +
      geom_label_repel(data=df2[df2$save_percentage < 84.25,], aes(label=Team),
                       ylim=c(82,85), alpha=.8, force=10) +
      geom_label_repel(data=df2[df2$save_percentage > 92.75,], aes(label=Team),
                       ylim=c(93,95), alpha=.8, force=10) +
      geom_smooth(alpha=.6, se=T) +
      labs(title = "Save % Across NHL Seasons",
           x = "Season", y="Sv%") +
      scale_colour_identity() +
      scale_y_continuous(limits=c(83,95)) +
      scale_x_continuous(breaks = xbreaks,
                         labels = xlabels,
                         expand = c(.01,0)) +
      theme_bw() +
      theme(title = element_text(size=16),
            axis.text.y = element_text(size=14),
            axis.text.x = element_text(size=13, angle=90, hjust=.5, vjust=.5),
            legend.position="none")

png("figures/0504_save_percentage_over_gp.png", width=900, height=425)
  p4
dev.off()






## ---- power play percentage (1959)
df2 = df[!is.na(df$`PP%`),,drop=F]

p5 = ggplot(df2, aes(x=season_num, y=`PP%`)) +
  geom_point(aes(colour=season_colour), size=1.75) +
  geom_smooth(alpha=.6, se=T) +
  geom_label_repel(data=df2[df2$`PP%` < 10 | df2$`PP%` > 29,], aes(label=Team),
                   alpha=.8) +
  labs(title = "Power play % Across NHL Seasons",
       x = "Season", y="PP%") +
  scale_colour_identity() +
  scale_x_continuous(breaks = xbreaks,
                     labels = xlabels,
                     expand = c(.01,0)) +
  theme_bw() +
  theme(title = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=13, angle=90, hjust=.5, vjust=.5),
        legend.position="none")


png("figures/0504_powerplay_percentage_over_gp.png", width=900, height=425)
  p5
dev.off()






t.test(df$`GF/GP`[df$Season == "1985-86"], df$`GF/GP`[df$Season == "2020-21"], alt="greater")
wilcox.test(df$`GF/GP`[df$Season == "1985-86"], df$`GF/GP`[df$Season == "2020-21"], alt="greater")

