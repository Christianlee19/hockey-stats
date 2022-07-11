library(ggplot2)
library(ggpubr)
library(ggrepel)

## load data
print(load("draft_2000_2021.rsav"))
head(df)

## remove duplicate GP
df[,15] = NULL


## -----------------------------------------------------------------------------
## plot number of players from draft year
df$GP = ifelse(is.na(df$GP), 0, df$GP)
from_draft_year = setDT(df)[,.(num_not_0 = sum(GP != 0), total_players = .N),by=year]
from_draft_year$years_removed = 2021 - from_draft_year$year
from_draft_year$percentage = paste0(round(from_draft_year$num_not_0 / from_draft_year$total_players, 2) * 100, "%")

p_draft1 = ggplot(from_draft_year, aes(x=years_removed, y=num_not_0)) +
  geom_bar(stat="identity", alpha =.85, fill = "#4747ed") +
  geom_text(aes(label=percentage), vjust=-.5, size=3.25) + 
  labs(x = "Years Removed From Draft", y = "# of Draft Picks",
       title="Players With 1+ NHL Games Since Draft Year",
       subtitle = "2000 - 2021 NHL Drafts") +
  theme_bw() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10))

png("figures/070922_draft_picks_with_1_or_more_games.png", width=700, height=400)
  p_draft1
dev.off()




## -----------------------------------------------------------------------------
## remove 2021 b/c too recent
df = df[!df$year %in% c("2021","2020","2019","2018"),]


## only keep players in the first 7 seven rounds
df$round = ceiling(df$Overall / 30)
df = df[df$round <= 7,]
df$round = as.character(df$round)


### probability of playing at least 1 NHL game by draft round
### make into a table
df$GP = ifelse(is.na(df$GP), 0, df$GP)
gp_by_round = setDT(df)[, .(num_players_0 = sum(GP != 0), num_players_1_season = sum(GP >= 82), num_players = .N), by="round"]
gp_by_round$fraction_0_GP = round(gp_by_round$num_players_0 / gp_by_round$num_players, 2)
gp_by_round$fraction_1_season = round(gp_by_round$num_players_1_season / gp_by_round$num_players, 2)

gp_melt = melt(gp_by_round, measure.vars = c("fraction_0_GP",  "fraction_1_season"))
gp_melt$variable = ifelse(gp_melt$variable == "fraction_0_GP", "1+ games", "1+ seasons")

breaks = c(0, 0.25, 0.5, 0.75, 1)
p0 = ggplot(gp_melt, aes(x = round, y = value, fill = variable)) +
  geom_bar(stat="identity", position="dodge", alpha=.85) +
  labs(x = "Draft Round", y = "% of Draft Picks", fill = "Cutoff",
       title="NHL Player Game Experience by Draft Round",
       subtitle = "2000 - 2017 NHL Drafts") +
  scale_y_continuous(breaks = breaks, labels = scales::percent(breaks)) +
  scale_fill_manual(values=c("#4747ed","#de3131")) +
  theme_bw() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10))

p0

png("figures/070922_draft_picks_with_1_or_more_games_by_round.png", width=700, height=400)
  p0
dev.off()



df100 = df[df$Overall <= 100,]

df100$GP = ifelse(is.na(df100$GP), 0, df100$GP)
gp_by_overall = setDT(df100)[, .(num_players_0 = sum(GP != 0), num_players_1_season = sum(GP >= 82), num_players = .N), by="Overall"]
gp_by_overall$fraction_0_GP = round(gp_by_overall$num_players_0 / gp_by_overall$num_players, 2)
gp_by_overall$fraction_1_season = round(gp_by_overall$num_players_1_season / gp_by_overall$num_players, 2)

gp_melt = melt(gp_by_overall, measure.vars = c("fraction_0_GP",  "fraction_1_season"))
gp_melt$variable = ifelse(gp_melt$variable == "fraction_0_GP", "1+ games", "1+ seasons")

breaks = c(0, 0.25, 0.5, 0.75, 1)
p00 = ggplot(gp_melt, aes(x = Overall, y = value, color = variable)) +
  # geom_bar(stat="identity", position="dodge") +
  geom_smooth(size=1.5, alpha=.6) +
  geom_line(size=.75, alpha=.55) +
  labs(x = "Top 100 Draft Picks", y = "% of Draft Picks", color = "Cutoff",
       title="NHL Player Game Experience by Draft Round",
       subtitle = "2000 - 2017 NHL Drafts") +
  scale_y_continuous(breaks = breaks, labels = scales::percent(breaks)) +
  scale_color_manual(values=c("#4747ed","#de3131")) +
  theme_bw() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10))

p00

png("figures/070922_100_draft_picks_with_1_or_more_games_by_position.png", width=700, height=400)
  p00
dev.off()













## remove goalies
df = df[df$Pos != "G",]


## per game
df$ppg = df$PTS / df$GP
df$gpg = df$G / df$GP
df$apg = df$A / df$GP


## set values to 0 if NA
df$GP = ifelse(is.na(df$GP), 0, df$GP)
df$G = ifelse(is.na(df$G), 0, df$G)
df$A = ifelse(is.na(df$A), 0, df$A)
df$PTS = ifelse(is.na(df$PTS), 0, df$PTS)
df$ppg = ifelse(is.na(df$ppg), 0, df$ppg)
df$gpg = ifelse(is.na(df$gpg), 0, df$gpg)
df$apg = ifelse(is.na(df$apg), 0, df$apg)


## scale values by draft year
setDT(df)[, scaled_GP := scale(GP, scale=F), by="year"]
setDT(df)[, scaled_G := scale(G, scale=F), by="year"]
setDT(df)[, scaled_A := scale(A, scale=F), by="year"]
setDT(df)[, scaled_PTS := scale(PTS, scale=F), by="year"]


## scale per gamevalues by draft year
setDT(df)[, scaled_gp := scale(GP, scale=F), by="year"]
setDT(df)[, scaled_gpg := scale(gpg, scale=F), by="year"]
setDT(df)[, scaled_apg := scale(apg, scale=F), by="year"]
setDT(df)[, scaled_ppg := scale(ppg, scale=F), by="year"]


## career length
#df$seasons_played = as.numeric(df$To) - df$year # draft year is the year before season so don't need to +1

## correlation b/w draft position and cats

#pairs(df[,c("Overall","ppg","gpg","apg")])



df$year = as.character(df$year)


## only keep players with at least 82 NHL games
# df = df[df$GP >= 82,]

#   
# p1 = ggplot(df, aes(x=Overall, y=scaled_ppg)) +
#   geom_point(aes(color=round),size=2, alpha=.75) +
#   geom_smooth(alpha=.75) + 
#   geom_vline(xintercept=seq(30, 210, 30), alpha=.75, color="red", linetype=3) +
#   labs(x="Overall Draft Selections", y="Scaled Points Per Game", color="Round", fill="Round") +
#   theme_bw()

df = df[order(df$scaled_ppg, decreasing=T),]
df2 = df[df$Overall <= 100,]
  
p2 = ggplot(df2, aes(x=Overall, y=scaled_ppg)) +
  geom_point(position = position_jitter(w=0.25, h=0), size=2, alpha=.55) +
  geom_smooth(alpha=.5) + 
  geom_smooth(method='lm', formula= y~x, color="forestgreen") +
  geom_label_repel(data=df2[df2$Player %in% c("Nikita Kucherov", "Brad Marchand", "Connor McDavid", "Brayden Point", "Jake Guentzel"),], 
                   aes(x=Overall, y=scaled_ppg),
                   label=df2[df2$Player %in% c("Nikita Kucherov", "Brad Marchand", "Connor McDavid", "Brayden Point", "Jake Guentzel"),]$Player,
                   size=3.5, alpha=.75, min.segment.length=0) +
  stat_cor(method = "spearman", label.x = 73, label.y = 1.15, size=4.5) +
  geom_vline(xintercept=seq(10, 90, 10), alpha=.75, color="red", linetype=3) +
  labs(x="Top 100 Draft Selections", y="Scaled Points Per Game", color="Round", fill="Round") +
  theme_bw() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10))


df3 = df2[df2$Overall <= 30,]

p3 = ggplot(df3, aes(x=Overall, y=scaled_ppg)) +
  geom_point(position = position_jitter(w=0.2, h=0), size=2.5, alpha=.65) +
  geom_smooth(alpha=.5) + 
  geom_smooth(method='lm', formula= y~x, color="forestgreen") +
  geom_label_repel(data=df3[1:3,], 
                   aes(x=Overall, y=scaled_ppg), label=df3[1:3,]$Player, size=3.5, alpha=.75) +
  stat_cor(method = "spearman", label.x = 22, label.y = 1.15, size=4.5) +
  geom_vline(xintercept=seq(10, 20, 10), alpha=.75, color="red", linetype=3) +
  labs(x="Top 30 Draft Selections", y="Scaled Points Per Game", color="Round", fill="Round") +
  theme_bw() + 
  theme(axis.text = element_text(size=11),
                     axis.title = element_text(size=12),
                     legend.text = element_text(size=10))




png("figures/070922_scaled_points_per_game.png", width=700, height=700)
  grid.arrange(p2, p3, ncol=1)
dev.off()










##
