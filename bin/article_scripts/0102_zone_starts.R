## note you will need Java Developer Kit (JDK)
library(RSelenium)
library(rvest)
library(dplyr)

url = "http://www.nhl.com/stats/teams?aggregate=0&report=percentages&reportType=game&seasonFrom=20182019&seasonTo=20182019&dateFromSeason&gameType=2&filter=gamesPlayed,gte,1&sort=a_gameDate&page=0&pageSize=100"
rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.88") #specify whichever version of chrome you are using
remDr = rD[['client']]
remDr$navigate(url) #this will open a chrome window (that means it is working)
src = remDr$getPageSource()[[1]] #we are just selecting everything for now


## read in the info from the first table
df = read_html(src) %>% 
  xml_nodes(css="div[role='gridcell']") %>%
  xml_text() %>%
  matrix(.,ncol=23, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)


## -----------------------------------------------------------------------------
for (i in 2:26) {
  cat(paste0(i, ": "))
  pages = remDr$findElement(using = "css selector",".-next") #we are selecting the next button
  pages$clickElement()  
  
  ## wait 10 seconds to load (can reduce this)
  Sys.sleep(10)
  
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
    xml_nodes(css="div[role='gridcell']") %>%
    xml_text() %>%
    matrix(., ncol=23, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  print(dim(temp))
  
  ## bind new data
  df = df %>% bind_rows(temp)
  
}


## remove empty rows and let's just keep the first 12 columns
df2 = df[nchar(df$X2) > 1, 1:23] #the blank rows have a space (1)
dim(df2)
#


## get header
header = read_html(src) %>% 
  xml_nodes(css="div[role='columnheader']") %>%
  xml_text() 


## add column names
colnames(df2) = header


## save file
save(df2, file = "~/Documents/hockey-stats/data/1231_team_game_pdo_2018-2019.rsav")


## close
remDr$close()
rD$server$stop() 
gc()





## --------------------
## ---- analyze ---- ##
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(formattable)
library(ggpubr)

print(load("~/Documents/hockey-stats/data/1231_team_game_pdo_2018-2019.rsav"))
dim(df2)

## remove unregistered pdo
colnames(df2)[23] = "pdo"
df2 = df2[df2$pdo != "--",]
df2$pdo = as.numeric(df2$pdo)
dim(df2)

df2$game_date = as.POSIXlt(gsub("(.+)vs(.+)", "\\1", df2$Game), format="%Y/%m/%d")
df2 = df2[order(df2$game_date),]


moving_avg = function(i, dfss) {
  x = dfss[1:i,,drop=F]
  val = mean(x$pdo)
  team = unique(x$Team)
  return(c(team, i, val))
}

res = do.call(rbind, lapply(unique(df2$Team), function(team){
  dfss = df2[df2$Team == team,]
  temp = data.frame(do.call(rbind, lapply(1:nrow(dfss), moving_avg, dfss)), stringsAsFactors = F)
  return(temp)
}))

colnames(res) = c("team", "gp", "avg_pdo")
res$avg_pdo = as.numeric(res$avg_pdo)
res$gp = factor(res$gp, levels=as.character(seq(1,82,1)))

teams_to_color = c("Nashville Predators","Anaheim Ducks","New York Islanders","Tampa Bay Lightning",
                   "Vegas Golden Knights","Detroit Red Wings","San Jose Sharks")
team_colors = brewer.pal(7, "Dark2")
names(team_colors) = teams_to_color


ggplot(res, aes(x=gp, y=avg_pdo, group=team)) +
  geom_line(color="gray40", alpha=.65) +
  geom_line(data=res[res$team %in% teams_to_color,], aes(color=team),
            size=1) +
  labs(title="NHL team PDO", subtitle="2018-2019 regular season\nn = 31 teams",
       x="Games played",y="PDO average",color="Select team") + 
  scale_x_discrete(breaks=as.character(seq(0,82,4))) +
  scale_color_manual(values = team_colors) +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        title=element_text(size=12))





## -------------------------------
## ---- player summary zone starts ---- ##
## load adv stats from: https://www.hockey-reference.com/leagues/NHL_2020_skaters-advanced.html
df = read.csv("~/Documents/hockey-stats/data/adv_hockey_stats.csv", header=T, skip=1, stringsAsFactors=F)
colnames(df)[2] = "name"


## keep unique and clean
df = df[!duplicated(df$name),]
df$name = gsub("\\\\.*","",df$name)


## keep players with more than 20 games
df = df[df$GP >= 20,]

## get groups
mean_value = mean(df$oZS.)
sd_value = sd(df$oZS.)
df$group = ifelse(df$oZS. > mean_value + 2*sd_value | df$oZS. < mean_value - 2*sd_value, "extreme")


## plot
ggplot(df, aes(x=`oZS.`)) +
  geom_histogram(aes(y=..density..), bins=50,fill="#396591", alpha=.65, color="dodgerblue4") + 
  geom_density(fill="#396591", alpha=.50, color = "dodgerblue4", size=1) +
  geom_vline(xintercept = c(mean_value, mean_value+2*sd_value, mean_value-2*sd_value), 
             color=c("red","gray25","gray25"), alpha=.5, linetype="dashed", size=.75) +
  
  ## add ons
  labs(title="NHL skater offensive zone starts", 
       subtitle="2019-2020 regular season\nMean = 49.59, SD = 6.98, n = 670 skaters",
       x="oZS%",y="Density") + 
  theme_bw() +
  theme(axis.text=element_text(size=10),
        title=element_text(size=12))


## table
df = df[order(df$oZS., decreasing = T),]
df_tab = df[c(1:10,661:670),c("name","Age","Tm","Pos","GP","oZS.","dZS.")]
colnames(df_tab) = c("name", "age", "team", "position", "games played", "oZS", "dZS")
df_tab = df_tab[nchar(df_tab$team) > 1,]
rownames(df_tab) = NULL
formattable(df_tab,
            list(area(row=c(11:20), col="name") ~ color_tile("#a0c5fa","#db9797"),
                 area(row=c(1:10), col="name") ~ color_tile("#db9797","#a0c5fa")))



## combine with points
bstats = read.csv("basic_stats_2020.csv", header=T, skip=1, stringsAsFactors=F)
bstats = bstats[,c(2,7:10,22)]
colnames(bstats) = c("name","goals","assists", "points","pm","toi")
bstats$name = gsub("\\\\.*","",bstats$name)
df2 = merge(df, bstats, by="name")



## scatter plot against points
df2 = df2[order(df2$oZS., decreasing = T),]
df2$ppg = df2$points / df2$GP
names_to_label = c(df2$name[1:10], "Leon Draisaitl", "Connor McDavid", "Artemi Panarin")

ggplot(df2, aes(x=oZS., y=ppg)) +
  geom_point(size=1.25, color="gray30", alpha=.7) +
  geom_smooth() +
  geom_label_repel(data=df2[df2$name %in% names_to_label,],aes(x=oZS., y=ppg, label=name),
                   size=3.5, alpha=1, force = 15,
                   # ylim=c(NA,2e6),
                   #xlim=c(65,NA),
                   segment.size  = 0.2,
                   min.segment.length = 0,
                   show.legend  = F) +

  labs(title="Season average offensize zone starts",
       subtitle="2019-2020 regular season\nn = 790 skaters",
       x="oZS%", y="Points per game") +
  stat_cor(method="spearman", alternative = "greater", size=5.5, show.legend = F) + 
  #scale_y_continuous(breaks=seq(10,90, 20), limits=c(0, 100)) +
  scale_x_continuous(limits=c(18, 72), breaks=seq(20,70,10)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=13),
        legend.text=element_text(size=11))


