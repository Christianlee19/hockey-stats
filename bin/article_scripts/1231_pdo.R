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
library(zoo)
library(RColorBrewer)

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
## ---- player summary PDO ---- ##
## load adv stats from: https://www.hockey-reference.com/leagues/NHL_2020_skaters-advanced.html
pdo = read.csv("adv_hockey_stats.csv", header=T, skip=1, stringsAsFactors=F)
colnames(pdo)[2] = "name"


## keep unique and clean
pdo = pdo[!duplicated(pdo$name),]
pdo$name = gsub("\\\\.*","",pdo$name)


## keep players with more than 20 games
pdo = pdo[pdo$GP >= 20,]


## plot
ggplot(pdo, aes(x=PDO)) +
  geom_histogram(aes(y=..density..),fill="#396591", alpha=.65, color="dodgerblue4") + 
  geom_density(fill="#396591", alpha=.50, color = "dodgerblue4", size=1) +
  geom_vline(xintercept = 100, color="gray25", alpha=.5, linetype="dashed", size=.75) +
  
  ## add ons
  labs(title="NHL skater PDO", subtitle="2019-2020 regular season\nn = 670 skaters",
       x="PDO",y="Density") + 
  theme_bw() +
  theme(axis.text=element_text(size=10),
        title=element_text(size=12))




