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

print(load("~/Documents/hockey-stats/data/1231_team_game_pdo_2018-2019.rsav"))
dim(df2)

## remove unregistered pdo
colnames(df2)[23] = "pdo"
df2 = df2[df2$pdo != "--",]
dim(df2)

df2$pdo = as.numeric(df2$pdo)
df2$game_date = as.POSIXlt(gsub("(.+)vs(.+)", "\\1", df2$Game), format="%Y/%m/%d")

test = df2[df2$Player == "John Tavares",]
rollmean(test$pdo)

aggregate(list(temperature = df2$temperature), 
          list(hourofday = cut(df2$game_date)), 
          mean)
#             hourofday temperature
# 1 2007-09-29 00:00:00   -1.744333
# 2 2007-09-29 01:00:00   -1.586000
# 3 2007-09-29 02:00:00   -1.751667
# 4 2007-09-29 03:00:00   -1.820000

ggplot(df2, aes(x=Game, y=pdo, color=Player)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_blank())





## 

