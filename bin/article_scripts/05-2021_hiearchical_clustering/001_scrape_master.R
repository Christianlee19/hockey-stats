## note you will need Java Developer Kit (JDK)
#install.packages("RSelenium")


## note you will need Java Developer Kit (JDK)
library(RSelenium)
library(rvest)
library(dplyr)


## set and navigate to url
url = "http://www.nhl.com/stats/skaters?reportType=season&seasonFrom=19171918&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=points,goals,assists&page=0&pageSize=100"

# rD = rsDriver(port=4444L, browser="firefox", version="4.0.0-alpha-1",chromever="90.0.4430.24", check=TRUE) #chromever can change depending on chrome version and updates (see below)
# remDr = rD[['client']]
# remDr$navigate(url) 
# src = remDr$getPageSource()[[1]] 
rD = rsDriver(port=4444L, browser="firefox", version="4.0.0-alpha-1", check=TRUE) #chromever can change depending on chrome version and updates (see below)
remDr = rD[['client']]
remDr$navigate(url) 
src = remDr$getPageSource()[[1]] 


## read in the info from the first table
df = read_html(src) %>% 
  xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
  xml_text() %>%
  matrix(.,ncol=23, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)


## At our url, there are 26 pages if viewing 50 rows at a time.
## We are starting at 2 because 1 was already done before.
## We will be adding to that data frame so we don't have to define an empty one.
for (i in 2:74) {
  print(i)
  pages = remDr$findElement(using = "css selector",".-next") #we are selecting the next button
  pages$clickElement()  
  
  ## wait 4 seconds to load (sometimes NHL.com is slow)
  Sys.sleep(4)
  
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
    xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
    xml_text() %>%
    matrix(., ncol=23, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  
  ## bind new data
  df = df %>% bind_rows(temp)
  
}


## add column names
header = read_html(src) %>%
  xml_nodes(css="div[role='columnheader']") %>%
  xml_text()
colnames(df) = header


## remove index column
df = df[,-1]
dim(df)
#[1] 7400   22


## save file
save(df, file = "~/Documents/hockey-stats/data/052021_skater_data_reg_season_1917_2021.rsav")

## close
remDr$close()
rD$server$stop() 
gc()






## ---- get 2020-2021 data only
## set and navigate to url
url = "http://www.nhl.com/stats/skaters?reportType=season&seasonFrom=20202021&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=points,goals,assists&page=0&pageSize=100"

# rD = rsDriver(port=4444L, browser="firefox", version="4.0.0-alpha-1",chromever="90.0.4430.24", check=TRUE) #chromever can change depending on chrome version and updates (see below)
# remDr = rD[['client']]
# remDr$navigate(url) 
# src = remDr$getPageSource()[[1]] 
rD = rsDriver(port=4444L, browser="firefox", version="4.0.0-alpha-1", check=TRUE) #chromever can change depending on chrome version and updates (see below)
remDr = rD[['client']]
remDr$navigate(url) 
src = remDr$getPageSource()[[1]] 


## read in the info from the first table
df = read_html(src) %>% 
  xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
  xml_text() %>%
  matrix(.,ncol=23, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)


## At our url, there are 26 pages if viewing 50 rows at a time.
## We are starting at 2 because 1 was already done before.
## We will be adding to that data frame so we don't have to define an empty one.
for (i in 2:74) {
  print(i)
  pages = remDr$findElement(using = "css selector",".-next") #we are selecting the next button
  pages$clickElement()  
  
  ## wait 4 seconds to load (sometimes NHL.com is slow)
  Sys.sleep(4)
  
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
    xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
    xml_text() %>%
    matrix(., ncol=23, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  
  ## bind new data
  df = df %>% bind_rows(temp)
  
}


## add column names
header = read_html(src) %>%
  xml_nodes(css="div[role='columnheader']") %>%
  xml_text()
colnames(df) = header


## remove index column
df = df[,-1]
dim(df)
#[1] 7400   22


## save file
save(df, file = "~/Documents/hockey-stats/data/052021_skater_data_reg_season_2020_2021.rsav")

## close
remDr$close()
rD$server$stop() 
gc()




