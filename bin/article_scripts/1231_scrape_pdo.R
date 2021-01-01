## note you will need Java Developer Kit (JDK)
library(RSelenium)
library(rvest)
library(dplyr)

url = "http://www.nhl.com/stats/skaters?aggregate=0&report=percentages&reportType=game&dateFrom=2019-10-02&dateTo=2020-03-11&gameType=2&playerPlayedFor=franchise.5&filter=gamesPlayed,gte,1&sort=a_gameDate&page=0&pageSize=100"
rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.88") #specify whichever version of chrome you are using
remDr = rD[['client']]
remDr$navigate(url) #this will open a chrome window (that means it is working)
src = remDr$getPageSource()[[1]] #we are just selecting everything for now


## read in the info from the first table
df = read_html(src) %>% 
  xml_nodes(css="div[role='gridcell']") %>%
  xml_text() %>%
  matrix(.,ncol=24, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)


## -----------------------------------------------------------------------------
for (i in 2:13) {
  pages = remDr$findElement(using = "css selector",".-next") #we are selecting the next button
  pages$clickElement()  
  
  ## wait 7 seconds to load (can reduce this)
  Sys.sleep(7)
  
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
    xml_nodes(css="div[role='gridcell']") %>%
    xml_text() %>%
    matrix(., ncol=24, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  print(dim(temp))
  
  ## bind new data
  df = df %>% bind_rows(temp)
  
}


## remove empty rows and let's just keep the first 12 columns
df = df[nchar(df$X3) > 1, 1:24] #the blank rows have a space (1)
dim(df)
#[1] 1260   24


## get header
header = read_html(src) %>% 
  xml_nodes(css="div[role='columnheader']") %>%
  xml_text() 


## add column names
colnames(df) = header


## save file
save(df, file = "~/Documents/hockey-stats/data/1231_pdo_2018-2019.rsav")


## close
remDr$close()
rD$server$stop() 
gc()


