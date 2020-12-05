## note you will Java Developer Kit (JDK)
### copied
library(RSelenium)
library(rvest)
library(dplyr)

#url = "http://www.nhl.com/stats/teams?aggregate=0&report=summaryshooting&reportType=game&dateFrom=2018-10-02&dateTo=2019-03-11&gameType=2&homeRoad=H&filter=gamesPlayed,gte,1&sort=satTotal&page=0"
url = "http://www.nhl.com/stats/teams?aggregate=0&report=summaryshooting&reportType=game&dateFrom=2018-10-02&dateTo=2019-04-06&gameType=2&homeRoad=H&filter=gamesPlayed,gte,1&sort=satTotal&page=0&pageSize=50"
rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.88") #specify whichever version of chrome you are using
remDr = rD[['client']]
remDr$navigate(url)

### adjust items you want to scrape 
src = remDr$getPageSource()[[1]]

pg = read_html(src)
df = 
  test = pg %>% 
  xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
  xml_text() %>%
  matrix(.,ncol=19, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)



## -----------------------------------------------------------------------------
## At our url, there are 26 pages if using 50 rows at a time.
## We are starting at 2 because 1 was already done before.
    ## We will be adding to that data frame so we don't have to define an empty one.
## based on: https://stackoverflow.com/a/51637923
for (i in 2:26) {
  pages = remDr$findElement(using = "css selector",".-next")
  pages$clickElement()  
  
  ## wait 5 sec to load
  Sys.sleep(3)
  
  src = remDr$getPageSource()[[1]]
  
  temp = read_html(src) %>% 
    xml_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
    xml_text() %>%
    matrix(., ncol=19, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  df = df %>% bind_rows(temp)
}



## remove empty rows and let's just keep the first 12 columns
df_cleaned = df[nchar(df$X3) > 1, 1:12] #the blank rows have a space (1)
dim(df_cleaned)
#[1] 1271   12


## double check it worked by counting the number of HOME games played by each team (41)
unique(table(df_cleaned$X2)) == 41
#[1] TRUE


## add column names
colnames(df_cleaned) = c("index", "team", "game", "gp", 
                         "shots", "sat_for", "sat_against", "sat",
                         "sat_tied", "sat_ahead", "sat_behind", "sat_close")

## save file
save(df_cleaned, file = "~/Documents/hockey_stats/1204_nhl_home_sat_stats_2018-2019.rsav")


## close
remDr$close()
rD$server$stop() 
gc()

#
