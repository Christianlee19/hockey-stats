## note you will need Java Developer Kit (JDK)
#install.packages("RSelenium")

library(RSelenium)
library(rvest)
library(dplyr)



## reports to scrape:
  ## summary, faceoff percentages, misc (giveaways) + hits + blocks,
  ## penalties, sat percentages

## assuming a full length season:
  ## 1271 games (2542 stats for home and away teams)
  ## 2542 * 3 seasons = 7626



## create df with info
## summary, faceoff percentages, leading/trailing, Miscellaneous, outshoot/outshot by, penalties, SAT %, Team Goal-Games
urls = c("http://www.nhl.com/stats/teams?aggregate=0&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=faceoffpercentages&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=leadingtrailing&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=realtime&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=outshootoutshotby&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=penalties&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=percentages&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50",
         "http://www.nhl.com/stats/teams?aggregate=0&report=goalgames&reportType=season&seasonFrom=20002001&seasonTo=20202021&gameType=2&filter=gamesPlayed,gte,1&sort=seasonId&page=0&pageSize=50"
         )


num_cols = c(24, 18, 16, 23, 23)
info = data.frame(cbind(urls, num_cols))
info$num_cols = as.numeric(info$num_cols)



## set up
#binman::list_versions("chromedriver")
rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.87") #specify whichever version of chrome you are using
remDr = rD[['client']]


## scrape data
data_list = lapply(1:nrow(info), get_scraped_data)
save(data_list, file = "~/Documents/hockey-stats/data/0215_random_forest.R")

## ensure all dfs are ordered the same
## create unique id that should match all 


## close
remDr$close()
rD$server$stop() 
gc()



## scraping function
get_scraped_data = function(i){
  
  url = info[i,1]
  ncol = info[i,2]
  
  remDr$navigate(url) #this will open a chrome window (that means it is working)
  Sys.sleep(5)
  src = remDr$getPageSource()[[1]] #we are just selecting everything for now
  
  
  ## read in the info from the first table
  df = read_html(src) %>% 
    xml_nodes(css="div[role='gridcell']") %>%
    xml_text() %>%
    matrix(., ncol=ncol, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  
  
  # -----------------------------------------------------------------------------
  for (i in 2:73) {
    cat(paste0(i, ": "))
    pages = remDr$findElement(using = "css selector",".-next") #we are selecting the next button
    pages$clickElement()

    ## wait 7 seconds to load (can reduce this)
    Sys.sleep(5)

    src = remDr$getPageSource()[[1]]
    temp = read_html(src) %>%
      xml_nodes(css="div[role='gridcell']") %>%
      xml_text() %>%
      matrix(., ncol=ncol, byrow = T) %>%
      data.frame(., stringsAsFactors = F)
    print(dim(temp))

    ## bind new data
    df = df %>% bind_rows(temp)

  }


  ## remove empty rows and let's just keep the first 12 columns
  df2 = df[nchar(df$X2) > 1,] #the blank rows have a space (1)
  dim(df2)


  ## get header
  header = read_html(src) %>%
    xml_nodes(css="div[role='columnheader']") %>%
    xml_text()


  ## add column names
  colnames(df2) = header
  print(dim(df2))
  return(df2)
}


