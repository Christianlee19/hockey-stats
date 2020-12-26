## note you will need Java Developer Kit (JDK)
library(RSelenium)
library(rvest)
library(dplyr)

url = "https://www.capfriendly.com/browse/active/2020?hide=age,handed,expiry-status"
rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.88")
remDr = rD[['client']]
remDr$navigate(url) 
src = remDr$getPageSource()[[1]] 


## read in the info from the first table
df = read_html(src) %>% 
      html_nodes("table") %>%
      html_table(fill = TRUE) %>%
      data.frame(., stringsAsFactors = F) %>%
      mutate_all(as.character)


## url approach
for (i in 2:31) {
  cat(i, "")
  Sys.sleep(2)
  
  url = paste0("https://www.capfriendly.com/browse/active/2020?hide=age,handed,expiry-status&pg=",i)
  remDr$navigate(url) 
  src = remDr$getPageSource()[[1]] 
  temp = read_html(src) %>% 
          html_nodes("table") %>%
          html_table(fill = TRUE) %>%
          data.frame(., stringsAsFactors = F) %>%
          mutate_all(as.character) #for now, make all columns type character
  df = df %>% bind_rows(temp)
}
save(df, file = "~/Documents/hockey-stats/data/1225_nhl_2019-2020_salary_cap.rsav")




## ---- METHOD 2 ---- ##
## pattern of page number and child number needed to advance to next page
#1 > 2
#2 > 3
#3 > 4
#4 > 5
#...
#30 > 5

url = "https://www.capfriendly.com/browse/active/2020?hide=age,handed,expiry-status"
#rD = rsDriver(port=4444L, browser="chrome", chromever="87.0.4280.88")
#remDr = rD[['client']]
remDr$navigate(url) 
src = remDr$getPageSource()[[1]] 


## read in the info from the first table
df2 = read_html(src) %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  data.frame(., stringsAsFactors = F) %>%
  mutate_all(as.character)
  


for (i in 1:30) { #note we are starting the index at 1 but will add +1 initially
  cat(i, " ")
  if(i <= 3){
    num = i + 1
    css_select = paste0('#pagin > div > div:nth-child(1) > a:nth-child(',num,')')
  }else{
    css_select = "#pagin > div > div:nth-child(1) > a:nth-child(5)"
  }
  
  ## click element
  pages = remDr$findElement(using = "css selector", css_select)
  pages$clickElement()  
  Sys.sleep(3)
  
  ## extract
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
          html_nodes("table") %>%
          html_table(fill = TRUE) %>%
          data.frame(., stringsAsFactors = F) %>%
          mutate_all(as.character)
    

  ## bind new data
  df2 = df2 %>% bind_rows(temp)
  df2 = data.frame(df2, stringsAsFactors = F)
  
}


## close
remDr$close()
rD$server$stop() 
gc()





