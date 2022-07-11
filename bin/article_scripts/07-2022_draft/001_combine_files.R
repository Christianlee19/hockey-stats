## combine excel files

library(data.table)
library(readxl)
library(xlsx)


setwd("~/Documents/hockey-stats/data/")


files = list.files("draft_files_2000_21/")
length(files)


## 
get_combined_files = function(i){
  
  df = fread(paste0("draft_files_2000_21/sportsref_download (", i, ").csv"), skip=1)
  df$year = 2021 - as.numeric(i)
  return(df)
}


## call function
df = do.call(rbind, lapply(0:(length(files)-1), get_combined_files))


save(df, file = "draft_2000_2021.rsav")
