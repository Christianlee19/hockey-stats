library(ggplot2)
library(tidyverse)
library(data.table)
library(cowplot)
library(zoo)

setwd("~/Documents/hockey-stats/data")


## -----------------------------------------------------------------------------
## PREPARE DATA
## load data
print(load("0215_random_forest.R"))


## merge data using unique id
get_ids = function(i, data_list){
  df = data.frame(data_list[[i]], stringsAsFactors = F)
  print(dim(df))
  df$id = paste0(df$Team, "_", gsub("@|vs|/", "_", df$Game))
  df$id = gsub(" ", "_", df$id)
  return(df)
}

data_list2 = lapply(1:length(data_list), get_ids, data_list)
df = data_list2 %>% reduce(left_join, by = "id")
dim(df)
#[1] 7415  105


## select columns
cols = c("id", "P.x","PP.","PK.","Net.PP.","Net.PK.","Shots.GP","SA.GP","FOW..x",
         "FO", "EV.FO", "PP.FO", "SH.FO", "OZ.FO", "NZ.FO", "DZ.FO", "EV.FOW.", "PP.FOW.", "SH.FOW.", "OZ.FOW.", "NZ.FOW.", "DZ.FOW.",
         "SAT..x", "Hits", "BkS", "GvA", "TkA", "MsS",
         "PIM", "Pen.Drawn", "Pen.Taken", "Net.Pen", "Minor", "Major",
         "SAT..Tied", "USAT..", "X5v5.OZ.", "X5v5.S..Sv.")

new_cols = c("id","points","PP","PK","Net.PP","Net.PK","Shots.GP","SA.GP","FOW",
            "FO", "EV.FO", "PP.FO", "SH.FO", "OZ.FO", "NZ.FO", "DZ.FO", "EV.FOW", "PP.FOW", "SH.FOW", "OZ.FOW", "NZ.FOW", "DZ.FOW",
            "SAT", "Hits", "BkS", "GvA", "TkA", "MsS",
            "PIM", "Pen.Drawn", "Pen.Taken", "Net.Pen", "Minor", "Major",
            "SAT..Tied", "USAT..", "X5v5.OZ", "X5v5.S..Sv")


length(cols) == length(new_cols)
df2 = df[,cols]
df2 = df2[rowSums(df2=="--") == 0,]


## removed NA and dupliated ids
df2 = df2[!is.na(df2$id) & !duplicated(df2$id),]
dim(df2)
#[1] 6319   38

nrow(df2) == length(unique(df2$id))
#[1] TRUE

## rename columns
colnames(df2) = new_cols




## -- include additional info -- ##
## first order by date
## create smaller matrix just for convenience
mat = data.frame(df2[,c("id","points","X5v5.S..Sv")], stringsAsFactors=F)
mat$left_team = gsub("(.+?)_([0-9]{4}.+)__([A-Z]{3})", "\\1", mat$id) 
mat$date = as.Date(gsub("(.+?)_([0-9]{4}.+)__([A-Z]{3})", "\\2", mat$id), format="%Y_%m_%d")
mat = mat[order(mat$date),]


## reorder by id AND date
mat$id_short = gsub("(.+?)_([0-9]{4}.+)__([A-Z]{3})", "\\1_\\3", mat$id)
mat = mat[order(mat$id_short, mat$date),]
length(unique(mat$id_short))
mat$points = as.numeric(mat$points)
mat$X5v5.S..Sv = as.numeric(mat$X5v5.S..Sv)


## POINTS
## previous match up and previous 3 match ups
setDT(mat)[, points_previous_1_matchups:=rollsumr(points, k = 2, fill = NA) - points, by=id_short]
mat[, points_previous_2_matchups:=rollsumr(points, k = 3, fill = NA) - points, by=id_short]
mat[, points_previous_3_matchups:=rollsumr(points, k = 4, fill = NA) - points, by=id_short]

## previous games
## first reorder df just by date
mat[, points_previous_1_game:=rollsumr(points, k = 2, fill = NA) - points, by=left_team]
mat[, points_previous_2_game:=rollsumr(points, k = 3, fill = NA) - points, by=left_team]
mat[, points_previous_3_game:=rollsumr(points, k = 4, fill = NA) - points, by=left_team]
mat[, points_previous_5_game:=rollsumr(points, k = 6, fill = NA) - points, by=left_team]



## X5v5.S..Sv
## previous match up and previous 3 match ups
mat[, X5v5.S..Sv_previous_1_matchups:=rollsumr(X5v5.S..Sv, k = 2, fill = NA) - X5v5.S..Sv, by=id_short]
mat[, X5v5.S..Sv_previous_2_matchups:=rollsumr(X5v5.S..Sv, k = 3, fill = NA) - X5v5.S..Sv, by=id_short]
mat[, X5v5.S..Sv_previous_3_matchups:=rollsumr(X5v5.S..Sv, k = 4, fill = NA) - X5v5.S..Sv, by=id_short]

## previous games
## first reorder df just by date
mat[, X5v5.S..Sv_previous_1_game:=rollsumr(X5v5.S..Sv, k = 2, fill = NA) - X5v5.S..Sv, by=left_team]
mat[, X5v5.S..Sv_previous_2_game:=rollsumr(X5v5.S..Sv, k = 3, fill = NA) - X5v5.S..Sv, by=left_team]
mat[, X5v5.S..Sv_previous_3_game:=rollsumr(X5v5.S..Sv, k = 4, fill = NA) - X5v5.S..Sv, by=left_team]
mat[, X5v5.S..Sv_previous_5_game:=rollsumr(X5v5.S..Sv, k = 6, fill = NA) - X5v5.S..Sv, by=left_team]


## combine with df2
mat = mat[,-c("X5v5.S..Sv","left_team","date","id_short")]
df3 = merge(df2, mat, by = c("id","points"))
dim(df3)
#[1] 6319   52




## -- create more features -- ##
df3$Shot_diff = as.numeric(df3$Shots.GP) - as.numeric(df3$SA.GP)
df3$OZDZ.FO_diff = as.numeric(df3$OZ.FO) - as.numeric(df3$DZ.FO)



## -- final cleanup -- ##
## double check none of the columns are the exact same
for (i in 1:ncol(df3)) {
  for (n in 1:ncol(df3)) {
    is_same = identical(df3[,i], df3[,n])
    if(is_same == T & i!=n) { print(paste(i, n, sep="_"))}
  }
}



## keep complete rows
#df3[df3 == "--"] = NA
#df = na.omit(df)


## rename and factor columns
df3[2:ncol(df3)] = sapply(df3[2:ncol(df3)], as.numeric)
str(df3)


save(df3, file = "rf_data.rsav")




