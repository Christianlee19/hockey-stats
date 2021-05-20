## clustering tiers of players over the most recent NHL season and over time
library(data.table)
library(ggplot2)

setwd("~/Documents/hockey-stats/data/")


## load data
df = fread("basic_stats_2020.csv", header=T, skip = 1)



## ---- agglomerative




## ---- divisive