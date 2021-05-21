## clustering tiers of players over the most recent NHL season and over time
library(data.table)
library(ggplot2)
library(ComplexHeatmap)
library(cluster)
#library(ggrepel)
library(dendextend)

setwd("~/Documents/hockey-stats/data/")


## load data
print(load("052021_skater_data_reg_season_1917_2021.rsav"))
dim(df)
#[1] 7400   22

head(df)


## subset and make numeric
df = df[,c(1,4:8)]

## make columns of interest numeric
df$GP = as.numeric(gsub(",", "", df$GP))
df$G = as.numeric(gsub(",", "", df$G))
df$A = as.numeric(gsub(",", "", df$A))
df$P = as.numeric(gsub(",", "", df$P))
df$plus_minus = as.numeric(gsub("+|-", "", df$`+/-`))
df$`+/-` = NULL


## remove any rows missing date and players with < 50 games
df = df[nchar(df$Player) > 3 & df$GP >= 50,,drop=F]

## get stats per GP
df[,3:6] = df[,3:6] / df[,2]
colnames(df)[3:6] = paste0(colnames(df)[3:6], "_GP")
df = df[order(df$P_GP, decreasing = T),,drop=F]
#df[,3:6] = scale(df[,3:6])


## final prep
df$GP = NULL
df = df[!duplicated(df$Player),,drop=F]
rownames(df) = df$Player
df$Player = NULL
dim(df)
#[1] 4605    4



## ------------------------------------------------------
## All seasons (1917-present)

## keep a max of 500 players for viz purposes
df2 = df[,c("G_GP", "A_GP")]
df2 = df2[1:100,]


## ---- kmeans
set.seed(1234)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(df2, i)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')



## ---- agglomerative
## get distances
d = dist(df2, method = "euclidean")
head(d)
length(d) == choose(100,2)

## quick maths
sqrt((df2[2,1] - df2[1,1])**2 + (df2[2,2] - df2[1,2])**2)
d[1]

## what if it is 3 dimensional??????
dummy_df = df[1:10,1:3]
test = dist(dummy_df, method = "euclidean")
sqrt((dummy_df[2,1] - dummy_df[1,1])**2 + (dummy_df[2,2] - dummy_df[1,2])**2 + (dummy_df[2,3] - dummy_df[1,3])**2)
test[1]



## hierarchical clustering
hc1 = hclust(d, method = "ward.D" )

## plot dendogram
plot(hc1, cex = 0.6, hang = -1, main = "Dendrogram of NHL skaters")



## comparing different dendrogram methods on a subset
d2 = dist(df2[1:50,], method = "euclidean")
test1 = hclust(d2, method = "ward.D") 
test2 = hclust(d2, method = "complete") #complete is maximum difference between ALL pairwise points of cluster

# Create two dendrograms
dend1 = as.dendrogram(test1)
dend2 = as.dendrogram(test2)

dend_list = dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = FALSE, # Color common branches
           highlight_distinct_edges = TRUE, 
           highlight_branches_lwd = FALSE,
           columns_width = c(5, 1.5, 5),
           main = paste("entanglement =", round(entanglement(dend_list), 2)),
           cex_main = 1.25,
           lwd = 2,
           lab.cex = 1,
           margin_inner = 7)




## make heatmap
df3 = as.matrix(t(df2))
Heatmap(df3, name = "G|A / GP", row_dend_side = "right", column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 8), row_names_gp = gpar(fontsize = 8),
        column_dend_height = unit(6, "cm"),
        clustering_method_rows = "ward.D",
        clustering_method_columns = "ward.D",
        column_title = "Hiearchical clustering of the top 100 NHL skaters by Points/GP\n1917-2021 Regular Seasons")









## ------------------------------------------------------
## 2020-2021 only
## question: are there "tiers" of players based on Goals, Assists, Plus/Minus
## or will we just discover positions? (positions akin to molecular subtype)









## visualizing clusters in 2D
#hc5 = cutree(hc1, 5)

# # Visualising the clusters
# fviz_cluster(list(data = df2, cluster = hc5),
#              labelsize = 10, geom="point") +
#     geom_text_repel(data=df2[1:10,], aes(x=G_GP, A_GP), label=rownames(df2)[1:10])
# 





#