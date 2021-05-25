## clustering tiers of players over the most recent NHL season and over time
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(factoextra))

setwd("~/Documents/hockey-stats/data/")

## load data
print(load("052021_skater_data_reg_season_1917_2021.rsav"))
dim(df)

## look at data
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

## remove any rows missing date and players with < 75 games
df = df[nchar(df$Player) > 3 & df$GP >= 75,,drop=F]

## get stats per GP
df[,3:6] = df[,3:6] / df[,2]
colnames(df)[3:6] = paste0(colnames(df)[3:6], "_GP")
df = df[order(df$P_GP, decreasing = T),,drop=F]
head(df)

## final prep
df$GP = NULL
df = df[!duplicated(df$Player),,drop=F]
rownames(df) = df$Player
df$Player = NULL
dim(df)


## ------------------------------------------------------
## All seasons (1917-present)

## keep a max num of players for viz purposes
df2 = df[,c("G_GP", "A_GP")]
df2 = df2[1:100,]

## make heatmap of top 100 players by P/GP
temp = df2
temp$player = rownames(temp)
df_melt = melt(temp, id.vars="player", measure.vars=c("G_GP", "A_GP"))
colnames(df_melt) = c("player", "stat", "value")
df_melt$stat = ifelse(df_melt$stat == "G_GP", "G/GP", "A/GP")

## factor
df_melt$player = factor(df_melt$player, levels=temp$player)

p1 = ggplot(df_melt, aes(player, stat, fill=value)) +
  geom_tile(color=alpha("black",.6), size=.075) +
  labs(title = "All-Time Top 100 NHL Skaters by Points / Games Played",
       subtitle = "Minimum 75 games played\n1917-2021",
       x ="Skater", y="Point type", fill="Career avg") +
  scale_fill_gradient2(high="red", low="blue", mid="gray90", midpoint=.5) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=6, angle=90, hjust=1, vjust=.5),
        legend.key.size = unit(.45, "cm"),
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8))
p1

pdf("figures/0521_heatmap_goals_assists.pdf", width=9, height=2.5)
  p1
dev.off()

## scale by (x - mean(x)) / sd(x)
df2 = scale(df2)
head(df2)

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

kmeans = kmeans(x = df2, centers = 7)
y_kmeans = kmeans$cluster


## visualising the clusters
p2 = fviz_cluster(list(data = df2, cluster = y_kmeans),
             labelsize = 10, geom="point", 
             xlim = c(-2.5,6), ylim =c(-3,5),
             show.clust.cent = FALSE) +
  geom_label_repel(data=data.frame(df2[c(1:20,45,96:100),],stringsAsFactors = F), 
                  aes(x=G_GP, A_GP), label=rownames(df2)[c(1:20,45,96:100)],
                  size=3.1, fill="#FFFFFF40", color="#000000BF") +
  geom_hline(yintercept = 0, color="black", linetype="dashed", alpha=.75, size=.2) +
  geom_vline(xintercept = 0, color="black", linetype="dashed", alpha=.75, size=.2) +
  labs(title = "Clustering the top 100 NHL skaters of all time by G/GP and A/GP",
        subtitle = "Selected by career P/GP average",
        x="Scaled G/GP", y="Scaled A/GP") +
  theme_bw()

p2

pdf("figures/0521_kmeans_clustering.pdf", width=8, height=6)
  p2
dev.off()


## ---- agglomerative
## get distances
d = dist(df2, method = "euclidean")
head(d)
length(d) == choose(100,2)
length(d)

## quick maths
head(df2,3)
sqrt((df2[2,1] - df2[1,1])**2 + (df2[2,2] - df2[1,2])**2)
d[1]

## what if it is 3 dimensional??????
dummy_df = df[1:10,1:3]
head(dummy_df,3)
test = dist(dummy_df, method = "euclidean")
sqrt((dummy_df[2,1] - dummy_df[1,1])**2 + (dummy_df[2,2] - dummy_df[1,2])**2 + (dummy_df[2,3] - dummy_df[1,3])**2)
test[1]

## hierarchical clustering
hc1 = hclust(d, method = "ward.D" )

## plot dendogram
plot(hc1, cex = 0.5, hang = -1, main = "Dendrogram of NHL skaters")

## aside: comparing different dendrogram methods on a subset
d2 = dist(df2[1:50,], method = "euclidean")
test1 = hclust(d2, method = "ward.D") 
test2 = hclust(d2, method = "complete") #complete is maximum difference between ALL pairwise points of cluster

# create two dendrograms
dend1 = as.dendrogram(test1)
dend2 = as.dendrogram(test2)
dend_list = dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           common_subtrees_color_lines = FALSE,
           common_subtrees_color_branches = FALSE,
           highlight_distinct_edges = TRUE,
           highlight_branches_lwd = FALSE,
           columns_width = c(5, 1.5, 5),
           main = paste("entanglement =", round(entanglement(dend_list), 2)),
           cex_main = 1.25,
           lwd = 2,
           lab.cex = 1,
           margin_inner = 7)

## make first heatmap
df3 = as.matrix(t(df2))
p3 = Heatmap(df3, name = "G|A / GP", row_dend_side = "right", column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 6), row_names_gp = gpar(fontsize = 7),
        column_dend_height = unit(4.5, "cm"),
        clustering_method_rows = "ward.D",
        clustering_method_columns = "ward.D",
        column_title = "Hiearchical clustering of the top 100 NHL skaters\n1917-2021 Regular season data")

pdf("figures/0524_hac_gpg_apg.pdf", width=9, height=4.5)
  p3
dev.off()





# ------------------------------------------------------
# 2020-2021 only
## load data
rm(df)
rm(df2)
rm(df3)
print(load("052021_skater_data_reg_season_2020_2021.rsav"))

## look at data
head(df)

## subset and make numeric
df = df[,c(1,5,6:10,14,16,18,21)]

## make columns of interest numeric
df[3:ncol(df)] = sapply(df[3:ncol(df)], as.numeric)
colnames(df)[7] = "Plus_minus"

## remove any rows missing date and players with < 50 games
df = df[nchar(df$Player) > 3 & df$GP >= 25,,drop=F]

## get stats per GP
df[,4:ncol(df)] = df[,4:ncol(df)] / df[,3]
colnames(df)[4:ncol(df)] = paste0(colnames(df)[4:ncol(df)], "_GP")
df = df[order(df$P_GP, decreasing = T),,drop=F]

## final prep
df2 = df[-3]
df2 = df2[!duplicated(df2$Player),,drop=F]
rownames(df2) = df2$Player
df2$Player = NULL
dim(df2)


set.seed(130)

## keep a max num of players for viz purposes
column_ha = columnAnnotation(Pos = df2$Pos[1:100], annotation_name_gp = gpar(fontsize = 8))
df2$Pos = NULL
df2 = df2[1:100,]

## scale
df2 = scale(df2)

## ---- agglomerative
## get distances
d = dist(df2, method = "euclidean")

## hierarchical clustering
hc1 = hclust(d, method = "ward.D" )

df3 = as.matrix(t(df2))
p4 = Heatmap(df3, name = "Scaled metric / GP", row_dend_side = "right", column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 7.25), row_names_gp = gpar(fontsize = 8),
        column_dend_height = unit(4, "cm"),
        clustering_method_rows = "ward.D",
        clustering_method_columns = "ward.D",
        bottom_annotation = column_ha,
        column_title = "Hiearchical clustering of the top 100 NHL skaters by Points/GP\n2020-21 Regular Season",
  rect_gp = gpar(col = alpha("black",.5), lwd = .01))

p4


pdf("figures/0524_hac_more_cats.pdf", width=12, height=6)
  p4
dev.off()






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



## ---- kmeans
set.seed(1234)
wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(df2, i)$withinss)
plot(1:20,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

kmeans = kmeans(x = df2, centers = 10)
y_kmeans = kmeans$cluster






# el fin
# el fin