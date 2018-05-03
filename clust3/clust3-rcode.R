### cluster analysis 3 ###
### assessing the quality of clustering ###

# load data - file reg_elect.csv
df <- read.csv(file.choose())
# look 
View(df)

# choose columns that ends with _perc
library(dplyr)
to_clust <- df %>% select(ends_with("_perc"))
# name rows after country names
rownames(to_clust) <- df$region

# create a distance matrix (no squares yet)
m <- dist(scale(to_clust))

# perform the cluster analysis and plot a dendrogram
hc <- hclust(m, method = "ward.D")
plot(hc, cex = 0.9)

# choose 5 clusters and add rectangles around them
plot(hc, cex = 0.9)
rect.hclust(hc, k = 5)

# alternative - add horizontal line at h=18
plot(hc, cex = 0.9)
abline(h = 18, col = "red") # h - horizontal line, col - color

# save cluster labels and add them as a new column in df
groups5 <- cutree(hc, k = 5)
df$groups5 <- factor(groups5)

# look at clusters - choose rowns corresponding to each group
df %>% filter(groups5 == 1) %>% View
df %>% filter(groups5 == 2) %>% View
df %>% filter(groups5 == 3) %>% View
df %>% filter(groups5 == 4) %>% View
df %>% filter(groups5 == 5) %>% View

# get summary statistics by group
df %>% group_by(groups5) %>% 
  summarise_at(.vars = vars(ends_with("_perc")), .funs = funs(mean)) %>% 
  View

# plot boxplots by group
library(ggplot2)
ggplot(data = df, aes(x = "", y = turnout_perc)) + geom_boxplot() + facet_grid(~groups5)

# plot violin plots by group
ggplot(data = df, aes(x = "", y = turnout_perc, fill = groups5)) + geom_violin() + 
  facet_grid(~groups5)

# plot histograms
# fill - fill color
# col - border color, common for all clusters
# bins - # of columns in histogram
ggplot(data = df, aes(x = turnout_perc , fill = groups5)) + geom_histogram(bins = 6, col = "black") + 
  facet_grid(~groups5)

# graphs for other indices
ggplot(data = df, aes(x = "", y = Grudinin_perc, fill = groups5)) + geom_violin() + 
  facet_grid(~groups5)

ggplot(data = df, aes(x = "", y = Putin_perc, fill = groups5)) + geom_violin() + 
  facet_grid(~groups5)

# scatterplots
ggplot(data = df, aes(x = turnout_perc, y = Putin_perc)) + geom_point(aes(color = groups5)) 
ggplot(data = df, aes(x = Grudinin_perc, y = Putin_perc)) + geom_point(aes(color = groups5)) 

# Kruskal-Wallis test
kruskal.test(df$turnout_perc ~ df$groups5)
kruskal.test(df$Grudinin_perc ~ df$groups5)
kruskal.test(df$Putin_perc ~ df$groups5)

# validate: compare two different clusterings
# groups5 - our current cluster labels
# groups5_2 - labels from CA with average link
hc2 <- hclust(m, method = "average")
groups5_2 <- cutree(hc2, k = 5)

# package fossil - for Rand index
install.packages("fossil")
library(fossil)
rand.index(groups5, groups5_2)

# package fpc - for other correspondence indices
install.packages("fpc")
library(fpc)
# m - distance matrix
cluster.stats(m, groups5, groups5_2)

# for p-values
install.packages("pvclust")
library(pvclust)
fit <- pvclust(t(to_clust), method.hclust = "ward", method.dist = "euclidean")
plot(fit, cex = 0.9) 

# highlight clusters that are supported by data with p=0.95
plot(fit, cex = 0.7)
pvrect(fit, alpha = 0.95)

# package factoextra - for optimal number of clusters
install.packages("factoextra")
library(factoextra)

# Elbow method
fviz_nbclust(to_clust, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# the same but with vertical line
# geom_vline - add vertical line
fviz_nbclust(to_clust, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

# Silhouette method
fviz_nbclust(to_clust, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# kmeans with k = 4
cl <- kmeans(to_clust, 4)
# result
cl
# cluster labels only
df$kmeans4 <- cl$cluster
View(df)

# hc - from the beginning
# rand.index - from package fossil
groups4 <- cutree(hc, k = 4)
rand.index(groups4, cl$cluster)

# two clusters - compare Ward with k = 2 and kmeans with k = 2
groups2 <- cutree(hc, k = 2)
cl2 <- kmeans(to_clust, 2)
rand.index(groups2, cl2$cluster)

# package NbClust - optimal clustering (not always stable)
install.packages("NbClust")
library(NbClust)
res <- NbClust(to_clust, min.nc = 2, max.nc = 8, method = "kmeans")
res$Best.nc

library(factoextra)
fviz_nbclust(res)