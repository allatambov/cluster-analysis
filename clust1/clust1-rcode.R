#### Cluster Analysis 1 ####

### part 1 ###

# reproduce example from lecture
x1 <- c(5, 5, 3, 2, 3.5)
x2 <- c(2, 1, 2, 1, 4)

# merge vectors into a dataframe
dat <- as.data.frame(cbind(x1, x2))
View(dat)

# distance matrix
Mdist <- dist(dat)
Mdist

# hierarchical clustering 
# with the nearest neighbour method (single linkage)
hc <- hclust(Mdist, method = "single")

# plot a dendrogram
plot(hc)

# do the same, but scale data first
Mdist1 <- dist(scale(dat))
hc1 <- hclust(Mdist1, method = "single")
plot(hc1)

### Part 2 ###

# choose file wgi_fh.csv
df <- read.csv(file.choose(), dec = ",")
View(df)

# delete missing values
df <- na.omit(df)

# as an example take 50 random rows from the dataset
# so as to get the same result, run two next lines simultaneously
set.seed(1234) 
data <- df[sample(nrow(df), 50), ]
View(data)

# select numeric columns from data
# from va to fh
library(dplyr)
d <- data %>% select(va:fh)

# name columns according to codes of countries
rownames(d) <- data$cnt_code
View(d)

# distance matrix
M <- dist(scale(d))^2

# cluster analysis using Ward method
hc <- hclust(M, method = "ward.D")

# plot a dendrogram
plot(hc, cex = 0.6) # cex = 0.6 - text size

# main - title of the graph
plot(hc, cex = 0.6, main = "2 clusters") 

# choose 2 clusters - red border around
rect.hclust(hc, k = 2, border="red") 

# the same for 4 clusters
plot(hc, cex = 0.6, main = "4 clusters")
rect.hclust(hc, k = 4, border="red") # 4 clusters

# cutree - split data into k clusters
# look at the cluster labels
groups4 <- cutree(hc, k = 4) 
groups4 

# add a new column to the dataset
# column with group labels
d <- d %>% mutate(groups4 = factor(groups4), country = data$cnt_code)
View(d)

# plot scatterplots with points colored by cluster
library(ggplot2)

# color = group4 - labels for  clusters
ggplot(data = d, aes(x = fh, y = va, color = groups4)) + geom_point() 

# add text labels as well
# vjust и hjust - to adjust labels
ggplot(data = d, aes(x = fh, y = va, color = groups4)) + geom_point() +
  geom_text(aes(label = country, vjust = 0, hjust = 0))

# the same for other pairs of indicators
ggplot(data = d, aes(x = fh, y = cc, color = groups4)) + geom_point() +
  geom_text(aes(label = country, vjust = 0, hjust = 0))

ggplot(data = d, aes(x = rl, y = ge, color = groups4)) + geom_point() +
  geom_text(aes(label = country, vjust = 0, hjust = 0))

# group data by cluster and compare their descriptive statistics
# summarise_at: estimate values for variables indicated in vars
# mean
d %>% group_by(groups4) %>% summarise_at(vars(va:fh), mean)

# median 
d %>% group_by(groups4) %>% summarise_at(vars(va:fh), median)

# min 
d %>% group_by(groups4) %>% summarise_at(vars(va:fh), min)

# max 
d %>% group_by(groups4) %>% summarise_at(vars(va:fh), max)