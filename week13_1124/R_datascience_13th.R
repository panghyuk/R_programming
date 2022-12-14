library(ggplot2)
sp500_px <- read.csv('./week13_1124/sp500_data.csv')

df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]

### K-Means
km <- kmeans(df, centers = 4, nstart = 1)

df$cluster <- factor(km$cluster)
head(df)

centers <- data.frame(cluster = factor(1:4), km$centers)
centers

ggplot(data = df, aes(x = XOM, y = CVX, color = cluster, shape = cluster)) +
  geom_point(alpha = .3) +
  geom_point(data = centers,  aes(x = XOM, y = CVX), size = 2, stroke = 2)  +
  theme_bw() +
  scale_x_continuous(expand = c(0,0), lim = c(-2, 2)) + 
  scale_y_continuous(expand = c(0,0), lim = c(-2.5, 2.5)) 

pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)
totalss <- kmeans(df, centers=14, nstart=50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(df, centers=i, nstart=50, iter.max = 100)$betweenss/totalss
}

ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% Variance Explained', x='Number of Clusters') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()


### ex1
spn <- read.csv('./week13_1124/Student Performance new.csv')
df <- spn[,c("math.percentage","reading.score.percentage")]

# clustering model training
km <- kmeans(df, centers = 4, nstart = 1)

# add cluster information for visualization
df$cluster <- factor(km$cluster)

# add cluster information from km model for visualize center
centers <- data.frame(cluster = factor(1:4), km$centers)

# Visualization
ggplot(data = df, aes(x = math.percentage, y = reading.score.percentage, color = cluster, shape = cluster)) +
  geom_point(alpha = .3) +
  geom_point(data = centers,  aes(x = math.percentage, y = reading.score.percentage), size = 2, stroke = 2)

# elbow method
pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)

totalss <- kmeans(df, centers=14, nstart=50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(df, centers=i, nstart=50, iter.max = 100)$betweenss/totalss
}

ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% Variance Explained', x='Number of Clusters') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()


### Hierarchical Clustering
syms1 <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 
           'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP',
           'WMT', 'TGT', 'HD', 'COST')

df <- sp500_px[row.names(sp500_px)>='2011-01-01', syms1]
d <- dist(t(df))
# modeling
hcl <- hclust(d)

plot(hcl, ylab='distance', xlab='', sub='', main='')

### iris example 
ir = iris[c(1,2,3,4)]
d <- dist(ir)
hcl <- hclust(d)
plot(hcl, ylab = 'distance', xlab = '', sub = '', main = '')

cutree(hcl , k = 3)

plot(iris$Sepal.Length, iris$Sepal.Width , col=cutree(hcl , k = 3), 
     pch = 19)


### ex2
student <- read.csv('./week13_1124/Student Performance new.csv')

df <- student[,c("math.percentage","reading.score.percentage")]

d <- dist(df)

hcl <- hclust(d)

df$cluster = factor(cutree(hcl, k = 3))

ggplot(data = df, aes(x = math.percentage, y = reading.score.percentage, color = cluster, shape = cluster)) +
  geom_point(alpha = .3)

### DBSCAN
library(fpc)
library(ggplot2)

db = dbscan(ir , eps = 0.5 , MinPts = 4)
db$cluster
db$isseed

plot(iris$Sepal.Length, iris$Sepal.Width , col=as.factor(db$cluster), 
     pch = db$isseed*1)

legend(7.5,4.0,legend = as.numeric(levels(as.factor(db$cluster))),
       col = as.numeric(levels(as.factor(db$cluster)))+1, pch = 1)


### ex3
student <- read.csv('./week13_1124/Student Performance new.csv')
df <- student[, c("math.percentage","reading.score.percentage")]
db = dbscan(df , eps = 0.03, MinPts = 3)
df$cluster = factor(db$cluster)
df$isseed = factor(db$isseed)

ggplot(data = df, aes(x = math.percentage, y = reading.score.percentage, color = cluster, shape = isseed)) + 
  geom_point(alpha = .3)

