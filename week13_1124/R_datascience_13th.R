setwd('C:/Users/USER/Documents/data/r_datascience_13th_data')
sp500_px <- read.csv('sp500_data.csv')

df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
km <- kmeans(df, centers=4, nstart=1)

df$cluster <- factor(km$cluster)
head(df)

centers <- data.frame(cluster=factor(1:4), km$centers)
centers

ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  geom_point(data=centers,  aes(x=XOM, y=CVX), size=2, stroke=2)  +
  theme_bw() +
  scale_x_continuous(expand=c(0,0), lim=c(-2, 2)) + 
  scale_y_continuous(expand=c(0,0), lim=c(-2.5, 2.5)) 

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


syms1 <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 
           'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP',
           'WMT', 'TGT', 'HD', 'COST')

df <- sp500_px[row.names(sp500_px)>='2011-01-01', syms1]
d <- dist(t(df))
hcl <- hclust(d)

plot(hcl, ylab='distance', xlab='', sub='', main='')

#iris example 
ir = iris[c(1,2,3,4)]
d <- dist(ir)
hcl <- hclust(d)
plot(hcl, ylab='distance', xlab='', sub='', main='')

cutree(hcl , k=3)

plot(iris$Sepal.Length, iris$Sepal.Width , col=cutree(hcl , k=3), 
     pch=19)

#####
library(fpc)
library(ggplot2)

db = dbscan(ir , eps=0.5 , MinPts = 4)
db$cluster
db$isseed

plot(iris$Sepal.Length, iris$Sepal.Width , col=as.factor(db$cluster), 
     pch = db$isseed*1)
legend(7.5,4.0,legend=as.numeric(levels(as.factor(db$cluster))),
       col=as.numeric(levels(as.factor(db$cluster)))+1, pch=1)

