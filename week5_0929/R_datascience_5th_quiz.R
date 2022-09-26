netflix <- read.csv('./data/r_datascience_3rd_data/NetflixOriginals.csv')
netflix$score = as.double(netflix[["IMDB.Score"]])

t = (mean(netflix$score) - 6.0 ) / (sd(netflix$score)/sqrt(length(netflix$score)))
t
qt(0.975, df=length(netflix$score)-1)

docu = netflix[netflix$Genre=='Documentary',]
comedy = netflix[netflix$Genre=='Comedy',]
netflix_sub = rbind(docu,comedy)

var.test(netflix_sub$IMDB.Score ~ netflix_sub$Genre)

mean(docu$IMDB.Score)
mean(comedy$IMDB.Score)

sd(docu$IMDB.Score)
sd(comedy$IMDB.Score)

t.test(netflix_sub$IMDB.Score ~ netflix_sub$Genre,	mu=0,	alternative='less',	var.equal=TRUE )



docu = netflix[netflix$Genre=='Documentary',]
comedy = netflix[netflix$Genre=='Comedy',]
drama = netflix[netflix$Genre=='Drama',]
netflix_sub = rbind(docu,comedy,drama)

y1 = netflix$IMDB.Score[netflix$Genre=='Documentary']
y2 = netflix$IMDB.Score[netflix$Genre=='Comedy']
y3 = netflix$IMDB.Score[netflix$Genre=='Drama']

y1.mean <- mean( y1 )
y2.mean <- mean( y2 )
y3.mean <- mean( y3 )

sse.1 <- sum( (y1 - y1.mean)^2 )
sse.2 <- sum( (y2 - y2.mean)^2 )
sse.3 <- sum( (y3 - y3.mean)^2 )

(sse <- sse.1 + sse.2 + sse.3)
(dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1))

y <- mean(netflix_sub$IMDB.Score)

sst.1 <- length(y1) * sum((y1.mean - y)^2)
sst.2 <- length(y2) * sum((y2.mean - y)^2)
sst.3 <- length(y3) * sum((y3.mean - y)^2)

(sst <- sst.1 + sst.2 + sst.3)
(dft <- 3 - 1)

( tsq <- sum( (netflix$IMDB.Score - y)^2 ) )
( ss <- sst + sse )

mst <- sst / dft
mse <- sse / dfe
f.t <- mst / mse

df = length(y1)+length(y2)+length(y3)-3

alpha <- 0.05
(tol <- qf(1-alpha, 2, dfe))
(p.value <- 1 - pf(f.t, 2, dfe))


docu = netflix[netflix$Genre=='Documentary',]
comedy = netflix[netflix$Genre=='Comedy',]
drama = netflix[netflix$Genre=='Drama',]
netflix_sub = rbind(docu,comedy,drama)



ow <- lm(IMDB.Score~Genre, data=netflix_sub)
anova(ow)

