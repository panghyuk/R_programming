netflix <- read.csv('./week5_0929/r_datascience_5th_data/NetflixOriginals.csv')

docu = netflix[netflix$Genre=='Documentary',]
comedy = netflix[netflix$Genre=='Comedy',]
netflix_sub = rbind(docu,comedy)

### documentary와 comedy의 분산 차이 확인
var.test(netflix_sub$IMDB.Score ~ netflix_sub$Genre)

# t 분포를 활용하여 t test
t.test(netflix_sub$IMDB.Score ~ netflix_sub$Genre,	mu=0,	alternative='less',	var.equal=TRUE )


### documentary/comedy/drama imdb.score 평균 비교
docu = netflix[netflix$Genre=='Documentary',]
comedy = netflix[netflix$Genre=='Comedy',]
drama = netflix[netflix$Genre=='Drama',]
netflix_sub = rbind(docu,comedy,drama)

# 각 장르 별 imdb.score
y1 = netflix$IMDB.Score[netflix$Genre=='Documentary']
y2 = netflix$IMDB.Score[netflix$Genre=='Comedy']
y3 = netflix$IMDB.Score[netflix$Genre=='Drama']

# 평균 구하기
y1.mean <- mean( y1 )
y2.mean <- mean( y2 )
y3.mean <- mean( y3 )

# SSE 구하기
sse.1 <- sum( (y1 - y1.mean)^2 )
sse.2 <- sum( (y2 - y2.mean)^2 )
sse.3 <- sum( (y3 - y3.mean)^2 )
(sse <- sse.1 + sse.2 + sse.3)

# SSE 자유도 구하기 
(dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1))

# SSt 구하기
y <- mean(netflix_sub$IMDB.Score)

sst.1 <- length(y1) * sum((y1.mean - y)^2)
sst.2 <- length(y2) * sum((y2.mean - y)^2)
sst.3 <- length(y3) * sum((y3.mean - y)^2)
(sst <- sst.1 + sst.2 + sst.3)

# SSt 자유도 구하기
(dft <- 3 - 1)

# 
( tsq <- sum( (netflix$IMDB.Score - y)^2 ) )
( ss <- sst + sse )

#
mst <- sst / dft
mse <- sse / dfe
f.t <- mst / mse

# 
df = length(y1)+length(y2)+length(y3)-3

alpha <- 0.05
(tol <- qf(1-alpha, 2, dfe))
(p.value <- 1 - pf(f.t, 2, dfe)) # p.value = 0 -> 완벽히 오른쪽에 위치


### R 함수를 활용해서 구하기
docu = netflix[netflix$Genre == 'Documentary',]
comedy = netflix[netflix$Genre == 'Comedy',]
drama = netflix[netflix$Genre == 'Drama',]
netflix_sub = rbind(docu,comedy,drama)

ow <- lm(IMDB.Score~Genre, data = netflix_sub)
anova(ow)
