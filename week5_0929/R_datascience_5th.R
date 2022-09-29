pchisq(3, df=3)
qchisq(0.95, df=3)
rchisq(10, df=3)


pt(0, df=5)
pt(0, df=5) - pt(-2.571, df=5)
qt(0.975, df=5)
rt(10, df=5)



pf(5.409, df1=3, df2=5)
qf(0.95, df1=3, df2=5)
rf(10, df1=3, df2=5)

data = read.table('./week5_0929/r_datascience_5th_data/chapter7.txt', header=TRUE)
var.test(data$weight ~ data$gender)
t.test(data$weight ~ data$gender,	mu=0,	var.equal=TRUE ) # (alternative=less) 생략

data = read.csv('./week5_0929/r_datascience_5th_data/01.anorexia.csv', header=TRUE)

n <- length(data$Prior - data$Post)
m <- mean( data$Prior - data$Post)
s <- sd (data$Prior - data$Post)
tt = (t.t <- m / (s / sqrt(n)))
pt(tt, df=n-1)

t.test(data$Prior, data$Post, 	paired=TRUE, 	alternative="less")


ad = read.csv('./week5_0929/r_datascience_5th_data/age.data.csv', header=TRUE)
y1 <- ad$age[ad$scale=="1"]
y2 <- ad$age[ad$scale=="2"]
y3 <- ad$age[ad$scale=="3"]

y1.mean <- mean( y1 )
y2.mean <- mean( y2 )
y3.mean <- mean( y3 )

sse.1 <- sum((y1 - y1.mean)^2)
sse.2 <- sum((y2 - y2.mean)^2)
sse.3 <- sum((y3 - y3.mean)^2)

(sse <- sse.1 + sse.2 + sse.3)
(dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1))

y <- mean(ad$age) # 전체 평균

sst.1 <- length(y1) * sum((y1.mean - y)^2)
sst.2 <- length(y2) * sum((y2.mean - y)^2)
sst.3 <- length(y3) * sum((y3.mean - y)^2)

(sst <- sst.1 + sst.2 + sst.3)
(dft <- 3 - 1)

( tsq <- sum( (ad$age - y)^2 ) )
( ss <- sst + sse )

mst <- sst / dft
mse <- sse / dfe
f.t <- mst / mse

alpha <- 0.05
(tol <- qf(1-alpha, 2, 147))
(p.value <- 1 - pf(f.t, 2, 147))

ad$scale2 = as.character(ad$scale)
ow <- lm(age~scale2, data=ad)
anova(ow)
