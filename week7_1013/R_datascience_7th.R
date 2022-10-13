library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
library(lubridate)
library(splines)
library(mgcv)
library(matlib)

# setwd('C:/Users/USER/Documents')
lung = read.table('./week7_1013/LungDisease.csv', sep=',' ,header=TRUE)

# lm: Linear Method lm(종속변수(y) ~ 독립변수(x))
model <- lm(PEFR ~ Exposure, data = lung) 
model

# 노가다 계산
mean.x = mean(lung$Exposure)
mean.y = mean(lung$PEFR)
sxy = sum((lung$Exposure - mean.x)*(lung$PEFR - mean.y))
sxx = sum((lung$Exposure - mean.x)^2)
b1 = sxy / sxx
b0 = mean.y - b1*mean.x

# 시각화
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")
# 회귀선
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)

## 넷플릭스 예제
netflix = read.csv('./week7_1013/NetflixOriginals.csv', sep=',' ,header=TRUE)
model2 <- lm(IMDB.Score ~ Runtime, data = netflix)
model2

mean.x = mean(netflix$Runtime)
mean.y = mean(netflix$IMDB.Score)
sxy = sum((netflix$Runtime - mean.x)*(IMDB.Score - mean.y))
sxx = sum((netflix$Runtime - mean.x)^2)
b1 = sxy / sxx
b0 = mean.y - b1*mean.x


## 수업 예제
data = data.frame( height = c(177, 180, 160, 185, 175, 173, 178, 172, 179, 175, 175), 
            weight = c(67, 80, 55, 85, 70, 69, 60, 80,79, 71, 72 ))

model_weight <- lm(height ~ weight, data = data)
model

mean.x = mean(data$height)
mean.y = mean(data$weight)
sxy = sum((data$height - mean.x)*(data$weight - mean.y))
sxx = sum((data$height - mean.x)^2)
b1 = sxy / sxx
b0 = mean.y - b1*mean.x

plot(data$weight, data$height, xlab = "weight", ylab = "height")
abline(a=model_weight$coefficients[1], b=model_weight$coefficients[2], col="blue", lwd=2)


## MTCARS 예제
lr = lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
lr

y = as.matrix(mtcars$mpg)
ones = replicate(length(mtcars$mpg), 1)
x = as.matrix(cbind(mtcars[c('disp', 'hp', 'wt', 'qsec')],ones ))

beta = inv(t(x)%*%x)%*%t(x)%*%y
beta

predict = x %*% beta
predict

MSE = sum((1/length(y)) * ((y-predict)^2))
MSE

SSE = sum((mtcars$mpg - predict(lr, newdata = mtcars))^2) 
SST = sum((mtcars$mpg - mean(mtcars$mpg))^2)
1 - SSE/SST


# house 예제
house = read.table('./week7_1013/house_sales.csv', sep='\t',header=TRUE)

head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                            Bedrooms + BldgGrade,  
                          data=house, na.action=na.omit)
summary(house_lm)


## Lung 예제
lung = read.table('./week7_1013/LungDisease.csv', sep=',' ,header=TRUE)

model <- lm(PEFR ~ Exposure, data=lung)
MSE = mean((lung$PEFR - predict(model , newdata = lung))^2)
MSE

lung$ex2 = lung$Exposure^2
lung$ex3 = lung$Exposure^3
lung$ex4 = lung$Exposure^4
model_poly <- lm(PEFR ~ Exposure + ex2 + ex3 + ex4, data=lung)
MSE = mean((lung$PEFR - predict(model_poly , newdata = lung))^2)
MSE


library(glmnet)

x = as.matrix(house[, c("SqFtTotLiving", "SqFtLot", "Bathrooms", 
              "Bedrooms", "BldgGrade")])
y = as.matrix(house[, c("AdjSalePrice")])

fit1 = glmnet(x,y, alpha=0)
cv1 = cv.glmnet(x,y, alpha = 0 )

plot(fit1 , xvar = 'lambda')
plot(cv1)

best_lam <- cv1$lambda.min
best_lam

fit1 = glmnet(x,y, alpha=1)
cv1 = cv.glmnet(x,y, alpha = 1 )

plot(fit1 , xvar = 'lambda')
plot(cv1)

best_lam <- cv1$lambda.min
best_lam


## Quiz
library(car)

# x: education, women, prestige, census
# y: income
lr2 = lm(income ~ education + women + prestige + census, data = Prestige)
lr2

y = as.matrix(prestige_data$income)
ones = replicate(length(prestige_data$income), 1)
x = as.matrix(cbind(prestige_data[c('education', 'women', 'prestige', 'census')],ones ))

beta = inv(t(x)%*%x)%*%t(x)%*%y
beta

predict = x %*% beta
predict

MSE = sum((1/length(y)) * ((y-predict)^2))
MSE

SSE = sum((prestige_data$income - predict(lr2, newdata = prestige_data))^2) 
SST = sum((prestige_data$income - mean(prestige_data$income))^2)
1 - SSE/SST


## 교수님 풀이
# Q.1
model = lm(income ~ education + women + prestige + census, data = Prestige)
model

# Q.2
MSE = mean((Prestige$income - predict(model,data = Prestige))^2)
SSE = sum((Prestige$income - predict(model,data = Prestige))^2)
SST = sum((Prestige$income - mean(Prestige$income))^2)
r2 = 1 - SSE/SST

# Q.3
new = data.frame(education = c(13.00), women = c(10.00), prestige = c(74.3), census = c(1000))
pred = predict(model,new)
