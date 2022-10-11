library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
library(lubridate)
library(splines)
library(mgcv)
library(matlib)
setwd('C:/Users/USER/Documents')
lung = read.table('./data/r_datascience_6th_data/LungDisease.csv', sep=',' ,header=TRUE)

model <- lm(PEFR ~ Exposure, data=lung)
model

mean.x = mean(lung$Exposure)
mean.y = mean(lung$PEFR)
sxy = sum((lung$Exposure - mean.x)*(lung$PEFR - mean.y))
sxx = sum((lung$Exposure - mean.x)^2)
b1 = sxy / sxx
b0 = mean.y - b1*mean.x

plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)

data = data.frame( height = c(177, 180, 160, 185, 175, 173, 178, 172, 179, 175, 175), 
            weight = c(67, 80, 55, 85, 70, 69, 60, 80,79, 71, 72 ))

model_weight <- lm(height ~ weight, data=data)

plot(data$weight, data$height, xlab="weight", ylab="height")
abline(a=model_weight$coefficients[1], b=model_weight$coefficients[2], col="blue", lwd=2)


lr = lm(mpg ~ disp + hp + wt + qsec, data=mtcars)
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


house = read.table('./data/r_datascience_6th_data/house_sales.csv', sep='\t',header=TRUE)

head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                            Bedrooms + BldgGrade,  
                          data=house, na.action=na.omit)
summary(house_lm)



lung = read.table('./data/r_datascience_6th_data/LungDisease.csv', sep=',' ,header=TRUE)

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