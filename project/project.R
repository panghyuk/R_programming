library(randomForest)
library(xgboost)
library(dplyr)
library(ggplot2)
set.seed(20221122)

diab <- read.csv('./project/diabetes.csv')
summary(diab)
str(diab)

### 결측치(null값) 확인 -> 결측치 X
sum(is.na(dia))

### 이상치 확인


### 데이터 스케일링


### EDA


### 모델링
