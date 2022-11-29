library(randomForest)
library(xgboost)
library(dplyr)
library(magrittr)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(e1071)
library(graphics)
library(ggplot2)
library(pROC)
library(caret)
library(ModelMetrics)
set.seed(20221122)

### 데이터 불러오기
diab <- read.csv('./project/diabetes.csv')
summary(diab)
str(diab)


### EDA
# 데이터 분포 확인
hist(diab$Pregnancies)
hist(diab$Glucose)
hist(diab$BloodPressure)
hist(diab$SkinThickness)
hist(diab$Insulin)
hist(diab$BMI)
hist(diab$DiabetesPedigreeFunction)
hist(diab$Age)


# 당뇨병 환자 수
ggplot(data = diab, aes(x = Outcome)) +
  stat_count(fill = 'steelblue')+
  labs(title = 'Outcome Bar Plot', x = 'Outcome', y = 'Count')+
  theme_minimal()

# 상관관계 확인
corr <- cor(diab[,setdiff(names(diab), 'Outcome')])
corrplot::corrplot(corr)
corrplot::corrplot(corr, type = "lower", method = "number")
corrplot::corrplot(corr, type = "lower", method = "pie")


### 결측치(null값) 확인 -> 결측치 X
sum(is.na(diab))


### 이상치 확인
sum(diab$Glucose == 0)
sum(diab$BloodPressure == 0)
sum(diab$SkinThickness == 0)
sum(diab$Insulin == 0)
sum(diab$BMI == 0)
sum(diab$DiabetesPedigreeFunction == 0)
sum(diab$Age == 0)


### Glucose/BMI는 이상치 제거
idx <- diab$Glucose != 0 & diab$BMI != 0
df <- diab[idx,]
nrow(df)
nrow(diab)


### 나머지는 평균값 대체
df$BloodPressure[df$BloodPressure == 0] <- mean(df$BloodPressure)
df$SkinThickness[df$SkinThickness == 0] <- mean(df$SkinThickness)
df$Insulin[df$Insulin == 0] <- mean(df$Insulin)
nrow(df)
nrow(diab)

### 데이터 분할(Train Test Split)
n_idx <- sample(1:nrow(df), round(nrow(df) * 0.8))
train <- df[n_idx,]
test <- df[-n_idx,]

x_train <- select(train, -c("Outcome"))
y_train <- select(train, c("Outcome"))
x_test <- select(test, -c("Outcome"))
y_test <- select(test, c("Outcome"))


### 모델링(LR)
lr2 <- glm(y_train$Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction,
           data = x_train, family = 'binomial')

summary(lr2)

## 최종 모델 검증
lr <- glm(y_train$Outcome ~.,
           data = x_train, family = 'binomial')
summary(lr)

lr_pred <- predict(lr, newdata = x_test, type = 'response')
lr_prob <- 1/(1 + exp(-lr_pred))

result <- as.factor(ifelse(lr_pred >= 0.5, 1, 0))
caret::confusionMatrix(data = result, reference = as.factor(y_test$Outcome))
ModelMetrics::auc(as.factor(y_test$Outcome),predicted = as.factor(result))


### 모델링(RF)
diab <- read.csv('./project/diabetes.csv')
idx <- diab$Glucose != 0 & diab$BMI != 0
df <- diab[idx,]
df$BloodPressure[df$BloodPressure == 0] <- mean(df$BloodPressure)
df$SkinThickness[df$SkinThickness == 0] <- mean(df$SkinThickness)
df$Insulin[df$Insulin == 0] <- mean(df$Insulin)

df$Outcome <- ifelse(df$Outcome == 1, "yes", "no")
df$Outcome <- as.factor(df$Outcome)

n_idx <- sample(1:nrow(df), round(nrow(df) * 0.8))
train <- df[n_idx,]
test <- df[-n_idx,]

x_train <- select(train, -c("Outcome"))
y_train <- select(train, c("Outcome"))
x_test <- select(test, -c("Outcome"))
y_test <- select(test, c("Outcome"))

rf <- randomForest(y_train$Outcome ~ ., data = x_train, importance = TRUE)
rf

rf_pred <- predict(rf, newdata = x_test)
caret::confusionMatrix(data = rf_pred, reference = as.factor(y_test$Outcome))
ModelMetrics::auc(as.factor(y_test$Outcome),predicted = as.factor(rf_pred))


### 모델링 (XGB)
diab <- read.csv('./project/diabetes.csv')
idx <- diab$Glucose != 0 & diab$BMI != 0
df <- diab[idx,]
df$BloodPressure[df$BloodPressure == 0] <- mean(df$BloodPressure)
df$SkinThickness[df$SkinThickness == 0] <- mean(df$SkinThickness)
df$Insulin[df$Insulin == 0] <- mean(df$Insulin)

n_idx <- sample(1:nrow(df), round(nrow(df) * 0.8))
train <- df[n_idx,]
test <- df[-n_idx,]

x_train <- select(train, -c("Outcome"))
y_train <- select(train, c("Outcome"))
x_test <- select(test, -c("Outcome"))
y_test <- select(test, c("Outcome"))

x_train <- data.matrix(x_train)
x_test <- data.matrix(x_test)
y_train <- data.matrix(y_train$Outcome)
y_test <- data.matrix(y_test$Outcome)

xgb <- xgboost(data = x_train, label = y_train, objective = 'binary:logistic',
               params=list(subsample=0.8, eta=0.1), nrounds=100)
xgb

xgb_pred <- predict(xgb, newdata = x_test)
xgb_pred <- as.numeric(xgb_pred > 0.5)
caret::confusionMatrix(data = as.factor(xgb_pred), reference = as.factor(y_test))
ModelMetrics::auc(as.factor(y_test),predicted = as.factor(xgb_pred))

