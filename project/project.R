library(randomForest)
library(xgboost)
library(dplyr)
library(ggplot2)
library(pROC)
library(caret)
library(ModelMetrics)
set.seed(20221122)

diab <- read.csv('./project/diabetes.csv')
summary(diab)
str(diab)

### 결측치(null값) 확인 -> 결측치 X
sum(is.na(diab))

### 이상치 확인
sum(diab$Pregnancies == 0)
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

### 데이터 분할(Train Test Split)
n_idx <- sample(1:nrow(df), round(nrow(df) * 0.8))
train <- df[n_idx,]
test <- df[-n_idx,]

x_train <- select(train, -c("Outcome"))
y_train <- select(train, c("Outcome"))
x_test <- select(test, -c("Outcome"))
y_test <- select(test, c("Outcome"))

### 데이터 분할 2(Train Validation Split)



### 데이터 스케일링



### EDA



### 모델링(LR)
lr <- glm(y_train$Outcome ~ .,data=x_train,family='binomial')
lr2 <- glm(y_train$Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction,
           data = x_train, family = 'binomial')

lr2_pred <- predict(lr2, newdata = x_test, type = 'response')
result <- as.factor(ifelse(lr2_pred >= 0.5, 1, 0))

ModelMetrics::auc(as.factor(y_test$Outcome),predicted = as.factor(result))
roc(result,y_test$Outcome)


### 모델링(RF)
rf <- randomForest(y_train$Outcome ~ ., data = x_train, importance = TRUE, ntree = 500, max = 5)
rf_pred <- predict(rf, newdata = x_test)
