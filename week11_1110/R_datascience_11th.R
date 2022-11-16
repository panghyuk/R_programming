credit = read.csv('credit.txt')
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

set.seed(123) # use set.seed to use the same random number sequence as the tutorial
train_sample <- sample(1000, 900)

str(train_sample)

# split the data 
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)

credit_model <- C5.0(credit_train[-17], as.factor(credit_train$default))

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

sum(predict(credit_model, credit_train)==credit_train$default)/length(credit_train$default)
sum(predict(credit_model, credit_test)==credit_test$default)/length(credit_test$default)


c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = TRUE, CF=0.01)

credit_model <- C5.0(credit_train[-17], as.factor(credit_train$default),
                     control=c5_options,)
library(caret)
pred = predict(credit_model, credit_test , type='class')
confusionMatrix(pred, 
                as.factor(credit_test$default))
pred_prob = predict(credit_model, credit_test , type='prob')

library(Epi)
ROC(form=(credit_test$default=='yes')~pred_prob[,1], plot="ROC")


###################### 회귀 트리
library(rpart)
library(rpart.plot)
wine = read.csv('whitewines.csv')
len = length(wine$fixed.acidity)

train_sample <- sample(len, as.integer(len*0.8))
wine_train <- wine[train_sample, ]
wine_test  <- wine[-train_sample, ]

wine_model <-rpart(quality ~ ., wine_train)
rpart.plot(wine_model)

pred_test = predict(wine_model, wine_test)
pred_train = predict(wine_model, wine_train)

mean((pred_train-wine_train$quality)^2)
mean((pred_test-wine_test$quality)^2)

plotcp(wine_model)

opt <- which.min(wine_model$cptable[, "xerror"])
cp <- wine_model$cptable[opt,"CP"]
prune.wine_model <- prune(wine_model, cp=cp)
rpart.plot(prune.wine_model)
