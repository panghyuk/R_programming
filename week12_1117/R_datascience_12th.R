library(randomForest)
library(dplyr)
library(ggplot2)
library(xgboost)

loan3000 = read.csv('./week12_1117/loan3000.csv')
loan3000$outcome <- ordered(loan3000$outcome, levels=c('paid off', 'default'))

# RF 모델 생성
rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio,
                   data = loan3000)
rf

error_df = data.frame(error_rate = rf$err.rate[,'OOB'], 
                      num_trees = 1:rf$ntree)
# RF error 시각화
ggplot(error_df , aes(x = num_trees , y = error_rate)) + geom_line() # 선으로 그려라

pred = predict(rf)
rf_df = cbind(loan3000 , pred_default = pred)

ggplot(data = rf_df , aes(x = borrower_score , y=payment_inc_ratio, 
        color = pred_default , shape = pred_default))+
geom_point(alpha = 0.6 , size=2)
scale_shape_manual ( values = c(46,4))

#####
pred <- predict(rf, prob=TRUE)
rf_df <- cbind(loan3000, pred = pred)

ggplot(data=rf_df, aes(x=borrower_score, y=payment_inc_ratio, 
                       shape=pred, color=pred)) +
  geom_point(alpha=.6, size=2) +
  scale_shape_manual( values=c( 46, 4)) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), lim=c(0, 20)) + 
  theme_bw()

#####
loan3000 <- select(loan3000, -X) # X칼럼 제외
rf_all <- randomForest(outcome ~ ., data = loan3000, importance = TRUE) # 모든 변수 선택
rf_all

# 얼마나 var가 중요한지? 
varImpPlot(rf_all , type=1)
varImpPlot(rf_all , type=2)

##### Practice
loan_data = read.csv('./week12_1117/loan_data.csv', stringsAsFactors = TRUE)
loan_data = select(loan_data,-X)
rf_all <- randomForest(outcome ~ ., data = loan_data, importance = TRUE, max = 2)
rf_all

varImpPlot(rf_all, type = 1)
varImpPlot(rf_all, type = 2)

#########################################
predictors <- data.matrix(loan3000[, c('borrower_score', 'payment_inc_ratio')])
label <- as.numeric(loan3000[,'outcome'])-1
xgb <- xgboost(data=predictors, label=label, objective = "binary:logistic", 
               params=list(subsample=.63, eta=0.1), nrounds=100)

pred <- predict(xgb, newdata=predictors)
xgb_df <- cbind(loan3000, pred_default=pred>.5, prob_default=pred)

ggplot(data=xgb_df, aes(x=borrower_score, y=payment_inc_ratio, 
                        color=pred_default, shape=pred_default)) +
  geom_point(alpha=.6, size=2) +
  scale_shape_manual( values=c( 46, 4)) +
  scale_x_continuous(expand=c(.03, 0)) + 
  scale_y_continuous(expand=c(0,0), lim=c(0, 20)) + 
  theme_bw()

mean((pred>0.5) ==label)

seed <- 400820
loan_data = read.csv('./week12_1117/loan_data.csv')
loan_data <- select(loan_data, -X, -status)
loan_data = na.omit(loan_data)
predictors <- data.matrix(loan_data[,-which(names(loan_data) %in% 'outcome')])
label <- loan_data$outcome=='default'
test_idx <- sample(nrow(loan_data), 10000)

# Overfitting 피하기
xgb_default <- xgboost(data = predictors[-test_idx,], label=label[-test_idx], 
                       objective = "binary:logistic", nrounds=250, verbose=0)
pred_default <- predict(xgb_default, predictors[test_idx,])
error_default <- abs(label[test_idx] - pred_default) > 0.5
xgb_default$evaluation_log[250,]
mean(error_default)

# Overfitting 피하기 -> Ridge & Lasso
xgb_penalty <- xgboost(data = predictors[-test_idx,], 
                       label = label[-test_idx], 
                       params = list(eta=.1, subsample = .63, lambda = 1000),
                       objective = "binary:logistic", nrounds = 250, verbose = 0)
pred_penalty <- predict(xgb_penalty, predictors[test_idx,])
error_penalty <- abs(label[test_idx] - pred_penalty) > 0.5
xgb_penalty$evaluation_log[250,]
mean(error_penalty)

##### Xgboost_Regression
sp = read.csv('./week12_1117/Student Performance new.csv')
label = sp$reading.score.percentage
sp <- select(sp, -X, -reading.score.percentage)
sp = data.matrix(sp)
xgb_default <- xgboost(data=sp, label=label, 
                       objective = 'reg:squarederror', nrounds=250, verbose=0)
pred_default <- predict(xgb_default, sp)
