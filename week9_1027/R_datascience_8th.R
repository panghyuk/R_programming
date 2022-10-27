library(klaR)
loan_data <- read.csv( './week9_1027/loan_data.csv', stringsAsFactors = TRUE)


## Naive Bayes
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, 
                          data = na.omit(loan_data))
naive_model$table

#train data accuracy
pred = predict(naive_model, loan_data)
accuracy = mean(pred$class == loan_data$outcome)

#new data application
new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
new_loan

predict(naive_model, new_loan)


## 강의자료 예제
bank <- read.csv('./week9_1027/bank.csv', stringsAsFactors = TRUE)

# Naive Bayes 모델 생성
bank_model <- NaiveBayes(y ~ job + marital + education + default + housing + loan + contact + month + poutcome
                         , data = na.omit(bank)) # model(종속변수 ~ 독립변수, data = 사용하는 데이터)

bank_model$table

# 모델 예측
pred = predict(bank_model, bank)
acc = mean(pred$class == bank$y)



loan_data <- read.csv( './week9_1027/loan_data.csv', stringsAsFactors = TRUE)

## Logistic Regression
logistic_model <- glm(outcome ~ payment_inc_ratio + purpose_ + 
                        home_ + emp_len_ + borrower_score,
                      data=loan_data, family='binomial')
logistic_model
summary(logistic_model)

#prediction
pred = predict(logistic_model, loan_data)
prob = 1/(1+exp(-pred))
true_y = loan_data$outcome == 'paid off'
pred_y = prob > 0.5
#accuracy
sum(true_y == pred_y)/length(true_y)


## iris
iris2 = iris[iris$Species=='setosa' | iris$Species=='versicolor',]
iris2$Species = iris2$Species=='setosa'
logistic_model <- glm(Species ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width ,
                      data=iris2, family='binomial')

pred = predict(logistic_model , iris2)
prob = 1/(1+exp(-pred))
true_y = iris2$Species
pred_y = prob > 0.5
sum(true_y==pred_y)/length(true_y)

## bank
bank <- read.csv('./week9_1027/bank.csv', stringsAsFactors = TRUE)
lr <- glm(y ~ age + job + marital + education + default + balance + housing,
                      data = bank, family = 'binomial')

pred = predict(lr, bank)
prob = 1/(1+exp(-pred))
true_y = (bank$y == 'yes')
pred_y = prob > 0.5

# accuracy
sum(true_y == pred_y)/length(true_y)
