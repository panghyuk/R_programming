library(FNN)
library(dplyr)
library(ggplot2)
library(gmodels)

loan200 <- read.csv('loan200.csv')
newloan <- loan200[1, 2:3, drop=FALSE]
knn_pred <- knn(train=loan200[-1,2:3], test=newloan, cl=loan200[-1,1], k=20)
knn_pred

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

sum(wbcd_test_pred==wbcd_test_labels)/length(wbcd_test_labels)

CrossTable(x=wbcd_test_labels , y = wbcd_test_pred, prop.chisq = FALSE)

#SVM 
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

library(e1071)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

plot(svmfit, dat)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 10, cex = .01)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

svmfit$coefs
svmfit$rho
coef(svmfit)
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta
beta0 = svmfit$rho


plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


#Kernel SVM 
svmfit = svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length
             + Petal.Width , data=iris, kernel="radial", cost=10, gamma=0.1)
summary(svmfit)
plot(svmfit, iris , Petal.Width~Petal.Length , 
     slice = list(Sepal.Width=3, Sepal.Length=4))

pred <- predict(svmfit, iris)
(acc <- table(pred, iris$Species))
sum(pred==iris$Species)/length(pred)

#hyperparams tuning
tuned = tune.svm(Species ~. , data=iris, gamma=10^(-6:-1), 
                 cost = 10^(1:2))
summary(tuned)