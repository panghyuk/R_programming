a = c(1,2,3,4,5)
b = c('Tiger','Lion','Elephant')

x<- 1:5
y = 2

z = x+y

print('Welcome to R')
paste('Tiger','Lion','Elephant')

myclass <- readline("What is your favorite class??")
paste("My class is", myclass)

urban.pop <- c(50,47,69,47,47,57,72,47,51,40)
life.exp <- c(67,72,77,65,62,74,98,56,77,66)
mean(life.exp)
sd(life.exp)
cor(life.exp , urban.pop)

life.lm <- lm(life.exp ~ urban.pop)
life.lm

plot(urban.pop, life.exp, col='blue', pch=10, 
     main = "Urban population Percent vs life expectancy", 
     xlab = "Urban population Percent (%)", 
     ylab = "Life Expectancy (AGE)")
abline(life.lm, col='red')

getwd()
setwd("D:/R_programming/practice")
getwd()

x = 100
y = c(2,3,5,7)
hero = c('Spiderman', 'Batman', 'Superman')
f <- function(y) y-32
ls()
ls(pattern='e')

save(hero, file='hero.rdda')
rm(hero)
load("hero.rdda")
hero

m<- matrix(c(1,2,3,4,5,6), ncol=2)
m

t(m)

t = function(x) x+100
t(m)

base::t(m)

data()
head(cars,10)


library(MASS)
head(Animals)


