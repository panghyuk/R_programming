library(ggplot2)
library(dplyr)
library(lmPerm)

session_times <- read.csv('./week4_0922/r_datascience_4th_data/web_page_data.csv')
session_times[,2] <- session_times[,2] * 100
four_sessions  <- read.csv('./week4_0922/r_datascience_4th_data/four_sessions.csv')
click_rate <-  read.csv('./week4_0922/r_datascience_4th_data/click_rates.csv')
imanishi <-  read.csv('./week4_0922/r_datascience_4th_data/imanishi_data.csv')

## Code snippet 3.1
ggplot(session_times, aes(x=Page, y=Time)) + 
  geom_boxplot()

mean_a <- mean(session_times[session_times['Page']=='Page A', 'Time'])
mean_b <- mean(session_times[session_times['Page']=='Page B', 'Time'])
mean_b - mean_a

## Permutation test example with stickiness
perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Session time differences (in seconds)', main='')
abline(v = mean_b - mean_a)


### Documentary와 comedy 간 IMDB 점수 차이가 유의미한지?
netflix <- read.csv('./week4_0922/r_datascience_4th_data/NetflixOriginals.csv')
netflix_reduced = netflix$IMDB.Score[(netflix$Genre == 'Documentary') | (netflix$Genre == 'Comedy')]
len_doc = sum(netflix$Genre == 'Documentary')
len_com = sum(netflix$Genre == 'Comedy')
perm_fun(netflix_reduced, len_doc, len_com)

perm_diffs <- rep(0, 10000)
for(i in 1:10000)
  perm_diffs[i] = perm_fun(netflix_reduced, len_doc, len_com)
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Session time differences (in seconds)', main='')

mean_diff = mean(netflix$IMDB.Score[(netflix$Genre == 'Documentary')]) - 
  mean(netflix$IMDB.Score[(netflix$Genre == 'Comedy')])

abline(v = mean_diff)

### perm_fun 예시
a <- c(95,94,91,97,95,91) # vector 생성
b <- c(80,84,84,88,86)

x = c(95,94,91,97,95,91,80,84,87,88,86)



### 
obs_pct_diff <- 100 * (200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588 )
hist(perm_diffs, xlab = 'Conversion rate (percent)', main = '')
abline(v = obs_pct_diff, lty = 2, lwd = 1.5)
text("  Observed\n  difference", x = obs_pct_diff,  y = par()$usr[4]-20, adj=0)

mean(perm_diffs > obs_pct_diff) # True/False

t.test(Time ~ Page, data=session_times, alternative = 'less')



### 연습문제
a <- c(95,84,71,67,55,41)
b <- c(80,74,68,88,86)
x <- c(a,b)

mean_a <- mean(a)
mean_b <- mean(b)
mean_diff = mean_b - mean_a
#mean_diff = mean_a - mean_b
sample_5 = sample(x, 5)
sample_6 = setdiff(x, sample_5) # 차집합

perm_fun(x,6,5)

perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(x, 6, 5)

hist(perm_diffs, xlab='Score Diff', main='')
abline(v = mean_diff)

mean(perm_diffs > mean_diff)
