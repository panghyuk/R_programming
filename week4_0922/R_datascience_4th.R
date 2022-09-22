library(ggplot2)
library(dplyr)
library(lmPerm)

session_times <- read.csv('./data/r_datascience_4th_data/web_page_data.csv')
session_times[,2] <- session_times[,2] * 100
four_sessions  <- read.csv('./data/r_datascience_4th_data/four_sessions.csv')
click_rate <-  read.csv('./data/r_datascience_4th_data/click_rates.csv')
imanishi <-  read.csv('./data/r_datascience_4th_data/imanishi_data.csv')

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

cconversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588 )
hist(perm_diffs, xlab='Conversion rate (percent)', main='')
abline(v = obs_pct_diff, lty=2, lwd=1.5)
text("  Observed\n  difference", x=obs_pct_diff,  y=par()$usr[4]-20, adj=0)

mean(perm_diffs > obs_pct_diff)


t.test(Time ~ Page, data=session_times, alternative='less' )
