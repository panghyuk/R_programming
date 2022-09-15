library(ggplot2)
loans_income  <- read.csv('./week3_0915/r_datascience_3rd_data/loans_income.csv')[,1]

samp_data <- data.frame(income = sample(loans_income, 1000),
                        type = 'data_dist')

# take a sample of means of 5 values
samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5), 
                  rep(1:1000, rep(5, 1000)), FUN = mean),
  type = 'mean_of_5')

# take a sample of means of 20 values
samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20), 
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type = 'mean_of_20')

# bind the data.frames and convert type to a factor
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, 
                     levels = c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels = c('Data', 'Mean of 5', 'Mean of 20'))

# plot the histograms
ggplot(income, aes(x = income)) +
  geom_histogram(bins = 40) +
  facet_grid(type ~ .)


### Netflix 예제에서 score를 가지고 그림을 그려보시오
netflix  <- read.csv('./week3_0915/r_datascience_3rd_data/NetflixOriginals.csv')
sample_num = 10000

samp_data <- data.frame(score = sample(netflix$IMDB.Score, sample_num, replace=TRUE),
                        type='data_dist') #replace = TRUE: 복원추출

# take a sample of means of 5 values
samp_mean_05 <- data.frame(
    score = tapply(sample(netflix$IMDB.Score, sample_num*5, replace = TRUE), 
                 rep(1:sample_num, rep(5, sample_num)), FUN = mean),
  type = 'mean_of_5')

# take a sample of means of 20 values
samp_mean_20 <- data.frame(
  score = tapply(sample(netflix$IMDB.Score, sample_num*20, replace = TRUE), 
                 rep(1:sample_num, rep(20, sample_num)), FUN = mean),
  type = 'mean_of_20')

# bind the data.frames and convert type to a factor
netflix_score <- rbind(samp_data, samp_mean_05, samp_mean_20)
netflix_score$type = factor(netflix_score$type, 
                      levels = c('data_dist', 'mean_of_5', 'mean_of_20'),
                      labels = c('Data', 'Mean of 5', 'Mean of 20'))

# plot the histograms
ggplot(netflix_score, aes(x = score )) +
  geom_histogram(bins = 100) +
  facet_grid(type ~ .)


### 부트스트랩
library(boot)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun)
boot_obj


norm_samp <- rnorm(100) # 랜덤하게 추출
qqnorm(norm_samp, main = '', xlab = '', ylab = '')
abline(a = 0, b = 1, col = 'grey')

samp = 1:100
qqnorm(samp, main = '', xlab = '', ylab = '')
abline(a = 0, b = 1, col = 'grey')


sp500_px <- read.csv('./week3_0915/r_datascience_3rd_data/sp500_data.csv')
nflx <- sp500_px[,'CA']
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx, main = '', xlab = '', ylab = '')
abline(a = 0, b = 1, col = 'grey')

# 이항분포포
dbinom(x = 0, 200, 0.02)
pbinom( 2, 5, 0.1)

rpois(100, lambda = 2)
mean(rpois(100, lambda = 2))

mean(rexp(100,0.2))