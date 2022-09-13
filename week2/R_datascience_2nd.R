# packages needed for chapter 2
library(dplyr)
library(tidyr)
library(ggplot2)
library(vioplot)
library(ascii)
library(corrplot)
library(descr)

state <- read.csv('./data/state.csv')

mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])

## Code snippet 1.2
mean(state[["Murder.Rate"]])
library("matrixStats")
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])

sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])


## Code snippet 1.4
quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))
ascii(
  quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentiles of murder rate by state.")

state[["Population"]]/1000000
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")
boxplot(state[["Murder.Rate"]]/1000000, ylab="Murder.Rate")


breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
state['PopFreq'] <- pop_freq
table(pop_freq)

hist(state[["Population"]], breaks=breaks)

hist(state[["Murder.Rate"]], freq=FALSE )
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")

dfw <- read.csv('./data/dfw_airline.csv')
ascii(
  100*as.matrix(dfw/sum(dfw)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentage of delays by cause at Dallas-Ft. Worth airport.")

barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)


sp500_sym <- read.csv('./data/sp500_sectors.csv')
sp500_px <- read.csv('./data/sp500_data.csv')

telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
telecom_cor <- cor(telecom)
ascii(telecom_cor, digits=c( 3,3,3,3,3), align=c("l", "r", "r", "r", "r", "r"), caption="Correlation between telecommunication stock returns.",
      include.rownames = TRUE, include.colnames = TRUE)


etfs <- sp500_px[row.names(sp500_px)>"2012-07-01", 
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]
corrplot(cor(etfs), method = "ellipse")


plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ", cex=.8)


kc_tax <- read.csv('./data/kc_tax.csv') 
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 &
                    SqFtTotLiving<3500)
nrow(kc_tax0)

ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")


lc_loans  <- read.csv('./data/lc_loans.csv') 
x_tab <- CrossTable(lc_loans$grade, lc_loans$status, 
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)
x_tab 



airline_stats <- read.csv('./data/airline_stats.csv') 
airline_stats$airline <- ordered(airline_stats$airline, levels=c('Alaska', 'American', 'Jet Blue', 'Delta', 'United', 'Southwest'))
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))



ggplot(data=airline_stats, aes(airline, pct_carrier_delay))  + 
  ylim(0, 50) + 
  geom_violin() +
  labs(x="", y="Daily % of Delayed Flights")


ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")


