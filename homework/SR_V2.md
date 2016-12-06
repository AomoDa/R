---
title: "Portfolio Optimization"
author: "Your Nmae"
date: "2016-12-06"
output: word_document
---

##Q1

The Federal Funds Interest Rate Reached the maximum value betweeen 2006 and 2008.Probably since 2009, reaching a minimum and known to stabilize until 2015.The financial crisis of 2007–2008, also known as the global financial crisis and the 2008 financial crisis, is considered by many economists to have been the worst financial crisis since the Great Depression of the 1930s.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
#loading data
data.x <- read.csv('C://Users//mali//Documents//asset_data.txt',
                   header = T,
                   stringsAsFactors = F)
#turn the first column from a character string to a date1.
data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")
#Extract only the observations where the federal funds rate is available
data.x <- na.omit(data.x)
#the federal funds interest rate as a time series
with(data.x,plot(date,fed.rate,type='l',
                 main='The Federal Funds Interest Rate'))
```

##Q2

- There are 570 observations are in train dataset.
- There are 43 observations are in test dataset.

-----

Data|Observations|Date
-----|-----------|------
train dataset|570|before 2014 year
test dataset|43|in 2014 year


-----


```{r, echo=FALSE, message=FALSE, warning=FALSE}
data.train <- subset(data.x,date < as.Date('2014-01-01'))
data.test <- subset(data.x,date >= as.Date('2014-01-01'))
nrow(data.train);nrow(data.test)
```

##Q3

For both assets,the two returns fluctuate in the vicinity of 0, but the spy is much more volatile than tlt.


```{r, echo=FALSE, message=FALSE, warning=FALSE}
#order training data
data.train <- data.train[order(data.train$date),]

# convert federal funds interest rate to decimal
data.train$fed.rate.deci <- data.train$fed.rate / 100
# compute rate for both assets
data.train$rate.spy <- NA
data.train$rate.tlt <- NA
data.train$rate.spy[-1] <- diff(data.train$close.spy,lag = 1) / data.train$close.spy[-nrow(data.train)]
data.train$rate.tlt[-1] <- diff(data.train$close.tlt,lag = 1) / data.train$close.tlt[-nrow(data.train)]

# time series plots of the returns
par(mfrow=c(1,2))
with(data.train,plot(date,rate.spy,type='l',ylim=c(-0.1,0.1),main='rate.spy'))
abline(h=0,lty=2,col='red',lwd=1)
with(data.train,plot(date,rate.tlt,type='l',ylim=c(-0.1,0.1),main='rate.tlt'))
abline(h=0,lty=2,col='red',lwd=1)
par(mfrow=c(1,1))
```

##Q4

The two assets returns Seems to be  normally distributed, which the Sharpe ratio calculation assumes is satisfied.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
##normal quantile plots
par(mfrow=c(1,2))
qqnorm(data.train$rate.spy,main='rate.spy \n  Normal Quantile Plot')
qqline(data.train$rate.spy,col='red')
qqnorm(data.train$rate.tlt,main='rate.tlt  \n Normal Quantile Plot')
qqline(data.train$rate.tlt,col='red')
par(mfrow=c(1,1))

```

##Q5

- The correlation between the S&P500 and long term treasury bond returns in the training set is -0.34,whose  absolute is relatively small so that we can think of the two returns are independent of each other.
- The rolling-window correlations within 24  are negative mostly,which indicated in a 24 weekly period  the two  returns are negatively correlated.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
#Compute the correlation
with(data.train,cor(rate.spy,rate.tlt,
                    method='pearson',
                    use='complete.obs'))
# Use for loop to compute the rolling-window correlations[cor_24rw_spy_tlt]
data.train$cor_24rw_spy_tlt <- NA
for (i in 25:nrow(data.train)) {
data.train$cor_24rw_spy_tlt[i] <-
  with(data.train,cor(
    rate.spy[(i-23):i],
    rate.tlt[(i-23):i],
    method='pearson',
    use='complete.obs'))
}

#make a time series plot of the rolling-window correlation
with(data.train,plot(date,cor_24rw_spy_tlt,
                     type='l',
                     main='rolling-window correlation'))
abline(h=0,lty=2,lwd=1,col=gray(0.5))

```

##Q6



- At the end date of train dataset,the excess returns of spy is  $g_{spy}=163.9029$ and the excess returns of tlt is $g_{tlt}=99.14221$.
- And the the number of years of data is $num_{year}=10.94231$.
- The compounded annual growth rate $CAGR_{spy}=0.04619039$ and $CAGR_{tlt}=0.0007869919$.
- The annualized volatility $v_{spy}=0.1682114$ and $v_{tlt}=0.1304339$
- The Sharpe Ratio $SR_{spy}=0.2745972$ and$SR_{tlt}=-0.006033644$. 
- The spy would better because of $SR_{spy} >SR_{tlt}$


```{r, echo=FALSE, message=FALSE, warning=FALSE}
## step 1
e_spy <- data.train$rate.spy[-1] - (data.train$fed.rate.deci[-nrow(data.train)]) / 52
e_tlt <- data.train$rate.tlt[-1] - (data.train$fed.rate.deci[-nrow(data.train)]) / 52

#step 2
g_spy <- numeric()
g_tlt <- numeric()
g_spy[1] <- 100
g_tlt[1] <- 100
for (i in 2:(nrow(data.train)-1)) {
    g_spy[i] <- g_spy[i-1]*(1+e_spy[i])
    g_tlt[i] <- g_tlt[i-1]*(1+e_tlt[i])
}

# step 3
num_years <- (nrow(data.train) - 1 ) / 52

# step 4
cagr_spy <- (g_spy[nrow(data.train)-1]/ g_spy[1]) ^(1/num_years) -1
cagr_tlt <- (g_tlt[nrow(data.train)-1]/ g_tlt[1]) ^(1/num_years) -1

# step 5
v_spy <- sqrt(52) * sd(e_spy)
v_tlt <- sqrt(52) * sd(e_tlt)

#step 6
sr_spy <- cagr_spy / v_spy
sr_tlt <- cagr_tlt / v_tlt
```

##Q7

I see a portfolio weight that produces the maximum Sharpe ratio. And the weight of the maximum Sharpe ratio maybe 0.6.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# my function
get_sr_portfolio <- function(x,ret_asset1,ret_asset2,fft) {
   r_portfolio <- x * ret_asset1 + (1 - x) * ret_asset2
   e_portfolio <- r_portfolio - fft / 52
   g_portfolio <- numeric(length(r_portfolio))
   g_portfolio[1] <- 100
   for (i in 2:(length(r_portfolio)) ) {
   	g_portfolio[i] <- g_portfolio[i-1] * (1+e_portfolio[i])
   }
   num_years_portfolio <- length(r_portfolio) / 52
   cagr_portfolio <- (g_portfolio[length(r_portfolio)] / g_portfolio[1]) ^(1 / num_years_portfolio) -1
   v_portfolio <- sqrt(52) * sd(e_portfolio)
   sr_portfolio <- cagr_portfolio / v_portfolio
   return(sr_portfolio)
}


curve_exp <- function(x){
 return(get_sr_portfolio(x,
 	ret_asset1 = data.train$rate.spy[-1],
 	ret_asset2 = data.train$rate.tlt[-1],
 	fft = (data.train$fed.rate.deci)[-570]) )
}

# plot Sharpe ratio of portfolio VS weight x
curve(sapply(x,curve_exp),
	from=0,
	to=1,
	ylab='Sharpe ratio',
	main='Sharpe ratio of portfolio VS weight x '
)

```

##Q8

- When weight $x=0.5904641$, the maximum of Sharpe ratio is $SR_{max}=0.359895$ .
- 59% of the funds should be allocated to spy asset and 41% of the funds should be allocated to tlt asset .
- The Sharpe Ratio $SR_{spy}=0.2745972$ and$SR_{tlt}=-0.006033644$ .For my portfolio,I have $SR_{max}=0.359895$. And  it is better to invest my portfolio.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
optimize(curve_exp,interval = c(0,1),maximum = T)
```

##Q9

- My portfolio in test data has a steady growth trend and finally 
- $g_{portfolio}=110.50387$,$g_{spy}=107.21200$ and $g_{tlt}=114.4147$ .


```{r, echo=FALSE, message=FALSE, warning=FALSE}
#order test data
data.test <- data.test[order(data.test$date),]

# convert federal funds interest rate to decimal
data.test$fed.rate.deci <- data.test$fed.rate / 100
# compute rate for both assets
data.test$rate.spy <- NA
data.test$rate.tlt <- NA
data.test$rate.spy[-1] <- diff(data.test$close.spy,lag = 1) / data.test$close.spy[-nrow(data.test)]
data.test$rate.tlt[-1] <- diff(data.test$close.tlt,lag = 1) / data.test$close.tlt[-nrow(data.test)]
x=0.59046
data.test$rate.portfolio <- x * data.test$rate.spy + (1-x) * data.test$rate.tlt

##compute excess returns

e_spy_test <- data.test$rate.spy[-1] - (data.test$fed.rate.deci[-nrow(data.test)]) / 52
e_tlt_test <- data.test$rate.tlt[-1] - (data.test$fed.rate.deci[-nrow(data.test)]) / 52
e_portfolio_test <- data.test$rate.portfolio[-1] - (data.test$fed.rate.deci[-nrow(data.test)]) / 52

g_spy_test <- numeric()
g_tlt_test <- numeric()
g_portfolio_test <- numeric()

g_spy_test[1] <- 100
g_tlt_test[1] <- 100
g_portfolio_test[1] <- 100
for (i in 2:(nrow(data.test)-1)) {
    g_spy_test[i] <- g_spy_test[i-1]*(1+e_spy_test[i])
    g_tlt_test[i] <- g_tlt_test[i-1]*(1+e_tlt_test[i])
    g_portfolio_test[i] <- g_portfolio_test[i-1]*(1+e_portfolio_test[i])
}

## plot 

plot(g_spy_test,type='l',col='red',main='',ylim=c(95,120))
points(g_tlt_test,type='l',col='blue',lty=2)
points(g_portfolio_test,type='l',col='orange',lty=3)
abline(h=100,lty=2,lwd=1,col=gray(0.5))
legend('topleft',
	   col=c('red','blue','orange'),
	   lty=1:3,
	   legend=c('spy','tlt','portfolio'),
	   cex=0.6
	   )
```

##Q10



 The SR of my  portfolio perform well in the test set beacuse of the SR of my  portfolio  is 2.116505.

-----

Item|end excess returns|SR
-----|-------------|-------
spy|107.212|0.7455637
tlt|114.4147|1.753351
portfolio|110.5039|2.116505

-----



```{r, echo=FALSE, message=FALSE, warning=FALSE}
#end of the test set period 
g_spy_test[42]
g_tlt_test[42]
g_portfolio_test[42]

#spy sr
get_sr_portfolio(x=1,
 	ret_asset1 = data.test$rate.spy[-1],
 	ret_asset2 = data.test$rate.tlt[-1],
 	fft = (data.test$fed.rate.deci)[-43]
 	) 
#tlt sr
get_sr_portfolio(x=0,
 	ret_asset1 = data.test$rate.spy[-1],
 	ret_asset2 = data.test$rate.tlt[-1],
 	fft = (data.test$fed.rate.deci)[-43]
 	) 
#portfolio sr
get_sr_portfolio(x=0.5904641,
 	ret_asset1 = data.test$rate.spy[-1],
 	ret_asset2 = data.test$rate.tlt[-1],
 	fft = (data.test$fed.rate.deci)[-43]
 	) 
```

