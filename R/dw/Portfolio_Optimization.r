

#q1

#loading data
data.x <- read.csv('asset_data.txt',header = T,stringsAsFactors = F)
#turn the first column from a character string to a date1.
data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")
#Extract only the observations where the federal funds rate is available
data.x <- na.omit(data.x)
summary(data.x)

with(data.x,plot(date,fed.rate,type='l',main='The Federal Funds Interest Rate'))

#q2

data.train <- subset(data.x,date < as.Date('2014-01-01'))
data.test <- subset(data.x,date >= as.Date('2014-01-01'))
nrow(data.train);nrow(data.test)

#q3

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

#q4

##normal quantile plots
par(mfrow=c(1,2))
qqnorm(data.train$rate.spy,main='rate.spy Normal Quantile Plot')
qqline(data.train$rate.spy,col='red')
qqnorm(data.train$rate.tlt,main='rate.tlt Normal Quantile Plot')
qqline(data.train$rate.tlt,col='red')
par(mfrow=c(1,1))

#q5

#Compute the correlation
with(data.train,cor(rate.spy,rate.tlt,method='pearson',use='complete.obs'))

# Use for loop to compute the rolling-window correlations[cor_24rw_spy_tlt]
data.train$cor_24rw_spy_tlt <- NA
for (i in 25:nrow(data.train)) {
	data.train$cor_24rw_spy_tlt[i] <- with(data.train,cor(rate.spy[(i-23):i],rate.tlt[(i-23):i],method='pearson',use='complete.obs'))
}

#make a time series plot of the rolling-window correlation
with(data.train,plot(date,cor_24rw_spy_tlt,type='l',main='rolling-window correlation'))
abline(h=0,lty=2,lwd=1,col=gray(0.5))



#q6

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


#q7

#
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


#q8
optimize(curve_exp,interval = c(0,1),maximum = T)




#q9

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

#q10

