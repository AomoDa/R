library("xts") #for time series
library("nloptr") #for optimization
library("zoo")
library("dygraphs") #for plots
library("plotly") # for 3D plots
library("magrittr") # pipes
library("webshot")
library("fBasics")
library('PerformanceAnalytics')


AAPL <- read.csv(paste(getwd(),"/data/AAPL.txt",sep=''))
stock <- AAPL[,c('DATE','CLOSE')]
names(stock)[2] <- 'AAPL'
ticker <- read.table(paste(getwd(),"/data/Tickers.txt",sep=''), header=FALSE,stringsAsFactors = FALSE)
for (k in 3:nrow(ticker)){
  prices <- read.csv(paste(getwd(),"/data/", ticker$V1[k],".txt",sep =''))
  prices <- prices[,c('DATE','CLOSE')]
  names(prices)[2] <- ticker$V1[k]
  stock <- merge(stock,prices,by='DATE',all.x=TRUE)
}
stock$DATE <- as.Date(as.character(stock$DATE),'%Y%m%d')

# delete some stocks whose have most miss values
select_stock <- c('DATE',names(sort(apply(stock[,-1],2,function(x)sum(is.na(x))),decreasing = TRUE))[-c(1:10)])
stock <- stock[,select_stock]

# order stocks order by DATE
stock <- xts(stock[,-1],order.by =stock$DATE )



#Question 1

##1.1
r_stock <- na.omit(100*diff(log(stock)))
# r_stock <- na.omit(CalculateReturns(stock,method = 'log'))*100

##1.2
#the sample mean,
round(apply(r_stock,2,mean),4)
# variance,
round(apply(r_stock,2,var),4)
# standard deviation
round(apply(r_stock,2,sd),4)


##1.3
Return.annualized(r_stock,scale = 252,geometric = FALSE)
StdDev.annualized(r_stock,scale = 252)


##1.4
# calculate a  Sharpe Ratio of Return over StdDev

Sharpe_ratios <- SharpeRatio.annualized(r_stock,Rf = 0,scale = 252,geometric = FALSE)[1,]
Sharpe_ratios <- sort(Sharpe_ratios,decreasing = TRUE)
Sharpe_ratios


##1.5
par(las=2)
barplot(sort(Sharpe_ratios,decreasing = TRUE),horiz = FALSE,cex.names = .3)


#Question 2


##2.1
# Ignore the most recent 3 years of data (754 observations),
r_stock_without_3years <- r_stock[1:(nrow(r_stock)-754),]
names(r_stock_without_3years) <- names(r_stock)
Sharpe_ratios_without_3years <- SharpeRatio.annualized(r_stock_without_3years,Rf = 0,scale = 252,geometric = FALSE)[1,]
top20 <- head(sort(Sharpe_ratios_without_3years,decreasing = TRUE),20)
top20
pick_stock <- names(top20)
n <- nrow(r_stock_without_3years)
pick_rows <- (n-1008+1):n
pick_r_stock <- r_stock_without_3years[pick_rows,pick_stock]
# plot
dygraph(pick_r_stock)
# recent 3 years
r_stock_recent_3years <- r_stock[(nrow(r_stock)-754+1):nrow(r_stock),pick_stock]
# full
full_data <- rbind(pick_r_stock,r_stock_recent_3years)

##2.2
# init
N1=20

# function to be minimized (maximum Sharpe ratio portfolio,
#where mu1 is a predicted mean of the returns of the assets in the portfolio,
#Sig a covariance matrix, and N1 number of assets in the portfolio).
# minus(!) SR function
SR_P<-function(x) { 
  x<-matrix(x,1,N1)
  return( -(x %*% mu1)/sqrt(x %*% Sig %*% t(x) ))
}

# equality constraints
equal_con <- function ( x ) {
  h<- sum(x)-1  #weights sum to 1
  return(h)
} 
#Lower and upper bound for the portfolio weights
lb<-rep(0,N1) # long only portfolio
ub<-rep(0.2,N1) # no more than 20% in one asset

#starting values for the portfolio optimizer
b<-rep(1/N1,N1) #starting values for optimization


ar1_mean <- function(x){
  a1_model <- arima(x,order=c(1,0,0))
  rt_mean <- predict(a1_model,n.ahead = 1,se.fit = F)
  rt_mean <- as.numeric(rt_mean)
  return(rt_mean)
}

ar1_res <- function(x){
  a1_model <- arima(x,order=c(1,0,0))
  rt_res <- a1_model$residuals
  return(rt_res)
}

# where mu2 is a predicted mean of the returns of the assets in the portfolio 
# using AR(1)
SR_P_AR<-function(x) { 
  x<-matrix(x,1,N1)
  return( -(x %*% mu2)/sqrt(x %*% Sig2 %*% t(x) ))
}



# evaluate its out-of-sample performance by computing portfolio returns
SR_P_OUT<-function(R,weights){ 
  N1 <- length(weights)
  weights<-matrix(weights,1,N1)
  mu <- colMeans(R)
  Sig <- cov(R)
  rt_sha <- sqrt(252)*(weights %*% mu)/sqrt(weights %*% Sig %*% t(weights) )
  return( as.numeric(rt_sha))
}


prot_sharpe <- data.frame(window=numeric(),
                 IID_model_cumret=numeric(),
                 IID_model_sharpe=numeric(),
                 AR1_model_cumret=numeric(),
                 AR1_model_sharpe=numeric()
                 )

for (w in 1:(1008-251)) {
  win_rows <- seq(from=w, to=251+w, by=1)
  win_rows_next <- win_rows + 252
  returns.data <- pick_r_stock[win_rows,]
  Sig <- cov(returns.data)

  # IID model:
  mu1 <- colMeans(returns.data)
  # compute weight
  weights <-slsqp(b, SR_P, gr=NULL, lower=lb, upper=ub, hin=NULL ,heq=equal_con)$par
  # 0 if weights<0.01 (standardized)
  weights<-(weights*(weights >= 0.01)) / sum(weights*(weights >= 0.01))
  # Sharpe Ratios
  max_value <- -slsqp(b, SR_P, gr=NULL, lower=lb, upper=ub, hin=NULL ,heq=equal_con)$value
  max_SR <- max_value *sqrt(252)
  max_SR_OUT <- SR_P_OUT(full_data[win_rows_next,],weights=weights)
  # Cumulated Returns
  cum_ret <- Return.cumulative((full_data[win_rows_next,]/100) %*% weights) *100

  #AR(1) model
  mu2 <- apply(pick_r_stock[win_rows,],2,ar1_mean)
  Sig2 <- cov(apply(pick_r_stock[win_rows,],2,ar1_res))
  # compute weight
  weights_ar <- slsqp(b, SR_P_AR, gr=NULL, lower=lb, upper=ub, hin=NULL ,heq=equal_con)$par
  # 0 if weights<0.01 (standardized)
  weights_ar<-(weights_ar*(weights_ar >= 0.01)) / sum(weights_ar*(weights_ar >= 0.01))
  # Sharpe Ratios
  max_value_ar <- -slsqp(b, SR_P_AR, gr=NULL, lower=lb, upper=ub, hin=NULL ,heq=equal_con)$value
  max_SR_ar <- max_value_ar *sqrt(252)
  max_SR_ar_OUT <- SR_P_OUT(full_data[win_rows_next,],weights=weights_ar)
  # Cumulated Returns
  cum_ret_ar <- Return.cumulative((full_data[win_rows_next,] / 100) %*% weights_ar) * 100
  # store
  prot_sharpe[w,'window']=w
  prot_sharpe[w,'IID_model_sharpe']=max_SR_OUT
  prot_sharpe[w,'IID_model_cumret']=cum_ret
  prot_sharpe[w,'AR1_model_cumret']=cum_ret_ar
  prot_sharpe[w,'AR1_model_sharpe']=max_SR_ar_OUT
  cat(w,'\n')
  #cat('-----------------------\n')
  #cat(max_SR,'======',max_SR_ar,'\n') 
  #cat(max_SR_OUT,'======',max_SR_ar_OUT,'\n') 
  
}

# plot 
dygraph(prot_sharpe[,c(1,2,4)],main='Cumulated Returns')
dygraph(prot_sharpe[,c(1,3,5)],main='Sharpe Ratios')


#
