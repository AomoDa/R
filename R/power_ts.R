library(mice)
library(TSA)
library(forecast)

x <- read.csv('power.csv',header = T,stringsAsFactors = F)
x$date <- as.Date(x$date)
x$years <- as.numeric(substr(x$date,1,4))
x$months <- as.numeric(substr(x$date,6,7))
x$days <- as.numeric(substr(x$date,9,10))

# 补全缺失数据
imp <- mice(x[,-1],method = c('sample','pmm','logreg','norm'))
mydf <- cbind(data.frame(date=x[,1]),complete(imp))

# 绘制时间序列图
xyplot.ts(ts(mydf[,c('power')]))


# 进行频谱分析并计算周期
par(mfrow=c(1,2))
spec.pgram(mydf$power,main='Spectral Density')
periodogram(mydf$power,lwd=1,main='Periodogram')
par(mfrow=c(1,1))
power_spectrum <- periodogram(mydf$power,plot = F)
period <- round(1/power_spectrum$freq[which.max(power_spectrum$spec)])
period

# 分解时间序列
power_ts <- ts(mydf$power,frequency = period)
plot(decompose(power_ts))


# arima

set.seed(2017)
m1 <- auto.arima(power_ts,ic='bic')

