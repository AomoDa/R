

# installs the needed libraries
library(fBasics)
library(TSA)

x <- read.csv('GDP_t3_t12.csv',header = T,stringsAsFactors = F)

#Take the series of t3 and t12 from 1954q1 to the end
x <- x[28:251,]

#Plot t3 and t12.
plot.ts(ts(x[,6:7],frequency =4,start=c(1954,1) ),
        col='red',main='time series t3 and t12 ')

#Test whether t3 and t12 are unit root processes.
adf.test(x$t3)
adf.test(x$t12)

#Plot their ACF graphs.
par(mfrow=c(2,2))
acf(x$t3, lag.max = 24)
pacf(x$t3, lag.max = 24)
acf(x$t12, lag.max = 24)
pacf(x$t12, lag.max = 24)
par(mfrow=c(1,1))

# fit: t3 = alpha  + beta * t12 + u
fit1 <- lm(t3~t12,data=x)
summary(fit1)
#plot fit1 

p1 <-predict(object = fit1,newdata = x,level = 0.95,interval = 'confidence')
plot.ts(ts(x[,6],frequency =4,start=c(1954,1) ),main='t3 real vs fit',ylab='')
lines(ts(p1[,1],frequency =4,start=c(1954,1) ),col='red')
legend('topleft',legend=c('real','fit'),col=c('black','red'),lty=1)


# fit: t12 = a  + b * t3 + u
fit2 <- lm(t12~t3,data=x)
summary(fit2)
#plot fit2
p2 <-predict(object = fit2,newdata = x,level = 0.95,interval = 'confidence')
plot.ts(ts(x[,7],frequency =4,start=c(1954,1) ),main='t12 real vs fit',ylab='')
lines(ts(p2[,1],frequency =4,start=c(1954,1) ),col='red')
legend('topleft',legend=c('real','fit'),col=c('black','red'),lty=1)

