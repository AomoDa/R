## lm1

library(car)
library(psych)
library(forecast)

## part1
mid_data <- read.csv('MidtermData9 (LUCILLE).csv')
head(mid_data)
str(mid_data)
summary(mid_data)



##EDA

## normal test
par(mfrow=c(2,2))
hist(mid_data$x)
qqnorm(mid_data$x)
qqline(mid_data$x)
hist(mid_data$y)
qqnorm(mid_data$y)
qqline(mid_data$y)
par(mfrow=c(1,1))

shapiro.test(mid_data$x)
shapiro.test(mid_data$y)



##pairs
pairs.panels(mid_data)
corr.test(mid_data)


## plot 

with(mid_data,plot(x,y,main='Scatter Plots with  Linear Model'))
abline(lm(y~x,data=mid_data),col='red',lwd=1,lty=1)
abline(lm(y~x-1,data=mid_data),col='blue',lwd=2,lty=3)
legend('topright',col=c('red','blue'),
       lwd=1:2,lty=c(1,3),cex=0.6,
       legend=c('Linear Model with Intercept',
       	        'Linear Model without Intercept'))



# MODEL


## train data and test data

set.seed(100)
ind <- sample(x=1:2,size=nrow(mid_data),replace = T,prob = c(0.8,0.2))
mid_data.train <-mid_data[ind==1,]
mid_data.test  <-mid_data[ind==2,]


# fit models
fit1 <- lm(y~x,data=mid_data.train)
summary(fit1)
fit2 <- lm(y~x-1,data=mid_data.train)
summary(fit2)




# MODEL diagnostics

##Compute analysis of variance (or deviance) tables for one or more fitted model objects.
anova(fit1,fit2)

##residuals analysis
par(mfrow=c(1,3))
plot(residuals(fit2),main='residuals plot')
abline(h=0,col='red',lty=2)
hist(residuals(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2),col='red',lty=2)
par(mfrow=c(1,1))

## fitted analysis

with(mid_data,plot(x,y,col='red',type='p',pch=1))
points(mid_data.train$x,as.numeric(fitted.values(fit2)),col='blue',pch=2)
legend('topright',col=c('red','blue'),pch=1:2,
      legend=c('real values','fitted values'))


# MODEL Predct

pred <- forecast.lm(object = fit2,newdata = mid_data.test,level = 0.95)

plot(pred)
points(mid_data.test,col='red',pch=16)
legend('topright',col=c('white','blue','red'),pch=c(1,16,16),cex=0.8,
      legend=c('train real values','pred test values','real test values'))

