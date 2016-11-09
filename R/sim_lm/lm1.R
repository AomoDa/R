## lm1

library(car)
library(ggplot2)

## load data
mydata <- read.csv('MidtermData10(HENRY).csv',header = T)
str(mydata)
summary(mydata)
cor(mydata)

#----------------------------------------------------------------------
##EDA
#----------------------------------------------------------------------

par(mfrow=c(1,2))
boxplot(mydata$x,main='boxplot of x',col=3)
boxplot(mydata$y,main='boxplot of x',col=4)
par(mfrow=c(1,1))

##histogram 
par(mfrow=c(1,2))
hist(mydata$x,xlab='x',freq=F)
lines(density(mydata$x),col='red',lty=2)
qqPlot(mydata$x,main='qqplot of y')
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(mydata$y,xlab='y',freq=F)
lines(density(mydata$y),col='red',lty=2)
qqPlot(mydata$y,main='qqplot of y')
par(mfrow=c(1,1))

##normal text
shapiro.test(mydata$x)
shapiro.test(mydata$y)

## plot 


ggplot(data=mydata,aes(x,y))+geom_point()+    
      geom_smooth(method = 'lm',col='blue')+
      labs(title='y~x')

ggplot(data=mydata,aes(x,y))+geom_point()+
    geom_smooth(col='red',formula = y~x+I(x^2),method = 'lm')+
    labs(title='y~x+x^2')

ggplot(data=mydata,aes(x,y))+geom_point()+
    geom_smooth(col='orange',formula = y~x+I(x^2)+I(x^3),method = 'lm')+
    labs(title='y~x+x^2+x^3')



#----------------------------------------------------------------------
# MODEL
#----------------------------------------------------------------------

lm0 <- lm(y~x,data=mydata)
lm1 <- lm(y~x-1,data=mydata)
lm2 <- lm(y~x+I(x^2)-1,data=mydata)



#----------------------------------------------------------------------
# MODEL diagnostics
#----------------------------------------------------------------------
vif(lm2)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))


influencePlot(lm1)

