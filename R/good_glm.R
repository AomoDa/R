

#Logistic Regression

library(plyr)
library(lattice)
library(lsmeans)

x <- read.csv('credit_cards.csv',header=T,stringsAsFactors = F,na.strings='NA')
str(x)
summary(x)

x <- na.omit(x)
ddply(.data =x,.variables = .(fullrepay),
     .fun = summarise,bal=mean(bal),min=mean(min))

# scatter plot 
xyplot(fullrepay~bal,data=x,
      jitter.x=T,jitter.y=T,
      main='fullrepay~bal')

xyplot(fullrepay~min,data=x,
       jitter.x=T,jitter.y=T,
       main='fullrepay~min',col='red')

# boxplot
par(mfrow=c(1,2))
boxplot(bal~fullrepay,data=x,ylim=c(0,1e4),col=2:3,main='fullrepay~bal')
boxplot(min~fullrepay,data=x,ylim=c(0,200),col=4:5,main='fullrepay~min')
par(mfrow=c(1,1))


# glm
m1 <- glm(fullrepay ~ bal+min, family = binomial, data =x)
summary(m1)

#Confidence Intervals on Coefficients
coef(m1)
confint(m1)
(coefs.with.CIs <- cbind(coef(m1), confint(m1)))
round(exp(coefs.with.CIs),3)


# accuracy
pred <- ifelse(m1$fitted.values>0.5,1,0)
table(x$fullrepay,pred)

#lsmeans
plot(lsmeans(m1, ~bal,at = list(bal = seq(100, 4000, 200)), type = "response"))
plot(lsmeans(m1, ~min,at = list(min = seq(5, 200, 10)), type = "response"))



#--------------------------------------------------------------------------
#Poisson Regression

verdict <- data.frame(fault=c(rep('Low',3),rep('High',3)),
                      moral_character=rep(c('Low','Neutral','High'),2),
                      verdict =c(rep('Guilty',6),rep('NOT Guilty',6)),
                      freq=c(32,79,42,17,65,23,8,12,4,24,41,11))
                      
verdict

library(tidyr)
(tb1 <- spread(data = verdict,key = verdict,value = freq))

#Pearson's Chi-squared Test for Count Data
summary(xtabs(freq~fault+verdict,data=verdict))
summary(xtabs(freq~moral_character+verdict,data=verdict))


#model
m2 <- glm(freq ~ (fault * moral_character) * verdict, family = poisson,data = verdict)
summary(m2)



drop1(m2, test = "Chisq")

#Predicted frequencies with CIs
(m2.lsm <- lsmeans(m2, ~(fault * moral_character) * verdict, type = "response"))
plot(m2.lsm)


#

(m2.lsm <- lsmeans(m2, ~fault  * verdict, type = "response"))
