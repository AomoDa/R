

#-----------------------------------------
#part 1
#-----------------------------------------

library(ggplot2)
library(psych)
library(car)

x <- read.csv('MMRCountry.csv',header = T,stringsAsFactors = F,na.strings = -99)
x$TFR.level <- factor(x$TFR.level,levels = c(0,1,2),labels = c('Low','Medium','High'))


# task1
#correlation analysis
pairs.panels(x[,c(2,3,5)],method = 'pearson')
ggplot(data=x,aes(x=GDP,y=MMR))+geom_point(na.rm = T)+geom_smooth(method = 'lm',na.rm = T,col=I('red'))
ggplot(data=x,aes(x=LifeExpBW,y=MMR))+geom_point(na.rm = T)+geom_smooth(method = 'lm',na.rm = T,col=I('orange'))

#Analysis of Variance
summary(aov(MMR~TFR.level,data=x))
ggplot(data=x,aes(x=TFR.level,y=MMR,fill=TFR.level))+geom_boxplot(na.rm = T)

#task2

##a

lm1 <- lm(MMR~GDP,data=x)
summary(lm1)


par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))

#
qqPlot(lm1)
residualPlots(lm1)

##b

lm2<- step(lm1,scope = list(upper = ~LifeExpBW+TFR.level+GDP),direction = 'both')
summary(lm2)
lm3 <- lm(MMR~LifeExpBW,data=x)
summary(lm3)
anova(lm2,lm3)

qqPlot(lm3)
par(mfrow=c(2,2))
plot(lm3)
par(mfrow=c(1,1))


##c

lm4 <- lm(formula = MMR ~ LifeExpBW+TFR.level, data = x)
plot(allEffects(lm4))
qqPlot(lm4)

##task3

cor(x$MMR,x$LifeExpBW,use='complete.obs',method ='spearman')
x$ty <- ifelse(x$LifeExpBW>70,'a','b')

ggplot(data=x,aes(x=LifeExpBW,y=MMR))+geom_point(na.rm = T)+
geom_smooth(method = 'lm',na.rm = T,aes(col=ty,group=ty),show.legend = F)

#-----------------------------------------
#part 2
#-----------------------------------------


