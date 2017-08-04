# Y      利润额
# X1     客户满意度 
# X2     平均单价 
# X3     海报费用
# X4     装修道具
# X5     微信文章发布数
# X6     微信文章平均阅读量 
# X7     折扣
# X8     店铺类型
# X9     店铺等级
# X10    店铺格式
# X11    店铺面积
# X12    城市等级
# X13    开业时间（月份数） 
# X14    培训
# X15    店员人数


library(mice)
library(ggplot2)
library(plotly)
library(psych)
library(caret)
library(quantreg)
library(rqPen)
library(caret)
library(corrplot)
library(effects)

x <- read.csv('mydata_v2.csv',header = T,stringsAsFactors = TRUE)
str(x)
# 数据转换
x$y <- round(x$y / 10000,2)
x$x3 <- round(log(x$x3 +1 ),2)
x$x4 <- round(log(x$x4),2) 
x$x6 <- round(x$x6 /10000,2)
x$x11 <- round(log(x$x11),2)
x$x13 <- round(sqrt(x$x13),2)

#----------------------------------------------
# EDA
#----------------------------------------------

myqa <- function(x,q) {
	qa <- quantile(x,q)
	cut_qa <- cut(x,breaks = c(min(x),qa),include.lowest = T,labels = q)
	return(cut_qa)
}


mydf <- x
# 计算Y的分位
mydf$QUAN <- myqa(mydf$y,q = c(seq(0.1, 1, 0.2),1))

# 利润额

par(mfrow=c(1,3))
hist(mydf$y,breaks =30 ,probability = T,main='Histogram')
lines(density(mydf$y),col='blue')
boxplot(mydf$y,main='Boxplot')
qqPlot(mydf$y)
par(mfrow=c(1,1))

# Quality Control Charts
qcc(data = mydf$y,type = 'xbar.one',nsigmas = 3)


# 箱图
ggplot(data = mydf,aes(x=x8,y=y,fill=x8))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺类型',y='Y',title='Y VS x8')

ggplot(data = mydf,aes(x=x9,y=y,fill=x9))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺等级',y='Y',title='Y VS x9')


ggplot(data = mydf,aes(x=x10,y=y,fill=x10))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺格式',y='Y',title='Y VS x10')

ggplot(data = mydf,aes(x=x12,y=y,fill=x12))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='城市等级',y='Y',title='Y VS x12')


# 相关性分析
corPlot(mydf[,-c(9,10,11,13,17)],n.legend = 5,las=2,upper = F,numbers = T,diag = F)


# 线性关系
ggplot(data = mydf,aes(x=x15,y=y,col=as.factor(QUAN)))+
    geom_point(show.legend = F,position='jitter')+
    geom_smooth(col=I(gray(0.2)),method = 'lm',se = F,lty=2)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店员人数',y='Y',title='Y VS x15')

ggplot(data = mydf,aes(x=x3,y=y,col=as.factor(QUAN)))+
    geom_point(show.legend = F,position='jitter')+
    geom_smooth(col=I(gray(0.2)),method = 'lm',se = F,lty=2)+
    theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='海报费用',y='Y',title='Y VS x3')

#----------------------------------------------
# Quantile Regression
#----------------------------------------------



# OLS Linear Models
lm1 <- glm(y~ . -1 ,data=mydf[,-17])
lm2 <- step(lm1,direction = 'both',trace = F)
summary(lm2)
lm3 <- update(object = lm2,formula. = .~.-x2 - x7-x1)
# summary
summary(lm3)

# anova
anova(lm1,lm2,lm3,test = 'F')

# effect
plot(allEffects(lm3),rows = 2,cols = 2)

# rmse
(lm_rmse <- sqrt(mean(lm3$residuals^2)))


# Quantile Regression

rq1 <- rq(lm3$formula,data=mydf,method = 'fn',tau = c(seq(0.1, 1, 0.2),0.99))
# coefficients
round(rq1$coefficients,2)
# rmse 
apply(rq1$residuals,2,function(x)sqrt(mean(x^2)))
