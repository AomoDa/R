
# part 3
library(gmodels)
library(ggplot2)
library(psych)
library(car)
library(forecast)
library(MASS)
library(randomForest)
library(kernlab)

# loading data
x <- read.csv('data.csv',header = T,sep=',',stringsAsFactors = F)
x$five_plan <- as.factor(x$five_plan)
# 修改单位量级，以K为单位统计。
for(i in 4:11) x[,i]  <- x[,i] /1000

# 查看数据结构
str(x)
summary(x)
hist(x$y,freq = F,xlab='区间',ylab='频率/密度',main='固定资产密度图')
lines(density(x$y),col='red')

boxplot(as.matrix(x[,4:11]),ylim=c(0,15),main='自变量分位数')
describe(x)

# 固定资产年度趋势图
ggplot()+geom_path(data=x,aes(year,y,group=1),col='red',lwd=1)+
        labs(x='年份',y='固定资产(亿元)',title='固定资产年度趋势图')

#固定资产年度增长率
x$y_year_add <- NA
for(i in 2:nrow(x)) { x$y_year_add[i] <- round(x$y[i] / x$y[i-1]-1,4)  }

ggplot()+geom_path(data=x,aes(year,y_year_add*100,group=1),na.rm=T)+
        labs(x='年份',y='固定资产增长率 %',title='固定资产年度增长率趋势图')



# 固定资产与五年计划分布。

ggplot(data=x,aes(five_plan,y,fill=five_plan))+geom_boxplot()+labs(x='五年计划',y='固定资产(亿元)',title='固定资产与五年计划分布')

#五年计划累计固定资产(亿元)
ggplot(data=x,aes(five_plan,weight=y,fill=five_plan))+geom_bar()+guides(fill = guide_legend(title = "五年计划", title.position = "top"))+
   labs(x='五年计划',y='累计固定资产(亿元)',title='五年计划累计固定资产(亿元)')



# 固定资产时间序列部分

y.train <- ts(x$y[-17],start = 1994)
y.test <- ts(x$y[17],start = 2010)


y.train.ts.model<-holt(y.train,h = 1,damped = T,level = 0.95,alpha = 0.8,beta = 0.4)
summary(y.train.ts.model)

plot.ts(y.train,ylab='累计固定资产',main='累计固定资产时间序列图')
lines(y.train.ts.model$fitted,col='red')
legend("topleft",legend=c('实际值','拟合值'), lwd=1, col=c("black", "red"))

# 2010年预测值
y.train.ts.model
#2010年实际值
y.test


# 自变量年度趋势图

par(mfrow=c(3,3))
plot(x$year,x$x1,type='b',col='red',xlab='年份',ylab='新增机车台数(千辆)',main ='新增机车台数(千辆)')
plot(x$year,x$x2,type='b',col='red',xlab='年份',ylab='新增客车辆数(千辆)',main ='新增客车辆数(千辆)')
plot(x$year,x$x3,type='b',col='red',xlab='年份',ylab='新增货车辆数(千辆)',main ='新增货车辆数(千辆)')
plot(x$year,x$x4,type='b',col='red',xlab='年份',ylab='新增营业里程（千千公里）',main ='新增营业里程（千千公里）')
plot(x$year,x$y,type='l',col='red',lwd=2,xlab='年份',ylab='固定资产(亿元)',main ='固定资产(亿元)')
plot(x$year,x$x5,type='b',col='red',xlab='年份',ylab='新线铺轨里程（千公里）',main ='新线铺轨里程（千公里）')
plot(x$year,x$x6,type='b',col='red',xlab='年份',ylab='复线铺轨里程（千公里）',main ='复线铺轨里程（千公里）')
plot(x$year,x$x7,type='b',col='red',xlab='年份',ylab='新线投产里程（千公里）',main ='新线投产里程（千公里）')
plot(x$year,x$x8,type='b',col='red',xlab='年份',ylab='复线投产里程（千公里）',main ='复线投产里程（千公里）')
par(mfrow=c(1,1))


# 相关系数与相关性检验

corr.test(x[-17,3:11],method = 'pearson')
pairs.panels(x[,3:11],method = 'pearson')


# 线性图

par(mfrow=c(3,3))
plot(x$x1,x$year,type='p',ylab='固定资产(亿元)',xlab='新增机车台数(千辆)',main ='新增机车台数(千辆) vs  固定资产(亿元)');abline(lm(y~x1,data=x),,col='red')
plot(x$x2,x$year,type='p',ylab='固定资产(亿元)',xlab='新增客车辆数(千辆)',main ='新增客车辆数(千辆) vs  固定资产(亿元)');abline(lm(y~x2,data=x),,col='red')
plot(x$x3,x$year,type='p',ylab='固定资产(亿元)',xlab='新增货车辆数(千辆)',main ='新增货车辆数(千辆) vs  固定资产(亿元)');abline(lm(y~x3,data=x),,col='red')
plot(x$x4,x$year,type='p',ylab='固定资产(亿元)',xlab='新增营业里程（千公里）',main ='新增营业里程（千公里） vs  固定资产(亿元)');abline(lm(y~x4,data=x),,col='red')
plot(x$x5,x$year,type='p',ylab='固定资产(亿元)',xlab='新线铺轨里程（千公里）',main ='新线铺轨里程（千公里） vs  固定资产(亿元) ');abline(lm(y~x5,data=x),,col='red')
plot(x$x6,x$year,type='p',ylab='固定资产(亿元)',xlab='复线铺轨里程（千公里）',main ='复线铺轨里程（千公里） vs  固定资产(亿元)');abline(lm(y~x6,data=x),,col='red')
plot(x$x7,x$year,type='p',ylab='固定资产(亿元)',xlab='新线投产里程（千公里）',main ='新线投产里程（千公里） vs  固定资产(亿元)');abline(lm(y~x7,data=x),,col='red')
plot(x$x8,x$year,type='p',ylab='固定资产(亿元)',xlab='复线投产里程（千公里）',main ='复线投产里程（千公里） vs  固定资产(亿元)');abline(lm(y~x8,data=x),,col='red')
par(mfrow=c(1,1))






# part 4 多元线性回归建模

# 不考虑截距

lm1 <- lm(y~-1+x2+x4+x5+x6+x7+x8,data=x)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))



stepAIC(object = lm(y~-1,data=x),scope = ~x2+x4+x5+x6+x7+x8,direction = 'forward')

stepAIC(object = lm1,direction = 'backward')

stepAIC(object = lm1,direction = 'both')


lm2 <-lm(formula = y ~ x6 -1, data = x)
summary(lm2)


par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))


anova(lm1,lm2)


lm3 <- lm( sqrt(y) ~-1+x2+x4+x5+x6+x7+x8,data=x)
summary(lm3)
stepAIC(lm3)

lm4 <- lm(formula = sqrt(y) ~ x2 + x4 + x5 + x7 - 1, data = x)
summary(lm4)

lm5<-lm(formula = sqrt(y) ~ I(x2 ^2)+x2 + x4 + x5 +x7- 1, data = x)

summary(lm5)


anova(lm4,lm5)

par(mfrow=c(2,2))
plot(lm5)
par(mfrow=c(1,1))




qqPlot(lm5)
residualPlot(lm5)
residualPlots(lm5)

influencePlot(lm5)


plot.ts(x$y,lwd=2,main='多元线性回归模型拟合值 VS 实际值')
lines(lm5$fitted.values^2,col='blue',type='b')
lines(lm2$fitted.values,col='red',type='b')
legend("topleft",legend=c('lm5','lm2','实际值'), lwd=1, col=c("blue", "red",'black'))




# 残差数据
plot.ts(residuals(lm2),ylim=c(-1000,1000),type='b',col='red',xlab='年',ylab='残差',main='多元线性回归模型残差值')
abline(h=0,lwd=1.5)
lines(lm5$residuals^2,type='b',col='blue')
legend("topleft",legend=c('lm5','lm2','0'), lwd=1, col=c("blue", "red",'black'))


# part 5 机器学习部分


# 随机森林
rf <- randomForest(y~x1+x2+x3+x4+x5+x6+x7+x8,data=x,ntree=500,mtry=100)
rf
plot(rf,main='随机森林误差率')
varImpPlot(rf,col='red',pch=16,main='随机森林变量相对重要度')
barplot(importance(rf),beside = T,legend.text = row.names(importance(rf)),main='随机森林变量相对重要度')



plot.ts(x$y,lwd=2,main='随机森林预测值 VS 实际值')
lines(rf$predicted,col='red',type='b')


# SVM

svm1 <-ksvm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=x,kernel='rbfdot',type='eps-svr')
svm1
svm2 <-ksvm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=x,kernel='vanilladot',type='eps-svr')
svm2
svm3 <-ksvm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=x,kernel='anovadot',type='eps-svr')
svm3
svm4 <-ksvm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=x,kernel='anovadot',type='eps-bsvr')
svm4

plot.ts(x$y,lwd=2,main='支持向量机预测值 VS 实际值')
lines(predict(svm1,x),col='red',type='b')
lines(predict(svm2,x),col='blue',type='b')
lines(predict(svm3,x),col='green',type='b')
lines(predict(svm4,x),col='purple',type='b')
legend("topleft",legend=c('实际值','svm1--kernel = rbfdot, type = eps-svr','svm2--kernel = vanilladot, type = eps-svr',
    'svm3--kernel = anovadot, type = eps-svr','svm4--kernel = anovadot, type = eps-bsvr'), 
       lwd=1 ,col=c("black", "red",'blue','green','purple'))


# part 6结论


# 拟合效果分析
plot.ts(x$y,lwd=2,main='各模型预测值 VS 实际值',xlab='年',ylab='固定资产(亿元)')
lines( c(as.vector(y.train.ts.model$fitted) ,y.train.ts.model$mean) ,col='red',type='b')
lines(lm5$fitted.values^2,col='blue',type='b')
lines(rf$predicted,col='green',type='b')
lines(predict(svm4,x),col='purple',type='b')
legend("topleft",legend=c('实际值','holt时间序列','多元线性回归','随机森林','SVM'),lwd=1 ,col=c("black", "red",'blue','green','purple'))



# 残差分析

plot.ts(c(as.vector(y.train.ts.model$fitted) ,y.train.ts.model$mean)-x$y,type='b',xlab='年',ylab='残差',main='各模型残差对比',col='red',ylim=c(-500,500))
abline(h=0,lwd=1)
lines(lm5$residuals^2,type='b',col='blue')
lines(predict(rf,x)-x$y,type='b',col='green')
lines(predict(svm4,x)-x$y,type='b',col='purple')
legend("bottomleft",legend=c('0','holt时间序列','多元线性回归','随机森林','SVM'),lwd=1.5 ,col=c("black", "red",'blue','green','purple'))
