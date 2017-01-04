library(quantreg)
library(foreign)

# 读取数据
x <- read.spss(file = 'data.sav',to.data.frame = T,use.value.labels = T)
summary(x)

#-----------------------------------
# part 1
#-----------------------------------

# 腰围数据
#Histograms and Q-Q normplot
par(mfrow=c(1,2))
hist(x$CDJ3,breaks = 30,probability = T,main='腰围直方图',xlab='腰围')
lines(density(x$CDJ3,from = 0),col='red',lwd=1)
qqnorm(x$CDJ3)
qqline(x$CDJ3,col='red',lwd=2,lty=2)
par(mfrow=c(2,2))


#按照性别将数据分组
data_man <- subset(x,subset = CDA2=='男')
data_woman <- subset(x,subset = CDA2=='女')


# 腰围与性别
par(mfrow=c(1,2))
boxplot(CDJ3~CDA2,data=x,ylim=c(50,120),main='性别与腰围箱图')
plot(density(data_man$CDJ3),main='性别与腰围箱核密度图',xlab='')
lines(density(data_woman$CDJ3),col='red')
legend('topright',col=c('black','red'),lty=1,legend=c('男','女'))
par(mfrow=c(1,1))

# 腰围与性别，年龄
ggplot(data=x,aes(x=CDAGE,y=CDJ3,col=CDA2))+geom_point()+geom_smooth(method = 'lm',se=F)+lims(y=c(50,120))+labs(title='腰围')

#-----------------------------------
# part 2
#-----------------------------------

# 设定分位数
taus <- c(0.05,  0.25, 0.40 ,0.5, 0.6 ,0.75, 0.8 ,0.90, 0.95)



#与职业
rq_man<- rq(formula = CDJ3 ~ CDAGE + CDA6,tau = taus, data = data_man, method = "fn")
rq_woman<-  rq(formula = CDJ3 ~ CDAGE + CDA6,tau = taus, data = data_woman, method = "fn")
plot(summary(rq_man))
plot(summary(rq_woman))
lm_man<- lm(formula = CDJ3 ~ CDAGE + CDA6, data = data_man)
lm_woman<- lm(formula = CDJ3 ~ CDAGE + CDA6, data = data_woman)



#与婚姻状况
rq_man<- rq(formula = CDJ3 ~ CDAGE + CDA5,tau = taus, data = data_man, method = "fn")
rq_woman<-  rq(formula = CDJ3 ~ CDAGE + CDA5,tau = taus, data = data_woman, method = "fn")
plot(summary(rq_man))
plot(summary(rq_woman))
lm_man<- lm(formula = CDJ3 ~ CDAGE + CDA5, data = data_man)
lm_woman<- lm(formula = CDJ3 ~ CDAGE + CDA5, data = data_woman)




#与饮食习惯
rq_man<- rq(formula = CDJ3 ~ CDAGE + CDD3,tau = taus, data = data_man, method = "fn")
rq_woman<-  rq(formula = CDJ3 ~ CDAGE + CDD3,tau = taus, data = data_woman, method = "fn")
plot(summary(rq_man))
plot(summary(rq_woman))
lm_man<- lm(formula = CDJ3 ~ CDAGE + CDD3, data = data_man)
lm_woman<- lm(formula = CDJ3 ~ CDAGE + CDD3, data = data_woman)


#与锻炼身体情况
# 无关系
rq_man<- rq(formula = CDJ3 ~ CDAGE + CDE2,tau = taus, data = data_man, method = "fn")
rq_woman<-  rq(formula = CDJ3 ~ CDAGE + CDE2,tau = taus, data = data_woman, method = "fn")
plot(summary(rq_man))
plot(summary(rq_woman))
lm_man<- lm(formula = CDJ3 ~ CDAGE + CDE2, data = data_man)
lm_woman<- lm(formula = CDJ3 ~ CDAGE + CDE2, data = data_woman)


