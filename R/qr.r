
library(quantreg)
library(foreign)

# 读取数据
x <- read.spss(file = 'data.sav',to.data.frame = T,use.value.labels = T)
summary(x)


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


# 设定分位数
taus <- c(0.05,  0.25, 0.40 ,0.5, 0.6 ,0.75, 0.8 ,0.90, 0.95)

rq_man<- rq(formula = CDJ3 ~ CDAGE + CDA6+CDA4+CDA5+CDB1+CDC1+CDD1+CDD2+CDD3+CDD5+CDE1+CDE2+CDE4, 
	tau = taus, data = data_man, method = "fn")

rq_woman<- rq(formula = CDJ3 ~ CDAGE + CDA6+CDA4+CDA5+CDB1+CDC1+CDD1+CDD2+CDD3+CDD5+CDE1+CDE2+CDE4, 
	tau = taus, data = data_woman, method = "fn")
