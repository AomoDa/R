
## 第一种


# 第一题
# 安装
# install.packages('rattle')
library(rattle)
# 加载
library(rattle)
#查看
(.packages())

#  第二题
# 读取txt文件
setwd("C://Users//AomoDa//Documents")
weather <- read.csv('weather.txt',sep=' ',header = T)
#读取csv文件
weather<- read.csv('weather.csv',header = T,sep=',')
x1 <- weather[weather$RainTomorrow=='Yes',]
head(x1)
x2 <- weather[weather$MinTemp >10 & weather$MinTemp <20,]
head(x2)
x3  <- weather[order(weather$Rainfall,decreasing = T),]
head(x3)
x4 <-weather[sample(x=1:nrow(weather),size = 10,replace = F),]
head(x4)
apply(weather[,c(3:7,9,12:21,23)],MARGIN = 2,FUN = median,na.rm=T)
set.seed(12345)
int <- sample(x = 1:3,size = nrow(weather),replace = T,prob = c(0.7,0.2,0.1))
weather_train <- weather[int==1,]
weather_verify <- weather[int==2,]
weather_test <- weather[int==3,]
nrow(weather_train);nrow(weather_verify);nrow(weather_test)

# 第三题

# 读取数据
setwd("C://Users//AomoDa//Documents")
mobile  <- read.csv('mobile.csv')
mobile$流失 <- factor(x = mobile$流失,levels = c(0,1),labels = c('未流失','流失'))
mobile$客户等级 <- as.factor(mobile$客户等级)
with(mobile,table(流失))

# 逻辑回归模型建立
set.seed(54321)
int <- sample(x = 1:2,size = nrow(mobile),replace = T,prob = c(0.7,0.3))
mobile.train <- mobile[int==1,]
mobile.test <- mobile[int==2,]
fit1 <- glm(流失~., family = binomial(),data=mobile.train)
summary(fit1)
fit2 <- glm(formula = 流失 ~ 客户等级 + 主叫次数 + 被叫次数 + 费用, family = binomial(), data = mobile.train)
summary(fit2)
AIC(fit1);AIC(fit2)
BIC(fit1);BIC(fit2)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))
fit2$coefficients

# 模型预测及评估
m.p <- predict(fit2,newdata = mobile.test,type='response')
xx <- data.frame(r=as.character(mobile.test$流失),p='未流失',stringsAsFactors=F)
xx$p[m.p>0.5] <- '流失'
rate <- sum(xx$r==xx$p) / nrow(xx)
rate


## 第二种
# part 1
# 安装rattle包
# install.packages('rattle')
library(rattle)
# 加载rattle包
library(rattle)
#查看当前加载的包
(.packages())

# part 2 
# load data
# 读取txt文件
weather1 <- read.csv('weather.txt',sep=' ',header = T)
#读取csv文件
weather2 <- read.csv('weather.csv',header = T,sep=',')
# 赋值
weather <- weather2

#Q1
raintomoyes <- weather[weather$RainTomorrow=='Yes',]
head(raintomoyes)

#Q2
mintemp <- weather[weather$MinTemp >10 & weather$MinTemp <20,]
head(mintemp)

#Q3
weaorder  <- weather[order(weather$Rainfall,decreasing = T),]
head(weaorder)

#Q4


#Q5
weasample <-weather[sample(x=1:nrow(weather),size = 10,replace = F),]
head(weasample)

#Q6
summary(weather[,c(3:7,9,12:21,23)])[3,]

#Q7
set.seed(10)
int <- sample(x = 1:3,size = nrow(weather),replace = T,prob = c(0.7,0.2,0.1))
table(int)
weather_train <- weather[int==1,]
weather_verify <- weather[int==2,]
weather_test <- weather[int==3,]



# part 3

library(lmtest)
mobile  <- read.csv('mobile.csv',header = T)
mobile$流失 <- factor(x = mobile$流失,levels = c(0,1),labels = c('NO','YES'))
mobile$客户等级 <- as.factor(mobile$客户等级)


barplot(with(mobile,table(流失)),main = '客户流失统计')

# 卡方独立性检验
with(mobile,table(客户等级,流失))
summary(with(mobile,table(客户等级,流失)))
# 箱图 
boxplot(mobile$主叫次数~mobile$流失,main='主叫次数箱图')
# T 检验
with(mobile,t.test(费用~流失))

# 逻辑回归模型建立
set.seed(100)
int <- sample(x = 1:2,size = nrow(mobile),replace = T,prob = c(0.7,0.3))
mobile.train <- mobile[int==1,]
mobile.test <- mobile[int==2,]

m1 <- glm(流失~., family = binomial(),data=mobile.train)
step(m1)

# 模型检验
summary(m1)
lrtest(m1)
plot(m1)

# 模型预测及评估
pre <- predict(object = m1,newdata = mobile.test,type = 'resp')
final <- data.frame(real=mobile.test$流失,pred=NA)
final$pred[pre<0.5] <- 'NO'
final$pred[pre>=0.5] <- 'YES'

table(final)


