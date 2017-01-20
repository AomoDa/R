


# 平均绩点达到2就及格
# 只要带文字的删除掉
# 没学分的数据也删掉了吧

# 加载所需要的包
library(ggplot2)
library(ROCR)
library(rpart)
library(rpart.plot)
library(effects)

##------------------------------------------
## Part1 读取数据
##------------------------------------------

j1 <- read.csv('J1.csv',header = T,stringsAsFactors = F)
j2 <- read.csv('J2.csv',header = T,stringsAsFactors = F)
# 合并数据
x <- rbind(j1,j2)
x <- x[x$课程性质!=''&!is.na(x$绩点)&!is.na(x$平时成绩) ,]
x$学年 <- as.factor(x$学年)
x$课程性质 <- as.factor(x$课程性质)

# 期末成绩和实验成绩整合
# x$期末成绩 <- ifelse( (is.na(x$期末成绩) | x$期末成绩==0) & (x$实验成绩!=0 & !is.na(x$实验成绩)),x$实验成绩,x$期末成绩)
x <- x[,c(1:6,8,14:15,19)]



##------------------------------------------
## Part2 绩点分布
##------------------------------------------

hist(x$绩点,breaks=30,probability=T,xlab='绩点区间',main='学生绩点分布',ylab='频率')
abline(v=2,col='red',lty=2,lwd=2)
lines(density(x$绩点,from=0,to=5),col='orange')

# 预警 标准
x$是否预警 <- ifelse(x$绩点>=2,0,1)
table(x$是否预警)

##------------------------------------------
## Part3 预警与学年
##------------------------------------------
ggplot(data=x,aes(x=学年,y=绩点,fill=学年))+geom_boxplot(na.rm = T,show.legend = F)

ggplot(data=x,aes(x=学年,fill=as.factor(是否预警))) +
      geom_bar(na.rm = T,show.legend = T,position = 'fill')+
      coord_polar(theta = "y") + 
      labs(x='',y='百分比',title='预警与学年')


###卡方检验 和马赛克图
summary(table(x$学年,x$是否预警))
mosaicplot(table(x$学年,x$是否预警),shade=T,main='马赛克图 \n 预警与学年')


##------------------------------------------
## Part4 预警与课程性质
##------------------------------------------
ggplot(data=x,aes(x=课程性质,y=绩点,fill=课程性质))+geom_boxplot(na.rm = T,show.legend = F)

ggplot(data=x,aes(x=课程性质,fill=as.factor(是否预警)))+
      geom_bar(na.rm = T,show.legend = T,position = 'fill')+
      coord_polar(theta = "y") + 
      labs(x='',y='百分比',title='预警与课程性质')

###卡方检验 和马赛克图
summary(table(x$课程性质,x$是否预警))
mosaicplot(table(x$课程性质,x$是否预警),shade=T,main='马赛克图 \n 预警与课程性质')



##------------------------------------------
## Part5 预警与平时成绩
##------------------------------------------

aggregate(平时成绩~是否预警,data=x,mean)

ggplot(data=x,aes(x=平时成绩,y=绩点,col=是否预警))+
   geom_point(na.rm = T)+
   labs(x='平时成绩',y='绩点',title='散点图\n预警与平时成绩')

ggplot(data = x,aes(x=是否预警,fill=as.factor(是否预警),y=平时成绩))+
  geom_boxplot(na.rm = T) +
  labs(x='是否预警',y='平时成绩',title='箱图\n预警与平时成绩')

#---------

##### 加入学年因素
ggplot(data=x,aes(x=平时成绩,y=绩点,col=as.factor(是否预警)))+
   geom_point(na.rm = T)+
    facet_wrap(~学年)+
   labs(x='平时成绩',y='绩点',title='散点图\n预警与平时成绩')


##### 加入课程性质因素

ggplot(data = x,aes(fill=as.factor(是否预警),x=课程性质,y=平时成绩))+
  geom_boxplot(na.rm = T) +
  labs(x='是否预警',y='平时成绩',title='箱图\n预警与平时成绩')


##------------------------------------------
## Part 6 建立模型
##------------------------------------------


## 训练数据集(80%) 和 测试数据集(20%)

set.seed(2017)
ind <- sample(x = 2,size = nrow(x),replace = T,prob = c(0.8,0.2))
table(ind)

train_data <- x[ind==1,]
test_data <- x[ind==2,]


## 逻辑回归模型

glm1 <- glm(是否预警~平时成绩+课程性质+学年,data=train_data,family=binomial())
summary(glm1)
# 效应分析
plot(allEffects(glm1))

# 似然比检验（LRT）
anova(glm1,test = 'LRT')
glm_fit <- ifelse(glm1$fitted.values>0.5,1,0)

# 训练样本精度
table(glm_fit,train_data$是否预警)

# 预测

glm_pred <- predict.glm(glm1,newdata = test_data,type = 'response')
#预测样本精度
table(ifelse(glm_pred>0.5,1,0),test_data$是否预警)

# ROC曲线
pred <- prediction(glm_pred,test_data$是否预警)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize = T,main='ROC曲线图\n逻辑回归')



## 决策树模型

rt <- rpart(是否预警~平时成绩+课程性质+学年,data=train_data,control=rpart.control(cp=0.01))
rt$cptable
rpart.plot(rt)

fitted <- predict(rt,newdata = train_data,type='vector')
fitted <- ifelse(fitted>0.5,1,0)
#测试样本精度
table(fitted,train_data$是否预警)


# 预测
rt_pred <- predict(rt,newdata = test_data,type='vector')

#测试样本精度
table(ifelse(rt_pred>0.5,1,0),test_data$是否预警)

# ROC曲线
pred <- prediction(rt_pred,test_data$是否预警)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize = T,main='ROC曲线图\n决策树')
