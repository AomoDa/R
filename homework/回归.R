


install.packages("xlsx") # 读取 Excel 文件
install.packages("ggplot2") #绘图工具
install.packages("car") #线性回归检验及多重共线性检验
install.packages("rpart") #分类回归决策树
install.packages("randomForest") #随机森林
install.packages("e1071") # SVM
install.packages("lars") # lasso回归
install.packages("mboost")# boosting回归
install.packages("ipred") # bagging回归
install.packages("rpart.plot") 
install.packages("forecast") # 预测


library(xlsx) # 读取 Excel 文件
library(ggplot2) #绘图工具
library(car) #线性回归检验及多重共线性检验
library(rpart) #分类回归决策树
library(randomForest) #随机森林
library(lars) # lasso回归
library(e1071) # SVM
library(mboost)# boosting回归
library(ipred) # bagging回归
library(rpart.plot) 
library(forecast) # 预测



# 数据预处理

data_load <- function() {
  x <- read.csv('data_ok.csv',header = T,stringsAsFactors = F,sep = ',')
  x <- as.data.frame(x =x[,-1],row.name=x[,1])
  x <- na.omit(x)
  x$income <- x$income / 10000 # 以万元表示，方便处理和显示数据
  x$p82 <- x$p82 / 10000
  x$p85 <- x$p85 / 10000
  x$p16 <- factor(x$p16,levels = c(0,1),labels = c('No','Yes'))
  x$p18 <- factor(x$p18,levels = c(0,1),labels = c('No','Yes'))
  x$p20 <- factor(x$p20,levels = c(0,1),labels = c('No','Yes'))
  x$p33 <- factor(x$p33,levels = c(0,1),labels = c('No','Yes'))
  x$p35 <- factor(x$p35,levels = c(0,1),labels = c('No','Yes'))
  x$p39 <- factor(x$p39,levels = c(0,1),labels = c('No','Yes'))
  x$p50 <- factor(x$p50,levels = c(0,1),labels = c('No','Yes'))
  x$p52 <- factor(x$p52,levels = c(0,1),labels = c('No','Yes'))
  x$p68 <- factor(x$p68,levels = 1:4,labels = c('土木结构','砖混结构','钢筋混泥土结构','其他结构'))
  x$p73 <- factor(x$p73,levels = 1:5,labels = c('illness','disability','old','accident','Other'))
  x$p77 <- factor(x$p77,levels = c(0,1),labels = c('No','Yes'))
  return(x)
}

x <- data_load()

set.seed(100)
ind <- sample(x = 1:5,size = nrow(x),replace = T,prob = rep(1/5,5))
x_train_1 <- x[ind!=1,]
x_train_2 <- x[ind!=2,]
x_train_3 <- x[ind!=3,]
x_train_4 <- x[ind!=4,]
x_train_5 <- x[ind!=5,]


# 分类回归决策树

##建模
rt1 <- rpart(formula = income~.,data=x_train_1)
rt2 <- rpart(formula = income~.,data=x_train_2)
rt3 <- rpart(formula = income~.,data=x_train_3)
rt4 <- rpart(formula = income~.,data=x_train_4)
rt5 <- rpart(formula = income~.,data=x_train_5)

##剪枝
prt1 <- prune(rt1, cp= rt1$cptable[which.min(rt1$cptable[,"xerror"]),"CP"])
prt2 <- prune(rt2, cp= rt1$cptable[which.min(rt2$cptable[,"xerror"]),"CP"])  
prt3 <- prune(rt3, cp= rt1$cptable[which.min(rt3$cptable[,"xerror"]),"CP"])  
prt4 <- prune(rt4, cp= rt1$cptable[which.min(rt4$cptable[,"xerror"]),"CP"])  
prt5 <- prune(rt5, cp= rt1$cptable[which.min(rt5$cptable[,"xerror"]),"CP"])  

##绘图
rpart.plot(prt1,type = 1,fallen.leaves = T,main=' Regression Trees 1 ')
rpart.plot(prt2,type = 1,fallen.leaves = T,main=' Regression Trees 2 ')
rpart.plot(prt3,type = 1,fallen.leaves = T,main=' Regression Trees 3 ')
rpart.plot(prt4,type = 1,fallen.leaves = T,main=' Regression Trees 4 ')
rpart.plot(prt5,type = 1,fallen.leaves = T,main=' Regression Trees 5 ')

##预测
p1 <- predict(object = prt1,newdata = x[ind==1,])
p2 <- predict(object = prt2,newdata = x[ind==2,])
p3 <- predict(object = prt3,newdata = x[ind==3,])
p4 <- predict(object = prt4,newdata = x[ind==4,])
p5 <- predict(object = prt5,newdata = x[ind==5,])

# 交叉验证

NMSE <-function(p1,p2,p3,p4,p5) {
 p1 <- p1 
 p2 <- p2 
 p3 <- p3 
 p4 <- p4 
 p5 <- p5 
 NMSE_RT <- vector()
 NMSE_RT[1] <- mean( (x[ind==1,]$income - p1)^2 ) /mean( (x[ind==1,]$income - mean(x[ind==1,]$income) )^2 )
 NMSE_RT[2] <- mean( (x[ind==2,]$income - p2)^2 ) /mean( (x[ind==2,]$income - mean(x[ind==2,]$income) )^2 )
 NMSE_RT[3] <- mean( (x[ind==3,]$income - p3)^2 ) /mean( (x[ind==3,]$income - mean(x[ind==3,]$income) )^2 )
 NMSE_RT[4] <- mean( (x[ind==4,]$income - p4)^2 ) /mean( (x[ind==4,]$income - mean(x[ind==4,]$income) )^2 )
 NMSE_RT[5] <- mean( (x[ind==5,]$income - p5)^2 ) /mean( (x[ind==5,]$income - mean(x[ind==5,]$income) )^2 )
 return(NMSE_RT) 
}

(nmse_rt <- NMSE(p1,p2,p3,p4,p5))


# 随机森林

##建模
set.seed(1000)
rf1 <- randomForest( formula = income~.,data=x_train_1, ntree=100,na.action = na.omit,importance=T,keep.forest=T)
rf2 <- randomForest( formula = income~.,data=x_train_2, ntree=100,na.action = na.omit,importance=T,keep.forest=T)
rf3 <- randomForest( formula = income~.,data=x_train_3, ntree=100,na.action = na.omit,importance=T,keep.forest=T)
rf4 <- randomForest( formula = income~.,data=x_train_4, ntree=100,na.action = na.omit,importance=T,keep.forest=T)
rf5 <- randomForest( formula = income~.,data=x_train_5, ntree=100,na.action = na.omit,importance=T,keep.forest=T)

##绘图

rf_error_plot <- function(){
  plot(rf1,main='RandomForest Error',lty=1)
  plot(rf2,add=T,col='red',lty=2)
  plot(rf3,add=T,col='blue',lty=3)
  plot(rf4,add=T,col='green',lty=3)
  plot(rf5,add=T,col='orange',lty=5)
  legend("topright",legend=c('rf1','rf2','rf3','rf4','rf5') ,col=c('black','red','blue','green','orange'),lty=1:5)
}

rf_error_plot()



#MeanDecreaseAccuracy描述的是当把一个变量变成随机数时,
#随机森林预测准确度的降低程度，
#该值越大表示该变量的重要性越大。
#MeanDecreaseGini通过基尼指数计算每个变量对分类树上每个节点的观测值的异质性影响。该值越大表示该变量的重要性越大。



##预测

prf1 <- predict(object = rf1,newdata = x[ind==1,])
prf2 <- predict(object = rf2,newdata = x[ind==2,])
prf3 <- predict(object = rf3,newdata = x[ind==3,])
prf4 <- predict(object = rf4,newdata = x[ind==4,])
prf5 <- predict(object = rf5,newdata = x[ind==4,])

## 交叉验证

(nmse_rf <- NMSE(prf1,prf2,prf3,prf4,prf5))

#bagging回归

