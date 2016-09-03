

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
  return(na.omit(x))
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

NMSE <-function(ob1,ob2,ob3,ob4,ob5) {
 NMSE_RT <- vector()
 NMSE_RT[1] <- mean( (x[ind==1,]$income - ob1)^2 ,na.rm=T) /mean( (x[ind==1,]$income - mean(x[ind==1,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[2] <- mean( (x[ind==2,]$income - ob2)^2 ,na.rm=T) /mean( (x[ind==2,]$income - mean(x[ind==2,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[3] <- mean( (x[ind==3,]$income - ob3)^2 ,na.rm=T) /mean( (x[ind==3,]$income - mean(x[ind==3,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[4] <- mean( (x[ind==4,]$income - ob4)^2 ,na.rm=T) /mean( (x[ind==4,]$income - mean(x[ind==4,]$income,na.rm=T) )^2,na.rm=T )
 NMSE_RT[5] <- mean( (x[ind==5,]$income - ob5)^2,na.rm=T ) /mean( (x[ind==5,]$income - mean(x[ind==5,]$income,na.rm=T) )^2 ,na.rm=T)
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

# error
rf_error_plot <- function(){
  plot(rf1,main='RandomForest Error',lty=1)
  plot(rf2,add=T,col='red',lty=2)
  plot(rf3,add=T,col='blue',lty=3)
  plot(rf4,add=T,col='green',lty=3)
  plot(rf5,add=T,col='orange',lty=5)
  legend("topright",legend=c('rf1','rf2','rf3','rf4','rf5') ,col=c('black','red','blue','green','orange'),lty=1:5)
}

rf_error_plot()

### importance
varImpPlot(rf1)
varImpPlot(rf2)
varImpPlot(rf3)
varImpPlot(rf4)
varImpPlot(rf5)


##预测

prf1 <- predict(object = rf1,newdata = x[ind==1,])
prf2 <- predict(object = rf2,newdata = x[ind==2,])
prf3 <- predict(object = rf3,newdata = x[ind==3,])
prf4 <- predict(object = rf4,newdata = x[ind==4,])
prf5 <- predict(object = rf5,newdata = x[ind==5,])

## 交叉验证

(nmse_rf <- NMSE(prf1,prf2,prf3,prf4,prf5))



#bagging回归

##建模
set.seed(1234)
bg1 <- bagging(formula = income ~ ., data = x_train_1,coob=F,nbagg=300)
bg2 <- bagging(formula = income ~ ., data = x_train_2,coob=F,nbagg=300)
bg3 <- bagging(formula = income ~ ., data = x_train_3,coob=F,nbagg=300)
bg4 <- bagging(formula = income ~ ., data = x_train_4,coob=F,nbagg=300)
bg5 <- bagging(formula = income ~ ., data = x_train_5,coob=F,nbagg=300)

##预测
pbg1 <- predict(object = bg1,newdata = x[ind==1,])
pbg2 <- predict(object = bg2,newdata = x[ind==2,])
pbg3 <- predict(object = bg3,newdata = x[ind==3,])
pbg4 <- predict(object = bg4,newdata = x[ind==4,])
pbg5 <- predict(object = bg5,newdata = x[ind==5,])

## 交叉验证
(nmse_bg <- NMSE(pbg1,pbg2,pbg3,pbg4,pbg5))



# boosting回归

get_mboost_formula <- function(){
  a <- 'income~btree(p11)'
  for (i in names(x)[c(-25,-1)]) { a <- paste(a,paste('+btree(',i,')',sep=''))}
  return(formula(a))
}

##建模
mb1 <- mboost(formula=get_mboost_formula(),data=x_train_1)
mb2 <- mboost(formula=get_mboost_formula(),data=x_train_2)
mb3 <- mboost(formula=get_mboost_formula(),data=x_train_3)
mb4 <- mboost(formula=get_mboost_formula(),data=x_train_4)
mb5 <- mboost(formula=get_mboost_formula(),data=x_train_5)

##预测

pmb1 <- predict(object = mb1,newdata = x[ind==1,])
pmb2 <- predict(object = mb2,newdata = x[ind==2,])
pmb3 <- predict(object = mb3,newdata = x[ind==3,])
pmb4 <- predict(object = mb4,newdata = x[ind==4,])
pmb5 <- predict(object = mb5,newdata = x[ind==5,])

## 交叉验证
(nmse_mb <- NMSE(pmb1,pmb2,pmb3,pmb4,pmb5))



# SVR
##建模
svr1 <- svm(formula = income ~ ., data = x_train_1)
svr2 <- svm(formula = income ~ ., data = x_train_2)
svr3 <- svm(formula = income ~ ., data = x_train_3)
svr4 <- svm(formula = income ~ ., data = x_train_4)
svr5 <- svm(formula = income ~ ., data = x_train_5)

##预测
psvr1 <- predict(object = svr1,newdata = x[ind==1,])
psvr2 <- predict(object = svr2,newdata = x[ind==2,])
psvr3 <- predict(object = svr3,newdata = x[ind==3,])
psvr4 <- predict(object = svr4,newdata = x[ind==4,])
psvr5 <- predict(object = svr5,newdata = x[ind==5,])

## 交叉验证
(nmse_svr <- NMSE(psvr1,psvr2,psvr3,psvr4,psvr5))


################################################################

# 综合比较

A <- cbind(nmse_svr,nmse_mb,nmse_bg,nmse_rf,nmse_rt)
A_mean <- apply(A,MARGIN = 2,FUN = mean)
barplot(sort(A_mean),col= 3:7,density = 30,main = '不同模型的平均 MSE ')




