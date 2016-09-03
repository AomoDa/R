

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
install.packages("party")#CTREE
install.packages("nnet")# BP

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
library(party)
library(nnet)

#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>>>>>># 数据预处理
#-------------------------------------------------------------------------------------

data_load <- function(file) {
  setwd(dir = getwd())
  x <- read.csv(file,header = T,stringsAsFactors = F,sep = ',')
  x <- as.data.frame(x =x[,-1],row.name=x[,1])
  x <- na.omit(x)
  x$income <- x$income / 10000 # 以万元表示，方便处理和显示数据
  x$p82 <- x$p82 / 10000
  x$p85 <- x$p85 / 10000
  x$p16 <- factor(x$p16,levels = c(0,1),labels = c('No','Yes'))
  x$p18 <- factor(x$p18,levels = c(0,1),labels = c('No','Yes'))
  x$p33 <- factor(x$p33,levels = c(0,1),labels = c('No','Yes'))
  x$p35 <- factor(x$p35,levels = c(0,1),labels = c('No','Yes'))
  x$p39 <- factor(x$p39,levels = c(0,1),labels = c('No','Yes'))
  x$p50 <- factor(x$p50,levels = c(0,1),labels = c('No','Yes'))
  x$p68 <- factor(x$p68,levels = 1:4,labels = c('土木结构','砖混结构','钢筋混泥土结构','其他结构'))
  x$p77 <- factor(x$p77,levels = c(0,1),labels = c('No','Yes'))
  return(as.data.frame(na.omit(x)))
}

x <- data_load(file = 'data_ok_v2.csv')

set.seed(100)
ind <- sample(x = 1:5,size = nrow(x),replace = T,prob = rep(1/5,5))
x_train_1 <- x[ind!=1,]
x_train_2 <- x[ind!=2,]
x_train_3 <- x[ind!=3,]
x_train_4 <- x[ind!=4,]
x_train_5 <- x[ind!=5,]


#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>定义MSE函数
#-------------------------------------------------------------------------------------

# 训练样本MSE
NMSE_TRAIN <-function(ob1,ob2,ob3,ob4,ob5) {
 NMSE_RT <- vector()
 ob1 <- predict(ob1,x[ind!=1,])
 ob2 <- predict(ob2,x[ind!=2,])
 ob3 <- predict(ob3,x[ind!=3,])
 ob4 <- predict(ob4,x[ind!=4,])
 ob5 <- predict(ob5,x[ind!=5,])
 NMSE_RT[1] <- mean( (x[ind!=1,]$income - ob1)^2 ,na.rm=T) /mean( (x[ind!=1,]$income - mean(x[ind!=1,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[2] <- mean( (x[ind!=2,]$income - ob2)^2 ,na.rm=T) /mean( (x[ind!=2,]$income - mean(x[ind!=2,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[3] <- mean( (x[ind!=3,]$income - ob3)^2 ,na.rm=T) /mean( (x[ind!=3,]$income - mean(x[ind!=3,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[4] <- mean( (x[ind!=4,]$income - ob4)^2 ,na.rm=T) /mean( (x[ind!=4,]$income - mean(x[ind!=4,]$income,na.rm=T) )^2,na.rm=T )
 NMSE_RT[5] <- mean( (x[ind!=5,]$income - ob5)^2,na.rm=T ) /mean( (x[ind!=5,]$income - mean(x[ind!=5,]$income,na.rm=T) )^2 ,na.rm=T)
 return(round(NMSE_RT,3)) 
}

# 预测样本MSE
NMSE <-function(ob1,ob2,ob3,ob4,ob5) {
 NMSE_RT <- vector()
 NMSE_RT[1] <- mean( (x[ind==1,]$income - ob1)^2 ,na.rm=T) /mean( (x[ind==1,]$income - mean(x[ind==1,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[2] <- mean( (x[ind==2,]$income - ob2)^2 ,na.rm=T) /mean( (x[ind==2,]$income - mean(x[ind==2,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[3] <- mean( (x[ind==3,]$income - ob3)^2 ,na.rm=T) /mean( (x[ind==3,]$income - mean(x[ind==3,]$income,na.rm=T) )^2 ,na.rm=T)
 NMSE_RT[4] <- mean( (x[ind==4,]$income - ob4)^2 ,na.rm=T) /mean( (x[ind==4,]$income - mean(x[ind==4,]$income,na.rm=T) )^2,na.rm=T )
 NMSE_RT[5] <- mean( (x[ind==5,]$income - ob5)^2,na.rm=T ) /mean( (x[ind==5,]$income - mean(x[ind==5,]$income,na.rm=T) )^2 ,na.rm=T)
 return(round(NMSE_RT,3)) 
}



#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>>>># 多元线性回归
#-------------------------------------------------------------------------------------

##建模
lm1 <- step(lm(formula = income~.,data=x_train_1))
lm2 <- step(lm(formula = income~.,data=x_train_2))
lm3 <- step(lm(formula = income~.,data=x_train_3))
lm4 <- step(lm(formula = income~.,data=x_train_4))
lm5 <- step(lm(formula = income~.,data=x_train_5))


##预测

plm1 <- predict(object = lm1,newdata = x[ind==1,])
plm2 <- predict(object = lm2,newdata = x[ind==2,])
plm3 <- predict(object = lm3,newdata = x[ind==3,])
plm4 <- predict(object = lm4,newdata = x[ind==4,])
plm5 <- predict(object = lm5,newdata = x[ind==5,])
# 交叉验证

(nmse_lm <- NMSE(plm1,plm2,plm3,plm4,plm5))
# [1] 0.749 0.738 0.827 0.562 0.762

(nmse_lm_train <- NMSE_TRAIN(lm1,lm2,lm3,lm4,lm5))
# [1] 0.652 0.660 0.637 0.741 0.655





#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>>>>非线性回归
#-------------------------------------------------------------------------------------
#  还没搞定这个
# get_nlm_formula <- function(){
# a <-'income~ p13 + I(p13 ^2)  + I(p13 ^3) '
# x <-x[,-21]
# for (i in 2:length(x)) {  
#    a <-  ifelse(is.numeric(x[,i]),
#        paste(a,paste( ' + I(' , names(x)[i],'^2) + ' ,' + I(' , names(x)[i],'^3) + ', names(x)[i]  ,sep=''), sep='' ) ,
#        paste(a, '+' ,names(x)[i],sep=''))   }
# return(formula(a))
#}
#
#
#
#nlm1 <- step(lm(formula =get_nlm_formula() ,data=x_train_1))
#nlm2 <- step(lm(formula =get_nlm_formula() ,data=x_train_2))
#nlm3 <- step(lm(formula =get_nlm_formula() ,data=x_train_3))
#nlm4 <- step(lm(formula =get_nlm_formula() ,data=x_train_4))
#nlm5 <- step(lm(formula =get_nlm_formula() ,data=x_train_5))
#pnlm1 <- (predict(object = nlm1,newdata = x[ind==1,]))
#pnlm2 <- (predict(object = nlm2,newdata = x[ind==2,]))
#pnlm3 <- (predict(object = nlm3,newdata = x[ind==3,]))
#pnlm4 <- (predict(object = nlm4,newdata = x[ind==4,]))
#pnlm5 <- (predict(object = nlm5,newdata = x[ind==5,]))
#
# 交叉验证
#(nmse_nlm <- NMSE(pnlm1,pnlm2,pnlm3,pnlm4,pnlm5))



#-------------------------------------------------------------------------------------
# >>>>>>>>>>>>>>>>>>分类回归决策树
#-------------------------------------------------------------------------------------


##建模
rt1 <- rpart(formula = income~.,data=x_train_1)
rt2 <- rpart(formula = income~.,data=x_train_2)
rt3 <- rpart(formula = income~.,data=x_train_3)
rt4 <- rpart(formula = income~.,data=x_train_4)
rt5 <- rpart(formula = income~.,data=x_train_5)

##剪枝
prt1 <- prune(rt1, cp= rt1$cptable[which.min(rt1$cptable[,"xerror"]),"CP"])
prt2 <- prune(rt2, cp= rt2$cptable[which.min(rt2$cptable[,"xerror"]),"CP"])  
prt3 <- prune(rt3, cp= rt3$cptable[which.min(rt3$cptable[,"xerror"]),"CP"])  
prt4 <- prune(rt4, cp= rt4$cptable[which.min(rt4$cptable[,"xerror"]),"CP"])  
prt5 <- prune(rt5, cp= rt5$cptable[which.min(rt5$cptable[,"xerror"]),"CP"])  

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

(nmse_rt <- NMSE(p1,p2,p3,p4,p5))
# [1] 0.504 0.647 0.534 0.584 0.585
(nmse_rt_train <- NMSE_TRAIN(prt1,prt2,prt3,prt4,prt5))
# [1] 0.526 0.505 0.573 0.493 0.581


#-------------------------------------------------------------------------------------
# >>>>>>>>>>>>>>>>>>>条件推理回归树
#-------------------------------------------------------------------------------------

##建模
ct1 <- ctree(formula = income~.,data=x_train_1,controls = ctree_control(minsplit = 20,minbucket = 20,maxdepth=10))
ct2 <- ctree(formula = income~.,data=x_train_2,controls = ctree_control(minsplit = 20,minbucket = 20,maxdepth=10))
ct3 <- ctree(formula = income~.,data=x_train_3,controls = ctree_control(minsplit = 20,minbucket = 20,maxdepth=10))
ct4 <- ctree(formula = income~.,data=x_train_4,controls = ctree_control(minsplit = 20,minbucket = 20,maxdepth=10))
ct5 <- ctree(formula = income~.,data=x_train_5,controls = ctree_control(minsplit = 20,minbucket = 20,maxdepth=10))
##预测
pct1 <- predict(object = ct1,newdata = x[ind==1,])
pct2 <- predict(object = ct2,newdata = x[ind==2,])
pct3 <- predict(object = ct3,newdata = x[ind==3,])
pct4 <- predict(object = ct4,newdata = x[ind==4,])
pct5 <- predict(object = ct5,newdata = x[ind==5,])
## 交叉验证

(nmse_ct <- NMSE(pct1,pct2,pct3,pct4,pct5))
# [1] 0.701 0.813 0.803 0.761 0.656

(nmse_ct_train <- NMSE_TRAIN(ct1,ct2,ct3,ct4,ct5))
# [1] 0.658 0.594 0.586 0.574 0.611



#-------------------------------------------------------------------------------------
# >>>>>>>>>>>>>>>>>>>>>>.随机森林
#-------------------------------------------------------------------------------------


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
# [1] 0.388 0.461 0.369 0.552 0.370
(nmse_rf_train <- NMSE_TRAIN(rf1,rf2,rf3,rf4,rf5))
#[1] 0.094 0.095 0.106 0.083 0.101

#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>>>>>bagging回归
#-------------------------------------------------------------------------------------


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
#[1] 0.465 0.588 0.444 0.595 0.472

(nmse_bg_train <- NMSE_TRAIN(bg1,bg2,bg3,bg4,bg5))
#[1] 0.468 0.440 0.468 0.419 0.468


#-------------------------------------------------------------------------------------
# >>>>>>>>>>>>>>>>>>>>>boosting回归
#-------------------------------------------------------------------------------------


get_mboost_formula <- function(){
  a <- 'income~btree(p13)'
  for (i in names(x)[c(-21,-1)]) { a <- paste(a,paste('+btree(',i,')',sep=''))}
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
#[1] 0.567 0.630 0.593 0.691 0.570
(nmse_mb_train <- NMSE_TRAIN(mb1,mb2,mb3,mb4,mb5))
#[1] 0.613 0.594 0.601 0.571 0.608



#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>>>>>>>SVR
#-------------------------------------------------------------------------------------


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
# [1] 0.671 0.650 0.686 0.812 0.611
(nmse_svr_train <- NMSE_TRAIN(svr1,svr2,svr3,svr4,svr5))
# [1] 0.655 0.653 0.641 0.596 0.661



#-------------------------------------------------------------------------------------
#>>>>>>>>>>>>> BP神级网络
#-------------------------------------------------------------------------------------


##数据预处理

get_bp_formula <- function(){
 a <-'scale(income)~scale(p13)'
 x <-x[,-21]
 for (i in 2:length(x)) {  
    a <-  ifelse(is.numeric(x[,i]),paste(a,paste( ' +scale(',names(x)[i],' )' ,sep=''  ),sep='' ) ,paste(a, '+' ,names(x)[i],sep=''   ))   }
 return(formula(a))
}

# 建模
set.seed(2345)
bp1 <- nnet(get_bp_formula(),data=x_train_1,size=10,decay=0.1 ,linout=T)
bp2 <- nnet(get_bp_formula(),data=x_train_2,size=10,decay=0.1 ,linout=T)
bp3 <- nnet(get_bp_formula(),data=x_train_3,size=10,decay=0.1 ,linout=T)
bp4 <- nnet(get_bp_formula(),data=x_train_4,size=10,decay=0.1 ,linout=T)
bp5 <- nnet(get_bp_formula(),data=x_train_5,size=10,decay=0.1 ,linout=T)


#预测
pbp1 <- predict(object = bp1,newdata = x[ind==1,]) * sd(x_train_1$income) + mean(x_train_1$income)
pbp2 <- predict(object = bp2,newdata = x[ind==2,]) * sd(x_train_2$income) + mean(x_train_2$income)
pbp3 <- predict(object = bp3,newdata = x[ind==3,]) * sd(x_train_3$income) + mean(x_train_3$income)
pbp4 <- predict(object = bp4,newdata = x[ind==4,]) * sd(x_train_4$income) + mean(x_train_4$income)
pbp5 <- predict(object = bp5,newdata = x[ind==5,]) * sd(x_train_5$income) + mean(x_train_5$income)


# 验证
(nmse_bp <- NMSE(pbp1,pbp2,pbp3,pbp4,pbp5))

# [1] 0.450 0.500 0.517 0.697 0.584
(nmse_bp_train <- NMSE_TRAIN(bp1,bp2,bp3,bp4,bp5))
# [1] 1.602 1.614 1.503 1.729 1.582


################################################################

# 综合比较



#--------------------------------------------------------------------------------------
## 训练样本比较
#--------------------------------------------------------------------------------------
(A <- cbind(nmse_svr_train,nmse_mb_train,nmse_bg_train,nmse_rf_train,
            nmse_rt_train,nmse_lm_train,nmse_bp_train,nmse_ct_train))
#     nmse_svr_train nmse_mb_train nmse_bg_train nmse_rf_train nmse_rt_train nmse_lm_train nmse_bp_train nmse_ct_train
#[1,]          0.655         0.613         0.468         0.094         0.526         0.652         1.602         0.658
#[2,]          0.653         0.594         0.440         0.095         0.505         0.660         1.614         0.594
#[3,]          0.641         0.601         0.468         0.106         0.573         0.637         1.503         0.586
#[4,]          0.596         0.571         0.419         0.083         0.493         0.741         1.729         0.574
#[5,]          0.661         0.608         0.468         0.101         0.581         0.655         1.582         0.611

(A_mean <- apply(A,MARGIN = 2,FUN = mean))
#nmse_svr_train  nmse_mb_train  nmse_bg_train  nmse_rf_train  nmse_rt_train  nmse_lm_train  nmse_bp_train  nmse_ct_train 
#        0.6412         0.5974         0.4526         0.0958         0.5356         0.6690         1.6060         0.6046 
barplot(sort(A_mean),col= 3:7,density = 30,main = '不同模型的训练样本平均 MSE ')

#--------------------------------------------------------------------------------------
## 测试样本比较
#--------------------------------------------------------------------------------------
(B <- cbind(nmse_svr,nmse_mb,nmse_bg,nmse_rf,
            nmse_rt,nmse_lm,nmse_bp,nmse_ct))
#     nmse_svr nmse_mb nmse_bg nmse_rf nmse_rt nmse_lm nmse_bp nmse_ct
#[1,]    0.671   0.567   0.465   0.388   0.504   0.749   0.450   0.701
#[2,]    0.650   0.630   0.588   0.461   0.647   0.738   0.500   0.813
#[3,]    0.686   0.593   0.444   0.369   0.534   0.827   0.517   0.803
#[4,]    0.812   0.691   0.595   0.552   0.584   0.562   0.697   0.761
#[5,]    0.611   0.570   0.472   0.370   0.585   0.762   0.584   0.656

(B_mean <- apply(B,MARGIN = 2,FUN = mean))
# nmse_svr  nmse_mb  nmse_bg  nmse_rf  nmse_rt  nmse_lm  nmse_bp  nmse_ct 
#  0.6860   0.6102   0.5128   0.4280   0.5708   0.7276   0.5496   0.7468 
barplot(sort(B_mean),col= 3:7,density = 30,main = '不同模型的测试平均 MSE ')






