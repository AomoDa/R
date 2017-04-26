library(leaps)
library(caret)
library(lattice)
library(MASS)
library(randomForest)
library(rpart)
library(gbm)
library(plyr)
library(e1071)
library(effects)
library(glmnet)
library(rpart.plot)
library(mice)


#--------------------------------------------------------------
# 十个关键词：
# 
# 武汉房价	k1
# 武汉房价走势	k2
# 武汉四环线	k3
# 武汉光谷房价	k4
# 武汉二手房	k5
# 武汉楼盘	k6
# 武汉新房	k7
# 武汉房产	k8
# 武汉房价网	k9
# 武汉公积金管理中心	k10
#--------------------------------------------------------------
# 数据处理注意
# 数据文件按照我给你的格式添加，不要随便更改数据格式
# 数据按照我给你的格式添加，横向添加即可。
# 所有包必须全部准确加载，如果加载少或者不对，代码无法运行
# 所有代码必须从前往后运行，不能跳过或生了，切记。
#--------------------------------------------------------------

# 读取关键词数据和房价数据
# header ： 表示数据文件带表头
# sep： 表示文件的分隔符。
# stringsAsFactors：文本型的变量不转成因子
# 有些参数是默认的，所有有时候也可以不加。
x <- read.table('x.csv',header = T,sep = ',',stringsAsFactors = F)
price <- read.csv('price.csv',header = T)
# 求上月房价
price$lag1_price <- c(NA,price$price[-nrow(price)])
# price$lag2_price <- c(NA,NA,price$price[ c(-nrow(price),-nrow(price)+1)])
# Date
# 处理日期
x$date <- as.Date(x$date)
x$years <- as.numeric(substr(x$date,1,4))
x$months <- as.numeric(substr(x$date,6,7))

# 处理缺失值，赋值-999
x[is.na(x)] <- -999
# delete near zero variance predictors	
# 删除方差是0的变量
x <- x[,-nearZeroVar(x)]

#Multivariate Imputation by Chained Equations 
x[x==-999] <- NA
set.seed(2017)
imp <- mice(x[,-1],method ='pmm')
a <- as.data.frame(complete(imp,action = 1))

newx <- as.data.frame(cbind(data.frame(date=x$date),a))

# 房价数据和关键词数据合并
# 讲关键词数据和房价数据合并，合并实在代码中处理的。
xxx <- merge(newx,price,all.x = T)
#--------------------------------------------------------------
# Feature Selection
# 变量选择
# 采用最优子集算法，选择最合适的变量,标段标准是BIC
# bic（Bayesian Information Criterions）： 贝叶斯信息规则
# 计算公式： BIC=-2 ln(L) +ln(n)*k
# L：似然函数
# n：样本大小
# K：参数数量
# 与AIC一样是对模型的拟合效果进行评价的一个指标，BIC值越小，则模型对数据的拟合越好。
#--------------------------------------------------------------

set.seed(2017)
# 最优子集算法拟合
reg1 <- regsubsets(price~.,data=na.omit(xxx[,-c(1:3)]),really.big = TRUE,nvmax=10)
reg.summ <- summary(reg1)
# 最合适的ID
best_id <- which.min(reg.summ$bic)
best_id
# 根据BIC，选择最优的变量子集
Feature_Name <- names(coef(reg1,id=best_id))[-1]
Feature_Name
# create new data set 
mydf <- cbind(xxx[,c(1:3,55)],subset(xxx,select=Feature_Name))

# data split
# 数据拆分处理，
# 将3月之前的数据用作模型训练
# 3月之后的数据用作模型对比，选择最小误差的作为预测模型。
train_data <- na.omit(subset(x = mydf,subset = date < as.Date('2017-03-01')))
test_data <- na.omit(subset(x = mydf,subset = date >= as.Date('2017-03-01')))
forecast_data <- subset(x = mydf,subset = date >= as.Date('2017-04-01'))

# 定义一个均值算法，
# 把每天的房价变成一个月的房价。
# define function
bs_mean <- function(x) {
	n <- length(x)
	bsmean <- mean(replicate(1000,median(sample(x,size = n,replace = T))))
	return(bsmean)
}



#--------------------------------------------------------------
# Linear Models
#--------------------------------------------------------------

lm1 <- lm(price~.,data=train_data[,-3])
lm2 <- stepAIC(lm1,direction = 'both',trace = F)
anova(lm1,lm2,test='Chisq')
summary(lm2)
# Constructing Effect Plots
plot(allEffects(lm2))


# fitted
lm.fitted <- data.frame(years=train_data$years,months=train_data$months,pred=lm2$fitted.values)
lm.result <- merge(aggregate(pred~years+months,data=lm.fitted,bs_mean),price[,-4])
lm.result

# pred
lm.pred <- data.frame(years=test_data$years,months=test_data$months,pred=predict(lm2,test_data))
lm.result <- merge(aggregate(pred~years+months,data=lm.pred,bs_mean),price[,-4])
lm.result
lm.error <- (lm.result$pred-lm.result$price) / lm.result$price
# 比较这个值就行，越小越好，其他的类似。
lm.error


#--------------------------------------------------------------
# LASSO Models
# 该方法是一种压缩估计。它通过构造一个罚函数得到一个较为精炼的模型，
# 使得它压缩一些系数，同时设定一些系数为零。
# 因此保留了子集收缩的优点，是一种处理具有复共线性数据的有偏估计。
# cv.glmnet 是交叉验证方法
# 交叉验证法。对lambda的格点值，进行交叉验证，
# 选取交叉验证误差最小的lambda值。最后，按照得到的lambda值，用全部数据重新拟合模型即可。
#--------------------------------------------------------------

X <- model.matrix(price~.,data=train_data[,-3])[,-1]
Y <- train_data$price
lasso1 <- cv.glmnet(X,Y,alpha = 1,nfolds=3)


# fitted
fitted <- predict(lasso1,
	s=lasso1$lambda.1se,
	newx = model.matrix(price~.,data=train_data[,-3])[,-1])
lasso.fitted <- data.frame(years=train_data$years,
	months=train_data$months,
	pred=as.numeric(fitted))
lasso.result <- merge(aggregate(pred~years+months,data=lasso.fitted,bs_mean),price[,-4])
lasso.result




# pred
pred <- predict(lasso1,
	s=lasso1$lambda.1se,
	newx = model.matrix(price~.,data=test_data[,-3])[,-1])
lasso.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=as.numeric(pred))
lasso.result <- merge(aggregate(pred~years+months,data=lasso.pred,bs_mean),price[,-4])
lasso.result
lasso.error <- (lasso.result$pred-lasso.result$price) / lasso.result$price
lasso.error



#--------------------------------------------------------------
# Regression Trees
# 修建cp值确定最优树
#--------------------------------------------------------------
rt1 <- rpart(price~.,data=train_data[,-3],cp=0.01,method='anova')

# fitted
rt.fitted <- data.frame(years=train_data$years,
	months=train_data$months,
	pred=predict(rt1,train_data))
rt.result <- merge(aggregate(pred~years+months,data=rt.fitted,bs_mean),price[,-4])
rt.result


# pred
rt.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(rt1,test_data))
rt.result <- merge(aggregate(pred~years+months,data=rt.pred,bs_mean),price[,-4])
rt.result
rt.error <- (rt.result$pred-rt.result$price) / rt.result$price
rt.error



#--------------------------------------------------------------
# randomForest
# 设置500课树，通常500已经足够大了。
#--------------------------------------------------------------

set.seed(12345)
p <- ncol(train_data[,-c(3)])-1
rf1 <- randomForest(price~.,data=train_data[,-3],ntree=500,mtry=sqrt(p))
par(mfrow=c(1,2))
plot(rf1)
varImpPlot(rf1)
par(mfrow=c(1,1))

# fitted

rf.fitted <- data.frame(years=train_data$years,
	months=train_data$months,
	pred=predict(rf1,train_data))
rf.result <- merge(aggregate(pred~years+months,data=rf.fitted,bs_mean),price[,-4])
rf.result


# pred
rf.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(rf1,test_data))
rf.result <- merge(aggregate(pred~years+months,data=rf.pred,bs_mean),price[,-4])
rf.result
rf.error <- (rf.result$pred-rf.result$price) / rf.result$price
rf.error


#--------------------------------------------------------------
# Generalized Boosted Regression Modeling
# 这些参数涉及到GBM原理，你可以在书上找到。
# n.trees：   the total number of trees to fit. 
#             This is equivalent to the number of iterations and the number of basis functions in the additive expansion.
# interaction.depth	：The maximum depth of variable interactions. 1 implies an additive model, 
#                     2 implies a model with up to 2-way interactions, etc.
# shrinkage	：  a shrinkage parameter applied to each tree in the expansion. 
#                Also known as the learning rate or step-size reduction.
# 这些参数已经是调整过的了，已经是很好了，当然你可以自己再改一下。
#--------------------------------------------------------------


set.seed(1234)
gbm1 <- gbm(price~.,data=train_data[,-3],
	n.trees=1000,
	distribution=list(name="quantile",alpha=0.2),
	interaction.depth=4,
	shrinkage=0.2,
	n.minobsinnode = 10,
	cv.folds=3)

# fitted
gbm.fitted <- data.frame(years=train_data$years,
	months=train_data$months,
	pred=predict(gbm1,train_data,n.trees = 1000))
gbm.result <- merge(aggregate(pred~years+months,data=gbm.fitted,bs_mean),price[,-4])
gbm.result


# pred
gbm.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(gbm1,test_data,n.trees = 1000))
gbm.result <- merge(aggregate(pred~years+months,data=gbm.pred,bs_mean),price[,-4])
gbm.result
gbm.error <- (gbm.result$pred-gbm.result$price) / gbm.result$price
gbm.error


#--------------------------------------------------------------
# Support Vector Machines
#--------------------------------------------------------------

set.seed(2018)
svr1 <- svm(x=train_data[,-c(3:4)],
	y=train_data[,4],
	type='nu-regression',
	kernel='linear',
	scale=TRUE,
	cost=2)

# fitted
svr.fitted <- data.frame(years=train_data$years,
	months=train_data$months,
	pred=predict(svr1,newdata = train_data[,-c(3:4)]))
svr.result <- merge(aggregate(pred~years+months,data=svr.fitted,bs_mean),price[,-c(4)])
svr.result


# pred
svr.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(svr1,newdata = test_data[,-c(3:4)]))
svr.result <- merge(aggregate(pred~years+months,data=svr.pred,bs_mean),price[,-c(4)])
svr.result
svr.error <- (svr.result$pred-svr.result$price) / svr.result$price
svr.error



#--------------------------------------------------------------
# 预测数据
#--------------------------------------------------------------
#LM
lm.forecast <- data.frame(years=forecast_data$years,
	months=forecast_data$months,
	pred=predict(lm2,newdata = forecast_data[,-c(3:4)]) )
lm.result <- merge(aggregate(pred~years+months,data=lm.forecast,bs_mean),price[,-4])
lm.result

#SVR
svr.forecast <- data.frame(years=forecast_data$years,
	months=forecast_data$months,
	pred=predict(svr1,newdata = forecast_data[,-c(3:4)]) )
svr.result <- merge(aggregate(pred~years+months,data=svr.forecast,bs_mean),price[,-4])
svr.result

# Final
mean(c(svr.result$pred,lm.result$pred))
