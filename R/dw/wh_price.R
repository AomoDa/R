library(leaps)
library(caret)
library(lattice)
library(MASS)
library(randomForest)
library(rpart)
library(gbm)
library(plyr)
library(e1071)

x <- read.table('x.csv',header = T,sep = '\t',stringsAsFactors = F)
price <- read.csv('price.csv',header = T)
price$lag1_price <- c(NA,price$price[-nrow(price)])

# Date
x$date <- as.Date(x$date)
x[is.na(x)] <- -999
# delete near zero variance predictors	
x <- x[,-nearZeroVar(x)]

x$years <- as.numeric(substr(x$date,1,4))
x$months <- as.numeric(substr(x$date,6,7))

# x$days <- as.numeric(substr(x$date,9,10))
# x$datenum <- x$years * 1e4 + x$months* 1e2 + x$days

x_agge <- ddply(.data = x[,-1],.variables = c('years','months'),.fun = colMeans)

# merge data
xxx <- merge(x,price)

#--------------------------------------------------------------
# Feature Selection
#--------------------------------------------------------------

# 
set.seed(2017)
reg1 <- regsubsets(price~.,data=xxx[,-c(1:3)],really.big = TRUE,nvmax=10)
reg.summ <- summary(reg1)
best_id <- which.min(reg.summ$bic)
best_id

Feature_Name <- names(coef(reg1,id=best_id))[-1]
Feature_Name
# create new data set 
mydf <- cbind(xxx[,c(1:3,55)],subset(xxx,select=Feature_Name))

# data split
train_data <- subset(x = mydf,subset = date < as.Date('2017-03-01'))
test_data <- subset(x = mydf,subset = date >= as.Date('2017-03-01'))

# define function
bs_mean <- function(x) {
	n <- length(x)
	bsmean <- mean(replicate(1000,median(sample(x,size = n,replace = T))))
	return(bsmean)
}



#--------------------------------------------------------------
# Linear Models
#--------------------------------------------------------------

lm1 <- lm(price~.,data=train_data[,-c(1:3)])
lm2 <- stepAIC(lm1,direction = 'both',trace = F)
anova(lm1,lm2,test='Chisq')
summary(lm2)


lm.pred <- data.frame(years=test_data$years,months=test_data$months,pred=predict(lm2,test_data))
lm.result <- merge(aggregate(pred~years+months,data=lm.pred,bs_mean),price[,-4])
lm.result
lm.error <- (lm.result$pred-lm.result$price) / lm.result$price
lm.error


#--------------------------------------------------------------
# LASSO Models
#--------------------------------------------------------------

X <- model.matrix(price~.,data=train_data[,-c(1:3)])[,-1]
Y <- train_data$price
lasso1 <- cv.glmnet(X,Y,alpha = 1,nfolds=3)
pred <- predict(lasso1,s=lasso1$lambda.1se,newx = model.matrix(price~.,data=test_data[,-c(1:3)])[,-1])
lasso.pred <- data.frame(years=test_data$years,months=test_data$months,pred=as.numeric(pred))
lasso.result <- merge(aggregate(pred~years+months,data=lasso.pred,bs_mean),price[,-4])
lasso.result
lasso.error <- (lasso.result$pred-lasso.result$price) / lasso.result$price
lasso.error



#--------------------------------------------------------------
# Regression Trees
#--------------------------------------------------------------
rt1 <- rpart(price~.,data=train_data[,-c(1:3)],cp=0.01)
rt.pred <- data.frame(years=test_data$years,months=test_data$months,pred=predict(rt1,test_data))
rt.result <- merge(aggregate(pred~years+months,data=rt.pred,bs_mean),price[,-4])
rt.result
rt.error <- (rt.result$pred-rt.result$price) / rt.result$price
rt.error



#--------------------------------------------------------------
# randomForest
#--------------------------------------------------------------

set.seed(12345)
p <- ncol(train_data[,c(1:3)])-1
rf1 <- randomForest(price~.,data=train_data[,-c(1:3)],ntree=500,mtry=sqrt(p))
rf.pred <- data.frame(years=test_data$years,months=test_data$months,pred=predict(rf1,test_data))
rf.result <- merge(aggregate(pred~years+months,data=rf.pred,bs_mean),price[,-4])
rf.result
rf.error <- (rf.result$pred-rf.result$price) / rf.result$price
rf.error


#--------------------------------------------------------------
# Generalized Boosted Regression Modeling
#--------------------------------------------------------------


set.seed(12345)
gbm1 <- gbm(price~.,data=train_data[,-c(1:3)],
	n.trees=5000,
	distribution=list(name="quantile",alpha=0.8),
	interaction.depth=4,
	shrinkage=0.1,
	n.minobsinnode = 10)

gbm.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(gbm1,test_data,n.trees = 5000))
gbm.result <- merge(aggregate(pred~years+months,data=gbm.pred,bs_mean),price[,-4])
gbm.result
gbm.error <- (gbm.result$pred-gbm.result$price) / gbm.result$price
gbm.error


#--------------------------------------------------------------
# Support Vector Machines
#--------------------------------------------------------------

set.seed(2018)
svr1 <- svm(x=train_data[,-c(1:4)],
	y=train_data[,4],
	type='nu-regression',
	kernel='linear',
	degree=5)
svr.pred <- data.frame(years=test_data$years,
	months=test_data$months,
	pred=predict(svr1,newdata = test_data[,-c(1:4)]))
svr.result <- merge(aggregate(pred~years+months,data=svr.pred,bs_mean),price[,-4])
svr.result
svr.error <- (svr.result$pred-svr.result$price) / svr.result$price
svr.error
