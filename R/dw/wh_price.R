
library(caret)
library(lattice)
library(MASS)
library(randomForest)
library(adabag)
library(rpart)
library(gbm)

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
xx <- merge(x_agge,price)
xxx <- merge(x,price)


# define function
bs_mean <- function(x) {
	n <- length(x)
	bsmean <- mean(replicate(1000,median(sample(x,size = n,replace = T))))
	return(bsmean)
}


# Linear Models

lm1 <- lm(price~.,data=xxx[,-c(1:3)])

stepAIC(lm1,direction = 'both')

lm2 <- lm(formula = price ~ k1_bd_mob + k1_weixin + k2_bd_pc + k2_bd_mob + 
    k2_sg_mob + k3_bd_mob + k3_weixin + k5_bd_pc + k5_bd_mob + 
    k5_sg_mob + k6_bd_mob + k6_weixin + k6_media + k7_sg_pc + 
    k7_media + k8_sg_pc + k8_media + k9_bd_mob + k10_bd_pc + 
    k10_bd_mob + lag1_price, data = xxx[, -c(1:3)])
lm.pred <- data.frame(years=xxx$years,months=xxx$months,pred=lm2$fitted.values)
merge(aggregate(pred~years+months,data=lm.pred,bs_mean),price[,-4])


# Regression Trees


rt1 <- rpart(price~.,data=xxx[,-c(1:3)])
rt.pred <- data.frame(years=xxx$years,months=xxx$months,pred=predict(rt1,type = 'vector'))
merge(aggregate(pred~years+months,data=rt.pred,bs_mean),price[,-4])



# randomForest
rf1 <- randomForest(price~.,data=xxx[,-c(1:3)],ntree=500,mtry=sqrt(53))
rf.pred <- data.frame(years=xxx$years,months=xxx$months,pred=rf1$predicted)
merge(aggregate(pred~years+months,data=rf.pred,bs_mean),price[,-4])


# Generalized Boosted Regression Modeling
