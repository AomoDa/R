

#--------------------------------------------------------------
## part 1 import data and packages
#--------------------------------------------------------------

library(ggplot2)
library(gmodels)
library(MASS)
library(klaR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(library(ROCR))

data('GermanCredit',package = 'klaR')
names(GermanCredit)
#These data have two classes for the credit worthiness: good or bad. 
#There are predictors related to attributes, such as: 
#checking account status, duration, credit history, 
#purpose of the loan, amount of the loan, 
#savings accounts or bonds, employment duration, 
#Installment rate in percentage of disposable income, 
#personal information, other debtors/guarantors, 
#residence duration, property, age, 
#other installment plans, housing, 
#number of existing credits, job information, 
#Number of people being liable to provide maintenance for, 
#telephone, and foreign worker status.


### response variables
table(GermanCredit$credit_risk)
pie(table(GermanCredit$credit_risk),
    main='Pie Charts of credit risk')


#--------------------------------------------------------------
## part 2 EDA and Hypotheses Test
#--------------------------------------------------------------



### Two Sample t-test of credit amount VS credit_risk
ggplot(data=GermanCredit,aes(x=credit_risk,y=amount,fill=credit_risk))+
      geom_boxplot()+labs(title='boxplot of credit amount VS credit_risk')

ggplot(data=GermanCredit,aes(x=amount,fill=credit_risk,col=credit_risk))+
      geom_freqpoly(bins =30,aes(y=..density..),col=I('black'))+
      facet_wrap(~credit_risk,ncol=1)+
      geom_histogram(alpha=0.5,bins =30,aes(y=..density..))+
      labs(title='histogramof credit amount VS credit_risk')

# Two Sample t-tes
with(GermanCredit,t.test(amount~credit_risk))
plotmeans(amount~credit_risk,data=GermanCredit,
    main='mean credit amount of \n diffrnrnt credit_risk')


#--------------------------------------------------------------

### Pearson's Chi-squared test of employment_duration VS credit_risk
ggplot(data=GermanCredit,aes(x=employment_duration,fill=employment_duration))+
      geom_bar(show.legend = F)+labs(title='employment_duration')

ggplot(data=GermanCredit,aes(x=employment_duration,fill=credit_risk))+
      geom_bar(show.legend = T,position='dodge')+labs(title='employment_duration')


#Pearson's Chi-squared test 
with(GermanCredit,
     CrossTable(employment_duration,credit_risk,
     prop.c=F,prop.t=F,prop.chisq=F,expected=T,digits=3))
with(GermanCredit,mosaicplot(table(employment_duration,credit_risk),
     shade = T,main='Mosaic Plots of \n employment_duration VS credit_risk '))




#--------------------------------------------------------------
## part 3 Model Analysis 
#--------------------------------------------------------------



### data preparation

set.seed(1234)
ind <- sample(x=1:2,size = nrow(GermanCredit),replace = T,prob = c(0.9,0.1))
data_train <- GermanCredit[ind==1,]
data_test <- GermanCredit[ind==2,]




##Generalized Linear Models--Logistic Regression

lr_data_train <- data_train
lr_data_test <- data_test
lr_data_train$credit_risk <- ifelse(lr_data_train$credit_risk=='good',1,0)
lr_data_test$credit_risk <- ifelse(lr_data_test$credit_risk=='good',1,0)


lr1 <- glm(credit_risk~.,data=lr_data_train,family = binomial())
lr2 <- stepAIC(lr1,direction = 'both')
pred_lr_train <- ifelse(lr2$fitted.values>0.5,'good','bad')
table(pred_lr_train,data_train$credit_risk)


##predict
pred_lr_test <- predict(lr2,newdata = lr_data_test)
pred_lr_test <- ifelse(pred_lr_test>0.5,'good','bad')
table(pred_lr_test,data_test$credit_risk)


#ROC curves
pred_lr_test <- prediction(ifelse(pred_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(pred_lr_test,'tpr','fpr'),colorize=T,main='ROC curves of Logistic Regression')
abline(0,1,lty=3)



#--------------------------------------------------------------


# Recursive Partitioning and Regression Trees
ct1 <- rpart(credit_risk~.,data=data_train,control = rpart.control(cp=0.001))
ct1$cptable
# choose the CP value,which is the smallest xerror corresponding to
ct2 <- prune.rpart(tree = ct1,cp = 0.012)
rpart.plot(ct2)


pred_train <- predict(object = ct2,newdata = data_train,type='class')
table(pred_train,data_train$credit_risk)

##predict

ct_pred_test <- predict(object = ct2,newdata = data_test,type='class')
table(ct_pred_test,data_test$credit_risk)


#ROC curves
ct_pred_test <- prediction(ifelse(ct_pred_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(ct_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of ct2 model')
abline(0,1,lty=3)

#--------------------------------------------------------------

#Random Forest
set.seed(6666)
rf <- randomForest(credit_risk~.,data=data_train,ntree=1000,mtry=16)
table(rf$predicted,data_train$credit_risk)

plot(rf)
varImpPlot(rf)

##predict
rf_pred_test <- predict(object = rf,newdata = data_test,type='class')
table(pred_test,data_test$credit_risk)

#ROC curves
rf_pred_test <- prediction(ifelse(rf_pred_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(rf_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of Random Forest')
abline(0,1,lty=3)


