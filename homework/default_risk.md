---
title: "A research report on using Machine Learning to predict Bank Credit Risks "
author: "Your Name"
date: "2016-11-22"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#1.Background and Introduction

The global financial crisis from 2007 to 2008 highlighted the importance of transparency and rigor in bank business. As credit supply was constrained, banks are increasingly tightening their lending systems and turning to algorithms such as data mining and machine learning to identify and evaluate credit risk. The idea behind the credit model is to determine the level of user’s default risk. We obtain some credit data from the UCI website, which is the data obtained from a German credit institution and contains 1,000 loan cases combined by  the credit feature and the applicant characteristic.
   
   

My data frame containing 1,000 observations on 21 variables:

- `status` : factor variable indicating the status of the existing checking account, with levels ... < 100 DM, 0 <= ... < 200 DM, ... >= 200 DM/salary for at least 1 year and no checking account.
- `duration` : duration in months.
- `credit_history` : factor variable indicating credit history, with levels no credits taken/all credits paid back duly, all credits at this bank paid back duly, existing credits paid back duly till now, delay in paying off in the past and critical account/other credits existing.
- `purpose` : factor variable indicating the credit's purpose, with levels car (new), car (used), furniture/equipment, radio/television, domestic appliances, repairs, education, retraining, business and others.
- `amount` : credit amount.
- `savings` : factor. savings account/bonds, with levels ... < 100 DM, 100 <= ... < 500 DM, 500 <= ... < 1000 DM, ... >= 1000 DM and unknown/no savings account.
- `employment_duration` : ordered factor indicating the duration of the current employment, with levels unemployed, ... < 1 year, 1 <= ... < 4 years, 4 <= ... < 7 years and ... >= 7 years.
- `installment_rate` : installment rate in percentage of disposable income.
- `personal_status_sex` :factor variable indicating personal status and sex, with levels male:divorced/separated, female:divorced/separated/married, male:single, male:married/widowed and female:single.
- `other_debtors` : factor. Other debtors, with levels none, co-applicant and guarantor.
- `present_residence` : present residence since?
- `property` : factor variable indicating the client's highest valued property, with levels real estate, building society savings agreement/life insurance, car or other and unknown/no property.
- `age` : client's age.
- `other_installment_plans` : factor variable indicating other installment plans, with levels bank, stores and none.
- `housing` : factor variable indicating housing, with levels rent, own and for free.
- `number_credits` : number of existing credits at this bank.
- `job` : factor indicating employment status, with levels unemployed/unskilled - non-resident, unskilled - resident, skilled employee/official and management/self-employed/highly qualified employee/officer.
- `people_liable` : Number of people being liable to provide maintenance.
- `telephone` : binary variable indicating if the customer has a registered telephone number.
- `foreign_worker` : binary variable indicating if the customer is a foreign worker.
- `credit_risk` : binary variable indicating credit risk, with levels good and bad.

#2.Import data and packages


In order to be able to complete the algorithms which are required to run the process in my paper, you maybe need to install the following packages.The GermanCredit data which I will analysis   has been stored in `klaR` package and the data have been cleaned before,so we can use it directly.The variable that we are going to study is `credit_risk`  and there are 70% of 1000 is "good" ,30% of 1000 is "bad".



```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(gmodels)
library(MASS)
library(klaR)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
data('GermanCredit',package = 'klaR')
names(GermanCredit)
# response variables
table(GermanCredit$credit_risk)
pie(table(GermanCredit$credit_risk),
    main='Pie Charts of credit risk')
```


#3.EDA and Hypotheses Test


```{r}
summary(GermanCredit)
```

There are too many variables can be analyzed. But with limiting space, I would analysis  a few key variables,which effected credit risk more likely I think.


## 3.1credit amount VS credit_risk


Is there a relationship between the risk of the credit and the amount of the credit? Is the higher the credit amount , the higher the risk of credit? From my personal experience, these conclusions may  be correct.But are they really true in this data set?



### 3.1.1 Graphic analysis

According to the boxplot graphical analysis, the distribution of bad credit risk is higher than the good credit risk. That is to say，bad credit risk may have a higher loan amount than good credit risk.

From the histogram graphics, the distribution of high loan amount in bad credit risk are much more than good credit risk. And the conclusion was the same as which was drawn from the boxplot.

So the  conclusion that  the higher the credit amount is the higher the risk of credit may be correct.



```{r}
ggplot(data=GermanCredit,aes(x=credit_risk,y=amount,fill=credit_risk))+
      geom_boxplot()+labs(title='boxplot of credit amount VS credit_risk')

ggplot(data=GermanCredit,aes(x=amount,fill=credit_risk,col=credit_risk))+
      geom_freqpoly(bins =30,aes(y=..density..),col=I('black'))+
      facet_wrap(~credit_risk,ncol=1)+
      geom_histogram(alpha=0.5,bins =30,aes(y=..density..))+
      labs(title='histogramof credit amount VS credit_risk')

```


### 3.1.2 Two Sample t-test 


Next,I will use Two Sample t-test to verify my guess about credit amount VS credit risk.

- null hypothesis :the mean of bad credit risk and  good credit risk is the same.
- alternative hypothesis : the mean of bad credit risk and  good credit risk is differrnt.

the t test tell us $t_stat=-4.2642$,$df = 421.86$ and $p-value = 2.478e-05 <0.05$, which indicated we should reject null hypothesis and the mean of bad credit risk and  good credit risk is differrnt. From mean plot ,we can know the mean credit amount   of  bad credit risk is more than  good credit risk.



```{r}
# Two Sample t-tes
with(GermanCredit,t.test(amount~credit_risk))
plotmeans(amount~credit_risk,data=GermanCredit,
    main='mean credit amount of \n diffrnrnt credit_risk')
```


## 3.2  employment duration VS credit risk



Is there a relationship between the risk of the credit and the employment duration? Is the less employment duration  , the higher the risk of credit? From my personal experience, these conclusions may  be correct.But are they really true in this data set?


### 3.2.1  Graphic analysis

From barplot, the people which employment duration < 1 year  appear to have more high bad credit risk.


```{r}
ggplot(data=GermanCredit,aes(x=employment_duration,fill=employment_duration))+
      geom_bar(show.legend = F)+labs(title='employment_duration')

ggplot(data=GermanCredit,aes(x=employment_duration,fill=credit_risk))+
      geom_bar(show.legend = T,position='fill')+labs(title='employment_duration')

```


### 3.2.2 Pearson's Chi-squared test 


Use Pearson's Chi-squared test  to verify my guess about employment durationt VS credit risk .
- null hypothesis : the credit risk of  people with  different employment durationt is the same.
- alternative hypothesis :  the credit risk of  people with  different employment durationt is different.

the $\chi^2=18.36827$,$df=4$ and $pvalue =0.0010 <0.05$,which indicated we should reject null hypothesis and the credit risk of  people with  different employment durationt is different. From "Mosaic Plots of  employment_duration VS credit_risk" plot,we can find that the people which employment duration < 1 year  appear to have more high bad credit risk.


```{r}
#Pearson's Chi-squared test 
with(GermanCredit,
     CrossTable(employment_duration,credit_risk,
     prop.c=F,prop.t=F,prop.chisq=F,expected=T,digits=3))
with(GermanCredit,mosaicplot(table(employment_duration,credit_risk),
     shade = T,main='Mosaic Plots of \n employment_duration VS credit_risk '))
```

# 4. Model Analysis 

Next, I will fit  some models to predict the risk credit, the steps of my analysis is these:

- Randomly  divide The data  into two parts, and 90% is train data and 10% is test data.
- use Logistic Regression,Recursive Partitioning and Regression Trees and Random Forest.
- Calculation model  train data accuracy  ,test data accuracy  and ROC of model.
- choose the best model
- finally draw my conclusion.


## 4.1 data preparation


Randomly  divide The data  into two parts, and 90% is train data and 10% is test data.

-----


data|sample size|bad credit size|good credit size
----|-----------|---------------|----------------
data_train|880|259|621
data_test|120|41|79


-----

```{r}
set.seed(1234)
ind <- sample(x=1:2,size = nrow(GermanCredit),replace = T,prob = c(0.9,0.1))
table(ind)
data_train <- GermanCredit[ind==1,]
data_test <- GermanCredit[ind==2,]
```


## 4.2Generalized Linear Models--Logistic Regression


In statistics, logistic regression, or logit regression  is a regression model where the dependent variable  is categorical.Logistic regression was developed by statistician David Cox in 1958. The binary logistic model is used to estimate the probability of a binary response based on one or more predictor variables .

Logistic regression measures the relationship between the categorical dependent variable and one or more independent variables by estimating probabilities using a logistic function, which is the cumulative logistic distribution. Thus, it treats the same set of problems as probit regression using similar techniques, with the latter using a cumulative normal distribution curve instead. Equivalently, in the latent variable interpretations of these two methods, the first assumes a standard logistic distribution of errors and the second a standard normal distribution of errors.


### 4.2.1 Fit model

Choose  the best model by AIC in a Stepwise Algorithm and finaly I get my bet mdoel $lr2$, and the train data accuracy = $\frac{135+557}{880}=78.63%$

```{r}
lr_data_train <- data_train
lr_data_test <- data_test
lr_data_train$credit_risk <- ifelse(lr_data_train$credit_risk=='good',1,0)
lr_data_test$credit_risk <- ifelse(lr_data_test$credit_risk=='good',1,0)
lr1 <- glm(credit_risk~.,data=lr_data_train,family = binomial())
lr2 <- stepAIC(lr1,direction = 'both')
pred_lr_train <- ifelse(lr2$fitted.values>0.5,'good','bad')
table(pred_lr_train,data_train$credit_risk)
```


###4.2.2 predict

Use test model $lr2$ to predict test data and the test data accuracy = $\frac{23+71}{120}=78.33%$.


```{r}
pred_lr_test <- predict(lr2,newdata = lr_data_test)
pred_lr_test <- ifelse(pred_lr_test>0.5,'good','bad')
table(pred_lr_test,data_test$credit_risk)
```


###4.2.3 OC curves


the roc of Logistic Regression Molde $lr2$ is good.

```{r}
pred_lr_test <- prediction(ifelse(pred_lr_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(pred_lr_test,'tpr','fpr'),colorize=T,main='ROC curves of Logistic Regression')
abline(0,1,lty=3)
abline(h=0.8,lty=3,col='blue')
```


## 4.3 Recursive Partitioning and Regression Trees

Recursive partitioning is a statistical method for multivariable analysis. Recursive partitioning creates a decision tree that strives to correctly classify members of the population by splitting it into sub-populations based on several dichotomous independent variables. The process is termed recursive because each sub-population may in turn be split an indefinite number of times until the splitting process terminates after a particular stopping criterion is reached.Recursive partitioning methods have been developed since the 1980s. Well known methods of recursive partitioning include Ross Quinlan's ID3 algorithm and its successors, C4.5 and C5.0 and Classification and Regression Trees. Ensemble learning methods such as Random Forests help to overcome a common criticism of these methods - their vulnerability to overfitting of the data - by employing different algorithms and combining their output in some way.


### 4.3.1 Fit model 


- use cp=0.001 get the first model and compute the xerror.
- choose the $cp=0.012$ which has  the smallest $xerror=0.8918919$
- get best tree $ct2$
- compue the train data accuracy = $\frac{571+118}{880}=78.30%$

```{r}
set.seed(2000)
ct1 <- rpart(credit_risk~.,data=data_train,control = rpart.control(cp=0.001))
ct1$cptable
# choose the CP value,which is the smallest xerror corresponding to
ct2 <- prune.rpart(tree = ct1,cp = 0.012)
rpart.plot(ct2)
pred_train <- predict(object = ct2,newdata = data_train,type='class')
table(pred_train,data_train$credit_risk)
```


###4.3.2 predict


Use test model $ct2$ to predict test data and the test data accuracy = $\frac{67+18}{120}=70.83%$.



```{r}
ct_pred_test <- predict(object = ct2,newdata = data_test,type='class')
table(ct_pred_test,data_test$credit_risk)
```


###4.3.3 ROC curves



the roc of Recursive Partitioning and Regression Trees $ct2$ is good.But the roc of Logistic Regression  $lr2$ is better.



```{r}
ct_pred_test <- prediction(ifelse(ct_pred_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(ct_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of ct2 model')
abline(0,1,lty=3)
```


##4.4 Random Forest

Random forests or random decision forests  are an ensemble learning method for classification, regression and other tasks, that operate by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees. Random decision forests correct for decision trees' habit of overfitting to their training set.

### 4.4.1 Fit model 

- build the Random Forest with $ntree=1000$,$mtry=16$.
- plot the error of Random Forest and find that 400 trees is enough.
- compue the train data accuracy = $\frac{677}{880}=76.93%$


```{r}
set.seed(6666)
rf <- randomForest(credit_risk~.,data=data_train,ntree=1000,mtry=16)
table(rf$predicted,data_train$credit_risk)

plot(rf)
varImpPlot(rf)
```


###4.4.2 predict

Use test model $ct2$ to predict test data and the test data accuracy = $\frac{73+16}{120}=74.17%$.

```{r}
rf_pred_test <- predict(object = rf,newdata = data_test,type='class')
table(rf_pred_test,data_test$credit_risk)
```



###4.4.3 ROC curves

the roc of Recursive Partitioning and Regression Trees $rf$ is good.But the roc of Logistic Regression  $lr2$ is better.

```{r}
rf_pred_test <- prediction(ifelse(rf_pred_test=='good',1,0),ifelse(data_test$credit_risk=='good',1,0))
plot(performance(rf_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of Random Forest')
abline(0,1,lty=3)
```


## 4.5 general comparison and best model 

Logistic Regression is best.

-----

model|train data accuracy|test data accuracy
-----|-------------------|------------------
Logistic Regression|78.63%|78.33%
Recursive Partitioning and Regression Trees|78.30%|70.83%
Random Forest|76.93% |74.17%

-----


```{r}
par(mfrow=c(2,2))
plot(performance(pred_lr_test,'tpr','fpr'),colorize=T,main='ROC curves of Logistic Regression')
abline(0,1,lty=3)
abline(v=0.45,lty=3,col='blue')
plot(performance(ct_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of ct2 model')
abline(0,1,lty=3)
abline(v=0.55,lty=3,col='blue')
plot(performance(rf_pred_test,'tpr','fpr'),colorize=T,main='ROC curves of Random Forest')
abline(0,1,lty=3)
abline(v=0.6,lty=3,col='blue')
par(mfrow=c(1,1))

```



#5.discuss


- The size of data is relatively small,which is  only one thousand samples and  for the random forest algorithm, the amount of data is too low.
- My data is relatively small and  conclusion may be unilateral.
- Other accidental factors may affect the accuracy of the data, such as random forest the tree is random in each time .


#6.conclusion

The result indicated that the prediction correct rate to 78%, forecast the credit risk is relatively difficult. And in the relaxed situation, the correct rate maybe acceptable, we still need to optimize the algorithms to improve the performance of our model.


#7.reference

- Petr P. Data Mining /[J]. Tools and Techniques”, BT Technol. J, 1994, 29(S1):47.
- Costa H D L. A FIRST PRINTING[J]. Philippine Studies, 1955, 3(2):214-216.
- Mitchell T M, Carbonell J G, Michalski R S. Machine learning.[M]// Machine Learning. Springer US, 1986:417-433.
- Wickham H. ggplot2: Elegant Graphics for Data Analysis[M]. Springer Publishing Company, Incorporated, 2009.
- Ginestet C. ggplot2: Elegant Graphics for Data Analysis, by H. Wickham[J]. Journal of the Royal Statistical Society, 2011, 174(1):245–246.
- Therneau T M, Atkinson E J. An Introduction to Recursive Partitioning Using the RPART Routine Technical Report 61[J]. Rochester Mayo Foundation, 1997.
- Therneau T, Atkinson B, Ripley B. rpart: Recursive Partitioning and Regression Trees[J]. 2015.
- Bannerman-Thompson H, Rao M B, Kasala S. Chapter 5–Bagging, Boosting, and Random Forests Using R[J]. Handbook of Statistics, 2013, 31:101-149.









