---
title: "Untitled"
author: "Your Name"
date: "2017年2月26日"
output: 
  html_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#ABSTRACT

The task of crime prevention is constrained by police resources. For the safety of a city, if the police is aware of what kinds of crimes are mainly going on, and what their distribution is over the city, they can identify where to target police resources and help alleviate crimes. In this paper, given time and location, we predicted the category of crimes that occurred from 2003 to 2015 in San Francisco's neighborhoods based on a dataset derived from SFPD Crime Incident Reporting System. We investigated classification models including Decision Tree, Random Forest,SVM and analyze their pros and cons on this prediction task. Since this is a competition currently held by Kaggle, we also submitted our results to Kaggle and compared them with the public leaderboard, ranking nearly top 10% on our best performance in the end.


#INTRODUCTION

San Francisco first boomed in 1849 during the California Gold Rush, and in the next few decades, the city expanded rapidly both in terms of land area and population. The rapid population increase led to social problems and high crime rate fueled in part by the presence of red-light districts. However, the San Francisco of today is a far cry from its origins as a mining town. San Francisco has seen an influx of technology companies and their workers. While this has resulted in the city being acclaimed as a technological capital, the gentrification of its neighbourhoods have not been entirely well-accepted. It comes as no surprise that a tech-savvy city like San Francisco have decided to publicly release their crime data on their open data platform, and this data is part of an open competition on Kaggle to predict criminal occurrences in the city.

#Exploratory Data Analysis(EDA)


##Dataset  Describe

The San Francisco Crime Classification dataset contains the following set of features:

- Date – The date of the crime in the following format: YYYY-mm-dd hh:MM:ss. Thus, you can deduce the following: Year, Month, Day, Hour, Minute, Second.
- Category – The type of the crime. This is the target/label that we need to predict.
- District – Police district to which the crime is assigned
- Day of Week – The day of the week (i.e. Thursday)
- PdDistrict: name of the Police Department District
- Resolution – The resolution taken to address the crime.
- Address – The address of the crime incident.
- Longitude – X coordinates on the map where the crime occurred.
- Latitude – Y coordinates on the map where the crime occurred.

Some interesting findings emerge from  878049 records of crimes. In this dataset, there are 35 types of crimes in total dataset.But I will analysis TOP 3 crime category only.


-----

Category|Count|Rank|
--------|------|-----
LARCENY/THEFT     |          174900|1
OTHER OFFENSES  |            126182|2
NON-CRIMINAL  |               92304|3
ASSAULT        |              76876|4
DRUG/NARCOTIC |               53971|5
VEHICLE THEFT  |              53781|6
VANDALISM    |                44725|7
WARRANTS    |                 42214|8
BURGLARY    |                 36755|9
SUSPICIOUS OCC |              31414|10
MISSING PERSON  |             25989|11
ROBBERY   |                   23000|12
FRAUD     |                   16679|13
FORGERY/COUNTERFEITING  |     10609|14
SECONDARY CODES |              9985|15
WEAPON LAWS |                  8555|16
PROSTITUTION|                  7484|17
TRESPASS |                     7326|18
STOLEN PROPERTY  |             4540|19
SEX OFFENSES FORCIBLE   |      4388|20
DISORDERLY CONDUCT  |          4320|21
DRUNKENNESS  |                 4280|22
RECOVERED VEHICLE   |          3138|23
KIDNAPPING |                   2341|24
DRIVING UNDER THE INFLUENCE |  2268|25
RUNAWAY  |                     1946|26
LIQUOR LAWS     |              1903|27
ARSON  |                       1513|28
LOITERING  |                   1225|29
EMBEZZLEMENT  |                1166|30
SUICIDE  |                      508|31
FAMILY OFFENSES      |          491|32
BAD CHECKS |                    406|33
BRIBERY   |                     289|34
EXTORTION   |                   256|35
SEX OFFENSES NON FORCIBLE|      148|36
GAMBLING  |                     146|37
PORNOGRAPHY/OBSCENE MAT  |       22|38
TREA     |                       6|39
-----




```{r, message=FALSE, warning=FALSE, include=FALSE}
library(ggplot2)
library(plyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(kernlab)
sfc <- read.csv(file = 'C://Users//AomoDa//Documents//train.csv',
                header = T,
                stringsAsFactors = F)
#----------------------------------------------------------
#EDA
#----------------------------------------------------------
# Category count and select top 3
cat_cnt <- as.matrix(sort(table(sfc$Category),decreasing = T))
sfc <- subset(x = sfc,subset = Category %in%c('LARCENY/THEFT','OTHER OFFENSES','NON-CRIMINAL'))
sfc <- na.omit(sfc)
# sfc_test <- read.csv(file = 'test.csv',header = T,stringsAsFactors = F)
# date 
sfc$hour <- as.numeric(substr(sfc$Dates,11,13))
sfc$year <- as.numeric(substr(sfc$Dates,1,4))
sfc$mon <- as.numeric(substr(sfc$Dates,6,7))
sfc$mon_dt <- as.Date(paste0(substr(sfc$Dates,1,7),'-01'))
sfc$tmp <- 1
sfc$id=1:nrow(sfc)
```

##Time Analysis

### Trend
```{r, echo=FALSE, message=FALSE, warning=FALSE}
# date count 
sfc_month_cnt <- count(sfc$mon_dt)

# times 
ggplot(data=sfc_month_cnt[-149,],aes(x=x,y=freq))+
    geom_smooth(se = F,method = 'loess')+
    geom_point(col=I('orange'))+
    geom_path(col=I('orange'))+
    labs(title='Fig 1.San Francisco Crime Month Plot')
```


###Distribution

```{r, echo=FALSE}
ggplot(data = sfc,aes(x=as.factor(mon),fill=Category))+
  geom_bar(col=I('white'))+
  labs(title='Fig 2.San Francisco TOP 3 Crime Month Distribution')
```


```{r, echo=FALSE}
ggplot(data = sfc,aes(x=DayOfWeek,fill=Category))+
  geom_bar(col=I('white'))+
  labs(title='Fig 3.San Francisco TOP 3 Crime Week Distribution ')
```



```{r, echo=FALSE}
ggplot(data = sfc,aes(x=as.factor(hour),fill=Category))+
  geom_bar(col=I('white'))+
  labs(title='Fig 4.San Francisco TOP 3 Crime Hour Distribution  ')
```

###PdDistrict

```{r, echo=FALSE}
ggplot(data=sfc,aes(x=PdDistrict,fill=Category))+
  geom_bar(col=I('white'))+
  labs(title='Fig 5.San Francisco TOP 3 Crime VS PdDistrict ')
```



# LogLoss

Our task is to predict the category of the crimes. We will evaluate the performance of our models on Kaggle which measures the prediction error by multi-class logarithmic loss. Each case is labeled with one true category. For each record, we calculated a set of predicted probabilities (one for every class). The formula is then 
$$logloss=-\frac{1}{N}\sum^N_{i=1}\sum^M_{j=1}y_{ij}log(p_{ij})$$
where $N$ is the number of incidents in the test set, $M$ is the number of class labels, log is the natural logarithm, $y_{ij}$ is 1 if observation $i$ is in class $j$ and 0 otherwise, and $p_{ij}$ is the predicted probability that observation $i$ belongs to class $j$.


#Recursive Partitioning Trees

Use 5 folds cross-validated and finally I find the final value used for my recursive partitioning tree model was $cp = 0.01325479$.

Resampling results across tuning parameters and Accuracy was used to select the optimal model using  the largest value.

-----

cp|Accuracy|Kappa
-------|--------------|----------
0.01325479 | 0.6680596 | 0.4544892
0.06848306 | 0.6414319|  0.4081609
0.34167894 |0.5049628 | 0.1413909

-----

```{r, include=FALSE}
MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

# train and test data
set.seed(100)
a <- sample(x = 1:nrow(sfc),size = 3000,replace = F)
sfc <- sfc[a,]
set.seed(2017)
ind <- sample(x = 2,size = nrow(sfc),replace = T,prob = c(0.8,0.2))
sfc_train <- sfc[ind==1,]
sfc_test <- sfc[ind==2,]
#------------------------------------------------------------------
# rpart
#------------------------------------------------------------------
# train control
set.seed(100)
ctrl <- trainControl(method = "cv",number=5)
tree1 <- train(Category~DayOfWeek+PdDistrict+Resolution+mon+hour,data=sfc_train,method='rpart',trControl=ctrl,metric='Accuracy')
tree1
# train model
tree2 <- rpart(Category~DayOfWeek+PdDistrict+Resolution+mon,data=sfc_train,control = rpart.control(cp = 0.009502924))
rpart.plot(tree2,type=1)
```

Fig 6.Parameter Selection  In Recursive Partitioning Trees With 5-CV 

```{r, echo=FALSE}
plot(tree1,main='Fig 6.Parameter Selection \n In Recursive Partitioning Trees With 5-CV')
```


Fig 7.Variable Importance In Recursive Partitioning Trees tell me that `Resolution` ,`PdDistrict` and `hour` are very important.


-----

Variable|Overall
--------|--------------------------------------------
ResolutionNONE  |                                   100.000
ResolutionARREST, CITED    |                         77.539
ResolutionPSYCHOPATHIC CASE   |                      71.276
ResolutionUNFOUNDED   |                              10.769
PdDistrictCENTRAL |                                   4.359
PdDistrictSOUTHERN  |                                 1.716
hour   |                                              1.498

-----


```{r, echo=FALSE}
plot(varImp(tree1),main='Fig 7.Variable Importance \n In Recursive Partitioning Trees')
```


##Logloss

- Logloss in Train dataset is 44.14153 and in Test dataset is 45.03265.
- The model result in Test dataset is follow:

-----

Accuracy |Kappa | AccuracyLower |  AccuracyUpper |   AccuracyNull|  AccuracyPValue | McnemarPValue
--------|--------|-------------|------------------|--------------|-----------------|--------------
 0.6695  | 0.4588 |  0.6301    |     0.7071    |     0.4295    |  0|0

-----



```{r, message=FALSE, warning=FALSE, include=FALSE}
#------------------------------------------------------------------
## logloss in train data set 
a <- predict(object = tree2,newdata = sfc_train)
b<- with(sfc_train,tapply(tmp,INDEX = list(id,Category),FUN = min))
b[is.na(b)] <- 0
MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = tree2,newdata = sfc_train,type='class')
round(confusionMatrix(table(pred,sfc_train$Category))$overall,4)

#------------------------------------------------------------------
## logloss in train data set 

a <- predict(object = tree2,newdata = sfc_test)
b<- with(sfc_test,tapply(tmp,INDEX = list(id,Category),FUN = min))
b[is.na(b)] <- 0
MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = tree2,newdata = sfc_test,type='class')
round(confusionMatrix(table(pred,sfc_test$Category))$overall,4)
#------------------------------------------------------------------

```

#Random Forest

Use 5 folds cross-validated and finally I find the final value used for my recursive partitioning tree model are $ntree=500$ and  $ mtry = 2$.

Resampling results across tuning parameters and Accuracy was used to select the optimal model using  the largest value.

-----

mtry|Accuracy |Kappa  
----|----------|-------
2 |   0.6505772 | 0.4187976
16 |   0.6264555|  0.4108988
31  |  0.6181421 | 0.4013111

-----


```{r, message=FALSE, warning=FALSE, include=FALSE}
#------------------------------------------------------------------
#Random Forest
#------------------------------------------------------------------
set.seed(200)
rf_ctrl <- trainControl(method = "cv",number=5)
rf1 <- train(Category~DayOfWeek+PdDistrict+Resolution+mon+hour,data=sfc_train,method='rf',trControl=rf_ctrl,metric='Accuracy')

```

Fig 8.Parameter Selection  In Random Forest With 5-CV


```{r, echo=FALSE}
plot(rf1,main='Fig 8.Parameter Selection  In Random Forest With 5-CV')
```

Fig 9.Variable Importance  In Random Forest


-----

Variable  |Importance
----------|-------------
ResolutionNONE   |           100.0000
ResolutionARREST, CITED    |  52.9031
ResolutionPSYCHOPATHIC CASE | 32.5393
hour |                        10.1283
mon |                          6.4671
ResolutionUNFOUNDED |          5.1543
PdDistrictSOUTHERN  |          4.4740
PdDistrictCENTRAL  |           3.8544
PdDistrictINGLESIDE  |         2.2570
PdDistrictMISSION  |           2.0588
PdDistrictTENDERLOIN |         1.8362
PdDistrictRICHMOND |           1.7880
DayOfWeekTuesday   |           1.7421
DayOfWeekSunday |              1.4396
PdDistrictNORTHERN  |          1.4160
DayOfWeekWednesday  |          1.2885
DayOfWeekSaturday  |           1.2456
DayOfWeekThursday   |          1.0734
ResolutionLOCATED  |           0.9629
PdDistrictPARK   |             0.9556

-----



```{r, echo=FALSE}
plot(varImp(rf1),main='Fig 9.Variable Importance  In Random Forest')
```

##Logloss


- Logloss in Train dataset is 40.97059 and in Test dataset is 42.20641
- The model result in Test dataset is follow:

-----

Accuracy |Kappa | AccuracyLower |  AccuracyUpper |   AccuracyNull|  AccuracyPValue | McnemarPValue
--------|--------|-------------|------------------|--------------|-----------------|--------------
 0.6409  | 0.4051 |  0.6010    |     0.6795    |     0.4295    |  0|0

-----


```{r, include=FALSE}
#------------------------------------------------------------------
## logloss in train data set 
a <- predict(rf1,sfc_train,type='prob')
b<- with(sfc_train,tapply(tmp,INDEX = list(id,Category),FUN = min))
b[is.na(b)] <- 0
MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = rf1,newdata = sfc_train)
round(confusionMatrix(table(pred,sfc_train$Category))$overall,4)

#------------------------------------------------------------------
## logloss in train data set 

a <- predict(object = rf1,newdata = sfc_test,type='prob')
b<- with(sfc_test,tapply(tmp,INDEX = list(id,Category),FUN = min))
b[is.na(b)] <- 0
MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = rf1,newdata = sfc_test)
round(confusionMatrix(table(pred,sfc_test$Category))$overall,4)
#------------------------------------------------------------------

```


# SVM


Use 5 folds cross-validated and finally I find the final value used for my recursive partitioning tree model are $\sigma = 0.03026816$ and $C = 1$

Resampling results across tuning parameters and Accuracy was used to select the optimal model using  the largest value.

-----

C   |  Accuracy  | Kappa  
-----|------------|--------
0.25 | 0.5752898|  0.2941678
0.50 |0.6118951 | 0.3585064
1.00 | 0.6518354 | 0.4262510

-----


```{r, message=FALSE, warning=FALSE, include=FALSE}
set.seed(300)
svm_ctrl <- trainControl(method = "cv",number=5)
svm1 <- train(Category~DayOfWeek+PdDistrict+Resolution+mon+hour,data=sfc_train,method='svmRadial',trControl=svm_ctrl,metric='Accuracy',scale=F)
svm2 <-  ksvm(Category~DayOfWeek+PdDistrict+Resolution+mon+hour,data=sfc_train,kernel="rbfdot",kpar=list(sigma=0.03194619),C=1,cross=3,prob.model=T)
```

```{r, echo=FALSE}
plot(svm1,main='Fig 10.Parameter Selection  In SVM With 5-CV')
```


##Logloss


- Logloss in Train dataset is 44.13548 and in Test dataset is 45.10722
- The model result in Test dataset is follow:

-----

Accuracy |Kappa | AccuracyLower |  AccuracyUpper |   AccuracyNull|  AccuracyPValue | McnemarPValue
--------|--------|-------------|------------------|--------------|-----------------|--------------
 0.6527  | 0.4305 |  0.6129    |     0.6909    |     0.4295    |  0|0

-----


```{r, include=FALSE}
## logloss in train data set 
a <- predict(object = svm2,newdata = sfc_train,type='prob')
b<- with(sfc_train,tapply(tmp,INDEX = list(id,Category),FUN = min))
b[is.na(b)] <- 0
MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = svm2,newdata = sfc_train,type='response')
round(confusionMatrix(table(pred,sfc_train$Category))$overall,4)

#------------------------------------------------------------------
## logloss in train data set 

##a <- predict(object = svm2,newdata = sfc_test,type='prob')
##b<- with(sfc_test,tapply(tmp,INDEX = list(id,Category),FUN = min))
##b[is.na(b)] <- 0
##MultiLogLoss(act = a,pred = b)

# confusionMatrix
pred <- predict(object = svm1,newdata = sfc_test,type='raw')
round(confusionMatrix(table(pred,sfc_test$Category))$overall,4)
#------------------------------------------------------------------

```

#Conclusion



Accuracy 

-----

model| Min.|  1st Qu.| Median  | Mean| 3rd Qu. |  Max. |NA's
-----|-----|--------|-----------|-----|--------|-------|-----
rpart |0.6507|  0.6611 |0.6715 |0.6681 | 0.6736 |0.6833 |   0
rf   | 0.6258 | 0.6271 |0.6515 |0.6506 | 0.6625 |0.6861 |   0
svm  | 0.6208 | 0.6486 |0.6674 |0.6568 | 0.6701| 0.6771  |  0

----


Kappa 

-----

model| Min.|  1st Qu.| Median  | Mean| 3rd Qu. |  Max. |NA's
-----|-----|--------|-----------|-----|--------|-------|-----
rpart| 0.4207|  0.4465 |0.4624| 0.4545 | 0.4637| 0.4792 |   0
rf  |  0.3762 | 0.3766| 0.4228 |0.4188 | 0.4379| 0.4805  |  0
svm |  0.3730 | 0.4224| 0.4499 |0.4349 | 0.4588 |0.4702 |   0

-----


```{r}
re <- resamples(list(rpart=tree1,rf=rf1,svm=svm1))
bwplot(re,main='Fig 11.Models Compare')
```

- The accuracy  rate of the three models are only about sixty percent and the results of the models are not particularly desirable ,not particularly bad.
- The logloss of the three models are only about 45 .




# Reference

1. Kabacoff R I. R in Action[M]. Manning, 2011.
2. Lantz B. Machine Learning with R[M]. Packt Publishing, 2015.
3. Osuna E, Freund R, Girosi F. Training svm: An application to face detection[C]// 1997.
4. Liaw A, Wiener M. Classification and Regression by RandomForest[J]. R News, 2002, 23(23).
5. Therneau T, Atkinson B, Ripley B. rpart: Recursive Partitioning and Regression Trees[J]. 2015.
6. Kuhn M. caret: Classification and Regression Training[J]. Journal of Colloid & Interface Science, 1989, 129(1):291–295.
7. Wickham H. ggplot2: Elegant Graphics for Data Analysis[M]. Springer Publishing Company, Incorporated, 2016.
