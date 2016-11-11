
---
title: "DSC5103 Test 2"
subtitle: 3 questions, 20 points in total
date: "Nov 2016"
output:
  html_document:
    highlight: tango
    theme: yeti
---
<!--
comments must be put in an HTML comment form
-->

```{r set-options, warning=FALSE, echo=FALSE, cache=FALSE}
options(width = 100)  # set output width
library("plyr")
library("ggplot2")
```

## Note
This is an *individual* test. You can refer to whatever sources of information or materials, online or offline, but do NOT communicate with any other people. You can work on this file directly and fill in your answers/code below. Please submit the output RMD and HTML file (name your file like e0012345.rmd and e0012345.html if your NUS ID is e0012345) onto IVLE/Files/Student Submission/Test2 folder. 

*There is no official time limit for this test. However, penalty will be given to late submission and is proportional to the time taken. The first 120 minutes will be penalty free, and after that, **one point** will be deducted for every extra **5-minute** chunk of time (e.g., if you take a total of 128 minutes, 2 points will be deducted.). Submission time is based on the last file you upload to the IVLE submission folder.*

Also, fill your info below. (**This is critical as your user id will be used in determining your random sample of the data!**)
```{r}
student.name <- "Wei Yulai"  # put your real name here
student.id <- 0056094  # put only the numeric digits of your NUS id here
```

## Disclaimer
In this test we will explore various prediction models and betting strategies, which can be potentially applied to real gambling. As an academic, I do this purely out of curiosity (and, to some extent, trying to be cool). Gambling may or may NOT be legal (I have no idea what the laws, terms, and conditions are). If you are to implement what we have in the test, it is your responsibility to make sure your conduct is legal. Also, I cannot be responsible for any potential economical loss. There is ABSOLUTELY NO WARRANTY on whether the ideas here will or will not work in practice.


## Overview
### Introduction
The file **football.rata** is a dataset about football matches in Season 2010/11 to Season 2015/16 of English Premier League. The data has been prepared in an RData format. You can load the file by doing the following. 
```{r}
load("C://Users//mali//Documents//football.rdata")  # load the data (make sure the .rdata file is in your working directory!)
```
After successfully loading the file, you should see a data frame called **matches**. 
```{r}
ls(all.names=T)  # list all the objects in the data file
dim(matches)  # show data dimensions
```

In the **matches** data frame, there are `r nrow(matches)` records. Each record is a particular football match, consisting of four groups of information: (i) basic match information (including date, time, team names, season, etc.), (ii) odds information at the beginning of the match, (iii) live match statistics recorded at the end of the first half, and (iv) final result of the matches. Below is the detailed description of the columns.

[**Basic match information**]

- **match.id**: the unique ID of the match;
- **date**: the date when the match was played, in the format of YYYY-MM-DD;
- **time**: the time of the day when the match started, in the format of HH:MM;
- **country**: the country of the match, here it is just *England*;
- **league**: the league that the match belongs to, here is it just *Premier League*;
- **season**: the season of the match (*2010* means the 2010/11 season);
- **hometeam**: the team that played at home;
- **awayteam**: the team that played as guest;


[**Odds information**] (obtained from [checkbestodds.com](http://checkbestodds.com)):

- **odds.home**: the odds of hometeam winning the match;
- **odds.draw**: the odds of having a draw;
- **odds.away**: the odds of awayteam winning the match;

*[Help]: If you bet $1 on home win, and **odds.home** is 2.26, it means that if the home team eventually wins the match, you will receive $2.26 (make $1.26 profit), but if the home team does not win, you will lose your $1.*


[**Live statistics**] at the end of the first half (45th minutes) of the match:

- **home.goals1**: the number of goals scored by the home team in the first half;
- **away.goals1**: the number of goals scored by the away team in the first half;
- **home.corners1**: the number of corner kicks awarded to the home team in the first half;
- **away.corners1**: the number of corner kicks awarded to the away team in the first half;
- **home.yc1**: the number of yellow cards given to the home team in the first half;
- **away.yc1**: the number of yellow cards given to the away team in the first half;
- **home.rc1**: the number of red cards given to the home team in the first half;
- **away.rc1**: the number of red cards given to the away team in the first half;
- **home.pen1**: the number of penalty kicks awarded to the home team in the first half;
- **away.pen1**: the number of penalty kicks awarded to the away team in the first half;


[**Final result**]

- **home.goals**: the number of goals scored by the home team in the whole match;
- **away.goals**: the number of goals scored by the away team in the whole match.


### Preparing data

We will randomly split the data into a training dataset (1800 rows) and a test dataset (272 rows), using your student ID as the seed.

```{r}
set.seed(student.id)
train.index <- sample(2072, 1800)
test.index <- - train.index
```



## Part 1: Predicting the total number of goals (6 points)

The total number of goals in a match (home.goals + away.goals) is a popular item that gamblers bet on with online bookmakers. In the following, we build prediction models for predicting the total number of goals **after observing the live statistics in the first half**.

#### a. Add to the *matches* data frame a new column *total.goals* that tracks the total number of goals in the matches. (1 point)



***ANSWER of Q1.a:***

```{r}
matches$total.goals <- matches$home.goals+matches$away.goals
```






#### b. Use the training data to build a LASSO model to predict *total.goals*. Feel free to select relevant variables. It is also ok to construct new variables or apply transformations on the given variables, but please provide brief justification/explanation. (2 points)



***ANSWER of Q1.b:***

```{r}
library(glmnet)
matches_train <- matches[train.index,]
matches_test <- matches[test.index,]
#train model
lasso_model <- glmnet(as.matrix(matches_train[,c(9:23)]),matches_train[,24],alpha=1)
```





#### c. According to your LASSO model, what are the coefficients with respect to variables *home.goals1* and *away.goals1*? What do they mean? (1 point)



***ANSWER of Q1.c:***

```{r}
summary(lasso_model)
lasso_model
```






#### d. Use your model to make prediciton on the test dataset, and evaluate the test error (MSE). (1 point)



***ANSWER of Q1.d:***

```{r}
pred <- predict(lasso_model,as.matrix(matches_test[,c(9:23)]) ,s = 0.01 )
mse_lasso <- sum( (as.numeric(pred)-matches_test$total.goals)^2)
mse_lasso
```






#### e. What is the equivalent R-squared of your prediction? (1 point)



***ANSWER of Q1.e:***

```{r}
R2 <- (var(matches_test$total.goals)-mse_lasso)/var(matches_test$total.goals)
R2
```







## Part 2: Predicting the match result (5 points)

In this part, we will predict the final result (home team wins, away team wins, or draw) **at the beginning of the match**.

First, let us add the "Y" variable to our dataset.
```{r}
matches$result <- as.factor(ifelse(matches$home.goals > matches$away.goals, "home", ifelse(matches$home.goals < matches$away.goals, "away", "draw")))  # assign home/draw/away
table(matches$result)  # counts of the results
```

#### a. Build a Random Forest model on the training data to predict *result*. Remember to tune the parameters *mtry*. (Hint/Note: 1. The prediction is to be made before the match starts. Make sure your model is viable.) (2 points)



***ANSWER of Q2.a:***

```{r}
library(randomForest)
#drop some  Basic match information
matches_train_rf <- matches[train.index,-c(1:6)]
matches_test_rf <- matches[test.index,-c(1:6)]
set.seed(student.id)
rf1 <- randomForest(result~.,data=matches_train_rf,ntree=500,mtry=5)

plot(rf1)
importance(rf1)
```






#### b. Examine the partial dependence of home win on the variable *odds.home* and briefly interpret the finding. (1 point)



***ANSWER of Q2.b:***

```{r}
partialPlot(rf1,matches_train_rf,result,"home")
```




#### c. Use your model to predict the most likely result of the matches in the test dataset. (1 point)



***ANSWER of Q2.c:***

```{r}
pred_rf <- predict(rf1,newdata = matches_test_rf)
table(pred_rf)
```





#### d. What is your classification error on the test data? (1 point)





***ANSWER of Q2.d:***

```{r}
table(matches_test_rf$result,pred_rf)
```








## Part 3: Predicting home win (9 points)
Predicting all three possible match results in Part 2 seems difficult. In practice (if we are just gambling), we do not have to do it. Now let's further simply the problem by only focusing on predicting/betting on whether the home team will win or not. We shall evaluate the prediction performance in terms of the potential profit if we bet with the given odds


#### a. Add to the *matches* data frame a binary variable *home.win* that tracks whether the home team wins the match. Make sure it is in the proper format for the following analysis using GBM. (1 point)



***ANSWER of Q3.a:***

```{r}
library(gbm)

matches$home.win <- ifelse(matches$odds.home>matches$odds.draw & matches$odds.home>matches$odds.away,1,0 )
#drop some  Basic match information
matches_train_gbm <- matches[train.index,-c(1:6,25)]
matches_test_gbm <- matches[test.index,-c(1:6,25)]

```






#### b. Build a GBM model on the training data to predict *home.win*. (Hint/Note: 1. Again, the prediction is to be made before the match starts. Make sure your model is viable. 2. You can fix the GBM parameters *interaction.depth=4* and *shrinkage=0.001* and only tune parameter *n.trees*. If you really have trouble tuning the parameter, choose some reasonable value and proceed.) (2 points)



***ANSWER of Q3.b:***

```{r}
set.seed(student.id)
model <- gbm(home.win~.,data=matches_train_gbm,shrinkage=0.001, 
      distribution='bernoulli', n.trees=3000,verbose=F,interaction.depth=4)

summary(model)
plot(model)
```





#### c. Use your GBM model to predict the probability of home win in the test dataset. (1 point)



***ANSWER of Q3.c:***

```{r}
pred_gbm <- predict(model,newdata = matches_test_gbm,n.trees = 3000,type='response')
pred_gbm <- ifelse(pred_gbm>0.5,1,0)
table(pred_gbm,matches_test_gbm$home.win)
```






#### d. To finalize the prediction, we need to set the cutoff probability. Apparently, cutoff at 0.5 is not very reasonable as the payoffs of right or wrong prediction are quite different. Since the median odds for home win is at 2.26, let's assume that a correct prediciton leads to a profit of $1.26 and a wrong one leads to a loss of $1. Find the optimal cutoff and use it to predict the outcome of *home.win* on the test dataset. (Hint: you may use the ROCR package here.) (2 points)



***ANSWER of Q3.d:***

```{r}
library(ROCR)
a <- prediction(matches_test_gbm$home.win,pred_gbm)
plot(performance(a,"rec","fpr") )
```







#### e. For the matches in the test data, if you bet $1 on home win only for those matches that are predicted to have a home win, how many matches will you bet on? What is your potential profit (according to the odds given in the data)? (1 point)




***ANSWER of Q3.e:***

```{r}

pred_gbm_num <- predict(model,newdata = matches_test_gbm,n.trees = 3000,type='response')

# win 57
table(pred_gbm)

p <- mean(pred_gbm_num)-1
p

```



#### f. Propose a direction to further improve the potential gambling profit (excluding further tuning the existing models). Implement it and evaluate the profit improvement if you have time. (2 points)









***[THE END]***
