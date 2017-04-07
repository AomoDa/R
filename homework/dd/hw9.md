---
title: "hw9"
author: "Your Name"
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q4

##a

- When $X_1>1$,then $Y=5$.
- When $X_1<1$ and $X_2>1$,then $Y=15$.
- When $X_1<0$ and $X_2<1$,then $Y=3$.
- When $0<X_1<1$ and $X_2<0$,then $Y=10$.
- When $0<X_1<1$ and $0<X_2<1$,then $Y=0$.

##b

```{r}
par(xpd = NA)
plot(x = NA,y= NA, 
     type = 'n', 
     xlim = c(-2, 2), 
     ylim = c(-3, 3), 
     xlab = 'X1', 
     ylab = 'X2')
lines(x = c(-2, 2), y = c(1, 1))
lines(x = c(1, 1), y = c(-3, 1))
text(x = (-2 + 1)/2, y = -1, labels = c(-1.8),col='red' )
text(x = 1.5, y = -1, labels = c(0.63),col='red' )
lines(x = c(-2, 2), y = c(2, 2))
text(x = 0, y = 2.5, labels = c(2.49),col='red' )
lines(x = c(0, 0), y = c(1, 2))
text(x = -1, y = 1.5, labels = c(-1.06),col='red' )
text(x = 1, y = 1.5, labels = c(0.21),col='red' )
```


#Q7

- It is reduced by adding more trees to the model and when  ntree is more than 100 the test error  is stationary trend.
- When $mtry=\sqrt p$,random Forest is best which has lowest test error.

```{r, message=FALSE, warning=FALSE}
library(randomForest)
library(MASS)
data('Boston')
set.seed(1)
#split data
train_ind <-  sample(x = nrow(Boston),size =nrow(Boston)*0.7 )
x.train = Boston[train_ind, -14]
x.test = Boston[-train_ind, -14]
y.train = Boston[train_ind, 14]
y.test = Boston[-train_ind, 14]

# mrty
m1 <- ncol(Boston)-1
m2 <- m1/2
m3 <- sqrt(m1)

# bulid random Forest
rf1 <- randomForest(x = x.train,
                    y = y.train,
                    xtest = x.test,
                    ytest = y.test,
                    mtry = m1,
                    ntree = 500)
rf2 <- randomForest(x = x.train,
                    y = y.train,
                    xtest = x.test,
                    ytest = y.test,
                    mtry = m2,
                    ntree = 500)
rf3 <- randomForest(x = x.train,
                    y = y.train,
                    xtest = x.test,
                    ytest = y.test,
                    mtry = m3,
                    ntree = 500)

plot(rf1$test$mse,
     type='l',
     col='red',
     lty=1,
     xlab='ntree',
     ylab='Test Error',
     ylim=c(10,20))
points(rf2$test$mse,
     type='l',
     col='blue',
     lty=2)
points(rf3$test$mse,
     type='l',
     col='orange',
     lty=2)
legend("topright", 
       c("mtry=p", "mtry=p/2", "mtry=sqrt(p)"), 
       col = c("red", "blue", "orange"),
       lty = c(1,2,2))

```

#Q10

##a

```{r, message=FALSE, warning=FALSE}
library(ISLR)
data("Hitters")
Hitters <- Hitters[!is.na(Hitters$Salary),]
Hitters$Salary = log(Hitters$Salary)
```

##b

```{r}
set.seed(100)
train.ind <- sample(x = nrow(Hitters),size = 200)
train_data <- Hitters[train.ind,]
test_data <- Hitters[-train.ind,]
```

##c


```{r, message=FALSE, warning=FALSE}
library(gbm)
lambdas <- seq(from=0.001,to = 0.5,length.out = 20)
rmse_train <- numeric(20)
rmse_test <- numeric(20)
set.seed(200)
for(i in 1:20){
  gbm1 <- gbm(Salary ~ .,
              data = train_data, 
              distribution = "gaussian", 
              n.trees = 1000, 
              shrinkage = lambdas[i])
  rmse_train[i] <- round(mean(gbm1$train.error^2),4)
  pred_test <- predict(gbm1,type='response',n.trees = 1000,newdata = test_data)
  rmse_test[i] <- round(mean((pred_test-test_data$Salary)^2),4)
}
```

##d

```{r}
plot(lambdas,rmse_test,
     type='b',
     xlab='lambda',
     ylab='Test Error',
     main='Test Error vs lambda ')
```

#Q12

The Wine data set from the UC Irvine Repository is already in `rattle` package .


- `rf1`(**randomForest**) with $ntree=500$ and $mtry=6$. Test set error rate is **6.12%**.
- `rf2`(**randomForest**) with $ntree=500$ and $mtry=4$. Test set error rate is **6.12%**.
- `bag1`(**bagging**) with $mfinal = 10$,$maxdepth=5$ and $minsplit=15$. Test set error rate is **16.33%**.
- `bag2`(**bagging**) with $mfinal = 10$,$maxdepth=3$ and $minsplit=10$. Test set error rate is **8.16%**.
- `gbm1`(**boosting**) with $ n.trees = 1000$,$ interaction.depth = 4$ and $shrinkage =0.1$ . Test set error rate is **4.08%**.
- `gbm2`(**boosting**) with $ n.trees = 1000$,$ interaction.depth = 4$ and $shrinkage =0.5$ . Test set error rate is **4.08%**.    
                
So boosting model is best.


```{r}
data(wine,package = 'rattle')
wine$Type <- as.factor(wine$Type)
str(wine)
set.seed(2017)
ind <- sample(x = 2,size = nrow(wine),replace = T,prob = c(0.7,0.3))
train_data <- wine[ind==1,]
test_data <- wine[ind==2,]
```

##randomForest

```{r}
library(randomForest)
set.seed(100)
m <- ncol(wine)-1
rf1 <- randomForest(x = train_data[,-1],
                    y = train_data[,1],
                    xtest =test_data[,-1],
                    ytest = test_data[,1] ,
                    ntree=500,
                    mtry=m/2)
rf2 <- randomForest(x = train_data[,-1],
                    y = train_data[,1],
                    xtest =test_data[,-1],
                    ytest = test_data[,1] ,
                    ntree=500,
                    mtry=sqrt(m))
rf1
rf2
```

##bagging

```{r, message=FALSE, warning=FALSE}
set.seed(100)
library(adabag)
library(caret)
bag1 <- bagging(Type~.,
                data = train_data,
                mfinal = 10,
                control=rpart.control(maxdepth=5, minsplit=15))
bag2 <- bagging(Type~.,
                data = train_data,
                mfinal = 10,
                control=rpart.control(maxdepth=3, minsplit=10))
pred1 <- predict.bagging(object = bag1,newdata = test_data)
pred2 <- predict.bagging(object = bag2,newdata = test_data)
#gbm1
confusionMatrix(table(pred1$class,test_data$Type))
#gbm2
confusionMatrix(table(pred2$class,test_data$Type))
```



##boosting


```{r, message=FALSE, warning=FALSE}
set.seed(100)
library(gbm)
library(caret)
gbm1 <- gbm(Type~.,
            data=train_data,
            n.trees = 1000,
            interaction.depth = 4,
            cv.folds = 5,
            shrinkage = 0.1)
gbm2 <- gbm(Type~.,
            data=train_data,
            n.trees = 1000,
            interaction.depth = 4,
            cv.folds = 5,
            shrinkage =0.5 )

pred1 <- round(predict.gbm(object = gbm1,
                           newdata = test_data,
                           n.trees = 1000,
                           type = 'response')[,,1])
pred2 <- round(predict.gbm(object = gbm2,
                           newdata = test_data,
                           n.trees = 1000,
                           type = 'response')[,,1])
pred1[,2] <- ifelse(pred2[,2]==1,2,0)
pred1[,3] <- ifelse(pred2[,3]==1,3,0)
pred2[,2] <- ifelse(pred2[,2]==1,2,0)
pred2[,3] <- ifelse(pred2[,3]==1,3,0)
pred1_class <- pred1[,1]+pred1[,2]+pred1[,3]
pred2_class <- pred2[,1]+pred2[,2]+pred2[,3]

#gbm1
confusionMatrix(table(pred1_class,test_data$Type))
#gbm2
confusionMatrix(table(pred2_class,test_data$Type))
```

