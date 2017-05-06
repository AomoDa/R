---
title: "C"
author: "Your Name"
date: '2017-05-06'
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---


# Loading Data and Data Split


There are 3 steps  to to pre process data.

- Load Data into R as a data frame.
- Complete missing data using MICE method.
- 80% of the data were taken out as training data and the remaining 20% were test data.I will use the training data to build  KNN,Tree and SVM models, and the I will use the test sample to assess the performance of these models.Finally I will choose best models as my prediction model.

```{r, message=FALSE, warning=FALSE}
library(mice)
library(caret)
library(kernlab)
library(rpart)

x <- read.csv('C://Users//AomoDa//Documents//mydata.txt',
              sep = '\t',
              header = F,
              na.strings='?')
# Multivariate Imputation by Chained Equations (MICE)
newx <- complete(mice(x, meth='pmm',printFlag=F))
#factor
newx$V10 <- as.factor(newx$V10)

#-------------------------------
# data split
#-------------------------------
set.seed(2019)
ind <- sample(x = 1:2,size = nrow(newx),replace = T,prob = c(0.8,0.2))
train.data <- newx[ind==1,]
test.data <- newx[ind==2,]
```


# Feature Exploration


The feature plots tell us that all variables are closely related to the response variable.So I will use **all variables** to build KNN,Tree and SVM models.

```{r}
# density
featurePlot(x = newx[, -10], 
            y = newx[,10],
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            auto.key = list(columns = 2))
# boxplot
featurePlot(x = newx[, -10], y = newx[,10],plot = "boxplot")
```


# Molde Building

molde building steps:

- Use 5 times 5-fold cross validations to determe the optimal parameter model using training data set.
- Assess prediction accuracy rate using test data set.


## KNN

- 5 times 5-fold cross validations method is used to select best  model parameter.
- In KNN model , accuracy was used to select the optimal model parameter using  the largest accuracy rate, and the final parameter used for the model was k = 13. So I will use KNN models with k=13.
- The prediction accuracy rate is 0.9806 and 95%  confidence interval is [0.9316,0.9976].

```{r}
fit.control <- trainControl(method='repeatedcv',number=5,repeats=5)

set.seed(2017)
knn1 <- train(V10~.,data=train.data,method='knn',
	trControl=fit.control,
	tuneGrid=expand.grid(k=1:20),
	preProcess = c("center", "scale"), 
	metric = "Accuracy")

knn1

plot(knn1)

# pred 
knn.pred <- predict(knn1,newdata = test.data,type = 'raw')
# confusion matrix
confusionMatrix(table(knn.pred,test.data$V10))
```

## Tree 

- 5 times 5-fold cross validations method is used to select best  model parameter.
- In Tree model , accuracy was used to select the optimal model parameter using  the largest accuracy rate, and the final parameter used for the model was $cp=0$.
- The prediction accuracy rate is 0.9417 and 95%  confidence interval is [0.8775,0.9783],which is lower than KNN model.

```{r}
set.seed(2017)
tree1 <- train(V10~.,data=train.data,method='rpart',
	trControl=fit.control,
	tuneGrid=expand.grid(cp=seq(0,0.1,by = 0.005)),
	metric = "Accuracy")

tree1

plot(tree1)

# pred 
tree.pred <- predict(tree1,newdata = test.data,type = 'raw')
# confusion matrix
confusionMatrix(table(tree.pred,test.data$V10))
```

## SVM 

- 5 times 5-fold cross validations method is used to select best  model parameter.
- In SVM model , accuracy was used to select the optimal model parameter using  the largest accuracy rate, and the final parameter used for the model were  $\sigma = 0.8494$ and $C = 2$.
- The prediction accuracy rate is 0.9515 and 95%  confidence interval is [0.8903,0.9841],which is lower than KNN model.

```{r}
set.seed(2017)
svm1 <- train(V10~.,data=train.data,method='svmRadial',
	trControl=fit.control,
	tuneLength=10,
	metric = "Accuracy"
	)

svm1
plot(svm1)

# pred 
svm.pred <- predict(svm1,newdata = test.data,type = 'raw')
# confusion matrix
confusionMatrix(table(svm.pred,test.data$V10))

```


## Molde Performance 

- KNN : the prediction accuracy rate is 0.9806.And KNN is best.
- TREE : the prediction accuracy rate is 0.9417.
- SVM : the prediction accuracy rate is 0.9515.


# Conclusion

I will use KNN model with k=13  as my prediction model.

```{r}
myfunction<- function(Xtest){
 Xtest <- as.data.frame(Xtest)
 Ytest <- predict(knn1,newdata = Xtest,type = 'raw')
 return(Ytest)
}
```

