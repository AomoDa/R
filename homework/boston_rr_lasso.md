---
title: "Exploring Boston"
author: "Meiyi Wang"
date: "February 18, 2017"
output:
  word_document:
    toc: yes
  pdf_document:
    keep_tex: yes
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
```

##Boston Housing Data

```{r GetBostonDataset, echo=FALSE}
data("Boston", package="MASS")
```

We use the `Boston` data in the MASS package.This data was collected by urban planners to generate quantitative estimates of the cost-benefit for improving air quality.
The response variable of interest 'medv' is the median value
of owner-occupied homes.The Boston data.frame has dimensions `r dim(Boston)`.The variables are described in Table 1.

```{r Table1-BostonVars, results="asis", echo=FALSE}
dfv <- matrix(c("crim",
"per capita crime rate by town",
"zn",
"proportion of residential land zoned for lots over 25,000 sq.ft",
"indus",
"proportion of non-retail business acres per town",
"chas",
"Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)",
"nox",
"nitrogen oxides concentration (parts per 10 million)",
"rm",
"average number of rooms per dwelling",
"age",
"proportion of owner-occupied units built prior to 1940",
"dis",
"weighted mean of distances to five Boston employment centres",
"rad",
"index of accessibility to radial highways",
"tax",
"full-value property-tax rate per $10,000",
"ptratio",
"pupil-teacher ratio by town",
"black",
"1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town",
"lstat",
"lower status of the population (percent)",
"medv",
"median value of owner-occupied homes in $1000s"), ncol=2, byrow=TRUE)
vars <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad",
          "tax", "ptratio", "black", "lstat", "medv")
dfv <- as.data.frame.matrix(dfv)
row.names(dfv) <- dfv[,1]
#describeVars <- dfv[dfv[,1]%in%(vars[-10]),]
describeVars <- dfv
row.names(describeVars) <- paste0(1:nrow(describeVars), "...")
names(describeVars) <- c("Variable", "Description")
kable(describeVars, format="html", row.names=TRUE, 
      col.names=c("Variable..", "...Description"),
      caption="Table 1. Variables in the Boston Dataset.")
```

##Ridge Regression


### fit model  with train data set

Seventy percent of the data were randomly selected from the whole data set as training samples, and the remaining twenty percent were used as test samples.Finally,there are **358** samples in train data set and **148** samples in test data set.


```{r, message=FALSE, warning=FALSE, include=FALSE}

library(glmnet)
library(MASS)
data("Boston")
# train and test data set
set.seed(2017)
ind <- sample(x = 2,
              size = nrow(Boston),
              replace = T,
              prob = c(0.7,0.3))
# select train data
train_dat <- Boston[ind==1,]
# select test data
test_dat <- Boston[ind==2,]
# fit 
rr1 <- glmnet(x = as.matrix.data.frame(train_dat[,-14]),
              y = train_dat[,14],
              alpha = 0)
```


###Regularization path plot


In the above regularization plot, the horizontal axis label “L1 Norm” indicates that the coefficients $\hat \beta_{\lambda,i},i=1,...13$ are plotted against $||\hat \beta_\lambda||$,where $\hat \beta_\lambda=\left(\hat \beta_{\lambda,1},\hat \beta_{\lambda,2},....\hat \beta_{\lambda,13} \right)$ is the L1 norm.

```{r, echo=FALSE}
##Regularization path plot
plot(rr1,label=T,main="Figure 1.Regularization Path Plot")
```

###traditional ridge trace plot

Note that log-scaling on the Lambda axis magnifies the variation when λ is close to zero (corresponding
to OLS). If we take log λ = 0 then $\lambda=e=2.7$.In my model when $\lambda=e=2.7$ corresponds to the $85^{th}$ value and hence the corresponding is  $\hat \beta_\lambda=2.6924567$.


```{r Table2-Beta, results="asis", echo=FALSE}
kable(round(as.data.frame(t(rr1$beta[,85])),4),
      format="html",
      row.names=F, 
      col.names=names(rr1$beta[,85]),
      caption="Table 2. Beat in the Ridge Regression")
```


```{r, echo=FALSE, message=FALSE, warning=FALSE}
#traditional ridge trace plot
plot(rr1, 
     xvar="lambda", label=TRUE,
     main='Figure 2.Traditional Ridge Trace Plot')
# rr1$lambda
```

###Cross-validation for glmnet

Regularized 10-fold cross-validation is implemented.

The left-most vertical dotted line shows the minimum value of CV, in this case MSE is the default since "gaussian" family is assumed. The next dotted line shows where the value of $\lambda$ is that corresponds to Breiman’s one-standard deviation rule. From the figure 3, I would guess $log(\lambda)=1.3$ so $\lambda \approx exp(1.3)=3.67$.Finally I get the actual value  is $\lambda =3.56$ which is available in the output of my model.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(1333)
rr1_cv <- cv.glmnet(x = as.matrix.data.frame(train_dat[,-14]),
                    y = train_dat[,14],alpha = 0)
plot(rr1_cv,
     main='Figure 3.10-CV of Ridge Regression Plot')
```

###Making predictions

We assume the tuning parameter is set, $\lambda =3.56$. The
RMSE on the test data is about 5.761.

```{r, message=FALSE, warning=FALSE, include=FALSE}
rr1_pred <- as.vector(predict(rr1_cv, newx=as.matrix.data.frame(test_dat[,-14])))
err <-rr1_pred-test_dat[,14] 
rmse <- sqrt(mean(err^2))
rmse
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
plot(err, 
     xlab="Test data, observation number.",
     ylab="Prediction error", 
     main='Figure 4.Error Plot in test data set with Ridge Regression',
     pch=16, 
     col="blue")
obsno <- 1:length(err)
rr1LO <- loess(err ~ obsno, span=0.8)
lines(obsno, fitted(rr1LO), lwd=2, col='red')
```

##LASSO Regression

### fit model

I guess ,the best $\lambda \approx exp(-1)=0.3679$.

Finally I get the actual value  is $\lambda =0.3728$ which is available in the output of my lasso regression model.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(100)
lar_cv <- cv.glmnet(x = as.matrix.data.frame(train_dat[,-14]),
                    y = train_dat[,14],
                    alpha = 1)
```

```{r Table3-Beta, results="asis", echo=FALSE}
kable(as.matrix(coef(lar_cv$glmnet.fit, s = lar_cv$lambda.1se)),
      format="html",
      row.names=T, 
      col.names='Coefficients',
      caption="Table 3.  coefficients in the LASSO Regression")
```


```{r, echo=FALSE, message=FALSE, warning=FALSE}
plot(lar_cv,
     main='Figure 5. 10-CV of LASSO Regression Plot')
```



###Making predictions

We assume the tuning parameter is set, $\lambda =0.4929$. The RMSE on the test data is about 7.218,which is larger than RR.

```{r, message=FALSE, warning=FALSE, include=FALSE}
lar_pred <- as.vector(predict(lar_cv$glmnet.fit, newx=as.matrix.data.frame(test_dat[,-14]), s=rr1_cv$lambda.1se))
err <-lar_pred-test_dat[,14] 
rmse <- sqrt(mean(err^2))
rmse
```




```{r, echo=FALSE}
plot(err, 
     xlab="Test data, observation number.",
     ylab="Prediction error", 
     pch=16, 
     col="orange",
     main='Figure 6.Error Plot in test data set \n  with LASSO Regression '
     )
obsno <- 1:length(err)
rr1LO <- loess(err ~ obsno, span=0.8)
lines(obsno, fitted(rr1LO), lwd=2, col='red')
```

