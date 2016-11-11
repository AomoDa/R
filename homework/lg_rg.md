---
title: "DSC5103 Assignment 3"
subtitle: 'Logistic Regression'
date: "Sep 2016"
output:
  html_document:
    highlight: tango
    theme: yeti
  pdf_document:
    highlight: zenburn
---
<!--
comments must be put in an HTML comment form
-->

```{r set-options, echo=FALSE, cache=FALSE}
options(width = 120)  # set output width
```

## NOTE:
This assignment is **due at 23:59 of Sep 15, Thursday**. You can work on this file directly and fill in your answers/code below. Please submit the output HTML file (name your file like G1Group02.html if you are from Group 02 of Section G1) onto IVLE/Files/Student Submission/Assignment3 folder.

Also, put the Section/Group and member info below.
```{r}
# Section G1
# Group 09
# Members: RUI YANG,RUI BAO,ZHEN YU,YULAI WEI
```

## Part I: Gender Discrimination in UC Berkeley Admissions
### Introduction
The *UCBAdmissions* dataset in R has aggregate data on applicants to graduate school at Berkeley for the six largest departments in 1973 classified by admission and sex. At issue is whether the data show evidence of sex bias in admission practices. There were 2691 male applicants, of whom 1198 (44.5%) were admitted, compared with 1835 female applicants of whom 557 (30.4%) were admitted. This gives a sample odds ratio of 1.83, indicating that males were almost twice as likely to be admitted.

Let's first convert the dataset into a dataframe.
```{r}
UCBAdmissions.df <- as.data.frame(UCBAdmissions)
head(UCBAdmissions.df)
```

We are going to use Logistic Regression to test the accusation.

### Questions

#### 1. Use the *reshape2* package to convert the dataset into proper shape with two separte columns showing the number of admitted and rejected applicants for each *Gender* and *Dept* combinations. (1 Mark)

Answer: 

```{r}
library("reshape2")
ad <- dcast(UCBAdmissions.df, Gender + Dept ~ Admit, value.var="Freq")
summary(ad)
ad
```

#### 2. Run Logistic Regression of *(admitted, rejected)* on predictor *Gender*. What is the probablity of a female being admitted? Briefly comment on whether there is sex bias based on the model output. (1 Mark)

Answer: 

```{r}
glm1 <- glm(cbind(Admitted, Rejected) ~ Gender, ad, family = binomial())
summary(glm1)
prob.female<-exp(glm1$coefficients[[1]]+glm1$coefficients[[2]])/(1+exp(glm1$coefficients[[1]]+glm1$coefficients[[2]]))
prob.female
#There is sex bias in admission.Compared with male,female has less chance to be admitted,nearly 54.3% lower.
```


#### 3.  Run Logistic Regression of *(admitted, rejected)* on predictor *Gender* and *Dept*. Briefly comment on whether there is sex bias based on the model output and the difference from the conclusion made by the previous model. (1 Mark)

Answer: 

```{r}
glm2 <- glm(cbind(Admitted, Rejected) ~ Gender+Dept, ad, family = binomial())
summary(glm2)
#There is no sex bias because there is not significant relationship between male and female,which means 
#The two models are just like the Simpson's Paradox.The previous model just cares about the gender difference,while in this model, Dept differences are also taken into consideration,in which case there is no sex bias.This can also be interpreted in real life situation,it seems that female are more likely to apply for the departments with lower admission rate.
```


#### 4.  Introduce interaction term between *Gender* and *Dept* into the previous model. Briefly interpret the model output. (1 Mark)

Answer: 

```{r}
glm3 <- glm(cbind(Admitted, Rejected) ~ Gender * Dept, ad, family = binomial())
summary(glm3)
#Female have lower admission rate than male only in Dept C and Dept E.When it comes to other Depts,female have higher chance to be admitted.Speaking of male,the highest admission rate is in Department B and the lowerest is in Department F,while for female,the highest admission rate is in Department A and the lowerest is also in Department F.
```


## Part II: . Logistic Regression on the mixture.example dataset
### Introduction
We have done k-Nearest Neighbour classification on the *mixture.example* dataset of the *ElemStatLearn* package. Here we want to do the same classification using Logistic Regression and compare their performance on the test dataset.

To save your time, below is copied from the previous *knn_demo.R* file with some minor modifications. You can simply continue from there.

```{r results='hide', message=FALSE, warning=FALSE}
library("ElemStatLearn")  # run install.packages("ElemStatLearn") if you haven't

# copy important ones out
x <- mixture.example$x
y <- mixture.example$y
prob <- mixture.example$prob
xnew <- mixture.example$xnew
px1 <- mixture.example$px1
px2 <- mixture.example$px2

# make dataframe for the training data (with x1, x2, and y)
df.training <- data.frame(x1=x[ , 1], x2=x[ , 2], y=y)
df.training$y <- as.factor(df.training$y)

# make dataframe for the "test" data (with xnew1, xnew2, and true prob, but not y!!)
df.grid <- data.frame(x1=xnew[ , 1], x2=xnew[ , 2])
df.grid$prob <- prob


# plot X and Y
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()

# add the true boundary into the plot
p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))
p.true
```

The above plot is the true boundary from the dataset.

### Questions
#### 1. Run Logistic Regression of *y* on *x1* and *x2* using the *df.training* dataset. (1 Mark)

Answer: 

```{r}
glm4 <- glm(y ~ x1+x2 , data=df.training, family=binomial(link="logit"))
summary(glm4)

```


#### 2. Predict the probability of *y* using *df.grid* as the newdata. Plot the decision boundary of model just like we did for the true decision boundary above. Interpret the boundary verbally. (1 Mark)

Answer: 

```{r}
df.grid$pred<-predict(glm4, newdata=df.grid, type="response")
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()

p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=pred), breaks=c(0.5))
p.true
# The boundary is a  straight line due to the fact that the glm4 is a first-order binomial regression and the boundary is largely infludenced by x2.
```


#### 3. Fit the Logistic Regression model with up to 6th-order polynomial of *x1* and *x2*. Repeat the prediction on *df.grid* and plot the decision boundary. (1 Mark)

Answer: 

```{r}
glm5 <- glm(y ~ x1+I(x1^2)+I(x1^3)+I(x1^4)+I(x1^5)+I(x1^6)+x2+I(x2^2)+I(x2^3)+I(x2^4)+I(x2^5)+I(x2^6), data=df.training, family=binomial(link="logit"))
summary(glm5)
df.grid$pred2<-predict(glm5, newdata=df.grid, type="response")
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()

p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=pred2), breaks=c(0.5))
p.true
```


Next, let's generate a test dataset and compare the performance of the two logistic regression models with kNN. Again, we can copy the code from *mixture_knn.R*.
```{r results='hide', message=FALSE, warning=FALSE}
library("mvtnorm")
set.seed(123)
centers <- c(sample(1:10, 5000, replace=TRUE), 
             sample(11:20, 5000, replace=TRUE))
means <- mixture.example$means
means <- means[centers, ]
x.test <- rmvnorm(10000, c(0, 0), 0.2 * diag(2))
x.test <- x.test + means
y.test <- c(rep(0, 5000), rep(1, 5000))
df.test <- data.frame(x1=x.test[, 1], x2=x.test[, 2], y=y.test)

# best possible misclassification rate
bayes.error <- sum(mixture.example$marginal * (prob * I(prob < 0.5) + (1-prob) * I(prob >= 0.5)))
```
Here *x.test* and *y.test* are the separate test data for the *knn()* function, whereas *df.test* is for *glm()*. They are the same data in different format. The *bayes.error* gives the best possible misclassification rate when the true model is known. We will use it as the limit.

The following code obtains probability prediction of kNN for k=1, 7, and 100 and save the probability predictions as three columns in the *df.test* dataframe.
```{r}
## predict with various knn models
library("FNN")
ks <- c(1, 7, 100)
for (i in seq(along=ks)) {
    mod.test  <- knn(x, x.test, y, k=ks[i], prob=TRUE)
    prob <- attr(mod.test, "prob")
    prob <- ifelse(mod.test == "1", prob, 1 - prob)
    df.test[, paste0("prob.knn", ks[i])] <- prob
}
head(df.test)
```

#### 4. Using *df.test* as new data, obtain the probability prediction of the two Logistic Regression models built earlier, and save them as two columns in *df.test*, too. (1 Mark)

Answer: 

```{r}
glm6 <- glm(y ~ x1+x2, data=df.test, family=binomial(link="logit"))
summary(glm6)
df.test$prob.lr1<-predict(glm6, newdata=df.test, type="response")

glm7 <- glm(y ~ x1+I(x1^2)+I(x1^3)+I(x1^4)+I(x1^5)+I(x1^6)+x2+I(x2^2)+I(x2^3)+I(x2^4)+I(x2^5)+I(x2^6), data=df.test, family=binomial(link="logit"))
summary(glm7)
df.test$prob.lr2<-predict(glm7, newdata=df.test, type="response")
head(df.test)

```


#### 5. Plot the misclassification rate of the 5 models against probability cutoff in one plot, and also plot *bayes.error* as the benchmark. (1 Mark)

?Answer: 

```{r}
library("ROCR")
lr1<- prediction(df.test$prob.lr1, df.test$y)
lr1.misclassification <- performance(lr1, measure="err")
str(lr1.misclassification)
lr2<- prediction(df.test$prob.lr2, df.test$y)
lr2.misclassification <- performance(lr2, measure="err")
str(lr2.misclassification)
knn1<- prediction(df.test$prob.knn1, df.test$y)
knn1.misclassification <- performance(knn1, measure="err")
str(knn1.misclassification)
knn7<- prediction(df.test$prob.knn7, df.test$y)
knn7.misclassification <- performance(knn7, measure="err")
str(knn7.misclassification)
knn100<- prediction(df.test$prob.knn100, df.test$y)
knn100.misclassification <- performance(knn100, measure="err")
str(knn100.misclassification)
plot(lr1.misclassification, col="red", ylab="",xlim=c(0,1),ylim=c(0.2,0.6))
plot(lr2.misclassification, col="blue", add=TRUE)
plot(knn1.misclassification, col="orange", add=TRUE)
plot(knn7.misclassification, col="green", add=TRUE)
plot(knn100.misclassification, col="purple", add=TRUE)
abline(h=bayes.error,col="black")
legend(x=0.4, y=0.6, legend=c("lr1", "lr2", "knn1","knn7","knn100","bayes error"), lty=c(1, 1, 1), lwd=c(2, 2, 2), col=c("red", "blue", "orange","green","purple","black"))


```


#### 6. Plot the ROC curve of all the 5 models in one plot, and compare the models. (1 Mark)

Answer: 

```{r}
lr1.ROC <- performance(lr1, measure="tpr", x.measure="fpr")
plot(lr1.ROC,col="red")
lr2.ROC <- performance(lr2, measure="tpr", x.measure="fpr")
plot(lr2.ROC, col="blue",add=TRUE)
knn1.ROC <- performance(knn1, measure="tpr", x.measure="fpr")
plot(knn1.ROC, col="orange",add=TRUE)
knn7.ROC <- performance(knn7, measure="tpr", x.measure="fpr")
plot(knn7.ROC, col="green",add=TRUE)
knn100.ROC <- performance(knn100, measure="tpr", x.measure="fpr")
plot(knn100.ROC, col="purple",add=TRUE)
legend(x=0.7, y=0.4, legend=c("lr1", "lr2", "knn1","knn7","knn100"), lty=c(1, 1, 1), lwd=c(2, 2, 2), col=c("red", "blue", "orange","green","purple"))

#The most acurate models in overall performance are lr2 and knn7.The best prediction is the green line,which is knn7.As we can see in the previous CUOTOFF graph,the knn7 has the lowest misclassification rate.


```

