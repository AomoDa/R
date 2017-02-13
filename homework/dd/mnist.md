---
title: "Logistic Regression Homework (2/10/17)"
output:
  pdf_document: default
---

```{r, echo = F}
load("C://Users//mali//Documents//mnist_data.RData")
```


## Image Classification 

In this problem, we'll tackle the task of image classification: labelling an image according to the content it represents. We'll be using a popular dataset of images of hand-written digits called the MNIST dataset. This dataset contains thousands of images of hand-written examples of single digits. For simplicity, we'll focus just on a subset of the images: the 0's and the 1's. 

_This dataset has been used as a test set for machine learning for a long time. For more details and the latest results, see e.g._
http://yann.lecun.com/exdb/mnist/
and
https://en.wikipedia.org/wiki/MNIST_database .


### Preparation

Load the dataset __mnist_data.RData__, available in Canvas, into your RStudio workspace.  There is a single data frame __images_df__ in this dataset. It has 2115 rows and 785 columns. Each row of the data frame represents a single image of a handwritten digit (0 or 1) that was a 28x28 grayscale pixel image (entries in $\{0, \dots, 255\}$ with 0 representing white and 255 representing black). The $28 \times 28$ matrix representing the image has been flattened into a 1x784 vector, which is stored in the first 784 entries of the row. The 785th entry of each row is called __labels__ and contains a 0 or a 1, depending on what the image in this row shows. 

Use the following function to "visualize" these images as R plots.  Examine a few of the rows of the matrix and get a sense of what these hand-written digits look like to the human eye. Make sure to see examples of each class.

```{r}
plot_digit <- function(j){
arr784 <- as.numeric(images_df[j,1:784])
col=gray(12:1/12)
image(matrix(arr784, nrow=28)[,28:1], col=col, 
      main = paste("this is a  ",images_df$labels[j]))
}
```
Example code: 
```{r, fig.width=3, fig.height=3}
plot_digit(34)
```


###A.

Plot six different images, three 0's and three 1's.

Using `head(images_df[,785],10)` command I find that {1,3,6} is 1's and {2,4,5} is 0's.

```{r}
#1
par(mfrow=c(1,3))
plot_digit(1)
plot_digit(3)
plot_digit(6)
par(mfrow=c(1,1))
#0
par(mfrow=c(1,3))
plot_digit(2)
plot_digit(4)
plot_digit(5)
par(mfrow=c(1,1))
```


###B.

Each pixel of an image is a "feature", represented as a lightness-darkness value. Do some exploration of these features. 

(i) Find five different features that have zero variability (all images have the same value in this pixel).

`X1`,`X2`,`X3`,`X4` and `X5` are five different features that have zero variability,whose features values are **0**.

```{r}
zero_variability <- apply(X = images_df,
                          MARGIN = 2,
                          FUN = function(x) min(x)==max(x) ) 
# five different features that have zero variability
head(names(images_df)[zero_variability],5)
# the same value
table(images_df[,1:5])
```

(ii) Find five different features that each have positive variability. 

`X97`,`X98`,`X99` ,`X100` and `X101` are five different features that each have positive variabilitys.



labels |X97_SD |   X98_SD |   X99_SD | X100_SD| X101_SD
------|-------|-------|----------|-----------|-------
0 | 12.47076| 19.82704 |29.75306 |35.082756|37.790391
1 | 12.94495 |13.60045 |13.75538 | 9.623613|8.654899


```{r, message=FALSE, warning=FALSE}
library(plyr)
features <- ddply(.data = images_df,
                  .variables = .(labels),
                  .fun =function(x)apply(as.matrix(x),MARGIN = 2,sd))
positive_variability <- apply(X = features,
                              MARGIN = 2,
                              FUN = function(x) x[1]>0 & x[2] >0  )
# five different features that each have positive
head(names(images_df)[positive_variability],6)
# difference
ddply(.data = images_df,
      .variables = .(labels),
      .fun = summarise,
      X97_SD=sd(X97),
      X98_SD=sd(X98),
      X99_SD=sd(X99),
      X100_SD=sd(X100),
      X101_SD=sd(X101)
      )
```

(iii) Pick any two features that have some non-zero variability. Make a scatterplot of these two features against each other. Label the datapoints with color corresponding to the __labels__ column. 

```{r, message=FALSE, warning=FALSE}
library(ggplot2)
ggplot(data=images_df,aes(X99,X101,col=as.factor(labels)))+
  geom_jitter(show.legend = F)+
  labs(title='X99 VS X101')
```


###C. 
Choose three different pairs of features that seem like they would allow separation of the two classes, then build logistic regression models for image classification that use these three pairs. Comment on the results and how well the models do. Provide confusion matrices for each model and give  interpretations for them. Also compute AUCs, using the R package __pROC.__ 

I choose three different pairs of features that are `X631`,`X570` and `X268`.Very luckily,this models are good .And the accuracy rate and auc is follow:



model|features|accuracy rate|auc
-----|--------|-------------|-----
glm1|X631|0.6288416|0.6631
glm2|X570|0.7744681|0.835
glm3|X268|0.6624113|0.7008



```{r, message=FALSE, warning=FALSE}
library(effects)
library(pROC)
# Choose three different pairs
set.seed(20)
sample(names(images_df)[positive_variability],size = 3)
#---------------------------------
# X631
#---------------------------------
glm1 <- glm(labels~X631,
            data=images_df,
            family=binomial)
summary(glm1)
# Effect Plots
plot(allEffects(glm1))
# confusion matrices
table(ifelse(glm1$fitted.values>0.5,1,0),images_df$labels)
# AUCs
roc(labels~X631,data=images_df)

#---------------------------------
# X570
#---------------------------------
glm2 <- glm(labels~X570,
            data=images_df,
            family=binomial)
summary(glm2)
# Effect Plots
plot(allEffects(glm2))
# confusion matrices
table(ifelse(glm2$fitted.values>0.5,1,0),images_df$labels)
# AUCs
roc(labels~X570,data=images_df)

#---------------------------------
# X383
#---------------------------------
glm3 <- glm(labels~X268,
            data=images_df,
            family=binomial)
summary(glm3)
# Effect Plots
plot(allEffects(glm3))
# confusion matrices
table(ifelse(glm3$fitted.values>0.5,1,0),images_df$labels)
# AUCs
roc(labels~X268,data=images_df)
```



###D.
Build a logistic model that uses at least five features. Make a confusion matrix and compute the AUC. The result should be better than the best model that you found in __C__. 


- The accuracy rate  of new model `glm5` is (911+1126)/2115=96.31%,which is better than  `glm2`.
- The auc  of new model `glm5` is  0.8948,which is better than  `glm2`.



features|Estimate| Std. Error| z value| P value
--------|---------|----------|---------|------------
(Intercept)|  4.181e+00|  3.610e-01 | 11.582|  0
X289   |     -4.786e-02 | 8.715e-03 | -5.492 |0
X259   |     -9.493e-03 | 4.057e-03 | -2.340 |  0.0193
X441   |     -5.813e-02 | 7.152e-03 | -8.128| 0
X133   |      4.241e-03 | 2.018e-03 |  2.101 |  0.0356  
X382    |     4.918e-05  |1.688e-03 |  0.029 |  0.9768    
X631   |     -7.903e-03 | 1.348e-03  |-5.863 |0
X570  |      -1.316e-02 | 1.488e-03 | -8.838 |0
X268   |      2.921e-03|  1.167e-03 |  2.504 | 0.0123  



```{r}
set.seed(100)
sample(names(images_df)[positive_variability],size = 5)

glm5 <- glm(labels~X289+X259+X441+X133+X382+X631+X570+X268,
            data=images_df,
            family=binomial)
summary(glm5)
# confusion matrices
table(ifelse(glm5$fitted.values>0.5,1,0),images_df$labels)
#AUCS
glm5.auc <- max(c(
  multiclass.roc(labels~X289,data=images_df)$auc,
  multiclass.roc(labels~X259,data=images_df)$auc,
  multiclass.roc(labels~X441,data=images_df)$auc,
  multiclass.roc(labels~X133,data=images_df)$auc,
  multiclass.roc(labels~X382,data=images_df)$auc,
  multiclass.roc(labels~X631,data=images_df)$auc,
  multiclass.roc(labels~X570,data=images_df)$auc,
  multiclass.roc(labels~X268,data=images_df)$auc
))
glm5.auc
```



###E.
Now try making a model with all the features. _This will probably take R several minutes and will result in some errors and warnings._ Look at the estimated coefficients and standard errors. Describe what you see. Explain why this approach fails.

There are so many coefficients assigned 'NA',which is as a result of **over-fitting phenomen**.

```{r}
glm.all <- glm(labels~.,
            data=images_df,
            family=binomial)
#estimated coefficients and standard errors
head(glm.all$coefficients,100)
glm.all$deviance
```

