---
title: "Logit Regression"
author: "Your Name"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Abstract 

First use descriptive statistical analysis to understand the data distribution and then  use hypothesis testing to explore the relationship between response variable `dem` and other variables in the dataset.Finally, use the logic regression model to explore the effect of each variable on the response variable.


#Introduction  




#Literature review





#EDA

The data set includes the following variables:

- **[Factor]** dem : whether the respondent identifies as a democrat (my dependent variable)
- **[Factor]**degree  : whether the respondent holds a BA or an advanced degree
- **[Factor]**choice: whether the respondent supports abortion (1-4 ordinal, higher values indicate more support).
-  **[Int]** fedgovft: feeling towards the Federal Government (0-100, higher values indicate positive views)
-  **[Int]** muslimsft: feeling towards the Muslims (0-100, higher values indicate positive views)
-  **[Int]** feministsft: feeling towards the Feminists (0-100, higher values indicate positive views)
- **[Factor]**  race  :a nominal variable indicating the respondent's race (Asian, Black, Hispanic, White)


```{r}
#load data
x <- read.csv('C://Users//AomoDa//Documents//exit_poll_2004.csv',
              header = T,
              na.strings = 'NA')
x$degree <- as.factor(x$degree)
x$choice <- as.factor(x$choice)
```


##Descriptive statistics


- Useing descriptive statistics,I know there are 382 democrats in my dataset and the  proportion of democrats is 51.7%. there are 357 non-democrats in my dataset and the  proportion of non-democrats is 48.3%. 
- There are 521 respondents who hold a BA or an advanced degree and the proportion in all data is 70.5%.There are 218 samples which hold a BA or an advanced degree and the proportion in all data is 29.5%.
- For the choice whether the respondent supports abortion ,there are 90  respondents did not choice any one. And respondents choice is 100:202:115:232 out of choice1,2,3,4.
- The proportion  of respondent's race is 17:116:51:550 with choice Asian,Black,Hispanic and White. And 2 respondents did not choice any one.
- The average of fedgovft is 58.31 ,  the minimum is 0 the and maximum is 100.And 86 respondents did not choice any one.
- The average of muslimsft is 54.23 ,  the minimum is 0 the and maximum is 100.And 116 respondents did not choice any one.
- The average of feministsft is 56.77 ,  the minimum is 0 the and maximum is 100.And 96 respondents did not choice any one.

```{r}
pairs(x,col=x$dem+2)
```


##Hypothesis Test


In this part ,I will use hypothesis testing to explore the relationship between  `dem` and other variables in the dataset.

###Visualization and T test

T test:

$$H_0:u_1=u_0$$
$$H_1:u_1 \neq u_0$$


I reject all null hypothesis, because the p values of t test are both statistically significant at 0.05 level,which indicated that `dem`,`muslimsft` and `feministsft` maybe affect the results of `dem`.

-----

T-test|T Value| df|p-value|conclusion
------|-------|---|-------|----------
fedgovft~dem|5.4159|648.26|0|reject $H_0$
muslimsft~dem|-3.538|620.55|0|reject $H_0$
feministsft~dem|-9.5437|638.87|0|reject $H_0$


-----

```{r}
par(mfrow=c(1,3))
boxplot(fedgovft~dem,data=x,col=2:3,main='edgovft boxplot',xlab='dem')
boxplot(muslimsft~dem,data=x,col=2:3,main='muslimsft boxplot',xlab='dem')
boxplot(feministsft~dem,data=x,col=2:3,main='feministsft boxplot',xlab='dem')
par(mfrow=c(1,1))
## test
t.test(fedgovft~dem,data=x)
t.test(muslimsft~dem,data=x)
t.test(feministsft~dem,data=x)
```


###Visualization and Chisq test


Chisq test:
$$H_0: The two variables are independent of each other$$
$$H_0: The two variables are dependent of each other$$

I reject all null hypothesis, because the p values of chisq test are both statistically significant at 0.05 level,which indicated that `degree`,`choice` and `race`  maybe affect the results of `dem`.


-----

Chisq Test|Chisq Value|DF|p-value |conclusion
----------|-----------|-----------|----------
dem~degree|5.621|1|0.018|reject $H_0$
dem~choice|28.23|3|0|reject $H_0$
dem~race|126.19|3|0|reject $H_0$

-----


```{r}
summary(with(x,table(degree,dem)))
summary(with(x,table(dem,choice)))
summary(with(x,table(dem,race)))

par(mfrow=c(1,3))
mosaicplot(with(x,table(degree,dem)),shade = T,color = TRUE,main='mosaicplot of dem vs degree')
mosaicplot(with(x,table(choice,dem)),shade = T,color = TRUE,main='mosaicplot of dem vs choice')
mosaicplot(with(x,table(race,dem)),shade = T,color = TRUE,main='mosaicplot of dem vs race')
par(mfrow=c(1,1))
```


#Logit Regression


Through the previous analysis I found that `dem` and other variables have some relationship. And  next  I will fit a logical regression model to accurately calculate the impact of the various indicators.

##Fit Best Model


- First I fit a full variables logical regression model $glm1$.But I observe that there are  some  variables which are  not statistically significant.
- Next I use backward step algorithm to select best variables and fit a new logical regression model $glm2$. And I find all variables are  statistically significant at 0.05 level in $glm2$ model.
- Finally I use likelihood ratio test to compare $glm1$ and $glm2$,the p value is $0.4621 >0.05$ which indicated we can not reject $H_0$ and $glm2$ is best model.



-----

model|Resid. Df| Resid. Dev| Df Deviance| P value
-----|---------|-----------|------------|--------
$glm1$|593|570.91                     
$glm2$|594|571.45|-1|-0.54078| 0.4621

-----



```{r, message=FALSE, warning=FALSE}
library(MASS)
x <- na.omit(x)
glm1 <- glm(dem~.,data=x,family = binomial(logit) )
glm2 <- stepAIC(glm1,direction='backward')
#LRT
anova(glm1,glm2,test = 'LRT')
```


##Model Comment


- When `degree` is 1,the odds ratio is 0.64;when `degree` is 0,the odds ratio is 1.
- When `choice` is 1 the the odds ratio is 1; When `choice` is 2 the the odds ratio is 1.16 and so on.And I find that odds ratio will increase  with the increase of value of `choice`.


-----

Item| Estimate| odds ratio
-----|--------|----------
Intercept | -1.03 |0.36
degree1  |-0.44 |0.64
choice2 | 0.14| 1.16
choice3 | 0.15| 1.16
choice4 | 0.69  | 2.00
fedgovft |  -0.04  | 0.96
raceBlack  |  4.35 | 77.48
raceHispanic |1.34  |3.81
raceWhite   | 0.25  |1.28
feministsft |  0.04 |1.0

-----

```{r, message=FALSE, warning=FALSE}
library(effects)
plot(allEffects(glm2), type="response", ylim=c(0,1), ask=FALSE)
```


## Model Performance

- The accuracy rate of $glm2$ is 76.16%.
- The ROC plot tell me that the  performance of my model is very good.


```{r, message=FALSE, warning=FALSE}
library(ROCR)
pred <- predict.glm(object = glm2,newdata = x,type = 'response')
ps <- prediction(pred,x$dem)
plot(performance(ps,'tpr','fpr'),colorize=T,main='ROC of glm2')
lines(x=c(0,1),y=c(0,1),lty=3,col='orange')
```


#Discussion and  Conclusion


The accuracy of the model is 75% , but I  think  it can  be still improved the accuracy rate  by useing the optimization model. because  of the length of this paper, I do dot research further discussion.When the model is processed, I only consider the prediction in the sample data, but not set up the training samples and test samples to make a comprehensive comparison.

In my paper, all the indicators have reached the expected value and the fitting value and the actual situation is very consistent. I think my model is very good.


#Reference


1. Hausman J, Mcfadden D. Specification Tests for the Multinomial Logit Model.[J]. Econometrica, 1981, 52(5):1219-40.
2. Kabacoff R I. R in Action[M]. Manning, 2011.
3. SimonN. Generalized additive models : an introduction with R[M]. Chapman & Hall/CRC, 2006.
4. Zuur A F, Ieno E N, Walker N J, et al. Mixed effects models and extensions in ecology with R[J]. Journal of the Royal Statistical Society, 2009, 173(4):938–939.

