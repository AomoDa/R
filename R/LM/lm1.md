---
title: "Untitled"
author: "Your Name"
date: '2016-12-17'
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Abstract

First use EDA to understand the data distribution and use plot analysis and hypothesis test to understand the relationship between independent variable and dependent variable.Next fit simple linear regression model but I find that the model performance is not good.And then, I use stepwise regression algorithm  to fit multiple regression linear model.Finally the model performs well and draws conclusions.


#Introduction and Background

I find a my dataset that contains the prices for a representative 804 samples, 2005 cars that were manufactured by companies owned by General Motors.And I am interested what variables affect the price of cars.If we can find the variables which affects the price of cars and find a good prediction model,  and then we will seize market opportunities.

In this article, I will use the following methods to complete my research report.

- Exploratory Data Analysis.
- Descriptive Statistics
- Correlation Analysis
- Hypothesis Test 
- Graphic analysis
- Simple Linear Regression
- Multiple Linear Regression
- Stepwise Regression
- Residual Analysis
- Influence Analysis
- Multicollinearity Analysis








#Exploratory Data Analysis

Exploratory Data Analysis is an approach/philosophy for data analysis that employs a variety of techniques (mostly graphical) to maximize insight into a data set.Most EDA techniques are graphical in nature with a few quantitative techniques. The reason for the heavy reliance on graphics is that by its very nature the main role of EDA is to open-mindedly explore, and graphics gives the analysts unparalleled power to do so, enticing the data to reveal its structural secrets, and being always ready to gain some new, often unsuspected, insight into the data. In combination with the natural pattern-recognition capabilities that we all possess, graphics provides, of course, unparalleled power to carry this out.


##Data Description

```{r, message=FALSE, warning=FALSE, include=FALSE}
library(ggplot2)
library(car)
library(effects)
library(MASS)
x <- read.csv('C://Users//AomoDa//Documents//UsedCarPricingData.csv',
              header = T)
x$Leather <- as.factor(x$Leather)
x$Sound <- as.factor(x$Sound)
x$Cruise <- as.factor(x$Cruise)
x$Doors <- as.factor(x$Doors)
```



I find a my dataset that contains the prices for a representative 804 samples, 2005 cars that were manufactured by companies owned by General Motors. And the description of the variables in my dataset are as follows:

- **Price** :`[Num]` Response variable. Suggested retail price of the used 2005 GM car in excellent condition. The condition of a car can greatly affect price. All cars in this data set were less than one year old when priced and considered to be in excellent condition.
- **Mileage**:`[Num]`  Number of miles the car has been driven.
- **Make** :`[Factor]` Manufacturer of the car such as Saturn, Pontiac, and Chevrolet.
- **Model** : `[Factor]` Specific models for each car manufacturer such as Ion, Vibe, Cavalier.
- **Trim (of car)** :`[Factor]`  Specific type of car model such as SE Sedan 4D, Quad Coupe 2D
- **Type** :`[Factor]`  Body type such as sedan, coupe, etc.
- **Cylinder** :`[Factor]`  Number of cylinders in the engine; This is a measure of the size of a car's engine.
- **Liter** : `[Num]`   A more specific measure of engine size, namely the total volume of all of the cylinders in an engine.
- **Doors** : `[Num]` Number of doors.
- **Cruise** :`[Factor]`  Indicator variable representing whether the car has cruise control (1 =cruise).
- **Sound** : `[Factor]`   Indicator variable representing whether the car has upgraded speakers (1 = upgraded).
- **Leather** :`[Factor]`   Indicator variable representing whether the car has leather seats (1 = leather).




##Descriptive Statistics


- The minimum value of `Price` is 8639 ,the maximum is 70755 and the median is 18025.The hisgogram of `Price` tell me that it is   approximate a  normal distribution.
- The minimum value of `Mileage` is 266 ,the maximum is 50387 and the median is 20914.The hisgogram of `Mileage` tell me that it is   approximate  a normal distribution.
- The minimum value of `Liter` is 1.600 ,the maximum is 6.000 and the median is 2.800.The hisgogram of `Mileage` tell me that it is  not a   normal distribution.



-----

Item|Price|Mileage|Liter  
-----|-----|-----|-----
Min|8639|266|1.600
1st Qu|14273 |14624 |2.200
Median |18025 |20914 |2.800
Mean|21343 |19832 |3.037
3rd Qu|26717 |25213 |3.800
Max.|70755 |50387 |6.000

-----


```{r, echo=FALSE}
par(mfrow=c(1,3))
hist(x$Price,probability = T,
     breaks = 30,
     main='Histogram of Price',
     xlab='Price')
lines(density(x$Price),
      col='red')
hist(x$Mileage,
     probability = T,
     breaks = 30,
     main='Histogram of Mileage',
     xlab='Mileage')
lines(density(x$Mileage),
      col='blue')
hist(x$Liter,
     probability = T,
     breaks = 10,
     main='Histogram of Liter',
     xlab='Liter')
lines(density(x$Liter),col='orange')
```


##ScatterPlot and Correlation Analysis

- Using price as the response variable ,I plot the scatter diagrams with `Mileage` .As can be seen from the scatter diagrams, the linear relationship between `price` and `Mileage` is very weak.
- Pearson's product-moment correlation : $t-value = -4.09, df = 802, p-value = 0$. The p-value of pearson's product-moment correlation tell is 0 which is very statistically significant at 0.01 level.So,We can use pearson's correlation coefficient to  describe a linear relationship between `price` and `Mileage`.
- The $Cor(price,Mileage)=-0.14$ and 95%  percent confidence interval of correlation coefficient is $[-0.21,-0.07]$.The linear relationship between `price` and `Mileage` is very weak.



```{r, echo=FALSE}
#Price VS Mileage 
ggplot(data=x,aes(x=Mileage,y=Price))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title='Price VS Mileage ' )
```


##Boxplot and Hypothesis Test 

- **Model** : Specific models for each car manufacturer such as Ion, Vibe, Cavalier.The boxplot tells me that different `Model` have different  means of `Price`, which indicated  `Price` is related to `Model`.
- **Trim (of car)** :  Specific type of car model such as SE Sedan 4D, Quad Coupe 2D.The boxplot also tells me that different `Trim` have different  means of `Price`, which indicated  `Price` is related to `Trim`.

In fact,`Price` and `Trim` are the basic information of my data. But from the purposes of my research, I do not care about these basic information, because the information is endless.And I would like to find other generic indicators and the relationship between `Price`.


```{r, echo=FALSE}
# boxplot
boxplot(Price~Model,data=x,main='boxplot of Model')
boxplot(Price~Trim,data=x,main='boxplot of Trim')
```


T test:

$$H_0:Means in different groups  are equal.$$
$$H_1:Means in different groups  are  not equal.$$


- **Doors** : Number of doors.The pvalue of t test with two-sided is 0.0020,which is very statistically significant and I reject $H_0$.Means with different Doors are not  equal.
- **Cruise** : Indicator variable representing whether the car has cruise control (1 =cruise).The pvalue of t test with two-sided is 0.0020,which is very statistically significant and I reject $H_0$.Means with different Cruise are not  equal.
- **Sound** :  Indicator variable representing whether the car has upgraded speakers (1 = upgraded).The pvalue of t test with two-sided is 0.0020,which is very statistically significant and I reject $H_0$.Means with different Sound are not  equal.
- **Leather** : Indicator variable representing whether the car has leather seats (1 = leather).The pvalue of t test with two-sided is 0.0020,which is very statistically significant and I reject $H_0$.Means with different Leather are not  equal.



-----

Item|Test|Alternative|$H_0$|T value|df|p value|Conclusion
-----|-----|---------|-----|-------|-----|-----|---------
Price~Doors|T-test|two-sided|equal|3.13|236.08|0.0020|reject $H_0$
Price~Sound|T-test|two-sided|equal|3.5663|510.38|0.0004|reject $H_0$ 
Price~Cruise|T-test|two-sided|equal|-22.031|757.88|0|reject $H_0$ 
Price~Leather|T-test|two-sided|equal|-5.737|698.47|0|reject $H_0$ 

-----


```{r, echo=FALSE}
par(mfrow=c(1,3))
boxplot(Price~Make,data=x,main='boxplot of Make')
boxplot(Price~Type,data=x,main='boxplot of Type')
boxplot(Price~Cylinder,data=x,main='boxplot of Cylinder')
par(mfrow=c(1,1))
```



#Simple Linear Regression

For an initial analysis, develop a simple linear regression model to investigate whether cars with lower mileage are worth more than cars with higher mileage.The equation of my simple linear regression model is 

$$Price=24764.5590 - 0.1725 \cdot Mileage $$

Results are analyzed as follows:

- Based on the previous correlation analysis, we find that the correlation coefficient is -0.14.
- The coefficients of intercept is 24760 and 95% CI is $[22989.36,26539.76]$.Which means that price  would be 24760 when mileage is zero.
-  The coefficients of mileage is -0.17 and 95% CI is $[-0.26,-0.09]$.Which means that price  would reduce 17 with mileage increase 100.
- The p value of `intercept` is 0,which is very statistically significant .
- The p value of `mileage` is 0,which is  very statistically significant.
- The model $R_{adj}=1.92\%$. Variance of Price has been explained only 1.9%.And I think that mileage does not  help me predict `price`.


```{r, echo=FALSE}
lm1 <- lm(Price~Mileage,data=x)
plot(allEffects(lm1))
```

#Multiple Linear Regression

In a simple linear regression model, a single response measurement Y is related to a single predictor  X for each observation. The critical assumption of the model is that the conditional mean function is linear: $E(Y |X) = \alpha + \beta X$. In most problems, more than one predictor variable will be available. This leads to the
following multiple regression mean function: $E(Y |X) = \alpha + \beta_1 X_1 + · · · + \beta_iX_i$.

Motivated by the results of your initial modelling, extend your simple linear regression analysis by developing a multiple linear regression model that could be used to price cars based on a variety of characteristics.


##Selection variety  characteristics

I will use stepwise regression to select best variables and fit my best model.

- Forward selection, which involves starting with no variables in the model, testing the addition of each variable using a chosen model fit criterion, adding the variable (if any) whose inclusion gives the most statistically significant improvement of the fit, and repeating this process until none improves the model to a statistically significant extent.
- Backward elimination, which involves starting with all candidate variables, testing the deletion of each variable using a chosen model fit criterion, deleting the variable (if any) whose loss gives the most statistically insignifiant deterioration of the model fit, and repeating this process until no further variables can be deleted without a statistically significant loss of fit.

I used forward stepwise and backward stepwise , and their final results were the same.Finally   I am pretty sure my final model was `Price ~ Mileage + Make + Type + Cylinder + Liter`.(Model debugging process and code at the end of my article.)


## Best Model


- As can be seen from the table below, the p values of all variables are statistically significant,which means  all coefficients are accurate and they can be used as a model parameter.
- $F-statistic=965.3$, $DF1=12$ , $DF2=791$ and $p-value=0$ ,which means the equation is very statistically significant at 0.01 level.
- $R_{adj}=93.51\%$,The Variance explanation degree of my model is very high and my model is very good.


-----

Item|Estimate |Std. Error| t value| P value
-----|--------|----------|--------|--------  
Intercept | 23317.52 | 963.5| 24.200 | 0
Mileage    |  -0.18 | 0.01088 |-17.001  |0
MakeCadillac |16016.4 | 464 | 34.516  |0
MakeChevrolet |-1673.88  | 349.7 | -4.787 |0
MakePontiac  | -1877.51  |363.2| -5.170 |0
MakeSAAB   |10593.90 | 445.3 | 23.789 |0
MakeSaturn | -1367.34  | 4656.e| -2.937 | 0.0034 
TypeCoupe  |  -11965.57 |  471.9| -25.358 | 0
TypeHatchback| -12333.40  | 548.1e |-22.503  |0
TypeSedan | -12326.16  | 410.3|-30.043 | 0
TypeWagon | -8335.84|  510.9| -16.315  |0
Cylinder |-1311.47 | 311.5| -4.210 |0
Liter  |5862.34 |  349.3 | 16.784 | 0

-----


```{r, echo=FALSE}
lm3 <- lm(formula = Price ~ Mileage + Make + Type + Cylinder + Liter, data = x)
plot(allEffects(lm3))
```



##Residual Analysis


- The three residual plots indicate that The  residuals of my model is a  normal distribution with mean zero,which  is consistent with the basic assumption of linear regression.
- "Residuals VS Fitted values" indicated that there may be some connection between Residual and fitted values.But  the curvature is small, we can tolerate this error.

```{r, echo=FALSE}
par(mfrow=c(1,3))
plot(residuals(lm3),main='residuals plot ',xlab='',ylab='residuals',pch=16,col=gray(0.15),ylim=c(-8000,8000))
abline(h=0,col='orange',lty=1,lwd=2)
hist(residuals(lm3),breaks = 50,probability = T,xlim=c(-5000,5000))
lines(density(residuals(lm3)),col='blue')
qqPlot(lm3,main='Q-Q plot of residuals')
par(mfrow=c(1,1))
residualPlot(lm3,main=' residuals VS Fitted values')
```


##Influence Analysis


The Hat of $151^{th}$ data is 0.40 and CookD is 0.12,which means it is a strong influence point.The Hat of $341^{th}$ data is 0.05 and CookD is 0.00,which means it is a strong influence point.These points are originally part in my data. When  deletE  these data , the equation can explain the degree, but to some extent we lost the data information.Considered that there are  only 2 points,  I decided to retain these data without any changes.


-----

ID|StudRes| Hat|CookD
-----|-----|-----|------
151|6.25| 0.40| 0.12
341|0.44 |0.05|0.00

-----


```{r, echo=FALSE}
influencePlot(lm3,id.method = 'identyfy',main='influencePlot')
```


##Multicollinearity Analysis

In statistics, the variance inflation factor ($VIF$) quantifies the severity of multicollinearity in an ordinary least squares regression analysis. It provides an index that measures how much the variance (the square of the estimate's standard deviation) of an estimated regression coefficient is increased because of collinearity.The generalized vifs are invariant with respect to the coding of the terms in the model (as long as the subspace of the columns of the model matrix pertaining to each term is invariant). To adjust for the dimension of the confidence ellipsoid, the function also prints $GVIF^{1/(2\cdot Df)}$ where df is the degrees of freedom associated with the term.Through a further generalization, the implementation here is applicable as well to other sorts of models, in particular weighted linear models and generalized linear models.

I calculated the $VIF$ value of each variable in my model.And I find that `Liter` and `Cylinder` exists  a multiple linear problem beacuse of $VIF >10$



-----

Variables|$GVIF$| $Df$| $GVIF^{1/(2\cdot Df)}$
---------|-----|-----|----------------
Mileage |1.01 | 1 |1.00
Make| 4.74 | 5|1.168
Liter |18.88 | 1|4.35
Type  |2.51 | 4 |1.12
Cylinder |23.67 | 1 |4.86

-----


# Discussion and Conclusion


The result of multiple linear regression tells us that using this equation can well explain and predict the dependent variable `price`.There are some minor problems, though. Considerd the actual situation, these small errors can tolerate receiving.

In a word,we can use `Mileage`,`Make`,`Type`,`Cylinder` and `Liter` to predict `Price`.




# References

1. Wilson P W. FEAR : A software package for frontier efficiency analysis with R[J]. Socio-Economic Planning Sciences, 2008, 42(4):247-254.
2. Kabacoff R I. R in Action[M]. Manning, 2011.
3. Analysis R C. Regression and Correlation Analysis[J]. 1959, 123(4):307-308.
4. Bach F R, Jordan M I. A probabilistic interpretation of canonical correlation analysis[J]. 2005.
5. Wickham H. ggplot2: Elegant Graphics for Data Analysis[M]. Springer Publishing Company, Incorporated, 2009.
6. Farrar D E, Glauber R R. Multicollinearity in regression analysis; the problem revisited[J]. Review of Economics & Statistics, 1967, 49(1):92-107.
7. Lourdes C. Montenegro, Victor H. Lachos, Heleno Bolfarine. Local Influence Analysis for Skew-Normal Linear Mixed Models[J]. Communication in Statistics- Theory and Methods, 2009, 38(4):484-496.
8. Chesher A, Irish M. Residual analysis in the grouped and censored normal linear model[J]. Journal of Econometrics, 1987, 34(1–2):33-61.
9. Hypothesis Test[M]. Springer US, 2009.





#Appendices: R Code

##Summary Table 

```{r}
summary(x[,c('Price','Mileage','Liter')])
```


##Pearson's product-moment correlation

```{r}
cor.test(~Price+Mileage,data=x)
```

##Welch Two Sample t-test

```{r}
t.test(Price~Doors,data=x)
t.test(Price~Sound,data=x)
t.test(Price~Cruise,data=x)
t.test(Price~Leather,data=x)
```


##Simple Linear Regression

```{r}
summary(lm1)
confint(lm1)
```


##Forward Stepwise Output

```{r}
stepAIC(object = lm1,
        scope = list(
          upper=~Mileage+Make+Type+
            Cylinder+Liter+Doors+
            Cruise+Leather,
          lower=~1),
        direction = 'forward')
```

##Back Stepwise Output

```{r}
# backward
lm2 <- lm(Price~Mileage+Make+
            Type+Cylinder+Liter+
            Doors+Cruise+
            Leather,
          data=x)
stepAIC(lm2,
        direction = 'backward')
```

##Multiple Linear Regression


```{r}
summary(lm3)
anova(lm2,lm3)
```

