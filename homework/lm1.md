---
title: "Untitled"
author: "Your Name"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Abstract 

First use descriptive statistical analysis to understand the data distribution and  and apply log transformation in data.Then use Correlation Analysis to explore the relationship between `ptvCon` and `log-Age`,`log-Income` in the dataset.Finally, use the linear regression model to explore the effect of `log-Age`and `log-Income` on `ptvCon`.

#Introduction 

#Literature review

#EDA

The data set, taken from the British Election Studies, includes the following variables:

- `Age`: Respondent age in years.
- `profile_gross_personal`: Respondent's gross personal income in pounds
sterling. (These have been converted to numbers by taking the mid-point
of each range.)
- `ptvCon`: Respondent's propensity to vote for the Conservative, Labour, Liberal Democrat, UKIP, and Green parties respectively. 10 indicates the respondent is highly likely to vote for this party, 0 that the respondent would never consider voting for this party.


```{r, message=FALSE, warning=FALSE, include=FALSE}
library(ggplot2)
x <- read.csv('C://Users//AomoDa//Documents//uk_voting.csv',header = T)
x <- x[,c(1,2,4)]
summary(x)
```

##Descriptive statistics

The difference of  order magnitudeThe in  response variable `ptvCon` and independent variables are too large, so I decided to use logarithmic transformation to independent variables `Age` and `Income`.And I get new variables `log-Age` and `log-Income`.

Next, I will use these three variables `log-Age` ,`log-Income` and `ptvCon` to conduct a follow-up analysis of my report.

- The mean value of `log-Age` is 1.342,the median is 1.369,maximum value is 1.527 and minimum value is 1.061.
- The mean value of `log-Income` is 2.306,the median is 2.305,maximum value is 2.057  and minimum value is 2.452 .
- The mean value of `ptvCon` is 3.669,the median is 2, maximum value is 0 and minimum value is 10.

-----

Item| `Age` |`Income` | `ptvCon`| `log-Age`|`log-Income`
 -----|-----|---------|---------|----------|-----------
Min|2.890 | 7.824| 0.000 |1.061  |2.057             
1st Qu|3.584| 9.770 | 0.000  |1.276  |2.279             
Median|3.932|10.021 | 2.000 |1.369 |2.305             
Mean  |3.844 |10.049| 3.669  |1.342  |2.306             
3rd Qu|4.127|10.389 | 8.000  |1.418 |2.341             
Max.  |4.605|11.608 |10.000 |1.527|2.452    

-----

## Visualization


- From the `log-Age` histogram and the density plot, I understand the distribution of `log-Age` is right  which seems to be not a  normal distribution.
- From the `log-Income` histogram and the density plot, I think the distribution of `log-Income` maybe a  normal distribution.
- From the `ptvCon` histogram and the density plot, I understand the distribution of `ptvCon` is relatively uniform which seems to be not a  normal distribution.


```{r, echo=FALSE}
x$Age <- log(x$Age)
x$profile_gross_personal <- log(x$profile_gross_personal)
par(mfrow=c(1,3))
hist(x$Age,probability = T,main='Histogram of log Age')
lines(density(x$Age,from = 0),col='red',lty=1,lwd=1)
hist(x$profile_gross_personal,probability = T,main='Histogram of log Income')
lines(density(x$profile_gross_personal,from = 0),col='blue',lty=1,lwd=1)
hist(x$ptvCon,probability = T,main='Histogram of ptvCon')
lines(density(x$ptvCon,from = 0),col='orange',lty=1,lwd=1)
par(mfrow=c(1,1))
```


##Correlation Analysis 

- The linear relationship between `ptvCon` and `Log-Age` appears to be very weak ,which scatter plot tell me.
- The scatter plot also tell me that there is a linear relationship between `ptvCon` and `Log-Income`.And `ptvCon` will increase with the increase of `Log-Income`.


```{r, echo=FALSE, message=FALSE, warning=FALSE}
# plots
ggplot(data=x,aes(x=Age,y=ptvCon))+geom_point()+geom_smooth(method = 'lm')+labs(title='ptvCon vs Log-Age')
ggplot(data=x,aes(x=profile_gross_personal,y=ptvCon))+geom_point()+geom_smooth(method = 'lm')+labs(title='ptvCon vs Log-Income')
```


Pearson's product-moment correlation 

- The correlation coefficients between `ptvCon` and `Log-Age` is $Cor(ptvCon,Age)=0.07$ and 95%confidence interval is $[0.05,0.09]$.The correlation coefficients is too small so that I think there is not a linear relationship between them.
- The correlation coefficients between `ptvCon` and `Log-Income` is $Cor(ptvCon,Age)=0.14$ and 95%confidence interval is $[0.12,0.16]$ .The correlation coefficients is too small so that I think there is not a linear relationship between them.
- Correlation analysis tells us that the linear relationship between `ptvCon` and `Log-Age`,`Log-Income`is very weak, which may be a major risk of subsequent linear regression analysis.


-----

Item|correlation coefficients|95%confidence interval|p-value of cor.test
-----|-----------------------|----------------------|--------------------
$Cor(ptvCon,Age)$|0.07|$[0.05,0.09]$|0
$Cor(ptvCon,Income)$|0.14$[0.12,0.16]$|0

-----

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(psych)
pairs.panels(x)
```

#Linear Regression

In statistics, linear regression is an approach for modeling the relationship between a scalar dependent variable `ptvCon`and  independent variables `Log-Age`and `Log-Income`. The case of one independent variable is called simple linear regression. But in my paper ,there is  more than one independent variable the process is called multiple linear regression. Linear regression was the first type of regression analysis to be studied rigorously, and to be used extensively in practical applications. This is because models which depend linearly on their unknown parameters are easier to fit than models which are non-linearly related to their parameters and because the statistical properties of the resulting estimators are easier to determine.Linear regression models are often fitted using the least squares approach and in my paper I will use OLS method.



##Fit My Model

- All of the variables passed the T test,which means  that all the variables are statistically significant.
- Intercept coefficients of my model is -7.90 and 95%confidence interval is $[-9.64,-6.16]$ .
- `Log-Age` coefficients of my model is 0.69  and 95%confidence interval is $[0.44,0.94]$ .`ptvCon` will increase by 0.69 with `Log-Age` per increase of 1
- `Log-Income` coefficients of my model is 0.89  and 95%confidence interval is $[0.74,1.04]$ .`ptvCon` will increase by 0.89 with `Log-Income` per increase of 1.

The equation of linear regression is
$$ptvCon= -7.90+ 0.69 \cdot e^{Age} + 0.89 \cdot  e^{Income} $$

Coefficients

-----

Item|  Estimate| 95%confidence interval| Std. Error | t value|  P value
-------|----------|-----------------------|------------|---------|--------
(Intercept)| -7.90 | $[-9.64,-6.16]$ |0.88765 |  -8.898 |   0
Log_Age | 0.69  | $[0.44,0.94]$ |  0.12833  |  5.373  |   0 
Log_Income|0.89 | $[0.74,1.04]$|   0.07608 |  11.665 |   0

-----


```{r, message=FALSE, warning=FALSE, include=FALSE}
lm1 <- lm(ptvCon~Age+profile_gross_personal,data=x)
confint(lm1)
summary(lm1)
```



## Model Comment


- $R_{adj}=29.22\%$,so about 29.22% of the `ptvCon` in the diameter changes is explained by this model.This level of interpretation is in fact is still relatively small, so that we may need to increase the independent variable by useing other methods to continue to improve the model's explanatory power and performance.
- $F_{stat}=86.22,DF_1=2,DF_2=7265 , p-value=0<0.05$.The linear regression model has passed the F test, which means that the parameters of the model are estimated to be reliable.
- However, the residual error of the linear regression seems to be different from the normal distribution.

```{r, message=FALSE, warning=FALSE}
library(car)
qqPlot(residuals(lm1),main='qqnorm of mdoel residuals ')
```


#Discussion and  Conclusion

-The linear regression equation is not strong enough, there are still some problems in the residual.
-The linear regression is not enough to explain the `ptvCon`, which means I may be needed to increase the degree of interpretation adding the independent variable.

Finally,the equation of linear regression is
$$ptvCon= -7.90+ 0.69 \cdot e^{Age} + 0.89 \cdot  e^{Income} $$


#Reference

1. Seber G A F. Linear Regression Analysis[M]// Linear regression analysis /. John Wiley, 2003:362-363.
2. Gromping U. Relative Importance for Linear Regression in R: The Package relaimpo[J]. Journal of Statistical Software, 2006, 17(1):925-933.
3. Muenchen R A. Graphics with ggplot2, (GPL), graphics[M]// R for SAS and SPSS Users. 2011:710-713.
4. Cohen. Applied multiple regression-correlation analysis for the behavioral sciences /Jacob Cohen, Patricia Cohen[J]. 1983.
5. Cohen J, Cohen P. Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences, Erlbaum, Hillsdale, NJ[J]. Journal of the American Statistical Association, 1985, 80(390).
6.Kutner M H, Nachtsheim C J, Neter J. Applied Linear Regression Model[J]. 2004, 26(4).

#Appendix R Code and Screenshot
