---
title: "GLM"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Part 1

```{r, message=FALSE, warning=FALSE, include=FALSE}
library(ggplot2)
library(psych)
library(car)
library(effects)
x <- read.csv('C://Users//mali//Documents//MMRCountry.csv',
              header = T,
              stringsAsFactors = F,
              na.strings = -99)
x$TFR.level <- factor(x$TFR.level,
                      levels = c(0,1,2),
                      labels = c('Low','Medium','High'))
x <- na.omit(x)
```

##Task 1

- Categorical variable indicating the level of Total Fertility Rate,LOW is 21, Medium is 94 and High is 66.
- $Cor(MMR,GDP)=-0.39$.which indicated `MMR` will be reduced with the increase of `GDP`.
- $Cor(MMR,LifeExpBW)=-0.86$,which indicated `MMR` will be reduced with the increase of `LifeExpBW`.
- Descriptive statistical analysis tells us the basic situation of the data, understanding of the basic situation of the data will help us better analysis with the dataset.


-----

Item|MMR|GDP|LifeExpBW
-----|-----|-----|-----
Min|2.0| 112|48.00  
1st Qu|20.0|1392|65.00  
Median| 66.0|4807|76.00  
Mean|169.2| 13498|71.94  
3rd Qu| 240.0|15738|79.00  
Max |1100.0|115377|87.00

-----



```{r, echo=FALSE, message=FALSE, warning=FALSE}
pairs.panels(x[,c(-1,-4)],method = 'pearson')
ggplot(data=x,aes(x=TFR.level,fill=TFR.level))+
  geom_bar(show.legend = F)+
  labs(title='TFR level')
```


##Task 2

###a


- $R_{adj}=0.1443$,the adjusted rsquared is very small, which means that the model fitting is very bad.
- [p1] The residual error of the model seems to be dissatisfied with the normal distribution,which does not accord with the basic assumptions of linear regression.
- [p2] There is some connection between the residuals of the model and the fitted value, which does not accord with the basic assumptions of linear regression.
-This model is not appropriate for describing the relationship between `MMR` and `GDP`

-----

Item|Estimate| Std. Error| t value| Pvalue
-----|--------|----------|--------|---------
Intercept | 227.6  |18.61  | 12.23| 0 
GDP |-0.004329  |0.0007731 | -5.60| 0 


-----


```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=x,aes(x=GDP,y=MMR))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',na.rm = T,col=I('red'))+
  labs(title='MMR vs GDP')
lm1 <- lm(MMR~GDP,data=x)
qqPlot(lm1,main='p1:Q-Q norm plot of residual of lm1')
residualPlots(lm1,main='p2:residualPlots of lm1')
```

###b


- First my model is `MMR ~ GDP`.
- Then use forward regression algorithm to select best model.When adding `LifeExpBW`,the AIC of new model is 1722.7,which is  the least.And I get new model `MMR ~ GDP + LifeExpBW`.
- Thirdly,when adding `GDP:LifeExpBW`,the AIC of new model is 1704.9,which is  the least.And I get new model `MMR ~ GDP + LifeExpBW + GDP:LifeExpBW`.
- Finally,when adding any other items,the AIC of new model will be larger.So,we get my new model is

$$MMR= 1666.38 - 0.03 \cdot GDP - 20.83 \cdot LifeExpBW +0.00037268 \cdot GDP \cdot LifeExpBW $$.

- the coefficients of intercept is 1666.38,which means MMR would be 1666.38 when GDP and LifeExpBW are 0.
- $R_{adj}=0.7598$,which indicated that my model is better than $lm1$.
- [p3]The residual error of the model seems to be dissatisfied with the normal distribution,which does not accord with the basic assumptions of linear regression.
- [p4]There is not a  connection between the residuals of the model and the fitted value, which  accords with the basic assumptions of linear regression.


-----

Item| Estimate |Std. Error |t value|Pvalue
-----|----------|---------|--------|-------    
(Intercept) ||   1.666e+03 | 6.861e+01|  24.289 | 0
GDP        |   -2.960e-02 | 6.732e-03 | -4.396 |0 
LifeExpBW  |   -2.083e+01 | 9.770e-01| -21.320 | 0 
GDP:LifeExpBW | 3.727e-04 | 8.241e-05 |  4.522 |0 

-----





Forward regression algorithm Trace:

```
Start:  AIC=1932.88
MMR ~ GDP

            Df Sum of Sq     RSS    AIC
+ LifeExpBW  1   5308280 2380466 1722.7
+ TFR.level  2   2944036 4744710 1849.5
<none>                   7688746 1932.9

Step:  AIC=1722.66
MMR ~ GDP + LifeExpBW

                Df Sum of Sq     RSS    AIC
+ LifeExpBW:GDP  1    246543 2133924 1704.9
<none>                       2380466 1722.7
+ TFR.level      2     45660 2334806 1723.2

Step:  AIC=1704.87
MMR ~ GDP + LifeExpBW + GDP:LifeExpBW

            Df Sum of Sq     RSS    AIC
<none>                   2133924 1704.9
+ TFR.level  2     21302 2112622 1707.0


```




```{r, include=FALSE}
lm2<- step(lm1,
           scope = list(
             upper = ~LifeExpBW*TFR.level*GDP
             ),
           direction = 'forward')
```

```{r, echo=FALSE, warning=FALSE}
#diagnostic
qqPlot(lm2,main='p3:Q-Q norm plot of residual of lm2')
residualPlots(lm2,main='p4:residualPlots of lm2')
```


##c

- When adding TFR.level to the model `lm3`,coefficients  of the model will not be statistically significant at 0.05 level. 
-[p5]The effect plot with different levels of TFR, controlling by GDP tell us there seems to be no relationship between `MMR` and `TFR.level`.
- Finally, I do not  decide adding TFR.level to my model.


```{r, echo=FALSE}
lm4 <- lm(formula = MMR ~ GDP + LifeExpBW + GDP:LifeExpBW+TFR.level:LifeExpBW, data = x)
plot(Effect(focal.predictors = c('TFR.level','LifeExpBW'),
            mod = lm4),
     main='p5:differences in MMR for \n countries with different levels of TFR \n controlling by GDP')
```


##Task 3


I use smooth spline with $df=3,8,12,16$ to fit `MMR` and `LifeExpBW`.With the increase of $DF$, the fitting degree of `MMR` and `LifeExpBW` is getting better and better. From the graph, we can see an intuitive curve, so that I think that the linear relationship between`MMR` and `LifeExpBW` is relatively weak.

```{r, echo=FALSE}
par(mfrow=c(2,2))
plot(smooth.spline(x = x$LifeExpBW,y=x$MMR,df = 3),type='l',main='smoothing splines with df=3',xlab='LifeExpBW',ylab='MMR',col='red',lty=1,lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,y=x$MMR,df = 8),type='l',main='smoothing splines with df=8',xlab='LifeExpBW',ylab='MMR',col='blue',lty=1,lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,y=x$MMR,df = 12),type='l',main='smoothing splines with df=12',xlab='LifeExpBW',ylab='MMR',col='orange',lty=1,lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,y=x$MMR,df = 16),type='l',main='smoothing splines with df=16',xlab='LifeExpBW',ylab='MMR',col='green',lty=1,lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
```

#Part 2


##Task 1

The equation corresponding to Model A is:
$$log(Occurrences)=log(n_i)+2.11021$$
where $log(n_i)$ is called an offset.

##Task 2

The equation corresponding to Model B is:
$$log(Occurrences)=log(n_i)+2.92585-2.36623 \cdot DCCTV$$
where $log(n_i)$ is called an offset.

- The Rate Ratio of intercept in Model B  is $e^2.92585=18.65$.
- The Rate Ratio of DCCTV in Model B  is $e^{-2.36623}=0.09$,which means that a one unit increase in `DCCTV` will have a multiplicative effect of size 0.09 on `Occurrences`.

##Task 3

- 95% confidence interval of intercept for the Rate Ratio estimated is $[2.92585-1.96*0.05178,2.92585+1.96*0.05178]$,which is equal $[2.82,3.03]$ .
- 95% confidence interval of DCCTV for the Rate Ratio estimated is $[-2.36623-1.96*0.14331,-2.36623+1.96*0.14331]$,which is equal $[-2.65,-2.09]$.

##Task 4

$\chi^2=545.4-110.6=434.8$ , $DF=51-50=1$ and then $p-value=1-pchisq(434.8,1)=0 <0.05$,which means **Model B is better than Model A**.


##Task 5


`Days` is a numeric variable, so that  I think it can be directly brought into the equation. And the equation corresponding to new model with adding $Days$ is:

$$log(Occurrences)=log(n_i)+\beta_0 + \beta_1 \cdot DCCTV + \beta_2 \cdot Days$$

where $log(n_i)$ is called an offset.


#Appendix R Code

```{r, eval=FALSE, include=T}
#-----------------------------------------
#part 1
#-----------------------------------------
library(ggplot2)
library(psych)
library(car)
library(effects)
x <- read.csv('MMRCountry.csv',
              header = T,
              stringsAsFactors = F,na.strings = -99)
x$TFR.level <- factor(x$TFR.level,
                      levels = c(0,1,2),
                      labels = c('Low','Medium','High'))
x <- na.omit(x)
# task1
#correlation analysis
pairs.panels(x[,c(2,3,5)],
             method = 'pearson')
#Analysis of Variance
summary(aov(MMR~TFR.level,data=x))
ggplot(data=x,
       aes(x=TFR.level,y=MMR,fill=TFR.level))+
  geom_boxplot(na.rm = T)
#task2
##a
ggplot(data=x,
       aes(x=GDP,y=MMR))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',na.rm = T,col=I('red'))
lm1 <- lm(MMR~GDP,data=x)
summary(lm1)
#diagnostic
qqPlot(lm1)
residualPlots(lm1)
##b
lm2<- step(lm1,
           scope = list(
             upper = ~LifeExpBW*TFR.level*GDP
             ),
           direction = 'forward')
#diagnostic
qqPlot(lm2)
residualPlots(lm2)
##c
lm4 <- lm(formula = MMR ~ GDP + LifeExpBW + GDP:LifeExpBW+TFR.level:LifeExpBW, data = x)
plot(Effect(focal.predictors = c('TFR.level','LifeExpBW'),
            mod = lm4))
summary(lm4)
##task3
cor(x$MMR,
    x$LifeExpBW,
    use='complete.obs'
    ,method ='spearman')
x$ty <- ifelse(x$LifeExpBW>70,'a','b')
ggplot(data=x,aes(x=LifeExpBW,y=MMR))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',
              na.rm = T,
              aes(col=ty,group=ty),
              show.legend = F)
par(mfrow=c(2,2))
plot(smooth.spline(x = x$LifeExpBW,
                   y=x$MMR,df = 3),
     type='l',
     main='smoothing splines with df=3',
     xlab='LifeExpBW',
     ylab='MMR',
     col='red',
     lty=1,
     lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,
                   y=x$MMR,df = 8),
     type='l',
     main='smoothing splines with df=8',
     xlab='LifeExpBW',
     ylab='MMR',
     col='blue',
     lty=1,
     lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,
                   y=x$MMR,df = 12),
     type='l',
     main='smoothing splines with df=12',
     xlab='LifeExpBW',
     ylab='MMR',
     col='orange',
     lty=1,
     lwd=2)
points(x = x$LifeExpBW,y=x$MMR)
plot(smooth.spline(x = x$LifeExpBW,
                   y=x$MMR,df = 16),
     type='l',
     main='smoothing splines with df=16',
     xlab='LifeExpBW',
     ylab='MMR',
     col='green',
     lty=1,
     lwd=2)
points(x = x$LifeExpBW,y=x$MMR)

```


