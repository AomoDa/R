---
title: "STAT6083 Generalised Linear Models"
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
x <- read.csv('C://Users//AomoDa//Documents//MMRCountry.csv',
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



```{r}
pairs.panels(x[,c(-1,-4)],method = 'pearson')
ggplot(data=x,aes(x=TFR.level,fill=TFR.level))+
  geom_bar(show.legend = F)+
  labs(title='TFR level')
```


##Task 2

###a


- $R_{adj}=0.1443$,the adjusted rsquared is very small, which means that the model fitting is very bad.
- The residual error of the model seems to be dissatisfied with the normal distribution,which does not accord with the basic assumptions of linear regression.
- There is some connection between the residuals of the model and the fitted value, which does not accord with the basic assumptions of linear regression.
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
qqPlot(lm1)
residualPlots(lm1)
```

###b

- Use forward stepwire regression algorithm to fit better model $lm2$,which is `lm(formula = MMR ~ GDP + LifeExpBW, data = x)` .But I find `GDP` in $lm2$ is not statistically significant at 0.05 level.
- And then I dorp ``GDP` to fit model $lm3$,which is `lm(formula = MMR ~ LifeExpBW, data = x)`. And I find all variables are statistically significant at 0.05 level.
- The pvalue of F-test of two model is 0.1265,which means $lm3$ is better.
- The coefficients of `LifeExpBW` in `lm3` is -18.64,which means  `MMR` will be reduced by 18.64 with `LifeExpBW` per 1 increase.
- The coefficients of Intercept in `lm3` is 1510.23,which means  `MMR` will be 1510.23 when $LifeExpBW=0$.
- $R_{adj}=0.7316$,which is far greater than model `lm1`.
- The residual error of the model `lm3` seems to be dissatisfied with the normal distribution,which does not accord with the basic assumptions of linear regression.
- There is some connection between the residuals of the model `lm3` and the fitted value, which does not accord with the basic assumptions of linear regression.



The equation of the new model is 
$$MMR=1510.23 -18.64 \cdot LifeExpBW$$


Analysis of Variance Table

``Model 1: MMR ~ GDP + LifeExpBW``

``Model 2: MMR ~ LifeExpBW``

------

Model|Res.Df| RSS| Df |Sum of Sq  |F |Pvalue
-----|-----|-----|-----|----------|------|----
1  |  178| 2380466  |                         
2  |  179 |2411994| -1 | -31528| 2.3575 |0.1265

-----




```{r, include=FALSE}
lm2<- step(lm1,
           scope = list(
             upper = ~LifeExpBW+TFR.level+GDP
             ),
           direction = 'forward')
```

```{r, echo=FALSE, warning=FALSE}
lm3 <- lm(MMR~LifeExpBW,data=x)
ggplot(data=x,aes(x=LifeExpBW,y=MMR))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',na.rm = T,col=I('orange'))+
  labs(title='MMR vs LifeExpBW')
#diagnostic
qqPlot(lm3)
residualPlots(lm3)
```


##c

- When adding TFR.level to the model `lm3`,coefficients  of the model will not be statistically significant at 0.05 level. 
-The effect plot with different levels of TFR, controlling by GDP tell us there seems to be no relationship between `MMR` and `TFR.level`.
- Finally, I do not  decide adding TFR.level to my model.


```{r}
lm4 <- lm(formula = MMR ~ TFR.level:LifeExpBW+GDP, 
          data = x)
plot(Effect(focal.predictors = c('TFR.level','LifeExpBW'),
            mod = lm4))
```


##Task 3


I use smooth spline with $df=3,8,12,16$ to fit `MMR` and `LifeExpBW`.With the increase of $DF$, the fitting degree of `MMR` and `LifeExpBW` is getting better and better. From the graph, we can see an intuitive curve, so that I think that the linear relationship between`MMR` and `LifeExpBW` is relatively weak.

```{r}
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
           scope = list(upper = ~LifeExpBW+TFR.level+GDP),
           direction = 'forward')
summary(lm2)
lm3 <- lm(MMR~LifeExpBW,data=x)
summary(lm3)
anova(lm2,lm3)
ggplot(data=x,
       aes(x=LifeExpBW,y=MMR))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',na.rm = T,col=I('orange'))
#diagnostic
qqPlot(lm3)
residualPlots(lm3)
##c
lm4 <- lm(formula = MMR ~ TFR.level:LifeExpBW+GDP, data = x)
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


