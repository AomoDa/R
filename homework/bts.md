---
title: "Untitled"
author: "Your Name"
date: "2016年11月10日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Part 1: Model diagnostics


description of CPS1985

-----

Min| 1st Qu|  Median|    Mean| 3rd Qu| Max
---|-------|--------|--------|-------|------
0.00|8.00|   15.00|   17.82|   26.00| 55.00 

-----

```{r, message=FALSE, warning=FALSE}
library(car)
data("CPS1985",package="AER")
rownames(CPS1985) <- 1:nrow(CPS1985)
str(CPS1985)
summary(CPS1985$experience)
par(mfrow=c(1,2))
hist(CPS1985$experience,breaks = 20,freq = F)
lines(density(CPS1985$experience),col='red',lty=2)
boxplot(CPS1985$experience,main='boxplot of experience',
     xlab='experience',ylab='experience')
par(mfrow=c(1,1))
```

##Task 1

###a


Based on the definition of the variables and the scatterplot matrix,  **age** and **experience**to be multicollinear,because $COR(age,experience)=0.98$,which indicate they are highly positive correlation.

```{r}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor=1)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(~log(wage)+education+age+experience,data=CPS1985,upper.panel = panel.cor)

```

###b

Fit model $lm1$, the t test p values of intercept,education and experience are both statistically significant.
And $$VIF(education)=1.142049 <4$$
$$VIF(experience)=1.142049 <4$$

VIF tell me there are not multicollinearity  with  my model $lm1$.

```{r}
lm1 <- lm(log(wage)~education+experience,data=CPS1985)
summary(lm1)
vif(lm1)
```

###c


Fit model $lm1$, the t test p values of intercept,education , experience  and age are not  both statistically significant.
And $$VIF(education)=229.5738 >4$$
$$VIF(experience)=5147.9190 >4$$
$$VIF(age)=4611.4008 >$$

VIF tell me there are  multicollinearity badly  with  my model $lm2$.

```{r}
lm2 <-lm(log(wage)~education+experience+age ,data=CPS1985)
summary(lm2)
vif(lm2)
```

##Task 2

###a and b

- InterceptT检验显著
- 数值型变量的T检验全部显著，(education,experience等等)
- 分类变量occupation = office and sales 的T检验不显著 as 0.05 level，其他都显著。
- $F-statistic:=30.05$,$p-value=0 <0.05$,方程整体显著。
- $R_{adj}^2=32.91%$,拟合结果并不好。

```{r}
lm3 <- lm(log(wage)~education+experience+gender+occupation+union ,data=CPS1985)
summary(lm3)
```

###c

Residual Plots and Curvature Tests for Linear Model Fits.

方程整体的Residual 似乎 fitted values 之间存在某种联系，这种联系似乎也存在和experience之间，如果需要删除变量，我可能会选择删除experience，或者删除occupation，因为occupation的T检验不显著。

```{r}
residualPlots(lm3)
```

##Task 3

###a

```{r}
qqPlot(lm3)
influenceIndexPlot(lm3)
avPlots(lm3)
```


###b

```{r}
lm4 <- update(lm3,formula. = .~.-occupation)
summary(lm4)
lm5 <- update(lm3,formula. = .~.-experience)
summary(lm5)
```


#Part 2

```{r}
library(foreign)
x <- read.spss('C://Users//AomoDa//Documents//SanduskyTemperature.sav',to.data.frame=T)
head(x)
```

##Task 4

-----

Item|Estimate| Std. Error| t value|Pr(>|t|)
----|--------|-----------|--------|-------
(Intercept)|  50.145855|   0.600117|  83.560| <2e-16 
t.sin |      -13.945078|   0.421934| -33.050|<2e-16 
t.cos|       -18.214306|   0.420795| -43.285|<2e-16 
time.idx|      0.006018|   0.008615|   0.699|0.486   

-----

```{r}
plot(x$time.idx,x$avg7447,type='b',col='red')
# annual cycle
t.sin <- sin(x$time.idx/12*2*pi)
t.cos <- cos(x$time.idx/12*2*pi)
lm.ts <- lm(avg7447~t.sin+t.cos+time.idx,data=x)
summary(lm.ts)
```

##Task 5

$$a=\sqrt{\beta_{cos}^2+\beta_{sin}^2}=22.94803$$


```{r}
t.sin <- -13.945078
t.cos <- -18.214306 
sqrt(t.sin ^2+t.cos^2)
```


##Task 6

Durbin-Watson Test for Autocorrelated Errors



-----

lag|Autocorrelation|D-WStatistic|pvalue
---|---------------|-------------|-----
1|0.35668803|1.241555|0.000
2|0.29921757|1.324992|0.000
3|0.11957449|1.668253|0.086
4|0.15550463|1.593974|0.078
5|0.01334885|1.874173|0.812
6|0.06983478|1.748841|0.396

-----  



根据autocorrelation and partial autocorrelation plots的结果来看，time series部门在一定程序上存在自相关，acf plot 显示，lag=2内存在自相关。



```{r}
#Durbin-Watson Test for Autocorrelated Errors
durbinWatsonTest(lm.ts,max.lag = 6)
#autocorrelation and partial autocorrelation plots
acf(residuals(lm.ts))
pacf(residuals(lm.ts))
```

