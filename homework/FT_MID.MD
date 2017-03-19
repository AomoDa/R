---
title: 'mid '
author: 'Author : Your Name'
date: '2017-03-19'
output:
  word_document: 
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Q1 Correlation Coefficient 

##a

The unadjusted correlation coefficient between final SBP and baseline SBP is **0.7955**.

```{r, message=FALSE, warning=FALSE}
# loading sbp.csv into R
sbp <- read.csv(file = 'C://Users//AomoDa//Documents//sbp.csv',header = T)	
# a
with(sbp,cor(FinalSBP,BslSBP,method = 'pearson'))
```


##b

$H_0: \rho=0$ ,That is there is no association between final SBP and baseline SBP.

$H_1: \rho \neq 0$ ,That is there is  association between final SBP and baseline SBP.


First compute t statistic $$t=\frac{\rho \sqrt{n-2}}{\sqrt{1-\rho^2}}=\frac{0.7955 \sqrt{40-2}}{1-0.7955^2}=8.0920$$


And $$df=40-2=38$$.


Finally compute the p value is $P_{Value}=0 < 0.05$,which is statistically significant at 0.05 level .So we should reject the null hypothesis $H_0$, and my conclusion is that there is  association between final SBP and baseline SBP .

##c

First convert from $\rho$ to $z_r$, $$z_r=\frac{1}{2} \frac{1+\rho}{1-\rho}=\frac{1}{2} \frac{1+0.7955}{1-0.7955}=1.0862$$.


And then  find CI for $z_r$ ,$$z_r \pm 1.96\sqrt{1/(n-3)}= 1.0862 \pm 1.96\sqrt{1/(40-3)} \in [ 0.7639,1.4084 ]$$.

Finally convert confidence limits from $z_r$ back to $\rho$ ,$$\rho= \frac{e^{2z_r}-1}{e^{2z_r}+1} \in [ 0.6434,0.8872 ]$$

The two-sided 95% confidence interal of the correlation coefficien is $[ 0.6434,0.8872 ]$


#Q2


$H_0: u_1 = u_2$ ,That is the mean of baseline SBP with different treatment groups  are the same.

$H_1: u_1 \neq u_2$ ,That is the mean of baseline SBP with different treatment groups  are different.


And the $t = 0.30285$,$df = 30.665$ and $p-value = 0.7641 >0.05$ .

The p value is not statistically significant at 0.05 level,which means that we cann't reject the null hypothesis $H_0$, and my conclusion is that tthe mean of baseline SBP with different treatment groups  are the same.

```{r}
t.test(BslSBP~treat,data=sbp)
```

#Q3


The result is follow :

-----

treatment groups |sample size| mean | standard deviation 
-----------------|-----------|------|-------------------
HTN-711|27|-10.6494|4.3800
Placebo|13|-4.8165|4.7776
Total| 40 |-8.7537| 5.2410


-----

```{r}
#calculate this change variable  from BSLSBP and FINSLSBP
sbp$change <- sbp$FinalSBP-sbp$BslSBP

#for each treatment 
aggregate(change~treat,data=sbp,FUN = length)
aggregate(change~treat,data=sbp,FUN = mean)
aggregate(change~treat,data=sbp,FUN = sd)

#both treatments combined. 
length(sbp$change);mean(sbp$change);sd(sbp$change)
```

#Q4 reference cell dummy coding

##a 


$H_0: u_1 = u_2$ ,That is there are no effect on SBP with different treatment groups.

$H_1: u_1 \neq u_2$ ,That is there are some effect on SBP with different treatment groups.


$F\_Value =16.48 $ and $P\_Value=0.0005 < 0.05$.The p value is very statistically significant at 0.05 level,which means that we should reject the null hypothesis $H_0$, and my conclusion is that there are some effect on SBP with different treatment groups.


```{r}
sbp$dummy.treat <- ifelse(sbp$treat==1,1,0)
lm1 <- lm(change~dummy.treat,data=sbp)
summary.aov(lm1)

# what this means in terms of treatment effect on SBP

```


##b


The estimates of the regression coefficients that my computer program calculated are follow:


-----

Items| Estimate| Std. Error| t value |P value
-----|----------|----------|---------|----------
(Intercept) |  -4.817  |    1.251|  -3.851| 0.000438
dummy.treat |  -5.833  |    1.522 | -3.832| 0.000464 

-----

The equation is that $$change=-4.817  -5.833P(treat=1)$$.And SO when treat is `HTN-711` the change would be $-4.817  -5.833=-10.65$ and when when treat is `Placebo` the change would be $-4.82$.


```{r}
#egression coefficients
summary(lm1)
```

##c


The prediction interval and the confidence interval of the predicted value for a patient from each treatment group are follow : 

-----

patient|treat| real value | predict value | prediction interval|confidence interval 
-----|-------|-----|---------------|--------------------|-------------------
120|1|-12.0816|-10.6494|[-19.9457,-1.3532]|[-12.4062,-8.8926]
130|2| -5.8106|-4.8165|[-14.2899,4.6568]|[ -7.3484,-2.2847]

-----

```{r}
# choose a patient from each treatment group
sbp.test <- sbp[c(2,3),]
# the confidence interval 
predict.lm(object = lm1,newdata = sbp.test,interval = 'conf',level = 0.95)
# the prediction interval
predict.lm(object = lm1,newdata = sbp.test,interval = 'pred',level = 0.95)
```


#Q5 population average coding


##a

$H_0: u_1 = u_2$ ,That is there are no effect on SBP with different treatment groups.

$H_1: u_1 \neq u_2$ ,That is there are some effect on SBP with different treatment groups.


$F\_Value =16.48 $ and $P\_Value=0.0005 < 0.05$.The p value is very statistically significant at 0.05 level,which means that we should reject the null hypothesis $H_0$, and my conclusion is that there are some effect on SBP with different treatment groups. 

**The result is the same as Q4.**



```{r}
#a
sbp$pop.treat <- ifelse(sbp$treat==1,1,-1)

lm2 <- lm(change~pop.treat,data=sbp)
summary.aov(lm2)

```


##b

The estimates of the regression coefficients that my computer program calculated are follow:


-----

Items| Estimate| Std. Error| t value |P value
-----|----------|----------|---------|----------
(Intercept) |  -4.817  |    1.251|  -3.851| 0.000438
dummy.treat |  -5.833  |    1.522 | -3.832| 0.000464 

-----

The equation is that $$change=-7.7330  -2.9164P(treat=1) +2.9164P(treat=2) $$.And SO when treat is `HTN-711` the change would be $-7.7330  -2.9164=-10.65$ and when when treat is `Placebo` the change would be $-4.82$.

**The result is the same as Q4.**

```{r}
summary(lm2)
# choose a patient from each treatment group
sbp.test <- sbp[c(2,3),]
```

##c

-----

patient|treat| real value | predict value | prediction interval|confidence interval 
-----|-------|-----|---------------|--------------------|-------------------
120|1|-12.0816|-10.6494|[-19.9457,-1.3532]|[-12.4062,-8.8926]
130|2| -5.8106|-4.8165|[-14.2899,4.6568]|[ -7.3484,-2.2847]

-----

**The result is the same as Q4.**

```{r}
# the confidence interval 
predict.lm(object = lm2,newdata = sbp.test,interval = 'conf',level = 0.95)
# the prediction interval
predict.lm(object = lm2,newdata = sbp.test,interval = 'pred',level = 0.95)
```

#Q6

##a


- Residual standard error: 4.533.
- djusted R-squared:  0.2518
- F-statistic: 5.376 on 3 and 36 DF,  p-value: 0.003666
- the regression coefficients and the p-values corresponding to each variable is follow :

-----

Items| Estimate| Std. Error| t value |P value
-----|----------|----------|---------|----------
Intercept|      -10.9665  |   1.4764 | -7.428| 0
treat=2|   5.9533  |   1.5345 |  3.880| 0.0004
site=2 |   1.7236  |   1.9380 |  0.889| 0.3797   
site=3 |  -0.4357 |    1.7355 | -0.251| 0.8032  

-----

```{r}
lm3 <- lm(change~as.factor(treat)+as.factor(site),data=sbp)
summary(lm3)
```


##b


$H_0: \beta=0$ ,That is there are no effect on SBP with different `SITE`.

$H_1: \beta \neq 0$ ,That is there are some effect on SBP with `SITE`.


For `site=2`,the $t-stat=0.889$ ,$df=40-2=38$and $p-value = 0.3797 > 0.05$,For `site=3`,the $t-stat=-0.251$,$df=40-2=38$ and $p-value = 0.8032 > 0.05$.The p values are  not statistically significant at 0.05 level,which means that we cann't reject the null hypothesis $H_0$, and my conclusion is that there are some effect on SBP with `SITE`.. 


##c


$H_0: \beta_{site}=0$ ,That is there are no effect on SBP with different `SITE`.

$H_1: \beta_{site} \neq 0$ ,That is there are some effect on SBP with `SITE`.


-----

Items | Df| Sum Sq |Mean Sq| F value |  P value |
------|-----|------|-------|---------|-----------
treat|  1 | 298.5|  298.55  | 14.53| 0.000521 
site |  2  | 32.9 |  16.44 |   0.80 |0.457201  

-----


The p values of `site` are  not statistically significant at 0.05 level,which means that we cann't reject the null hypothesis $H_0$, and my conclusion is that there are some effect on SBP with `SITE`.. 


```{r}
summary.aov(lm3)
```

##d

$H_0: \beta_{site}=0$ ,that is my linear regression model does not need to join the `site` variable

$H_1: \beta_{site} \neq 0$.that is my linear regression model  needs to join the `site` variable.

The p values of `site` are  not statistically significant at 0.05 level,which means that we cann't reject the null hypothesis $H_0$, and my conclusion is that my linear regression model does not need to join the `site` variable.


```{r}
# "reduced" model.
lm4 <- lm3 <- lm(change~as.factor(treat),data=sbp)
# "full"  model.
lm5 <- lm(change~as.factor(treat)+as.factor(site),data=sbp)
# partial F test
anova(lm4,lm5,test='F')
```

##e

$$\sum(Y-\hat Y)^2=\frac{SSE}{df}$$

And the calculate the least squares mean of change in SBP for each of the two treatment groups ,the results are follow:

-----

treatment groups | least squares mean
-----------------|-------------------
`HTN-711` | 18.4742
`Placebo` | 21.0693


-----
