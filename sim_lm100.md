---
title: "linear"
author: "Your Nmae"
date: "2017年2月3日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#a


Coefficients:

-----

Variables| Estimate| Std. Error| t value| P value
---------|----------|-----------|--------|---------
(Intercept)|  -0.6296  |   2.9117 | -0.216 | 0.82956    
Pairs    |     2.1339  |   0.5173|   4.125 | 0.00012 
SizeS    |    -3.9269   |  2.3435 | -1.676|  0.09919   
StatusR |      3.5300  |   2.5679 |  1.375 | 0.17453  

-----



Hypothesis Test :T-Test

$H_0$ : have no effect on time

$H_1$ : have some effect on time

The p value is 0.1281 ,which is not statistically significant and means there is no effect on `Time`.

```{r, message=FALSE, warning=FALSE}
library(Sleuth3)
data("ex1027")

# fit model
lm1 <- lm(Time~Pairs+Size+Status,data=ex1027)
summary(lm1)

# t.test
# H0: have some effect on time
# H1: have no effect on time
t.test(Time~Size,data=ex1027)
```

#b

Hypothesis Test :T-Test

$H_0$ : have no effect on time

$H_1$ : have some effect on time

The p value is 0.1077 ,which is not statistically significant and means that  I can reject null hypothesis  and  there is no effect on `Time`.

```{r}
t.test(Time~Size,data=ex1027,subset = Status=='M')
```

#c

confidenct  interval :


-----

fit   |     lwr   |   upr
------|-----------|------
3.978963 |-0.8424173 |8.800343

-----



```{r}
predict(object = lm1,
	newdata = data.frame(Pairs=4,Size='S',Status='M'),
	interval = 'conf',
	level = 0.95)

```

#d


prediction  interval :

-----

fit   |     lwr   |   upr
------|-----------|------
3.978963| -15.00604 | 22.96397

-----


```{r}
predict(object = lm1,
	newdata = data.frame(Pairs=4,Size='S',Status='M'),
	interval = 'pred',
	level = 0.95)
```

#e

Coefficients:

-----

Variables| Estimate |Std. Error| t value |P value
---------|----------|-----------|--------|--------
(Intercept)  |   5.1102   |  4.8673  | 1.050  |  0.298
Pairs      |     0.3228   |  1.3083  | 0.247  |  0.806
SizeS      |    -6.4163   |  4.3918  |-1.461   | 0.150
StatusR    |    -1.9762   |  4.6870 | -0.422 |   0.675
Pairs:SizeS   |  0.5781  |   1.0839 |  0.533  |  0.596
Pairs:StatusR  | 1.7888   |  1.3146  | 1.361   | 0.179

-----

Hypothesis Test :Analysis of Variance Table

$H_0$ : interactions are not  needed.

$H_1$ : interactions are  needed.


Model 1: Time ~ Pairs + Size + Status
Model 2: Time ~ Pairs + Size + Status + Size:Pairs + Status:Pairs

-----

Model|  Res.Df|   RSS| Df |Sum of Sq |     F | P value
-----|--------|------|-----|----------|--------|-------
1  |   58 |4880.8                           
2  |   56| 4686.3|  2 |   194.51| 1.1622| 0.3202

-----

The p value is 0.3202 ,which is not statistically significant and means interactions are not  needed。


```{r}
lm2 <- lm(Time~Pairs+Size+Status+Size:Pairs+Status:Pairs,data=ex1027)
summary(lm2)
anova(lm1,lm2)
```

#f

Analysis of Variance Table:

-----

Variables|Df |Sum Sq| Mean Sq| F value |   P value  
---------|-----|-----|-------|----------|---------
Pairs    |     1| 1688.4 |1688.40 |20.1760 |3.581e-05 
Size    |      1 | 251.0 | 251.01 | 2.9995 |  0.08879 
Status   |     1 | 159.0 | 159.02 | 1.9002 |  0.17353    
Pairs:Size  |  1  | 39.6 |  39.56 | 0.4727 |  0.49459    
Pairs:Status | 1  |155.0  |154.95 | 1.8517|   0.17904    
Residuals   | 56| 4686.3 |  83.68  

-----

```{r}
anova(lm2)
```

