---
title: "Untitled"
author: "Your Name"
date: "2016年11月12日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#5.4

```{r}
a <- c(-0.70,5.60,2.00,2.80,0.70,3.50,4.00,5.80,7.10,-0.50,
       2.50,-1.60,1.70,3.00,0.40,4.50,4.60,2.50,6.00,-1.40)
b <- c(3.70,5.60,5.00,5.20,0.80,0.20,0.60,3.40,6.60,-1.10,
       6.00,3.80,2.00,1.60,2.00,2.20,1.20,3.10,1.70,-2.00)
summary(a)
summary(b)
```


##Q1

###Shapiro-Wilk Normality Test

两组p值均大于0.05，无法拒绝$H_0$，因此认为试验组和对照组均服从正态分布。

```{r, warning=FALSE}
#Shapiro-Wilk Normality Test
shapiro.test(a)
shapiro.test(b)
```

###Kolmogorov-Smirnov Tests

两组p值均大于0.05，无法拒绝$H_0$，因此认为试验组和对照组均服从正态分布。

```{r, warning=FALSE}
#Kolmogorov-Smirnov Tests
ks.test(a,'pnorm',mean(a),sd(a))
ks.test(b,'pnorm',mean(b),sd(b))
```

###Pearson's Chi-squared Test

两组p值均大于0.05，无法拒绝$H_0$，因此认为试验组和对照组均服从正态分布。

```{r, warning=FALSE}
#Pearson's Chi-squared Test
aa <- table(cut(a,br=c(min(a),-3,0,3,6,max(a))))
ap <- pnorm(c(-3,0,3,6,max(a)),mean(a),sd(a))
ap<-c(ap[1],ap[2]-ap[1],ap[3]-ap[2],ap[4]-ap[3],1-ap[4])
chisq.test(aa,p=ap)

#---------------------------------------
bb <- table(cut(a,br=c(min(a),-3,0,3,6,max(a))))
bp <- pnorm(c(-3,0,3,6,max(a)),mean(a),sd(a))
bp<-c(bp[1],bp[2]-bp[1],bp[3]-bp[2],bp[4]-bp[3],1-bp[4])
chisq.test(bb,p=bp)

```


##Q2

### Two Sample t-test

p值大于0.05，无法拒绝$H_0$，因此认为试验组和对照组的平均血糖下降浓度相等。
```{r, warning=FALSE}
# Two Sample t-test
t.test(a,b,var.equal = T)
```


###Welch Two Sample t-test

p值大于0.05，无法拒绝$H_0$，因此认为试验组和对照组的平均血糖下降浓度相等。

```{r, warning=FALSE}
#Welch Two Sample t-test
t.test(a,b,var.equal = F)
```

###Paired t-test

p值大于0.05，无法拒绝$H_0$，因此认为试验组和对照组的平均血糖下降浓度相等。

```{r, warning=FALSE}
#Paired t-test
t.test(a,b,paired = T)
```


##Q3

###F test to compare two variances

p值大于0.05，无法拒绝$H_0$，因此认为试验组和对照组的血糖下降浓度的方差是相等。

```{r, warning=FALSE}
#F test to compare two variances
var.test(a,b)
```

