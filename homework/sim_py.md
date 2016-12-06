---
title: "Untitled"
author: "Your Nmae"
date: "2016-12-06"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Q1

#### F test to compare two variances

Var test 

$$H_0:\sigma^2_{survived} = \sigma^2_{perished}$$
$$H_0:\sigma^2_{survived} \neq \sigma^2_{perished}$$


The $F = 2.3938, num df = 35, denom df = 23, p-value = 0.03099 <0.05$,which indicated that we should reject $H_0$ and $\sigma^2_{survived} \neq \sigma^2_{perished}$.

```{r}
#loading data
survived <- read.csv('C://Users//mali//Documents//survived.txt')
perished <- read.csv('C://Users//mali//Documents//perished.txt')
survived <- as.vector(survived$survived)
perished <- as.vector(perished$perished)
#F test to compare two variances
var.test(survived,perished)
```

####Welch Two Sample t-test

Welch Two Sample t-test

$$H_O:u_{survived} = u_{perished}$$
$$H_O:u_{survived} \neq u_{perished}$$

$t = -1.2792, df = 57.969, p-value = 0.2059 >0.05$,which indicated that we can not reject $H_0$ and $u_{survived} = u_{perished}$ at 0.05 level,where$u_{survived}=25.67500$ and $u_{perished}=26.15833$


```{r}
# Welch Two Sample t-test
t.test(survived,perished,
       alternative = 'two.sided',
       var.equal = F)
```

##Q2


####a

one-way ANOVA test:

$$H_O:u_1 = u_2$$
$$H_O:u_1 \neq u_2$$


The p value of one-way ANOVA test is 0.000218,which is very statistically significant at 0.05 levle .We shoul reject $H_0$ and the samples from two classes have different mean
concentration of white blood cell. $u_1=9.23$ and $u_2=5.77$(smoker:class1; non-smoker:class2).

```{r}
#loading data
smoker <- read.csv('C://Users//mali//Documents//smoking.txt',
                   sep='\t')
#one-way ANOVA
summary(aov(concentration~class,data=smoker))
```


####b

Permutation Test

I use permutation test to compute  95% confidence interval of  two classes  different mean  is $[-2.008865,2.064373]$.
But two classes  different mean in our data is $3.459312 \not\in [-2.008865,2.064373]$ .My conclusion is that the samples from two classes have different mean concentration of white blood cell.


```{r}
table(smoker$class)
aggregate(concentration~class,
          data=smoker,mean)

set.seed(100)
diff_mean <- numeric(10000)
for (i in 1:10000) {
  ind <- sample(nrow(smoker),
                size = 9,
                replace = F)
  diff_mean[i] <- mean(smoker$concentration[ind])-mean(smoker$concentration[-ind])
}
quantile(diff_mean,c(0.025,0.975))
```

