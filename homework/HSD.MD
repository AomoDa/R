---
title: "hw5"
author: "Your Nmae"
date: "2017年3月13日"
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

#Q1

不赞同这个同学的说法。

这个同学混淆了p value 和 Confidence intervals的概念。
p value：以测量值为中心，在一定范围内，真值出现在该范围内的几率。一般设定在2σ，也就是95%，95%是通常情况下置信度（置信水平）的设定值。 
Confidence intervals：在某一置信度下，以测量值为中心，真值出现的范围。 我们在论文里经常看到CI，CI是置信区间，一定概率下真值得取值范围（可靠范围）称为置信区间。其概率称为置信概率或置信度（置信水平）。


#Q2

单单靠显著水平就得出这样的结论在实际中其实并不是非常妥当。该组实验没有考虑到其他影响的影响，没有控制变量分析和对照实验，因此结论并不具有说服力。另外，假设检验还需要计算一类和二类错误的概率等。


#Q3

YES,If there are only two groups, the results of a one-way ANOVA analysis would be the same as those of a two-sample t-test，which had  be proved by formula.I can simulate the process by following the procedure, and we can draw the same conclusion.


```{r}
for (i in 1:1e3) {
	set.seed(100)
  seeds <- round(runif(n=1,min = 0,max = 1e7),0)
	set.seed(seeds)
    aa <- data.frame(x=rnorm(n=200,mean = 10,sd = 10),y=1:2)
    a <- ifelse(t.test(x~y,data=aa)$p.value>0.05,0,1)
    b <- ifelse(summary(aov(x~y,data=aa))[[1]][1,5]>0.05,0,1)
    if(a!=b) print(seeds)
}
```

#Q4

```{r}
# write a function for t test
my.t.test <- function(mu1,mu2,s1,s2,n1,n2) {
    t.stat <- (mu1-mu2)/sqrt(s1^2/n1+s2^2/n2)
    t.df <- (s1^2/n1+s2^2/n2)^2 /  ( (s1^2/n1)^2/n1 + (s2^2/n2)^2/n2 )
    t0 <- pt(q = t.stat,df = t.df)
    # two sided
    t.p <- round(ifelse(t0 >0.5,1-t0,t0 )*2,4)
    return(data.frame(t.stat=t.stat,df=t.df,pvalue=t.p)) 
}
#contr1<-c(1,1,1,-3)
# Estimate of contrast
#contr1.est<-sum(contr1*x[,'avg'])
#contr1.se<- sqrt(sum(contr1^2/x[,'n']*x[,'s'])  )
#contr1.t<- contr1.est/contr1.se
#k<-4 #number of groups
#contr1.p<-2*pt(-abs(contr1.t),df=sum(x[,'n'])-k)
#Display 
#round(c(contr1.est, contr1.se, contr1.t, contr1.p),4)
x <- data.frame(n=c(73,105,240,1080),
	            avg=c(6.22,5.81,5.77,5.47),
	            s=c(1.62,1.43,1.24,1.31),
	            row.names = c('IC','Major','Minor','No')
	)
x
```

##d




```{r}

# For Bonferroni:
# m=6 contrasts, so compare p-values with α/m instead. At alpha=0.05 level, we need to compare with 0.05/6=0.0083

n1 <- sum(x$n[1:3])
n2 <- x$n[4]
mu1 <- sum(x$n[1:3] * x$avg[1:3])/n1
mu2 <- x$avg[4]  
s1 <- sqrt((sum(x$n[1:3] * x$avg[1:3]^2)  + sum((x$n[1:3]-1) * x$s[1:3 ]^2) - n1*mu1^2)  / (n1 -1))
s2 <- s2 <- x$s[4] 

# t.test
my.t.test(mu1 = mu1,mu2 = mu2,s1 = s1,s2 = s2,n1 = n1,n2 = n2)
```

##e

```{r}
n1 <- sum(x$n[2:4])
n2 <- x$n[1]
mu1 <- sum(x$n[2:4] * x$avg[2:4])/n1
mu2 <- x$avg[1]  
s1 <- sqrt((sum(x$n[2:4] * x$avg[2:4]^2)  + sum((x$n[2:4]-1) * x$s[2:4 ]^2) - n1*mu1^2)  / (n1 -1))
s2 <- s2 <- x$s[1] 

# t.test
my.t.test(mu1 = mu1,mu2 = mu2,s1 = s1,s2 = s2,n1 = n1,n2 = n2)
```

#Q5

- The pvalue of analysis of variance is statistically significant at 0.05 level,which indicated that the mean Ozone measurements of different months are different.
- LDA: 7-5,7-6,8-7,8-6,9-7,9-8 months are different at 0.05 level.
- bonferroni : 7-5,8-5,9-7,9-8 months are different at 0.05 level .
- dfr : 7-5,7-6,8-7,8-6,9-7,9-8 months are different at 0.05 level.
- HSD : 7-5,8-5,9-7,9-8 months are different at 0.05 level .


```{r}
data(airquality)
str(airquality)
# AOV
summary(aov(Ozone~as.factor(Month),
             data=airquality))
# LDA
pairwise.t.test(x =airquality$Ozone,
                g = airquality$Month,
                p.adjust.method = 'none')
#Bonferroni
pairwise.t.test(x =airquality$Ozone,
                g = airquality$Month,
                p.adjust.method = 'bonferroni')
# fdr
pairwise.t.test(x =airquality$Ozone,
                g = airquality$Month,
                p.adjust.method = 'fdr')
# Tukey
TukeyHSD(aov(Ozone~as.factor(Month),
             data=airquality),
         conf.level = 0.95)
```

#Q6


- The p-value for gender effect on systolic blood pressure with the blocking is not statistically significant at 0.05 level.
- the p-value for gender effect on systolic blood pressure without the blocking is not statistically significant at 0.05 level.
- The ANOVA F-test without blocking  would be equivalent to the independent two-sample t-test because each block has two subjects randomly assigned to the two groups.

```{r}
# sex 1=male, tox 1=toxemia, grmhem 1=hemorrhage
lowbwt <- read.table('C://Users//mali//Documents//lowbwt.txt',
                     header = T)
# with toxemia status as blocking variable
summary(aov(sbp~sex+tox,data = lowbwt))
summary(aov(sbp~sex,data = lowbwt))

#quivalent to the independent two-sample t-test
```

#Q7

- LSD : Medium-Small,Large-Small,Xlarge-Small are different at 0.05 level.
- HSD :Medium-Small,Large-Small,Xlarge-Small are different at 0.05 level.

```{r}
library(reshape)

response <- read.table('C://Users//mali//Documents//ResponseTime2.txt',
                       header = T)
melt.response <- melt(response)
aov.fit <- aov(value~variable,
               data=melt.response)
summary(aov.fit )

#LSD
with(melt.response,pairwise.t.test(x = value,
                                   g =variable,
                                   p.adjust.method = 'none' ))

#HSD
TukeyHSD(aov.fit,conf.level = 0.95)
```

