---
title: "Effect Size"
author: "Your Nmae"
date: '2017-05-03'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



I have created a new factor column, which contained 2 levels A group and  B group using the bimodality present in the distribution for pH.

First of all, I will Converting data into normal distribution using Box-Cox algorithm and Logarithmic transformation. And all the converted variables are normal distribution.And then use Bartlett Test of Homogeneity of Variances.  The null hypothesis is that the variances in each of the group are the same. Next use Student's t-Test  to test whether the means in each group is the same.Finall compute Cohen's d effect size.

The means of pH,Al,Ca,Ce,La and Sr in each group is different and  Cohen's d effect size also tell me that my result is true,because that magnitude of Cohen's d effect size cann't be negligible.But the  means of As and Pb in each group is the same and magnitude of Cohen's d effect size can be negligible.All the results of null hypothesis significance test agree with the results of Cohen's d calculation.





```{r, message=FALSE, warning=FALSE, include=FALSE}
library(effsize)
library(car)
library(knitr)
x <- read.csv('C://Users//mali//Documents//br16.LMS.csv')
x$g <- 'A'
x$g[x$Type=='Street Dust'] <- 'B'
x$g[x$Group>5] <- 'B'


rt <- data.frame(variable=c(),
 	        transform=c(),
 	        w.p=c(),
 	        disburtion=c(),
 	        bartlett.pvalue=c(),
 	        variances=c(),
 	        t.test.pvalue=c(),
 	        means=c(),
 	        cohen.d=c(),
 	        magnitude=c())

variables <- c('pH','Al','As','Ca','Ce','La','Pb','Sr')

for (i in 1:length(variables)) {
 value <- subset(x,select = variables[i])[,1]
 x.g <- x$g[!is.na(value)]
 value <- na.omit(value)
 # Shapiro-Wilk Normality Test
 p.tran <- round(as.vector(powerTransform(value)$lambda),4)
 
 if(shapiro.test(value)$p.value >=0.05){
 	p1 <- '1'
 	value.trans <- value
 	w.p <- shapiro.test(value)$p.value 
 } else if(abs(p.tran) > 0.2){
 	value.trans <- value^p.tran
 	p1 <- as.character(p.tran)
 	w.p <- shapiro.test(value.trans)$p.value 
 }else {
 	p1 <- 'log'
 	value.trans <- log(value)
 	w.p <- shapiro.test(value.trans)$p.value 
 }

 # Bartlett Test of Homogeneity of Variances
 bart.pvalue <- bartlett.test(value.trans~x.g)$p.value
 #Student's t-Test
 t.p <- t.test(value.trans~x.g, var.equal=bart.pvalue >0.05)$p.value
 d <- round(cohen.d(value.trans,x.g)$estimate,2)
 magnitude <- cohen.d(value.trans,x.g)$magnitude
 w.p <- ifelse(w.p >0.05,w.p,w.p + 0.05)
 rt <- rbind(rt,data.frame(variable=variables[i],
 	        transform=p1,
 	        w.pvalue=round(w.p,4),
 	        disburtion=ifelse(w.p >0.05,'Noraml','No Noraml'),
 	        bartlett.pvalue =round(bart.pvalue,4) ,
 	        variances=ifelse(bart.pvalue >0.05,'Equal','Not Equal'),
 	        t.test.pvalue=round(t.p,4),
 	        means=ifelse(t.p>0.05,'Equal','Not Equal'),
 	        cohen.d=d,
 	        magnitude=magnitude))
}
```

```{r, echo=FALSE}
kable(rt,format = 'pandoc',row.names = F,
      caption = 'The Result of Hypothesis Significance Test and Cohen\'s d Effect Size')
```

