---
title: "Assessment 2"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1


- First of all, I have drawn some diagrams about fullrepay with  bal and min.I find that with the increase in the outstanding balance and the minimum payment required, people is more inclined to pay the installment. And use  two sample t-test ,I find people who is a full repayment have  lower the outstanding balance and the minimum payment required, because of the t test value is very statistical significance ,which indicated that we can not reject the null hypothesis.
- Next I fit a Logistic Regression,bal and min are both very statistical significance. And  for every bal increase, the odds of passing the false belief task increase by 1.0  and 95% CI [1.000 ,1.001]; For every min increase, the odds of passing the false belief task increase by 0.965 and  95% CI [0.958 , 0.972].
- The accuracy of m1 is (4816+1640)/9743=66.26%.
- At the end of my analysis,I use lsmeans to test my model and finnal I have  proved that the analysis process is correct.



```{r, message=FALSE, warning=FALSE}
#Logistic Regression

# loading packages and data
library(plyr)
library(lattice)
library(lsmeans)
library(ggplot2)
x <- read.csv('C://Users//AomoDa//Documents//credit_cards.csv',
              header=T,stringsAsFactors = F,na.strings='NA')
str(x)
summary(x)
#Handle Missing Values in my data
x <- na.omit(x)
# group by fullrepay
ddply(.data =x,.variables = .(fullrepay),
     .fun = summarise,bal=mean(bal),min=mean(min))
# scatter plot 
xyplot(fullrepay~bal,data=x,
       jitter.x=T,jitter.y=T,
       main='fullrepay~bal')
xyplot(fullrepay~min,data=x,
       jitter.x=T,jitter.y=T,
       main='fullrepay~min',col='red')
# boxplot
ggplot(data=x,aes(x=fullrepay,y=bal,
                  fill=as.factor(fullrepay)))+
  geom_boxplot()+lims(y=c(0,4000))+
  labs(title='fullrepay~bal')

ggplot(data=x,aes(x=fullrepay,y=min,
                  fill=as.factor(fullrepay)))+
  geom_boxplot()+lims(y=c(0,200))+
  labs(title='fullrepay~min')

#Welch Two Sample t-test
with(x,t.test(bal~fullrepay))
with(x,t.test(min~fullrepay))

# glm fit model Logistic Regression
m1 <- glm(fullrepay ~ bal+min, 
          family = binomial, data =x)
summary(m1)
#Confidence Intervals on Coefficients
coef(m1)
confint(m1)
(coefs.with.CIs <- cbind(coef(m1), confint(m1)))
#For every bal increase, the odds of passing the false belief task increase by 1.0  and 95% CI [1.000 ,1.001].
#For every min increase, the odds of passing the false belief task increase by 0.965 and  95% CI [0.958 , 0.972].
round(exp(coefs.with.CIs),3)
# accuracy
pred <- ifelse(m1$fitted.values>0.5,1,0)
table(x$fullrepay,pred)

#lsmeans
plot(lsmeans(m1, ~bal,
             at = list(bal = seq(100, 4000, 200)), 
             type = "response"))
plot(lsmeans(m1, ~min,
             at = list(min = seq(5, 200, 10)), 
             type = "response"))

```


#Q2


- Use "Pearson's Chi-squared Test for Count Data",the p value is both  very statistical significance,which indicated the victim affect the jurors' decisions.
- Fit poisson regression .
- The interaction of single term deletions with chisq test is telling us about how relative frequency of guilty and not guilty verdicts  depends on  victim fault and  victim moral character.
- The odds of a guilty verdict depends on victim fault chisq=7.76,df=1 and p=0.005 The odds of a guilty verdict increase from 1.38 to 6.38 as victim faultincreases.
- The odds of a guilty verdict depends on victim moral character chisq=8.8 df=2 and p=0.012 The odds of a guilty verdict increase from 1.53 to 4.33 as victim moral character increases.



```{r, message=FALSE, warning=FALSE}
#Poisson Regression

# load packages
library(tidyr)
# my data
(verdict <- data.frame(fault=c(rep('Low',3),rep('High',3)),
                      moral_character=rep(c('Low','Neutral','High'),2),
                      verdict =c(rep('Guilty',6),rep('NOT Guilty',6)),
                      freq=c(32,79,42,17,65,23,8,12,4,24,41,11)))
#Pearson's Chi-squared Test for Count Data
tb_fault <- xtabs(freq~fault+verdict,
              data=verdict)
tb_fault
summary(tb_fault)
tb_moral_character <- xtabs(freq~moral_character+verdict,
              data=verdict)
tb_moral_character
summary(tb_moral_character)

# fit model Poisson Regression
m2 <- glm(freq ~ (fault + moral_character) * verdict, 
          family = poisson,data = verdict)
summary(m2)
#NHST Approach
drop1(m2, test = "Chisq")

#Anova Tables
anova(m2)

# odds and Reporting, NHST
#victim fault
tb_fault
tb_fault[,1]/tb_fault[,2]

#victim moral character
tb_moral_character
tb_moral_character[,1]/tb_moral_character[,2]

#Estimation Approach
(m2.lsm <- lsmeans(m2, ~(fault + moral_character) * verdict, 
                   type = "response"))
plot(m2.lsm)

```

