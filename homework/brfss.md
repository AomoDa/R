---
title: "Exploring the BRFSS data"
author: "AomoDa"
date: "2016-08-25"
output: 
    html_document:
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

 
# Part 1
## Data  Describe

 * The Behavioral Risk Factor Surveillance System (BRFSS) is the nation's premier system of health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services. BRFSS is used to collect prevalence data among adult U.S. residents regarding their risk behaviors and preventive health practices that can affect their health status. Respondent data are forwarded to CDC to be aggregated for each state, returned with standard tabulations, and published at year's end by each state.
 
 * The BRFSS is a cross-sectional telephone survey conducted by state health departments with technical and methodological assistance provided by the CDC. In addition to all 50 states, the BRFSS is also conducted by health departments in The District of Columbia, Guam, Puerto Rico, and the U.S. Virgin Islands.
 
  * By collecting behavioral health risk data at the state and local level, BRFSS has become a powerful tool for targeting and building health promotion activities. As a result, BRFSS users have increasingly demanded more data and asked for more questions on the survey

##Answer

 * Data used the random sampling not general survey.
 
# Part 2

## Q1

### question
How do the health status of the informants distribute in the data of BRFSS? How do the health status of the male and female distribute?

### Why am I interested in Q1? 
  
Because the health status of the informants will influence my result analysis, so I must know how the data distribute. 
   
##Q2 

### question
In recent month, how do the lasting days of the informants' poor physical or mental health distribute? Do the distributions of the male and female are the same?

### Why am I interested in Q2?

Because I want to know the distribution of health status further.


##Q3
    
### question

* How do the sleep time of the informants distribute? Is the sleep time subject to normal distribution?
   
* What is the average sleep time of male that have diverse health status?
What is the average sleep time of female that have diverse health status?
   

### Why am I interested in Q3?
  
According to normal experience, sleep time is closely linked to the health status, and I wwant to know that how the sleep time influence the health status.   
    
#Part 3

## Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(car)
```

##Load data and select data
```{r}
setwd('C://Users//AomoDa//Documents')
load('brfss2013.RData')
# sex
# Indicate sex of respondent.
# genhlth
# Would you say that in general your health is:
# poorhlth
# During the past 30 days, for about how many days did poor physical or mental health keep you from doing your usual activities, such as self-care, work, or recreation?
mydata <- brfss2013[,c('sex','genhlth','poorhlth','sleptim1')]
mydata <- na.omit(mydata)
```

## Data Summarise
  
*  Form the collected data of informants, the number of male is 89794 and the number of female is 153827. The number of female is much more than the number of male.   
    
*  Classifying the data set,the number of excellent is 25815, the number of very good is 67621, the number of good is 76818, the number of fair is 49126, the number of poor is 24241.Most people think their heath condition is better than mean level. 
    
* During the past 30 days, for about how many days did poor physical or mental health keep you from doing your usual activities, such as self-care, work, or recreation. Minimun is 0, Mediam is 0, Mean is 5.195, Quartile 3 is 5.0, Quartile 1 is 5.

* For sleep time,Minimun is 1, Mediam is 7, Mean is 6.921, Quartile 3 is 8 , Quartile 1 is 6.

 
```{r}
summary(mydata)
```

## Q1

* We can draw a conclusion form the bar chart of the genhlth that the health status of good is the most and the health status is the second. The whole distribution of genhlth is similar to the Normal Distribution.  
     
* We can draw a conclusion form the bar chart of the male and female genhlth that the male and the female evaluate their health condition maybe almost the same, because the thendency of their bar chat are almost the same.

* The P value of chi-square test is less than 0.05, it means that the test result is very notable. We can draw a conclusion that the male and the female evaluate their health condition are not the same.  
     
* We can draw a conclusion from the Mosaic Plot, the probability of the male evaluate their health condition is good and poor is higher than the female and the probability of the very good is higher than the male, other  conditions are not notable.

```{r}
ggplot(data=mydata,aes(x=genhlth))+geom_bar()
ggplot(data=mydata,aes(x=genhlth,fill=sex))+geom_bar(position='dodge')
mytable<-with(data = mydata,expr = table(genhlth,sex))
mytable
summary(mytable)
mosaicplot(mytable,shade = T)

```

## Q2

independent-sample T test

null hypothesis:The average of the male is equal to the average of the female

alternative hypothesis: The average of the male is not  equal to the average of the female


* The number of 0 day is 140826, is about 56.9%. The number of 30 days is 22152.

* Comparing the density plot of the male and the female, the whole distribution is almost the same.

* The P value of independent-sample T test is 0.02883 that less than 0.05 and it means that the test result is very notable. We can draw a conclusion that the average of male poor health lasting days is higher than the female. The average of the male is 5.250 days and the average of the female is 5.163 days.





```{r}
table(mydata$poorhlth)
ggplot(data = mydata,aes(x=poorhlth))+geom_density()
ggplot(data = mydata,aes(x=poorhlth,fill=sex))+geom_density(alpha=0.7)+facet_grid(sex~.)
with(data = mydata,expr = t.test(poorhlth~sex))


```


##Q3

 * According to the qqplot and histogram, we can draw a conclusion that sleep time is not subject to normal distribution.
 
 * The average sleep time of the male whose health status are excellent is 7.00 hours. And the average sleep time of the female whose health status are excellent is 7.11 hours and so on...


```{r}
ggplot(data=mydata,aes(x=sleptim1))+geom_histogram(binwidth = 1,col='white')+xlim(0,24)
qqPlot(mydata$sleptim1,ylab='')
aggregate(sleptim1~sex+genhlth,data=mydata,FUN = mean)
```

