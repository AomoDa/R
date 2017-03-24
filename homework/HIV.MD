---
title: "STAT500 Applied Statistics"
author: "Your Nmae"
date: '2017-03-24'
output:
  word_document: default
  html_document:
    toc: yes
ID: Your ID
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#1 Problem

**Topic**: How does the HIV infection rate change around the world?

Two specific questions:

- How is the distribution of HIV infection  rate around the world.
- Is there a relationship between HIV infection rate and population growth rate ?

#2 Plan


-----


Column| Short Name| Brief Description
------|-----------|------------------
1|Country|Country Name
2|CountryCode|Country Code
3|Region|Region
4|Population|Population, total
5|PopulationGrowth|Population growth (annual %)
6|PopulationDensity|Population density (people per sq. km of land area)
7|PopulationGrowthUrban|Urban population growth (annual %)
8|HIV|Prevalence of HIV, total (% of population ages 15-49)

-----

#3 Data


```{r}
#Read CSV file
fulldata <- read.csv(file = 'C://Users//mali//Documents//AppliedStatistics_Assignment2_GlobalIndicators.csv',
                     header = T,
                     stringsAsFactors = F)

#Extract chosen columns using names
mydata <- fulldata[,c('Country',
                      'CountryCode',
                      'Region',
                      'Population',
                      'PopulationDensity',
                      'PopulationGrowth',
                      'PopulationGrowthUrban',
                      'HIV')]

#Inspect data
head(mydata)
str(mydata)
```

#4 Analysis

## Analysis Question 1 


To answer question 1, I will use the following methods: 

- First descriptive statistics[1]:the mean, median, standard deviation and four quantiles of the  HIV infection  rate were calculated, respectively.
- Secondly plot the boxplot to o bserved data distribution of the  HIV infection  rate.
- And Next plot the histogram to o bserved data distribution of the  HIV infection  rate.
- Finally plot the Q-Q plot to observe whether the HIV infection  rate obey normal distribution and also do Shapiro-Wilk Normality Test.

```{r, message=FALSE, warning=FALSE}
# summary
summary(mydata$HIV)
# SD
sd(mydata$HIV,na.rm = T)
# Boxplot
boxplot(mydata$HIV,
        horizontal = T,
        ylim=c(0,10),
        main='BoxPlot of HIV')
# Histogram
hist(mydata$HIV,
     breaks =30,
     freq = F,
     main='Histogram of Hiv')
lines(density(mydata$HIV,
              na.rm = T,
              from = 0.001),
      col='orange')

# normality?
library(fBasics)
qqnormPlot(na.omit(mydata$HIV))
shapiro.test(na.omit(mydata$HIV))
```


## Analysis  Question 2

To answer question 2, I will use the following methods: 

- First Calculate the spearman correlation coefficient[2] of `HIV` and `PopulationGrowth`.
- And then Test for Association/Correlation Between Paired Samples of `HIV` and `PopulationGrowth`.
- Finally plot scatter[3] of `HIV` VS `PopulationGrowth`.


```{r, message=FALSE, warning=FALSE}
library(ggplot2)
cor(mydata$HIV,
    mydata$PopulationGrowth,
    use = 'complete.obs',
    method = 'spearman')

#Test for Association/Correlation Between Paired Samples
cor.test(mydata$HIV,
    mydata$PopulationGrowth,
    use = 'complete.obs',
    method = 'spearman')
# plot
ggplot(data=mydata,
       aes(x=PopulationGrowth,y=HIV))+
  geom_point(na.rm = T)+
  geom_smooth(se=F,method = 'lm')+
  lims(x=c(-1,5))+
  labs(title='HIV VS PopulationGrowth')
```

#5 Conclusion

##5.1 For Question 2 

How is the distribution of HIV infection around the world?

- There are 72 missing values in my dataset about 39.8% of the total
- The min of  HIV infection around the world is 0.1% and The max of  HIV infection around the world is 22.8% .
- The mean of  HIV infection around the world is 0.5%.
- In Shapiro-Wilk normality test,$W=0.50758$, and $P-value=0 <0.05$,which is not statistically significant at 0.05 level and means that the distribution of HIV infection around the world is not  a normal distribution.The Q-Q plot of HIV can also come to the same conclusion.

-----

Min| 1st Qu.|Median |Mean| 3rd Qu.|Max.| SD|   NA's 
-----|------|-------|----|--------|----|-----|-------
0.100|   0.200|   0.500 |  2.187|   1.600 | 22.800 | 4.372|    72 

-----


##5.2 For Question 2 

Is there a relationship between HIV infection rate and population growth rate ?

- The spearman correlation coefficient of `HIV` and `PopulationGrowth` is 0.45,which indicates that HIV infection rate will increase with the increase of population growth rate, and they are positively correlated.
- In Spearman's rank correlation test,$S=117700$ and $P-value=0 <0.05$,which is not statistically significant at 0.05 level and we cann't rejuct the null hypothesis $H_0$.There a  positively relationship between HIV infection rate and population growth rate.
- The scatter plot  of  `HIV` and `PopulationGrowth` can draw the same conclusion.


#6 Critique

In Question 1 ,I want to know how is the distribution of HIV infection around the world.The histogram plot would be useful.I have used R code to create the histogram of HIVE infection rate  and the density plot of HIVE infection rate in the same graph.The  histogram and density plot told  us that the distribution of hive  is skew and  is  seem not  to obey the normal distribution,which Shapiro-Wilk Normality Test also gives us the same conclusion.In Question 2,I want to know whether there is  a relationship between HIV infection rate and population growth rate.The scatter plot tell us that HIV infection rate will increase with the increase of population growth rate.


#7Reference

1. Bulman, J. S., & Osborn, J. F. (1989). Descriptive statistics. British Dental Journal, 166(2), 51-4.
2. Kabacoff, R. (2011). R in action. Manning.
3. Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer Publishing Company, Incorporated.


