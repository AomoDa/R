---
title: "hw1"
author: "Your Name"
date: "2017-01-21"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Q4

## three real-life applications in  classification:

- Classification 1 :  Whether I can pass the homework this time ?  (Response : Yes / Not, Predictors : accuracy,beautiful etc. Goal : Prediction).
- Classification 2 :  Is the film 'Untitled Planet of the Apes Sequel /War of the Planet' going to be successful or not (Response : Success/Failure, Predictors : Money spent, Talent, Running Time, Produce etc., Goal : Prediction). 
- Classification 3 :  Should Tom Smith be admitted into the university of Cambridge or not (Response : Y / N, Predictors : SAT Scores, GPA, etc., Goal : Prediction). 


## three real-life applications in regression:

- Regression 1 :  What is the average house sale price in Washington, dc. (Response : Average house sale price  Predictors : Proximity to transit, Parks, Schools, Average size of family, Average Income of Family, Crime Rate, Price Flux in surrounding neighborhoods etc., Goal : Inference). 
- Regression 2 :  How CPI change in USA next year ? (Response : CPI, Predictors : Population,Income, Education,  Government Spending etc., Goal : Inference)
- Regression 3 :  What does effect the dollar against the euro exchange rate . (Response : exchange rate. Predictors:The U.S. economy,The eu economy etc., Goal : Inference).

## three real-life applications in  cluster analysis

- Cluster 1 :  Cluster new movies being produced into ratings G/PG/R/PG-13 etc. (Response : This movie is a R/PG/PG-13, Predictors : Violent content, Sexual language, theme, etc., Goal : Prediction).
- Cluster 2 :  Division of countries into Developed, Developing and Third World (Response : By 2050, countries in Asia can be split into these following clusters, Predictors : Per Capita Income, Purchasing power parity, Average birth rate, Average number of years of education received, Average Death Rate, Population etc., Goal: Prediction). 
- Cluster 3 : Cluster consumers in Amazon into different group by buying power.(Response : A/B/C/D/E, Predictors : Consumption amount,Number of consumption etc., Goal : Prediction). 



#Q7

##a

Compute the Euclidean distance between each observation and
the test point, $X_1 = X_2 = X_3 = 0$.

------

Obs.|EuclideanDistance|Y
-----|----------------|------
1|3|Red
2|2|Red
3|3.162|Red
4|2.236|Green
5|1.414|Green
6|1.732|Red

------


```{r}
ED <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}
a <- c(0,0,0)
x1 <- c(0,3,0);ED(x1,a)
x2 <- c(2,0,0);ED(x2,a)
x3 <- c(0,1,3);ED(x3,a)
x4 <- c(0,1,2);ED(x4,a)
x5 <- c(-1,0,1);ED(x5,a)
x6 <- c(1,1,1);ED(x6,a)

```

##b

The result is **Green**

When $K=1$:

The euclidean distance of $Obs_5$  and $X_1 = X_2 = X_3 = 0$ is the mimimum and the $Obs_5$ is **Green**.


##c

The result is **Red**

When $K=3$:

- the euclidean distance of $Obs_5$,$Obs_6$,$Obs_2$   and $X_1 = X_2 = X_3 = 0$ is the mimimum.
- $P(Red)=\frac{2}{3}$ because of $Obs_6='Red'$ and $Obs_2='Red'$,
- $P(Green)=\frac{1}{3}$ because of $Obs_5='Green'$.
- Finally the result is **Red**  because of $P(Red) > P(Green)$

##d

**Small**

The figure 2-16 in textbook tell us that with K is small, the decision boundary is overly flexible, while with K is large  it is not sufficiently flexible. When bayes decision boundary in this problem is highly nonlinear,we expect the best value for K to be **small**.

#Q8

##a

```{r}
college <- read.csv('C://Users//mali//Documents//College.csv')
str(college)
```

##b
```{r}
# fix(college)
rownames(college) = college[,1]
college = college[,-1]
# fix(college)
head(row.names(college))
```

##c

###i

```{r}
summary(college)
```

###ii

```{r}
pairs(college[,1:10])
```

###iii

```{r}
plot(college$Private, college$Outstate)
```

###iv

```{r}
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)
```

###v


```{r}
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col='red')
hist(college$S.F.Ratio, col='orange', breaks=10)
hist(college$Expend, breaks=100,col='blue')
```

###vi

```{r}
library(ggplot2)
# Apps vs Accept
ggplot(data=college,aes(x=Apps,y=Accept))+
  geom_point()+
  geom_smooth()+
  labs(title='Apps vs Accept')

# Apps vs Accept with different Elite
ggplot(data=college,aes(x=Apps,y=Accept,col=Elite))+
  geom_point()+
  geom_smooth()+
  labs(title='Apps vs Accept with different Elite')

#Apps vs Accept with different Private
ggplot(data=college,aes(x=Apps,y=Accept,col=Private))+
  geom_point()+
  geom_smooth()+
  labs(title='Apps vs Accept with different Private ')
```


#Q10

##a

There are 506 rows  and 14 columns in this data set. Each row represents an example  and each column represents a variable, including the independent variable and dependent variable:

- **crim**: per capita crime rate by town
- **zn**: proportion of residential land zoned for lots over 25,000 sq.ft.
- **indus**: proportion of non-retail business acres per town.
- **chas**: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
- **nox**: nitrogen oxides concentration (parts per 10 million).
- **rm**:average number of rooms per dwelling.
- **age**:proportion of owner-occupied units built prior to 1940.
- **dis**:weighted mean of distances to five Boston employment centres.
- **rad**:index of accessibility to radial highways.
- **tax**:full-value property-tax rate per $10,000.
- **ptratio**:pupil-teacher ratio by town
- **black**:1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
- **lstat**:lower status of the population (percent).
- **medv**: median value of owner-occupied homes in $1000s.

```{r}
library (MASS)
data(Boston)
str(Boston)
dim(Boston)
```

##b

```{r}
pairs(Boston)
```

##c


```{r}
library(ggplot2)
#tax Vs crim
ggplot(data=Boston,aes(x=tax,y=crim))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title='tax Vs crim ')
#ptratio Vs crim
ggplot(data=Boston,aes(x=ptratio,y=crim))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title='ptratio Vs crim ')
#lstat Vs crim
ggplot(data=Boston,aes(x=lstat,y=crim))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title='lstat Vs crim ')

```


##d

```{r}
par(mfrow=c(1,3))
hist(Boston$crim, breaks=30)
hist(Boston$tax, breaks=30)
hist(Boston$ptratio, breaks=30)
par(mfrow=c(1,1))
```

##e

There are **35** suburbs in this data set bound the Charles river.

```{r}
sum(Boston$chas==1)
```

##f

The median pupil-teacher ratio among the towns in this data set is **19.05**.

```{r}
median(Boston$ptratio)
```

##g

- $399^{th}$ suburb of Boston has lowest median value of owneroccupied
homes.


```{r}
which.min(Boston$medv)
Boston[which.min(Boston$medv),]
```

##h

- In this data set, there are **64**  suburbs average more than seven rooms per dwelling and **13**  suburbs average  more than eight rooms per dwelling.



```{r}
sum(Boston$rm>7)
sum(Boston$rm>8)
# the suburbs that average 
# more than eight rooms per dwelling
summary(Boston[Boston$rm>8,])
```

#Q100

##a

With model complexity = 1,the average Residual SS is $$\frac{164.96+112.57+61.61+103.67+46.51+133.39+74.96+46.69+130.48+69.84}{10}=94.468$$.And the approximate range of the highest order coefficient for these 10 simulations $[-5,-5]$.

##b

With model complexity = 4,the average Residual SS is $$\frac{35.72+58.44+77.75+27.54+22.64+28.4+38.55+36.84+35.76+89.8}{10}=45.144$$.And the approximate range of the of the coefficient `#2` for these 10 simulations $[-32,32]$.

##c

With model complexity = 16,the average Residual SS is $$\frac{3.09+9.71+9.28+7.95+3.98+24.54+3.85+8.07+0.65+6.01}{10}=7.712$$.And `#6` coefficient has the largest range for these 10 simulations, and that range is $[-4e5,4e5]$.

##d

- When model complexity is small,bias is large but variance is small.
- When model complexity is large,bias is small but variance is large.


##e

**With model complexity = 4**
