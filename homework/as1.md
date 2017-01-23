---
title: "hw1"
author: "Your Nmae"
date: "2017年1月23日"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Part A (Wholesale)

##i

```{r}
# reading the datafile in as a data frame
x <- read.csv('C://Users//mali//Documents//Wholesale.csv',
              header = T)
str(x)
# create an additional column, Total, 
# consisting of the total sales to clients
x$Total  <- x$Fresh+x$Milk+
  x$Grocery+x$Frozen+
  x$Ppr_Dtgt+x$Deli
# obtain a subset of the data frame
# consisting only of these customers whose total purchases are less than 40,000 mu
summary(x[x$Total<40000,])
#Then remove the total sales column
x <- x[x$Total<40000,-9]
```

##ii

```{r}
x$Region <- factor(x = x$Region,levels = 1:3,labels = c('Lisbon','Oporto','Other'))
x$Channel <- factor(x = x$Channel,levels = 1:2,labels = c('Horeca','Retail'))
table(x$Channel)
table(x$Region)

```

##iii

```{r, message=FALSE, warning=FALSE}
library(ggplot2)
#Bar Plot of Channel
ggplot(data=x,aes(Channel,fill=Channel))+
  geom_bar()+
  labs(title='Bar Plot of Channel')

#Bar Plot of Region
ggplot(data=x,aes(Region,fill=Region))+
  geom_bar()+
  labs(title='Bar Plot of Region')
```

##iv


- Horeca is most common in  Lisbon and retail is most common in  Oporto. 
- 


```{r}
#Mosaic Plot of Channel and Region
mosaicplot(with(x,table(Region,Channel)),
           shade = T,
           main='Mosaic Plot of Channel and Region')
```

## v

```{r}
#scatterplot matrix of the six product categories
pairs(x[,3:8], 
      main = "ScatterPlot Matrix ",
      pch = 21, 
      bg = c("orange","green3")[unclass(x$Channel)])
```

## vi

```{r, message=FALSE, warning=FALSE}
library(reshape)
mydata <- melt(x, id=c("Channel", "Region"))
#rename
names(mydata)[3:4] <- c('Type','Sales')
head(mydata)
```

## vii

```{r}
ggplot(data=mydata,aes(x=Type,y=Sales,fill=Type))+
  geom_boxplot()+
  facet_grid(~Channel)+
  labs(title='Boxplots of Six Products')
```

## viii

```{r}
ggplot(mydata[mydata$Type %in% c('Fresh','Milk'),],aes(x=Sales,fill=Type))+
  geom_density(alpha=0.6)+
  facet_grid(Channel~Region)+
  labs(title='Density Plots of Milk and Fresh ')
```

##ix

#Part B (US Crime)

##i

```{r}
usc <- read.csv('C://Users//mali//Documents//CrimeRates.csv',
                header = T,
                stringsAsFactors = F)
usc$Location <- factor(usc$Location,
                       levels = 1:6,
                       labels = c('New England','Mid-Atlantic','The South','Midwest','The Southwest','The West'))
str(usc)
##i
ggplot(data=usc,aes(x=murder,y=burglary))+
  geom_point(aes(col=Location,size=population))+
  geom_text(aes(label=stCode),check_overlap=T,nudge_y = 0.5,nudge_x = 0.5,size=2)+
  scale_size(range=c(1,6))+
  labs(title='Bubble Chart')
```


##ii

```{r}
par(mfrow=c(1,2))
with(usc,hist(murder,breaks = 10,col='orange'))
with(usc,hist(burglary,breaks = 10,col='green'))
par(mfrow=c(1,1))
```

