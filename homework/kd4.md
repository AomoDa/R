---
title: "hw4"
author: "Your Nmae"
date: "2016年11月23日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1

```{r}
set.seed(200)
name <- paste('x',1:10,sep='')
file.name <- paste('C://Users//mali//Documents//data',1:10,'.csv',sep='')
data_all <- matrix(NA,ncol=10,nrow=10)
for (i in 1:10) {
 data_all[,i] <- assign(name[i],value = rnorm(10,2,5))
 write.csv(data_all[,i],file.name[i])
}

head(data_all)
```

#Q2

```{r}
set.seed(2000)
x <- read.csv('C://Users//mali//Documents//parameter.csv')
ind <- sample(x=1:3,size = nrow(x),replace = T)
x1 <- x[ind==1,]
x2 <- x[ind==2,]
x3 <- x[ind==3,]
write.csv(x1,'C://Users//mali//Documents//block1.csv')
write.csv(x2,'C://Users//mali//Documents//block2.csv')
write.csv(x3,'C://Users//mali//Documents//block3.csv')
```


#Q3

```{r}
set.seed(666)
a <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
b <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
mean(a==b)
```

#Q4

```{r}
barplot(prop.table(table(a+b)),ylab='Freq')
```

