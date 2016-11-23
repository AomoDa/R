---
title: "hw2"
author: "Your Nmae"
date: "2016年11月23日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1

```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(dplyr)
library(tidyr)
barplot(as.matrix(aggregate(mpg~cyl+am,data=mtcars,sd) %>% spread(am, mpg))[,-1],beside = T)
```

#Q2

```{r}
Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
(A <- table(Aye, Bee))
addmargins(A,margin = c(1,2))
prop.table(A,margin =2)
prop.table(A,margin =1)
```

#Q3

```{r}
library(ggplot2)
str(economics)
cor(economics$pce,economics$psavert)
```

