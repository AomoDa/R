---
title: "Untitled"
author: "Your Name"
date: "2016-12-04"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```




##clickheatmap

```{r, message=FALSE, warning=FALSE}
#---------------------------------------------------
# loading data
#---------------------------------------------------

x <- read.csv('C://Users//AomoDa//Documents//data1234.csv',
              header = T,
              sep = ';',
              stringsAsFactors = F)
x$datetime <- as.POSIXct(x$Time/1000,
                         origin = '1970-01-01 00:00:00')

library(stringr)
x$sim_url <- str_replace_all(x$URL,'[#?].*','')

#---------------------------------------------------
## clickheatmap
#---------------------------------------------------


library(ggplot2)

## all click
click <- subset(x,Event=='MouseButtonPressed')

ggplot(data=click,aes(X.Position,Y.Position)) + 
   geom_point(na.rm = T,shape=7,col=I('darkred'),size=3)+
   labs(title='MouseButtonPressed Heatmap ')

ggplot(data = click,aes(KeyOrButton,fill=KeyOrButton))+geom_bar()+
   labs(title='KeyOrButton Barplot')




## click_max_url
click_max_url <- subset(x,Event=='MouseButtonPressed' & URL=='https://anlegen.visualvest.de/app/depot/?prompt=none&error=login_required&state=bc46030b-521f-4731-876b-7506436a36ff')

ggplot(data=click_max_url,aes(X.Position,Y.Position,
                              shape=KeyOrButton,col=KeyOrButton)) +
  geom_point(na.rm = T,size=3)+
  labs(title='MouseButtonPressed Heatmap ')


```

##URL-Change

```{r}

library(networkD3)
url_change <- subset(x,Event=='URL-Change')

src <- url_change$URL[-22]
tar <- url_change$URL[-1]
NetworkData <- data.frame(src, tar)
simpleNetwork(NetworkData, fontFamily = "sans-serif")
```

## life-time


```{r}
life_time <- function(x) {
	return( round ( ( max(x/1000)-min(x/1000) )/60,2))
}
# 
(tb_life_time <- as.data.frame(aggregate(Time~sim_url,data = na.omit(x),life_time)))

ggplot(data = tb_life_time,aes(sim_url,weight=Time,fill=sim_url))+
   geom_bar()+labs(y='min',title='lift time (min)')
```

