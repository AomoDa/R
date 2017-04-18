---
title: "Untitled"
author: "Your Nmae"
date: "2017年4月17日"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# 发文量

```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(plyr)
library(stringr)

x <- read.csv('C://Users//mali//Documents//xx.csv',
              header = T,
              stringsAsFactors = F)
#-----------------------------------------------
#发文量 x1,x2,x3,x4
#-----------------------------------------------

aa <- x[x$x1!='0',c('x1','x2','x3','x4')]

q1 <- data.frame(x1=vector(),x2=vector(),x3=vector(),x4=vector())

for (i in 1:nrow(aa)) {
   x1 <- na.omit(unlist(strsplit(aa$x1[i],'、')))
   x2 <- na.omit(unlist(strsplit(aa$x2[i],'、')))
   x4 <- length(na.omit(unlist(strsplit(aa$x4[i],'、'))))
   q1 <- rbind(q1,data.frame(x1=x1,x2=x2,x3=aa$x3[i],x4=x4))
   # print(i)
}

q11 <- ddply(.data = q1,.variables = c('x1'),.fun = summarise,x3=sum(x3),x4=sum(x4))
q11 <- q11[order(q11$x3,decreasing = T),]
head(q11[order(q11$x4,decreasing = T),],10)
```

# 作者单位 

```{r}
q22 <- ddply(.data = q1,.variables = c('x2'),.fun = summarise,x3=sum(x3),x4=sum(x4))
q22 <- q22[q22$x2!='0',]
q22 <- q22[order(q22$x3,decreasing = T),]
head(q22[order(q22$x4,decreasing = T),],20)
```

# 发表情况 

## 时间

```{r}
bb <- x[x$x6!='0',c('x1','x6')]
q33 <- ddply(.data = bb,.variables = c('x6'),nrow)
library(ggplot2)
ggplot(data=q33,aes(x=x6,y=V1))+geom_point()+geom_path()+
    theme(panel.background = element_blank())+theme_bw()
```

## 期刊类型

```{r}
x5 <- factor(x=x$x5,levels = 1:3,labels = c('外语类刊物','各类大学学报','其他相关刊物'))
pie(table(x5),main='期刊类型')
```


## 发表期刊

```{r}
q44 <- ddply(.data = x,.variables = 'x4',nrow)
head(q44[order(q44$V1,decreasing = T),],10)
```

# 关键词 

```{r, message=FALSE, warning=FALSE}
library(wordcloud)
library(Rwordseg)
library(wordcloud2)
#关键词提取
keyword <- unlist(strsplit(x$x7,'、'))

#中文分词算法
segment_keyword <- unlist(segmentCN(keyword))

wordcloud2(table(keyword), size = 1,shape = 'star')  

wordcloud2(table(keyword),
           size = 2, 
           fontFamily = "微软雅黑",  
          color = "random-light", 
          backgroundColor = "grey")  
wordcloud2(table(keyword), size = 2, 
           minRotation = -pi/2, 
           maxRotation = -pi/2)  

```



# 对象范围

```{r}

x$x8 <- factor(x=x$x8,levels = 1:3,labels = c('某一特定媒体','一国主流媒体','多国主流媒体'))
pie(table(x$x8),main='对象范围')
```

# 媒体类型

```{r}
library(plyr)
a <- x[x$x9!='0',c('x6','x9')]
rt <- data.frame(year=numeric(),value=vector(),stringsAsFactors = F)
for (i in 1:nrow(a)) {
	rt <- rbind(rt,data.frame(year=a$x6[i],value=as.vector(unlist(strsplit(a$x9[i],split = '、')))))
}

rt1 <- ddply(.data = rt,.variables = c('year','value'),.fun = nrow)

ggplot(data=rt1,aes(x=year,y=V1,col=value))+geom_point()+geom_path()+
    theme(panel.background = element_blank())+theme_bw()

```

# 涉及媒体
```{r}
b <- x[x$x10!='0',c('x10','x11')]
b$x11 <- str_replace(b$x11,pattern = '[0-9][-]',replacement = '')
rt2 <- ddply(.data = b,.variables = c('x10','x11'),.fun = nrow)
rt2$prob <- round(rt2$V1 / sum(rt2$V1),4)
head(rt2[order(rt2$V1,decreasing = T),c('x10','x11','V1','prob')],10)
```

# 研究国家

```{r}
library(plyr)
library(stringr)
d <- x[,c('x6','x11')]
d$type <- unlist(str_extract(d$x11,pattern = '[1-2]'))
d$x11 <- str_replace(d$x11,pattern = '[0-9][-]',replacement = '')
rt3 <- ddply(.data = d[d$type==1,],.variables = c('x6','x11'),.fun = nrow)
ggplot(data=rt3,aes(x=x6,y=V1,col=x11))+geom_point()+geom_path()+
    theme(panel.background = element_blank())+theme_bw()+ scale_colour_brewer(palette = "Set1")

rt4 <- ddply(.data = d[d$type==2,],.variables = c('x6','x11'),.fun = nrow)
ggplot(data=rt4,aes(x=x6,y=V1,col=x11))+geom_point()+geom_path()+
    theme(panel.background = element_blank())+theme_bw()
rt33 <- aggregate(V1~x11,rt3,sum)
rt333 <- tail(rt33[order(rt33$V1),],10)
rt333$id <- 1:7
rt333$country <- factor(x=rt333$id,labels = rt333$x11)
rt44 <- aggregate(V1~x11,rt4,sum)
rt444 <- tail(rt44[order(rt44$V1),],10)
rt444$id <- 1:10
rt444$country <- factor(x=rt444$id,labels = rt444$x11)



ggplot(data=rt333,aes(x=country,weights=V1,fill=country))+geom_bar()+theme_bw()
pie(rt333$V1,labels = rt333$country,cex=0.5,radius = 1)

ggplot(data=rt444,aes(x=country,weights=V1,fill=country))+geom_bar()+theme_bw()
pie(rt444$V1,labels = rt444$country,cex=0.5,radius = 1)

```

# 研究时段

```{r, message=FALSE, warning=FALSE}
e <- x[x$x12!='0',c('x11','x12')]

par(xpd = NA)
plot(x = NA,y= NA, 
     type = 'n', 
     xlim = c(1920, 2020), 
     ylim = c(0,nrow(e)), 
     xlab = 'Year', 
     ylab = '')

for (i in 1:nrow(e)) {
   a <- unlist(strsplit(e$x12[i],'-'))
   if(length(a)==1){
   	points(a,i,type = 'p',pch=16)
   	} else{
        points(a[1],i,type = 'p',pch=16)
        points(a[2],i,type = 'p',pch=16)
        lines(x=c(a[1],a[2]),y=c(i,i))
   	}
   
}
```


```{r, message=FALSE, warning=FALSE}
e <- x[x$x12!='0',c('x11','x12')]

par(xpd = NA)
plot(x = NA,y= NA, 
     type = 'n', 
     xlim = c(1980, 2020), 
     ylim = c(0,nrow(e)), 
     xlab = 'Year', 
     ylab = '')

for (i in 1:nrow(e)) {
   a <- unlist(strsplit(e$x12[i],'-'))
   if(length(a)==1){
   	points(a,i,type = 'p',pch=16)
   	} else{
        points(a[1],i,type = 'p',pch=16)
        points(a[2],i,type = 'p',pch=16)
        lines(x=c(a[1],a[2]),y=c(i,i))
   	}
   
}
```

