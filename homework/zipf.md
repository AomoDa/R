---
title: "Untitled"
author: "Your Nmae"
date: "2017-01-11"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1

- Zipf's law
- $\alpha \approx 1$

```{r, message=FALSE, warning=FALSE}
library(plyr)
library(VGAM)
library(plotrix)
#Q1
library(stringr)
c2 <- read.delim('C://Users//mali//Documents//c2.txt',
                 header = F)
c2 <- tolower(as.character(c2[,1]))
c2 <- unlist(str_extract_all(c2,pattern = '[[:alnum:]]{2,}'))
word_count <- sort(table(c2),decreasing = T)
head(word_count)
round(head(prop.table(word_count)),4)
p <- ppoints(100)
par(mfrow=c(1,3))
plot(quantile(as.numeric(word_count),probs = p),p,type='l',xlab='',main='CDF')
plot(quantile(as.numeric(word_count),probs = p),1-p,type='l',xlab='',main='CCDF')
plot(log(quantile(as.numeric(word_count),probs = p)),log(1-p),xlab='',main='CCDF:log-log')
par(mfrow=c(1,1))
```

#Q2


```{r}
##alpha=0.5
set.seed(22)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 0.5, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',las=2,main='alpha=0.5')
legend('bottomright',col=c('black',gray(0.5)),legend = c('Mean','Max Value'),lty=1)

##alpha=1
set.seed(2000)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 1, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',las=2,main='alpha=1')
legend('bottomright',col=c('black',gray(0.5)),legend = c('Mean','Max Value'),lty=1)
##alpha=1.2
set.seed(2200)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 1.2, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',lty=c(1,2),main='alpha=1.2')
legend('bottomright',col=c('black',gray(0.5)),legend = c('Mean','Max Value'),lty=1)
```

#Q3

```{r}
#Q3
set.seed(300)
repeats <- 60
tosses <- 8
heads <- function(tosses,p) {
	h <- 0
	for (i in 1:tosses) {
       h<- h+ ifelse(runif(1)<p, 1, 0)
	}
  return(h)
}
x <- data.frame(p=numeric(),h=numeric())
p<-0 
rn <-1
while(p<1.01){
	for (i in 1:repeats) {
		x[rn,1] <- p
		x[rn,2] <- heads(tosses,p)
		rn <- rn + 1
	}
	p <- p +0.05
}
plot(jitter(x$p),jitter(x$h),xlab='Balance Parameter P',ylab='Number of Heads Observed')
abline(h=6,lty=3)
abline(v=0.5,lty=3)
```

