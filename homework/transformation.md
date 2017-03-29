---
title: "HW1"
author: "Your Nmae"
date: '2017-03-29'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

I try logit transformation,Box-Cox algorithm,Logarithmic transformation and Standardising.The result is follwo:

-----

Variable|Original W Vluae| Original P Value| Transformation| W Value| P Value
-------|-----------------|--------|--------|----------------|-------|----------
PH|0.93|0|Power 4.69|0.99|0.46
OM.pct|0.75|0|Log|0.95|0
Al|0.83|0|Power -0.11|0.96|0
Ni|0.98|0.01|Scale|0.98|0.02
EXCH_K|0.93|0|Power 0.7|0.96|0
EXCH_Pb|0.69|0|Sqrt|0.86|0
VERYFINESAND|0.96|0|Power 0.3|0.99|0.24

-----


```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(car)
x <- read.csv('C://Users//mali//Documents//Hubbard Brk valley_soil_chem1997-1998.csv',header = T)
mydata <- x[,c('PH','OM.pct','Al','Ni','EXCH_K','EXCH_Pb','VERYFINESAND')]
mydata$Ni[mydata$Ni<0] <- NA
mydata$Al[mydata$Al<0] <- NA
mydata$EXCH_K[mydata$EXCH_K<0] <- NA
mydata$EXCH_Pb[mydata$EXCH_Pb<0] <- NA
par(mfrow=c(3,3))
qqPlot(mydata$PH^4.69,main='PH')
qqPlot(log(mydata$OM.pct),main='OM.pct')
qqPlot(mydata$Al^-0.11,main='Al')
qqPlot(scale(mydata$Ni),main='Ni')
qqPlot(mydata$EXCH_K^0.7,main='EXCH_K')
qqPlot(mydata$EXCH_Pb^0.5,main='EXCH_Pb')
qqPlot(mydata$VERYFINESAND^0.272,main='VERYFINESAND')

```

