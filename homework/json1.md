---
title: "Homework 4"
author: "Your Name"
date: "2017年3月25日"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Problem 4

```{r}
library(jsonlite)
foo <- fromJSON("http://crandb.r-pkg.org/-/latest")
```


##a

There are **149** unique field names.

```{r}
#Produce a vector of all field names in all packages
field_names <- as.vector(unlist(sapply(foo, function(x) x$License)))
#How many unique field names are there
length(unique(field_names))
```

##b

**2648** packages need compilation, and the proportion of packages need compilation is **27.89%**.
 
```{r}
q4_b<-  as.vector(unlist(sapply(foo, function(x) x$NeedsCompilation)))
#How many packages need compilation?
sum(q4_b=='yes')
# What proportion of packages need compilation?
sum(q4_b=='yes')/length(q4_b)

```

##c

**8512** packages are licensed under the GPL and the  proportion of packages are licensed under the GPL is **82.30%**
 
```{r}
q4_c<-  as.vector(unlist(sapply(foo, function(x) x$License)))
#How many packages are licensed under the GPL? 
length(q4_c[grep(pattern = '\\bGPL\\b',x = q4_c)])
#What proportion of packages are licensed under the GPL?
length(q4_c[grep(pattern = '\\bGPL\\b',x = q4_c)])/length(q4_c)
```


##d

```{r}
q4_d<-  sapply(foo, function(x) x$Depends)

# Produce a list of the names of packages 
# on which each package depends
q4_d1<- lapply(q4_d,function(x) names(x)[names(x)!='R'])
head(q4_d1)

# Produce a vector of the names of packages 
# on which any other package depends
q4_d2 <- as.vector(unlist(q4_d1))
head(q4_d2)

# Produce a table of counts of how many times
# each package that appears in some Depends field does so
q4_d3 <- table(q4_d2)
head(q4_d3)
#Reorder your table so it is in decreasing order of the counts
q4_d4 <- sort(q4_d3,decreasing = T)
head(q4_d4)

rcore <- c("base", "compiler", "datasets", "graphics", "grDevices",
    "grid", "methods", "parallel", "splines", "stats", "stats4",
    "tcltk", "tools", "translations", "utils", "boot", "class", "cluster",
    "codetools", "foreign", "KernSmooth", "lattice", "MASS", "Matrix",
    "mgcv", "nlme", "nnet", "rpart", "spatial", "survival")

# Eliminate these packages from your table
# and again produce a table reordered so it is in decreasing order of the counts
q5_d5 <- sort(q4_d4[setdiff(names(q4_d4),rcore)],decreasing = T)
head(q5_d5)
```

#Problem 5


```{r}
load(url("http://www.stat.umn.edu/geyer/3701/data/q4p5.rda"))
library(DBI)
mydb <- dbConnect(RSQLite::SQLite(), "")
dbWriteTable(mydb, "depends", d$depends)
dbWriteTable(mydb, "imports", d$imports)
dbWriteTable(mydb, "suggests", d$suggests)
dbWriteTable(mydb, "linking", d$linking)
rm(d)
ls()
```

##a

```{r}
dbGetQuery(mydb," CREATE TABLE temp AS 
	select  packto from depends
	union all
	select  packto from imports
	union all
	select  packto from suggests
	union all
	select  packto from linking	
	")
dbListTables(mydb)
```

##b

```{r}
q5_b <- dbGetQuery(mydb," SELECT packto, 
	                               COUNT(packto) AS packcount 
	                           FROM temp 
	                           GROUP BY packto")
head(q5_b)
```

##c

```{r}

q5_c <- dbGetQuery(mydb," SELECT packto,
                                 COUNT(packto) AS packcount 
                              FROM temp 
                              GROUP BY packto 
                              ORDER BY packcount DESC ")
head(q5_c)
```


##d


After querying the sql help document, I found two ways to finish this task.

- Use `subquery` and `where`,as you can see q5_d1.
- Use `having`,as you can see q5_d5.
- The results are exactly the same.

```{r}
#  in one query use where and subquery
q5_d1 <- dbGetQuery(mydb,"SELECT * FROM 
	               (
                   SELECT packto, 
                          COUNT(packto) AS packcount 
                       FROM temp GROUP BY packto 
                  ) AS TB1
                  WHERE  packcount >100 ORDER BY packcount DESC ")
#in one query use having
q5_d2 <- dbGetQuery(mydb,
                  " SELECT packto, 
                           COUNT(packto) AS packcount 
	                  FROM temp 
	                  GROUP BY packto 
	                  HAVING COUNT(packto) >100 
	                  ORDER BY packcount DESC")

head(q5_d1)
head(q5_d2)
```

# Problem 6

```{r, message=TRUE, warning=TRUE}

library(KernSmooth)
foo <- read.csv("http://www.stat.umn.edu/geyer/3701/data/q4p3.csv")

h <- with(foo,dpill(x,y))
fit <- with(foo,locpoly(x, y, bandwidth = h))
plot(foo)
lines(fit)
```

