---
title: "Q4"
author: "Your Name"
date: '2017-03-19'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Q4

##a

First, I hvae imported the dataset of life expectancy in the world from Excel into R.There are 3 columns in the data,which are life expectancy of   male , female  and all.I have computed the average and the standard deviation of life expectancy in the world. The reslut is follow :

- The average of  life expectancy in the world is 72.49 and the standard deviation of life expectancy in the world is 8.36.
- The average of  male life expectancy in the world is 70.05 and the standard deviation of life expectancy in the world is 8.97.
- The average of  female life expectancy in the world is 75.04 and the standard deviation of life expectancy in the world is 8.88.



-----

SEX|Average|Standard  Deviation 
-----|------|-----------------
male|70.05|7.97
female|75.04|8.88
total|72.49|8.36

-----

**R CODE FOR A **

```{r, message=FALSE, warning=FALSE}
library(xlsx)
life <- read.xlsx2(file = 'C://Users//AomoDa//Documents//q4.xlsx',
                   sheetName = 'a',
                   stringsAsFactors=F,
                   colClasses = c('character',rep('numeric',3))
                   )
head(life)
# for total
mean(life$total,na.rm = T);sd(life$total,na.rm = T)
# for total
mean(life$male,na.rm = T);sd(life$male,na.rm = T)
# for total
mean(life$female,na.rm = T);sd(life$female,na.rm = T)
```

##b


**First**, I hvae imported the dataset  from Excel into R.There are 6 columns and 147 rows in the data:

- Country : capita country.
- Gini : Gini.
- GDP : GDP.
- government.consumption(%) : Government Consumption.
- percent.agriculture(%) : GDP—Percent Agriculture.
- public.debt (%): Public Debt as percent of GDP.


**And then** I fit a regression mdoel of GDP per capita as a function of Government Consumption,  GDP—Percent Agriculture, Gini, Public Debt as percent of GDP. The model infomations is follow:

$$GDP=53846.36 -699.31\cdot Gini + 376.11 \cdot government.consumption -32.36 \cdot percent.agriculture - 904.13 \cdot  public.debt$$



-----

Variables| Estimate| Std. Error| t value | P value
---------|---------|-----------|---------|--------
(Intercept)|53846.36  |  7857.13 |  6.853 |0
Gini      |              -699.31 |    135.39 | -5.165 |0
government.consumption |  376.11  |   262.25 |  1.434 |   0.154    
percent.agriculture |     -32.36  |    38.19 | -0.848  |  0.398    
public.debt      |       -904.13  |   112.05 | -8.069|0

-----

**Next** the new china data is follow:

-----

Country | Gini   | government.consumption | percent.agriculture | public.debt
--------|--------|------------------------|---------------------|-------------
China |46.9     |            14.2+5=19.2         |      20.1    |   8.6-5=3.6

-----

**Finally ** use `lm1` to predict,the GDP per capita in China be if its Gini and its public debt were 5% lower, and government consumption 5% higher  than their current values is 24364.7


**R CODE FOR B **

```{r}
#loading gdp dataset into R
library(xlsx)
library(effects)
gdp <- read.xlsx2(file = 'C://Users//AomoDa//Documents//q4.xlsx',
                  sheetName = 'b',
                  stringsAsFactors=F,
                  colClasses = c('character',rep('numeric',5)))
# drop missing values in my dataset
gdp <- na.omit(gdp)

# bulid the model
lm1 <- lm(GDP~Gini+government.consumption+percent.agriculture+public.debt,data=gdp)
summary(lm1)
# plot model
plot(allEffects(lm1))
# CHINA DATA
china <- gdp[gdp$Country=='China',]
china$public.debt <- china$public.debt-5
china$government.consumption <- china$government.consumption+5
china
predict(object = lm1,newdata = china)
```


