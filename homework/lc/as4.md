---
title: "HW4"
author: "--"
date: '2017-05-04'
output:
  word_document:
    toc: yes
  pdf_document:
    latex_engine: xelatex
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Q1 CONFIDENCE INTERVALS

- The sample means of Variety 1 is 4.04 and The standard deviation of Variety 1 is 1.4569.
- The sample means of Variety 2 is 2.36 and The standard deviation of Variety 2 is 0.3273.
- The standard errors of Variety 1 is $se=\sigma/\sqrt{n}=1.4569/\sqrt{10}=0.4607$ and the standard errors of Variety 2 is $se=\sigma/\sqrt{n}=0.3273/\sqrt{10}=0.1035$.
- $t_{1-\alpha/2}=t_{0.975}=2.2622$ with $df=10-1=9$ at 0.05 level.
- So the 95% confidence interval of Variety 1 is $4.04 \pm 2.2622*0.4607 =[ 2.9978, 5.0822]$ 
- The 95% confidence interval of Variety 2 is $2.36 \pm 2.2622*0.1035=[2.1259,2.5941]$

# Q2 UNDERSTANING DATA CORRELATIONS

## a

There is a negative relationship between temperature and crawling age,which means that the average crawling age of the babies would decrease with the increase of temperature.


## b

The  relationship would not change, because that Celsius and Fahrenheit are different units for measuring temperature and they are linear.

## c

The correlation would still be -.70, since correlation is a measure of the linear relationship and  changing unit for temperature would not change correlation .

# Q3 INFERENCE IN REGRESSION ANALYSIS

## a

There is a positive linear relationship between cans of beer consumed and BAC,which means that BAC would increase with the increase of cans of beer.

## b

- **Slope**: For every beer consumed an individual’s BAC increases by 0.0180 grams per deciliter.
- **Intercept**: When an individual consumes no beers,their blood alcohol content would be  -0.0128 grams per deciliter.

The equation of the regression line : 
$$BAC = 0.0180 * beers − 0.0127$$



## c

- Null hypotheses: $\beta_{beers}=0$ ,which means that the data cann't provide  a  strong evidence that drinking more cans of beer is associated with an increase in blood alcohol.
- Alternative hypotheses: $\beta_{beers} \neq 0$,which means that the data can provide a  strong evidence that drinking more cans of beer is associated with an increase in blood alcohol.
- The p value is zero,which is very statistically significant and means that we should reject the $H_0$ .So my conclusion is that the data can provide a  strong evidence that drinking more cans of beer is associated with an increase in blood alcohol.

## d


The $R^2=r^2=0.89^2=0.7921$,which means that our model can explain 79.21% variance of BAC using this linear model.


## e

**No**. In this question, people in bar  maybe a different population sampled under different conditions with many unknown variables including gender, weight and drinking habits.

# Q4 INFERENCE IN REGRESSION ANALYSIS

## a

- $H_0$ : the age difierence between husbands and wives is  consistent across ages.
- $H_1$ : the age difierence between husbands and wives is not  consistent across ages.
- The $t_{0.975}=1.9742$ with $df=168$.
- Confidence intervals for $age\_husband$ in this model is $0.9112 \pm 1.9742 * 0.0259 =[0.8601,0.9623]$,which does not include 1 and means that I should reject $H_O$. So the age difierence between husbands and wives is **not  consistent across ages**.

## b

$$Age_{wife}= 0.9112 ∗ Age_{husband} + 1.5740$$

## c

- **Slope** : His wife's age  woild increase by 0.9112 year when the husband increase in 1 year old
- **Intercept** : When a husband's age is zero, his wife's age would be  1.57 years.

## d

The correlation of ages in this data set is 0.9381.

$$r=\sqrt{R^2}=\sqrt{0.88}=0.9381$$.


## e

His wife's age would be 51.69.And because $R^2=0.88$,which is good ,I think this prediction is reliable.

$$Age_{wife}  = 0.9112 ∗ Age_{husband} + 1.5740 \\ = 0.9112*55+1.5740 \\= 51.69$$




## f

In this model,both partners' ages in the data set  are below 65 years.So it would not be wise to attempt to estimate the age of the wife of an 85 year old man.

#Q5 FITTING A REGRESSION LINE

## a

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(knitr)
 x <- read.csv('file:///C:/Users/mali/Downloads/urban.csv')
a <- as.data.frame(rbind(apply(x[,-1],MARGIN = 2,mean),
      apply(x[,-1],MARGIN = 2,sd),
      apply(x[,-1],MARGIN = 2,var),
      apply(x[,-1],MARGIN = 2,min),
      apply(x[,-1],MARGIN = 2,max),
      apply(x[,-1],MARGIN = 2,median),
      apply(x[,-1],MARGIN = 2,length)))

a$summary <- c('Mean','Standard Deviation','Variance','Min','Max','Median','Length')
kable(a[,c(3,1,2)],
      format = 'pandoc',
      caption = 'Table 1. Data Summary',
      row.names = F,
      digits = 2)
```

## b

The model is 
$$poppc\_urban=173.5751 -1.4882 * pct\_owner\_occupied $$

- **Slope**:  The percent of urban population would  reduce by 1.4882 when percent of houses occupied by owners increase by 1 .
- **Intercept**: The percent of urban population would would be 173.5751 when percent of houses occupied by owners is 0 .
- **R-squared** : $R^2=0.292$,which is very low and means my model is not good.

```{r, echo=FALSE}
lm1 <- lm(poppct_urban~pct_owner_occupied,data=x)
kable(summary(lm1)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 2. Model Summarise'
)
```

## c

I think a linear functional is not  appropriate for describing the relationship in this data.The Scatter plot tells me that there is not a strong linear correlation between percent of houses occupied by owners and percent of urban population.And the correlation coefficient of them is -0.54,which is low.


```{r, echo=FALSE}
with(x,plot(pct_owner_occupied,
            poppct_urban,
            main='Figure 1 . pct_owner_occupied vs poppct_urban'))
abline(lm1,lty=2,lwd=2,col='orange')
```


## d 

The new model is 
$$poppc\_urban=4.3279 * pct\_owner\_occupied - 0.0479 * pct\_owner\_occupied^2 $$

$R^2=0.9737$,which is far greater than 0.292.So I think the new model is better than old model.


```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(ggplot2)
lm2 <- lm(formula = poppct_urban ~ pct_owner_occupied+I(pct_owner_occupied^2)-1, data = x)
kable(summary(lm2)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 3. Model2 Summarise'
)
ggplot(data=x,
       aes(x=pct_owner_occupied,y=poppct_urban))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y~x+I(x^2)-1,se=F)+theme_bw()+theme()+
  labs(title='Figure 2. Model Compare \n  new model(blue) vs old model(orange)')+
  geom_smooth(method = 'lm',se=F,col=I('ORANGE'),lty=2)
  
```

