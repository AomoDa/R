---
title: "Lab06"
author: "Your Nmae"
date: "2016-11-28"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##Task 1: Time Series Process Identification (1.5 points)

####Time Series 1:

- it is stationary.
- autoregressive parameter $\varphi>0$
- moving average parameter $\theta \approx 0$

####Time Series 2:

- it is stationary.
- autoregressive parameter $\varphi \approx 0$
- moving average parameter $\theta> 0$

####Time Series 3:

- it is instationary.
- autoregressive parameter $\varphi=NA$
- moving average parameter $\theta=NA$

####Time Series 4:

- it is instationary.
- autoregressive parameter $\varphi < 0$
- moving average parameter $\theta \approx 0$

####Time Series 5:

- it is instationary.
- autoregressive parameter $\varphi > 0$
- moving average parameter $\theta > 0$

####Time Series 6:

- it is instationary.
- autoregressive parameter $\varphi > 0$
- moving average parameter $\theta < 0$


##Task 2: Time Series Analysis (4 points)


####a

- non-seasonal models is ARIMA(2,0,1).
- seasonal models is ARIMA(2,0,0)(2,1,0)[12].

```{r, message=FALSE, warning=FALSE}
library(forecast)
library(foreign)
library(nlme)
x <- read.spss('C://Users//mali//Documents//SanduskyTemperature.sav',to.data.frame = T)
#a
avg7447_ts <- ts(x$avg7447,frequency = 12,start = c(1990,1))
plot.ts(avg7447_ts)
# non-seasonal models
auto.arima(avg7447_ts,seasonal = F)
#seasonal models
auto.arima(avg7447_ts,seasonal = T)
```

####b

- Use an autoregressive model of first order with autocorrelation to estimate  $\varphi_1 = 0.3711552$.
- AC adjusted residuals seems to be normality.
- This model fully does not  control for the autocorrelation .



```{r, message=FALSE, warning=FALSE}
#b
# Full and half-year fourier cycles
# annual cycle
x$y.cos <- cos(x$time.idx/12*2*pi)
x$y.sin <- sin(x$time.idx/12*2*pi)

#not assuming autocorrelation
gls(avg7447~time.idx+y.cos+y.sin, 
    data=x, 
    method="ML")
#assuming autocorrelation
gls1<- gls(avg7447~time.idx+y.cos+y.sin, 
           data=x, 
           correlation=corARMA(p=1), 
           method="ML")
summary(gls1)
qqnorm(gls1)
anova(gls1)
## Check residuals of corrected model
# type="normalized": AC adjusted residuals 
gls1.res <- ts(residuals(gls1, type="normalized"),    
               frequency = 12, 
               start = c(1990, 1)) 
acf(gls1.res , 
    xlab="lag in years",
    main="Adjusted Residuals AC-Function")
pacf(gls1.res , 
     xlab="lag in years", 
     main="Adjusted Residuals PAC-Function")
```

####c

- Use an autoregressive model of first order with autocorrelation to estimate  $\varphi_1 = 0.2962848$ and $\varphi_2 = 0.2146894$.
- The pvlaue of  likelihood ratio test is 0.0205,which is less than 0.05 .gls2 is better than gls1.
- this model $gls2$ fully controls for the autocorrelation


```{r, message=FALSE, warning=FALSE}
gls2<- gls(avg7447~time.idx+y.cos+y.sin, data=x, correlation=corARMA(p=2), method="ML")
summary(gls2)
qqnorm(gls2)
anova(gls2,gls1,type = 'LRT')
## Check residuals of corrected model
# type="normalized": AC adjusted residuals 
gls2.res <- ts(residuals(gls2, type="normalized"),     
                 frequency = 12, start = c(1990, 1)) 
acf(gls2.res , xlab="lag in years", main="Adjusted Residuals AC-Function")
pacf(gls2.res , xlab="lag in years", main="Adjusted Residuals PAC-Function")

```

####d


```{r, message=FALSE, warning=FALSE}
# mt <- model.matrix(object = gls2)
mt <- read.csv('C://Users//mali//Documents//a.csv')
mod.ARMA <- TSA::arima(avg7447_ts, 
                  order=c(2,0,0),
                  xreg=mt[,-1],
                  include.mean=F)
# coefficients
mod.ARMA$coef 
# t-values
mod.ARMA$coef/sqrt(diag(mod.ARMA$var.coef)) 
```


##Task 3: Heteroscedasticity (2.5 points)

####a

- The pvalues of model $lm1$ are both statistical significance.
- There maybe some relation with log residuals and log CIVILPOP.

```{r, message=FALSE, warning=FALSE}
library(car)
y <- read.dbf('C://Users//mali//Documents//TXCounties.DBF')
source('C://Users//mali//Documents//m.r')
str(y)
lm1 <- lm(formula = UNINSURED ~ URBRURAL + 
            log(PARTBLACK + 0.01) +
            NOGOODENG + INCOME + 
            UNEMPL, 
          data = y)
summary(lm1)
plot(log(y$CIVILPOP),
     log(residuals(lm1)^2),
     xlab='the log of CIVILPOP',
     main='the log of the  \n squared OLS residuals')
abline(lm( log(residuals(lm1)^2)~log(y$CIVILPOP)),col='red')
```

####b

Pvalue of ncvTest for the OLS model with regards to CIVILPOP is $0$,which is statistical significance. We should reject $H_0$ and it is heteroscedasticity.

```{r, message=FALSE, warning=FALSE}
ncvTest(lm1)
ncvTest(lm1, var.formula=~log(CIVILPOP), data=y)  
```

####c

- Regression coefficients of multiplicatively weighted regression model  are both statistical significance.
- Gamma coefficients is statistical significance.
- Heteroscedasticity likelihood ratio test is statistical significance.
  
```{r, message=FALSE, warning=FALSE}
lm2 <- lmHetero(UNINSURED ~ URBRURAL + log(PARTBLACK + 0.01) + NOGOODENG + INCOME + UNEMPL,hetero= ~log(CIVILPOP), data = y)
summary(lm2)
all.equal(1/lm2$weights[1], lm2$sigma2)
wlm2 <- lm(UNINSURED ~ URBRURAL + log(PARTBLACK + 0.01) + NOGOODENG + INCOME + UNEMPL,data=y,weights=log(y$CIVILPOP))
summary(wlm2)
```

####d

The weighted residuals  and the weights variable seem independent by the homoscedastic weighted residuals plot.

```{r}
plot(log(weighted.residuals(wlm2)^2)~log(y$CIVILPOP))     
abline(lm(log(weighted.residuals(wlm2)^2)~log(y$CIVILPOP),
          col='red'))
title("Homoscedastic Weighted Residuals")
```


##Task 4: Logistic Regression (4 points)


####a

I think  there is not relation with husband's college attendance  and labor-force participatio. And their husband's college is random I guess.

####b

- the p value of k618 is $0.345942>0.05$,and p value of hcyes is $0.645306 >0.05$ which are not statistical significance and should be deleted.
- the p value of Intercept,k5,age,wcyes,lwg and inc is both less than 0.05 which are statistical significance and should be keep .


```{r, message=FALSE, warning=FALSE}
library(effects)  
data(Mroz, package = "car")
Mroz$lfp <- ifelse(Mroz$lfp=='yes',1,0)
## The probit model 
glm1 <- glm(lfp ~ ., 
            family=binomial(link=probit), 
            data=Mroz)
summary(glm1)
## Likelihood Ratio Test
anova(glm1,test="LRT")
```

####c

- Use a Stepwise Algorithm by AIC to refine the new model by dropping variables. The new model is $glm2$ ,which is $lfp ~ k5 + age + wc + lwg + inc$.
- The dropped variables jointly have explanatory power  with a likelihood ratio test  because of $pvalue =0.5727>0.05$.


```{r}
glm2 <- step(glm1,direction = 'both')
## Likelihood Ratio Test
anova(glm1,glm2,test="LRT")
```

####d

- The probability of $lfp=yes$ would  reduce with $k5$ increased.
- The probability of $lfp=yes$ would reduce with $age$ increased.
- The probability of $lfp=yes$ when $wc=yes$ is more than the probability when $wc=no$.
- The probability of $lfp=yes$ would  increase. with $k5$ increased.
- The probability of $lfp=yes$ would  reduce with $inc$ increased.

```{r}
plot(allEffects(glm2), 
     type="response", 
     ylim=c(0,1), 
     ask=FALSE)
```

####e

High probability respondents tells me that the probability of $lfp=yes$ would  reduce with $inc$ increased.

```{r}
#e
# Low prob respondent
eff.glm2.low <- effect("inc",glm2,
                       xlevels=20,              given.values=c(k5=2,age=49,"wcyes"=0,lwg=0.81))

plot(eff.glm2.low, 
     type="response", 
     ylim=c(0,1), 
     ylab=expression(Pr(lfp[i]=="Yes")),
     main="Low Probability Respondents")

# High prob respondent
eff.glm2.hi <- effect("inc",glm2,
                      xlevels=20,
                  given.values=c(k5=0,age=26,"wcyes"=1,lwg=1.40))

plot(eff.glm2.hi, 
     type="response", 
     ylim=c(0,1),
     ylab=expression(Pr(lfp[i]=="Yes")), 
     main="High Probability Respondents")
```

