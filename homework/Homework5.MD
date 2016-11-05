---
title: "Homework 5"
author: ""
date: 'Assigned: September 28, 2016'
output: 
  html_document:
    theme: paper
    highlight: tango
    toc: true
    toc_depth: 3
---

##### This homework is due by **11:59pm on Monday, October 10**.  

### Problem 1: Linear regression with bikeshare data

```{r}
library(ggplot2)
library(plyr)
```

For this problem we'll be working with two years of bikeshare data from the Capital Bikeshare system in Washington DC.  The dataset contains daily bikeshare counts, along with daily measurements on environmental and seasonal information that may affect the bikesharing.  

Let's start by loading the data.

```{r}
bikes <- read.csv("http://www.andrew.cmu.edu/user/achoulde/94842/data/bikes.csv", header = TRUE)

# Transform temp and atemp to degrees C instead of [0,1] scale
# Transform humidity to %
# Transform wind speed (multiply by 67, the normalizing value)

bikes <- transform(bikes,
                   temp = 47 * temp - 8,
                   atemp = 66 * atemp - 16,
                   hum = 100 * hum,
                   windspeed = 67 * windspeed)
```

Here's information on what the variables mean.

  - instant: record index
	- dteday : date
	- season : season (1:Winter, 2:Spring, 3:Summer, 4:Fall)
	- yr : year (0: 2011, 1:2012)
	- mnth : month ( 1 to 12)
	- hr : hour (0 to 23)
	- holiday : weather day is holiday or not (extracted from http://dchr.dc.gov/page/holiday-schedule)
	- weekday : day of the week
	- workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
	+ weathersit : 
		- 1: Clear, Few clouds, Partly cloudy, Partly cloudy
		- 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
		- 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
		- 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
	- temp : Temperature in Celsius. 
	- atemp: Feeling temperature in Celsius. 
	- hum: Humidity (%)
	- windspeed: Wind speed. 
	- casual: count of casual users
	- registered: count of registered users
	- cnt: count of total rental bikes including both casual and registered

**(a)** Season: Factor or numeric?

Consider the following two regression models.

```{r}
bikes.lm1 <- lm(cnt ~ yr + temp + hum + season, data = bikes)

bikes.lm2 <- lm(cnt ~ yr + temp + hum + as.factor(season), data = bikes)
```

**What is the difference between these two models?**

<font color="#157515">

 - bikes.lm1 模型将`season` 变量当作数值型变量带入了回归方程，这是不正确的。
 - 而bikes.lm2将`season`变量作为分类变量处理，在回归方程计算的时候，会先将`season`转为哑变量处理，bikes.lm2是正确的，这样估计的系数更加符实际。

</font>

**(b)** What is the interpretation the coefficient(s) of `season` in each of the models in part (a)?

```{r}
summary(bikes.lm1)
summary(bikes.lm2)
```


<font color="#157515">

 - bikes.lm1的`season`的系数为456.370；
 - bikes.lm2的`season`的系数当`season`=2的时候为1086.984，当`season`=3的时候为828.484，当`season`=4的时候为1615.981，当`season`=1的时候为0.

</font>

**(c)**  Use ggplot2 graphics to construct boxplots of count for each season.  Based on what you see from these plots, which model do you think makes more sense?  Explain.

```{r}
ggplot(data = bikes,aes(x =season,y = cnt,fill=as.factor(season)))+geom_boxplot(show.legend=T)+labs(title='boxplots of count for each season')
```

<font color="#157515">

bikes.lm2 is correct.

</font>

**(d)**  Using `ggplot2` graphics, construct a scatterplot of `cnt` (bikeshare count) across `mnth` (month of the year).  Describe what you see.  Would a linear model be a good way of modeling how bikeshare count varies with month?  

```{r}
ggplot(data = bikes,aes(x = mnth,y=cnt))+geom_point()+labs(x='month of the year',title='scatterplot of cnt  across mnth ')
```

**(e)**  Consider the following three models.  Figures of the model fits are shown to help you interpret the models.

```{r}
# Fit and plot first model
bikes.lm3 <- lm(cnt ~ mnth, data = bikes)
qplot(data = bikes, x = mnth, y = cnt) + stat_smooth(method = "lm")
```

```{r}
# Fit and plot second model
bikes.lm4 <- lm(cnt ~ as.factor(mnth), data = bikes)

# Construct data frame that has fitted values for each month
mnth.df <- data.frame(mnth = unique(bikes$mnth))
df.lm4 <- data.frame(mnth = unique(bikes$mnth),
                     fitted = predict(bikes.lm4, newdata = mnth.df))

# Red points are the fitted values from the model
qplot(data = bikes, x = mnth, y = cnt, alpha = 0.5) + 
  guides(alpha = FALSE) + 
  geom_point(data = df.lm4, aes(x = mnth, y = fitted), colour = I("red"),
             alpha = 1, size = I(5))
```


```{r}
# Fit and plot third model
bikes.lm5 <- lm(cnt ~ mnth + I(mnth^2), data = bikes)

qplot(data = bikes, x = mnth, y = cnt) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2))
```

**What are the differences between the models?  How many coefficients are used to model the relationship between bikeshare count and month in each of the models?**

*Answer here*

**(f)** Use the `plot()` function to produce diagnostic plots for `bikes.lm3`.  Do you observe any problems?  

```{r}
par(mfrow=c(2,2))
plot(bikes.lm3)
par(mfrow=c(1,1))
```

**(g)** Produce diagnostic plots for `bikes.lm4` and `bikes.lm5`.  Are any of the problems you identified in part (f) resolved?  Do any remain?  Explain.

```{r}
par(mfrow=c(2,2))
#bikes.lm4
plot(bikes.lm4)
#bikes.lm5
plot(bikes.lm5)
par(mfrow=c(1,1))
```

### Problem 2: Interpreting and testing linear models

This problem once again uses the `bikes` data.

**(a)** Use the `transform` and `mapvalues` functions to map the values of the `season` and `weathersit` variables to something more interpretable.  Your new `weathersit` variable should have levels: Clear, Cloud, Light.precip, Heavy.precip

```{r}
bikes_new <- transform(bikes,
                   season=mapvalues(bikes$season,from = 1:4,to=c('Winter','Spring','Summer','Fall'),warn_missing = F),
                   weathersit=mapvalues(bikes$weathersit,from=1:4,to=c('Clear','Cloud','Light.precip','Heavy.precip'),warn_missing = F))
```

**(b)** Fit a linear regression model with `cnt` as the outcome, and season, workingday, weathersit, temp, atemp and hum as covariates.  Use the `summary` function to print a summary of the model showing the model coefficients.

Note that you may wish to code the workingday variable as a factor, though this will not change the estimated coefficient or its interpretation (workingday is already a 0-1 indicator).


```{r}
m1 <- lm(cnt~as.factor(season)+as.factor(workingday)+as.factor(weathersit)+temp+atemp+hum,data=bikes_new)
summary(m1)
```

**(c)** How do you interpret the coefficient of `workingday` in the model?  What is its p-value?  Is it statistically significant?

```{r}
m2 <- lm(cnt~as.factor(workingday),data=bikes_new)
summary(m2)
```

**(d)** How do you interpret the coefficient corresponding to Light.precip weather situation?  What is its p-value?  Is it statistically significant?

```{r}
m3 <- lm(cnt~as.factor(weathersit),data=bikes_new)
summary(m3)
```

**(e)** Use the `pairs` function to construct a pairs plot of temp, atemp and hum.  The bottom panel of your plot should show correlations (see example in Lecture 9).


```{r}
pairs(bikes_new[,c('temp','atemp','hum')],lower.panel = 'panel.cor',upper.panel = 'panel.smooth')
```

**Do you observe any strong colinearities between the variables?**

```{r}
cor(bikes_new[,c('temp','atemp','hum')])
```

**(f)** Use the `update` function to update your linear model by removing the `temp` variable.  Has the coefficient of `atemp` changed?  Has it changed a lot?  Explain.

```{r}
m4 <- update(object = m1,formula. = .~.-temp)
summary(m4)
```

**(g)** How do you interpret the coefficient of `atemp` in the model from part (f)?



**(h)** Use the `anova()` function on your model from part (f) to assess whether `weathersit` is a statistically significant factor in the model.  Interpret your findings.  

```{r}
m5 <- update(object = m4,formula. = .~.-weathersit)
anova(m4,m5)
```


### Problem 3: Exploring statistical significance

This problem will guide you through the process of conducting a simulation to investigate statistical significance in regression.

In this setup, we'll repeatedly simulate `n` observations of 3 variables: an outcome `y`, a covariate `x1` that's associated with `y`, and a covariate `x2` that is unassociated with `y`.  Our model is:

$$ y_i = 0.5 x_1 + \epsilon_i$$

where the $\epsilon_i$ are independent normal noise variables having standard deviation 5 (i.e., Normal random variables with mean 0 and standard deviation 5).

Here's the setup.  

```{r}
set.seed(12345)  # Set random number generator
n <- 200  # Number of observations
x1 <- runif(n, min = 0, max = 10) # Random covariate
x2 <- rnorm(n, 0, 10)              # Another random covariate
```

To generate a random realization of the outcome `y`, use the following command.

```{r}
# Random realization of y
y <- 0.5 * x1 + rnorm(n, mean = 0, sd = 5)
```

Here's are plots of that random realization of the outcome `y`, plotted against `x1` and `x2`.

```{r}
qplot(x = x1, y = y)
qplot(x = x2, y = y)
```

**(a)** Write code that implements the following simulation (you'll want to use a for loop):

```
for 2000 simulations:
  generate a random realization of y
  fit regression of y on x1 and x2
  record the coefficient estimates, standard errors and p-values for x1 and x2
```

At the end you should have 2000 instances of estimated slopes, standard errors, and corresponding p-values for both `x1` and `x2`.  It's most convenient to store these in a data frame.

```{r, cache = TRUE}
# Note the cache = TRUE header here.  This tells R Markdown to store the output of this code chunk and only re-run the code when code in this chunk changes.  By caching you won't wind up re-running this code every time you knit.
s1<- function(a){
y <- 0.5 * x1 + rnorm(n, mean = 0, sd = 5)
lm1 <- lm(y~x1+x2)
dataset <- as.data.frame(summary(lm1)$coefficients[-1,])
dataset$variables <- row.names(dataset)
return(dataset[,c(5,1,2,4)])
}
set.seed(1000)
xx<-ldply(.data=dlply(.data = data.frame(id=1:2000),.variables = .(id),.fun = s1),.fun=data.frame)
head(xx)
```

**(b)**  This problem has multiple parts.

- Construct a histogram of the coefficient estimates for `x1`.  
- Calculate the average of the coefficient estimates for `x1`.  Is the average close to the true value?
- Calculate the average of the standard errors for the coefficient of `x1`.  Calculate the standard deviation of the coefficient estimates for `x1`.  Are these numbers similar?

```{r}
# Construct a histogram of the coefficient estimates for `x1`. 
hist(xx[xx$variables=='x1',3],main = 'histogram of the coefficient estimates for x1')

#Calculate the average of the coefficient estimates for x1.  Is the average close to the true value?
mean(xx[xx$variables=='x1',3])
t.test(xx[xx$variables=='x1',3],mu = 0.5)

#Are these numbers similar?
mean(xx[xx$variables=='x1',4])
sd(xx[xx$variables=='x1',3])
t.test(xx[xx$variables=='x1',4],mu=sd(xx[xx$variables=='x1',3]))
```

**Take-away from this problem**: the `Std. Error` value in the linear model summary is an estimate of the standard deviation of the coefficient estimates.  

**(c)** Repeat part (b) for `x2`.  

```{r}

#1
hist(xx[xx$variables=='x2',3],main = 'histogram of the coefficient estimates for x2')
#2
mean(xx[xx$variables=='x2',3])
t.test(xx[xx$variables=='x2',3],mu = 0)
#3
mean(xx[xx$variables=='x2',4])
sd(xx[xx$variables=='x2',3])
t.test(xx[xx$variables=='x2',4],mu=sd(xx[xx$variables=='x2',3]))
```

**(d)** Construct a histogram of the p-values for the coefficient of `x1`. What do you see?  What % of the time is the p-value significant at the 0.05 level?

```{r}
hist(xx[xx$variables=='x1',5],main = 'histogram of the p-values for the coefficient of x1',breaks = 20)
sum(xx[xx$variables=='x1',5]<0.05)/2000
```

**(e)** Repeat part (d) with `x2`.  What % of the time is the p-value significant at the 0.05 level?  

```{r}
hist(xx[xx$variables=='x2',5],main = 'histogram of the p-values for the coefficient of x2')
sum(xx[xx$variables=='x2',5]<0.05)/2000
```

**(f)** Given a coefficient estimate $\hat \beta$ and a standard error estimate $\hat{se}(\hat\beta)$, we can construct an approximate 95% confidence interval using the "2 standard error rule".  i.e.,
$$ [\hat \beta - 2  \hat{se}, \, \hat \beta + 2  \hat{se}] $$
is an approximate 95% confidence interval for the true unknown coefficient.

As part of your simulation you stored $\hat \beta$ and $\hat{se}$ values for 2000 simulation instances.  Use these estimates to construct approximate confidence intervals and answer the following questions.

- **Question**: In your simulation, what % of such confidence intervals constructed for the coefficnet of `x1` actually contain the the true value of the coefficient ($\beta_1 = 0.5$).

<font color="#157515">

Replace this text with your answer. (do not delete the html tags)

</font>

- **Question**: In your simulation, what % of such confidence intervals constructed for the coefficient of `x2` actually contain the the true value of the coefficient ($\beta_2 = 0$).

<font color="#157515">

Replace this text with your answer. (do not delete the html tags)

</font>

### Problem 4: ddply 

For this problem we'll return to the `gapminder` data set from Lecture 10.

```{r}
library(plyr)
gapminder <- read.delim("http://www.andrew.cmu.edu/user/achoulde/94842/data/gapminder_five_year.txt")
```

**(a)** Use `ddply` to produce a summary table showing the maximum `gdpPercap` and the country that had the maximum `gdpPercap` for each continent and year.

```{r}
a<-ddply(.data = gapminder,.variables = .(country),.fun = summarise,maxgdp=max(gdpPercap))
b<-ddply(.data = gapminder,.variables = .(continent,year),.fun = summarise,maxgdp=max(gdpPercap))
head(a)
head(b)
```

**(b)** Use `ddply` to produce a summary table showing the correlation between `lifeExp` and `gdpPercap` for each continent.

```{r}
c<-ddply(.data = gapminder,.variables = .(continent),.fun = summarise,cor=cor(lifeExp,gdpPercap))
head(c)
```

### Problem 5: dlply + ldply

This problem proceeds with the `gapminder` data.

**(a)** Use `dlply` to produce a list of linear regression models, one for each country, regressing `gdpPercap` on `year`.

```{r}
lm_gdp <- function(df) {
  lm(gdpPercap~year, data = df)
}
d <- dlply(.data = gapminder,.variables = .(country),.fun =lm_gdp )
head(d,2)
```

**(b)** Use `ldply` on your list from part (a) to produce a data frame displaying for each country whether the coefficient of `year` was significant at the 0.05 level.  Your output should be a two column data frame: the first column gives the country, and the second column displays a 0 if the coefficient of `year` was not significant, and a 1 if it was significant.  

```{r}
year_sign <- function(obj){
   x<-summary(obj)$coefficients[2,4]
   return(ifelse(x>0.05,0,1))
}
e <- ldply(.data = d,.fun = year_sign)
names(e)[2] <- 'significance'
head(e)
```

**(c)**  The following code produces a data frame giving the continent for each country.

```{r}
summary.continent <- ddply(gapminder, ~ country, summarize, continent = unique(continent))
```

Use the `merge` function to merge your data frame from part (b) with `summary.continent` to produce a data frame that shows both the slope significance indicator and also the continent.  (See Lecture 10 notes for examples of how to do this.)

```{r}
f <- merge(summary.continent,e)
```

**(d)** Using your data frame from part (c), produce a contingency table showing counts for each combination of slope significance and continent.

```{r}
head(f)
```


**(e)** What do you observe in the table from part (d)?  What does a non-significant slope (coefficient of year) suggest about a country's economic growth?  

<font color="#157515">

Replace this text with your answer. (do not delete the html tags)

</font>
