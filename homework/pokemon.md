---
title: "DSC5103 Test 1 solutions"
subtitle: 3 questions, 20 points in total
date: "Sep 2016"
output:
  html_document:
    highlight: tango
    theme: yeti
---
<!--
comments must be put in an HTML comment form
-->

```{r set-options, warning=FALSE, echo=FALSE, cache=FALSE}
options(width = 100)  # set output width
library("plyr")
library("ggplot2")
```

## NOTE:
This is an *individual* test. You can refer to whatever sources of information or materials, online or offline, but do not communicate with any other people. You can work on this file directly and fill in your answers/code below. Please submit the output RMD and HTML file (name your file like e0012345.rmd and e0012345.html if your NUS ID is e0012345) onto IVLE/Files/Student Submission/Test1 folder. *This test is to be finished in 100 minutes; submission folder will be closed automatically when time is up.*

Also, fill your info below. (**This is critical as your user id will be used in determining your random sample of the data!**)
```{r}
student.name <- "Lady Gaga"  # put your name here
student.id <- 0012345  # put only the numeric digits of your NUS id here
```

## Overview
The file **pokemon.rata** is a dataset of pokemon spawns in Singapore from September 17 to 20 in the game Pokemon Go, obtained from the website http://sgpokemap.com.

In the raw data, each record is a pokemon spawn, which consists of the **pokemon id** (if you are not familar with the game, check out the [Pokemon Index](http://handbooks.bulbagarden.net/pokemongo/pokemon-index), the **location** in longitude and latitude, the **despawn time** (meaning when the pokemon will disappear), and the **found time** (when the record was  first found). 

The data has been cleaned and prepared in an RData format. You can load the file by doing the following. 
```{r}
load("pokemon.rdata")  # load the data (make sure the .rdata file is in your working directory!)
```
After successfully loading the file, you should see two data frames called **data.I** and **data.II** together with some other auxilliary objects. 
```{r}
ls()  # list all the objects in the data file
```
We will check the data frames later when we need to use them.



## Part I: Analysis of Spawn Rate

In Part I, we will work on data frame **data.I**. You can check the dimension and summary.
```{r}
dim(data.I)  # show data.I dimensions
summary(data.I)  # summary
```
Here, every row is a particular pokemon spawn. Below is a brief description of the columns.

- "pokemon_id": the kind of the pokemon according to [Pokemon Index](http://handbooks.bulbagarden.net/pokemongo/pokemon-index) (1, 2, ..., 151);
- "lat": the latitude of the spawn location, in earth coordinates;
- "lon": the longitude of the spawn location, in earth coordinates;
- "found_date": the date when the spawn is found (17, 18, 19, 20);
- "found_weekday": the weekday when the spawn is found (6 for Saturday, 7 for Sunday, 1 for Monday, 2 for Tuesday);
- "found_hour": the hour when the spawn is found (0, 1, ..., 23);
- "found_min": the minute when the spawn is found (0, 1, ..., 59);
- "found_sec": the second when the spawn is found (0, 1, ..., 59);
- "found_night": whether it is found at night or not (TRUE/FALSE);
- "residual_life": the number of seconds until the pokemon disappear from the game;
- "type": the main type of the pokemon ("Normal", "Water", "Fire", ...), available at [Pokemon Index](http://handbooks.bulbagarden.net/pokemongo/pokemon-index);
- "region": the region in Singapore that the spawn location belongs to ("CLEMENTI", "WOODLANDS", ...), NA indicates the location is on the sea;
- "area": the area of the respective region that the spawn location belongs to (in the unit of KM^2).




### Q1. Spawn rate of [Dratini](http://handbooks.bulbagarden.net/pokemongo/pokemon/dratini) by region (4 points)
The first question is to investigate factors affecting the spawn rate of a particular pokemon, [Dratini](http://handbooks.bulbagarden.net/pokemongo/pokemon/dratini) <img src="`r pokemons[pokemons$pokemon_id == 147, "ImageURL"]`" style="width: 24px;"/> (pokemon_id == 147).

We first obtain a subset of the dataset that is relevant to Dratini.
```{r}
data1 <- subset(data.I, pokemon_id == 147)
dim(data1)  # check the dataset size
```
We can also have a quick visualization of where Dratini's are showing up in Singapore.
```{r}
sgp.plot + geom_point(data=data1, aes(x=lon, y=lat), col="lightblue", alpha=0.2)
```

In order to investigate the spawn rate, we need to first aggregate the data by counting the number of Dratini's in each hour and region.
```{r}
data1.by.hour.region <- count(data1, c("found_date", "found_hour", "found_night", "region", "area"))
head(data1.by.hour.region, n=10)  # preview aggregated data
```
The last column "freq" is the count in each hour and region.

#### a. Develop a model to analyze the effect of **region** on **freq**. Which region has the highest *freq*? Interpret the meaning of the corresponding coefficent of that region. (2 points)

-----

***ANSWER of Q1.a:***

Because *freq* is a discrete count, we should use Poisson regression. The model is as follows.
```{r}
model1a <- glm(freq ~ region, family=poisson(), data=data1.by.hour.region)
summary(model1a)
```

The region with the highest freq is **`r model1a$xlevels$region[which.max(coef(model1a))]`**. Its coefficient is **`r max(coef(model1a))`**, meaning that the average spawn rate of Dratini per hour is **`r exp(coef(model1a)[1] + max(coef(model1a)))`**.

-----

#### b. The previous analysis seems not fair for small regions because it does not take the area of the regions into account. Develop a model to take **area** into account. Which region has the highest *freq* per hour per unit area (square kilometer)? (2 points)

----

***ANSWER of Q1.b:***

What we can do is to convert the Y variable from absolute freq to the freq per unit area. Now the Y variable is no longer discrete counts, so we need to use linear regression.
```{r}
data1.by.hour.region$freq.per.unit.area <- data1.by.hour.region$freq / data1.by.hour.region$area
model1b <- lm(freq.per.unit.area ~ region, data=data1.by.hour.region)
summary(model1b)

```
The region with the highest freq per unit area is **`r model1b$xlevels$region[which.max(coef(model1b))]`**. Its coefficient is **`r max(coef(model1b))`**, meaning that the average spawn rate of Dratini per hour per square kilometer is **`r coef(model1b)[1] + max(coef(model1b))`**.

**Remark:** Alternatively, we can just pull out the coefficients obtained in Q1.a and divide them by the corresponding area.

----



### Q2. Overall spawn rate in a region by time and pokemon_id (6 points)
Next let us focus on a particular region. We will randomly choose one region to study.
```{r}
set.seed(student.id)  # the random sample is determined by the student.id!
my.region <- sample(regions, 1)
cat("My region of interest is", my.region)
```
We can subset the data that is only relevant to our region of interest **`r my.region`** and visualize it.
```{r}
data2 <- subset(data.I, region == my.region)
sgp.plot + geom_point(data=data2, aes(x=lon, y=lat), size=0.5, alpha=0.01)
```

In the following, we investigate how spawn rate is affected by hour of the day and pokemon type. We first aggregate the data.
```{r}
data2.by.hour.type <- count(data2, c("found_date", "found_hour", "found_night", "type"))
head(data2.by.hour.type)
```

#### a. Develop a model to check the effect of **found_night** on **freq**. Interpret the result. (2 points)

-----

***ANSWER of Q2.a:***

The Poisson regression model is as follows.
```{r}
model2a <- glm(freq ~ found_night, data=data2.by.hour.type, family=poisson)
summary(model2a)
p.value <- summary(model2a)$coefficients["found_nightTRUE", "Pr(>|z|)"]
significance <- (p.value < 0.05)
```

The coefficient with respect to *found_night* is `r coef(model2a)["found_nightTRUE"]`. It is `r ifelse(significance, "", "not")` significant, with a p-value **`r p.value`**. So spawn rate in `r my.region` is `r ifelse(significance, "", "not")` statistically different (at 95% confidence level) in day and night.

-----


#### b. Develop a model to check whether different **type**s have different **freq** in **day and night**. Interpret the result. (2 points)

-----

***ANSWER of Q2.b:***

We need to introduce interaction between *found_night* and *type*. The Poisson regression model is as follows.
```{r}
model2b <- glm(freq ~ found_night * type, data=data2.by.hour.type, family=poisson)
summary(model2b)
```

The coefficient with respect to the interaction term *found_nightTRUE:typeRock* has the smallest p-value **`r summary(model2b)$coefficients["found_nightTRUE:typeRock", "Pr(>|z|)"]`**. So spawn rate in `r my.region` is not statistically different (at 95% confidence level) in day and night for all the pokemon types.

-----


#### c. According to the model developed in Q2.b, what is the average number of **Fire** type pokemons spawned in an hour at **night**? What is the probability of seeing **at least 5** such pokemons. (2 points)

-----

***ANSWER of Q2.c:***

```{r}
lambda <- predict(model2b, newdata=data.frame(type="Fire", found_night=TRUE), type="response")
prob <- ppois(4, lambda, lower.tail=FALSE)  # Prob(X >= 5) = Prob(X > 4)
```
The mean spawn rate according to the Poisson regression model is **`r lambda`**, and the probability of seeing 5 or more is **`r prob`**.

-----



## Part II: Classification of Fire/Water type pokemon

In Part II, we will work on data frame **data.II**.
```{r}
dim(data.II)  # show data.I dimensions
summary(data.II)  # summary
```
Here, every row is a particular **Fire** or **Water** type pokemon spawn. In addition to the columns available in *data.I*, we have nine extra columns of geospatial features of the spawn location:

- "dist_coast": minimum distance from the spawn location to Singapore coastline (in meters);
- "dist_water": minimum distance from the spawn location to inland water bodies (e.g., river, reservoir, canal) (in meters);
- "dist_airport": minimum distance from the spawn location to airports (in meters);
- "dist_pcn": minimum distance from the spawn location to the [Park Connector Network](https://www.nparks.gov.sg/gardens-parks-and-nature/park-connector-network) (in meters);
- "dist_park": minimum distance from the spawn location to parks (in meters);
- "dist_forest": minimum distance from the spawn location to forests (in meters);
- "dist_golf": minimum distance from the spawn location to golf courses (in meters);
- "dist_univ": minimum distance from the spawn location to universities and polytechniques (in meters);
- "dist_industrial": minimum distance from the spawn location to industrial zones (in meters).


In the following, we will work on classification of Fire/Water type pokemon based on the time, region, and geospatial features. To make your analysis easier, we will focus on a random subset of rare Fire/Water type pokemons. Again, the random sampling is based on your **student.id**.
```{r}
set.seed(student.id)
my.water.type <- sample(water.types, 4)  # sample 4 out of 6 rare water types
my.fire.type <- sample(fire.types, 4)  # sample 4 out of 6 rare fire types
```

So, the water type pokemons of interest are [*`r pokemons[pokemons$pokemon_id == my.water.type[1], "name"]`*](`r pokemons[pokemons$pokemon_id == my.water.type[1], "ImageURL"]`), [*`r pokemons[pokemons$pokemon_id == my.water.type[2], "name"]`*](`r pokemons[pokemons$pokemon_id == my.water.type[2], "ImageURL"]`), [*`r pokemons[pokemons$pokemon_id == my.water.type[3], "name"]`*](`r pokemons[pokemons$pokemon_id == my.water.type[3], "ImageURL"]`), and [*`r pokemons[pokemons$pokemon_id == my.water.type[4], "name"]`*](`r pokemons[pokemons$pokemon_id == my.water.type[4], "ImageURL"]`), and the fire type pokemons of interest are [*`r pokemons[pokemons$pokemon_id == my.fire.type[1], "name"]`*](`r pokemons[pokemons$pokemon_id == my.fire.type[1], "ImageURL"]`), [*`r pokemons[pokemons$pokemon_id == my.fire.type[2], "name"]`*](`r pokemons[pokemons$pokemon_id == my.fire.type[2], "ImageURL"]`), [*`r pokemons[pokemons$pokemon_id == my.fire.type[3], "name"]`*](`r pokemons[pokemons$pokemon_id == my.fire.type[3], "ImageURL"]`), and [*`r pokemons[pokemons$pokemon_id == my.fire.type[4], "name"]`*](`r pokemons[pokemons$pokemon_id == my.fire.type[4], "ImageURL"]`).

<img src="`r pokemons[pokemons$pokemon_id == my.water.type[1], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.water.type[1], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.water.type[2], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.water.type[2], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.water.type[3], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.water.type[3], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.water.type[4], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.water.type[4], "name"]`" style="width: 80px;"/>
 v.s. 
<img src="`r pokemons[pokemons$pokemon_id == my.fire.type[1], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.fire.type[1], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.fire.type[2], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.fire.type[2], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.fire.type[3], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.fire.type[3], "name"]`" style="width: 80px;"/>
<img src="`r pokemons[pokemons$pokemon_id == my.fire.type[4], "ImageURL"]`" alt="`r pokemons[pokemons$pokemon_id == my.fire.type[4], "name"]`" style="width: 80px;"/>

We take a subset the data with only the selected pokemons.
```{r}
data3 <- subset(data.II, pokemon_id %in% c(my.water.type, my.fire.type))
data3$type <- droplevels(data3$type)  # drop types with zero instances
summary(data3)
```

Before we start, let us visualize the data.
```{r}
sgp.plot + geom_point(data=data3, aes(x=lon, y=lat, color=type))
```


### Q3. Classification on **type** (10 points)

#### a. Develop a kNN model to classfify Fire/Water type based *only* on the location (namely, **lon** and **lat**). Choose the optimal k (for the number of neighbors) among 6 possible values (1, 3, 5, 7, 9, 11) according to the LOOCV misclassification rate. (2 points)

-----

***ANSWER of Q3.a:***

We use knn.cv() function in the FNN package to do LOOCV.
```{r}
library("FNN")
ks <- c(1, 3, 5, 7, 9, 11)
err.loocv <- rep(0, length(ks))

for (i in 1:length(ks)) {
    knn.model <- knn.cv(data3[, c("lon", "lat")], data3$type, k=ks[i], prob=TRUE)
    prob <- attr(knn.model, "prob")
    knn.prob <- ifelse(knn.model == "Water", prob, 1 - prob)
    knn.pred <- ifelse(knn.prob > 0.5, "Water", "Fire")
    err.loocv[i] <- sum(knn.pred != data3$type)  # misclassification rate
}
k.opt <- ks[which.min(err.loocv)]
err.loocv
```
The optimal model is to have k to be **`r k.opt`**, and the optimal misclassification rate is **`r min(err.loocv) / nrow(data3)`**.

-----



#### b. Develop a full logistic regression model with all available and reasonable predictors (excluding **region** and **area** to avoid issues in cross validation later), and run a backward model selection using the stepAIC() function. (For the sake of simplicity, you may introduce up to 4th-order polynomials for predictors "lon" and "lat".) (2 points)

-----

***ANSWER of Q3.b:***

The full model:
```{r}
lg.full <- glm(type ~  factor(found_date) + factor(found_hour) + found_night + residual_life +  dist_coast + dist_water + dist_airport + dist_pcn + dist_park + dist_forest + dist_golf + dist_univ + dist_industrial + poly(lon, 4) + poly(lat, 4), family=binomial(), data=data3)
summary(lg.full)
```


**Remark:**

- *pokemon_id* cannot be a predictor. If you know pokemon_id, you already know whether it is Fire/Water type.
- Predictors related to the found time must be converted to factors first.
- *found_date* and *found_weekday* are redundant and only one of them can be kept in the model.
- Both *found_min* and *found_sec* will be 60-level factors, and most likely they do not matter. So it is ok to remove them, or you can have them here and leave the decision to stepAIC().
- Ideally, we should include *region*. It will work here, but the variable, being a multi-level factor, will create a lot of trouble later when we do 10-fold cross validation.


Backward model selection:
```{r warning=FALSE}
library("MASS")
lg.opt <- stepAIC(lg.full, direction="backward")
```

The final logistic regression model is
```{r}
summary(lg.opt)
```


-----


#### c. Interpret the sign of the coefficients of the distance features in the final logistic regression model obtained above. (2 points)

-----

***ANSWER of Q3.c:***

The coefficients of the distances to *coast*, *water*, and *pcn* are negative, meaning the closer to those places, the higher the chance of seeing Water type as opposed to Fire type pokemons; the coefficients of the distances to *park*, *forest*, and *golf* are positive, so Fire type pokemons are relatively closer to those places.


-----



#### d. Compare the two models developed in Q3.a and Q3.b in terms of the ROC curve using 10-fold cross validation. (4 points)

-----

***ANSWER of Q3.d:***

We need to run 10-fold CV manually and save the predicted probabilities by the two models.
```{r}
K <- 10  # 10-fold
n <- nrow(data3)  # sample size

## manual k-fold Cross-Validation for ROC plots
# create a random partition
shuffled.index <- sample(n, n, replace=FALSE)
data3[shuffled.index, "fold"] <- rep(1:K, length.out=n)

# start the k-fold CV
for (k in 1:K) {
    # kNN model
    knn.fitted <- knn(data3[data3$fold != k, c("lon", "lat")], data3[data3$fold == k, c("lon", "lat")], data3[data3$fold != k, c("type")], k=k.opt, prob=TRUE)
    data3[data3$fold == k, "knn.prob"] <- ifelse(knn.fitted == "Water", attr(knn.fitted, "prob"), 1 - attr(knn.fitted, "prob"))
    # glm model
    glm.fitted <- glm(lg.opt$formula, family=binomial(), data=data3, subset=(fold != k))
    data3[data3$fold == k, "glm.prob"] <- predict(glm.fitted, newdata=data3[data3$fold == k, ], type="response")
}
```

Then we can use the ROCR package to plot the ROC curves.
```{r warning=FALSE, message=FALSE} 
library("ROCR")
pred.glm <- prediction(data3$glm.prob, data3$type)
roc.glm <- performance(pred.glm, measure="tpr", x.measure="fpr")
pred.knn <- prediction(data3$knn.prob, data3$type)
roc.knn <- performance(pred.knn, measure="tpr", x.measure="fpr")
plot(roc.glm)
plot(roc.knn, add=TRUE)
abline(a=0, b=1, lty=2)
```

It seems kNN works better here.

-----





***[THE END]***
