---
title: "DSC5103 Test 1 solutions"
subtitle: three questions, 20 points in total
date: "Oct 2015"
output:
  html_document:
    highlight: tango
    theme: spacelab
  pdf_document:
    highlight: zenburn
---
<!--
comments must be put in an HTML comment form
-->

```{r set-options, echo=FALSE, cache=FALSE}
options(width = 120)  # set output width
```


The file **JamesHarden2014.RData** is a dataset of shot records of an NBA (basketball) player James Harden in NBA regular season 2014-2015, obtained from the official NBA website http://stats.nba.com. 

The data has been cleaned and prepared in an RData format. You can load the file by doing the following. After successfully loading the file, you should see a data frame called **data** with 1443 rows and 30 columns.
```{r}
load("JamesHarden2014.RData")  # load the data (make sure the .RData file is in your working directory!)
dim(data)  # show data dimensions
summary(data)  # summary
```

Every row is a shot attempt made by Harden. Here is a brief description of the columns.

- "GAME_ID": unique id of the game;
- "PERIOD": period of the game (NBA games have four periods, period==5 means extra time);
- "SHOT_NUMBER": id of the shot in the game
- "PLAYER_ID": Harden's player id in the NBA database
- "PLAYER_NAME": player name in text
- "TEAM_ID": id of the team Houston Rockets
- "TEAM_NAME": name of the team in text
- "MINUTES_REMAINING": the number of minutes remaining in the period when the shot is taken
- "SECONDS_REMAINING": the number of seconds remaining in the period when the shot is taken         
- "ACTION_TYPE": type of the shot action (categorical)
- "SHOT_TYPE": type of the shot (categorical: 2-point or 3-point)
- "SHOT_ZONE_BASIC": zone where the shot is taken (categorical)
- "SHOT_ZONE_AREA": area where the shot is taken (categorical)
- "SHOT_ZONE_RANGE": range of the shot (categorical)
- "SHOT_DISTANCE": shot distance in feet
- "LOC_X": X location of the player when taking the shot (in unknown unit)
- "LOC_Y": Y location of the player when taking the shot (in unknown unit)
- "MATCHUP": text description of the game
- "LOCATION": location of the game (categorical: home or away)
- "W": result of the game (categorical: win or loss)
- "FINAL_MARGIN": final points difference between Rockets and the opponent
- "GAME_CLOCK": time in game (in unknown unit)
- "SHOT_CLOCK": number of seconds left to shoot (NBA's 24-second rule)
- "DRIBBLES": the distance that Harden dribbled before the shot (in feet)
- "TOUCH_TIME": the time that Harden touches the ball before the shot (in seconds)
- "SHOT_RESULT": result of the shot (categorical: True or False)
- "CLOSEST_DEFENDER": name of the closest defender
- "CLOSEST_DEFENDER_PLAYER_ID": id of the closest defender
- "CLOSE_DEF_DIST": distance of the closest defender (in feet)
- "PTS": number of points gained by the shot


### Q1. Analysis on **FINAL_MARGIN** (5 points)
The first question to investigate is on the relationship between the FINAL_MARGIN and LOCATION. In other words, does playing a home or away game have an effect on the final margin of the game?

#### a. Develop a regression model using **data** to answer the question above.

-----

***ANSWER of Q1a:***

```{r}
lm1 <- lm(FINAL_MARGIN ~ LOCATION, data)
summary(lm1)
```
On average, away games will have **`r coef(lm1)[2]`** points less in the final margin. The difference is statistically very significant.

-----

#### b. Instead of using **data**, which is a dataset about shots, some may argue that we should run the analysis on the game level: regressing the FINAL_MARGIN on LOCATION for all the games. Below is an aggregated dataset about all the 80 games in **data**.
```{r}
library("plyr")
data.game <- ddply(data, .(GAME_ID), function(x) {x[1, c("FINAL_MARGIN", "LOCATION")]})
summary(data.game)
```
#### Develop a regression model using **data.game** to answer the same question above.

----

***ANSWER of Q1b:***

```{r}
lm2 <- lm(FINAL_MARGIN ~ LOCATION, data.game)
summary(lm2)
```
On average, away games will have **`r coef(lm2)[2]`** points less in the final margin, but the difference is not significant (p-value > 10%).

----

#### c. The two regression models seem to give different answers. Which one should be used? Briefly explain why.

----

***ANSWER of Q1c:***

The second model is more reasonable because every game is counted exactly once. In the first model, one game may have many repetitive entries (depending on how many shots by Harden in the game). This creates two problems: (1) the number of shots in home games and away games may be very different, which introduces bias to the estimation. (2) By repeatedly counting the games, we are artificially increasing the number of data points. The significance actually comes from the exaggerated amount of data.

----



### Q2. Analysis on **SHOT_TYPE** (5 points)
Let's classify whether a shot is a 3-point or 2-point shot (**SHOT_TYPE**) based on the player's location (**LOC_X** and **LOC_Y**). In case you do not know basketball: a shot will either be 2-point or 3-point depending on the location of the player when taking the shot. There is clearly defined boundary, roughly a half circle, and shots outside will be a 3-pointer. In this sense, we should be able to almost perfectly predict **SHOT_TYPE** by the X, Y locations.

First plot the shot locations and color the points by shot type.
```{r, warning=FALSE}
library("ggplot2")
p3 <- ggplot(data=data) + geom_point(aes(x=LOC_X, y=LOC_Y, color=factor(SHOT_TYPE))) + ylim(c(0,300)) + theme_bw()
p3
```

#### a. Construct a Logistic Regression model to predict **SHOT_TYPE** using **poly(LOC_X, 2)** and **poly(LOC_Y, 2)** (because the boundary is a half circle, you will need 2nd-order polynomial for both X and Y). 

Note: since the 2-pointer and 3-pointer are perfectly separable, you may see warnings such as *glm.fit: algorithm did not converge* and *glm.fit: fitted probabilities numerically 0 or 1 occurred*. Do not worry about them.

----

***ANSWER of Q2a:***

```{r}
glm.3pt <- glm(SHOT_TYPE ~ poly(LOC_X, 2) + poly(LOC_Y,2), family = binomial, data=data)
summary(glm.3pt)
```


----

Next, plot the boundary of the fitted Logistic Regression model on top of the previous plot. Like what we have done before, we need to create a grid on the basketball court and predict the class of each point on the grid. The following code does the creation of grid dataframe.
```{r}
data.grid <- expand.grid(LOC_X=seq(-250, 250), LOC_Y=seq(0, 300))
```

#### b. Make predictions on the new data **data.grid** using the Logistric Regression model we built in part (a), and plot the boundary on the previous plot (**p3**).

----

***ANSWER of Q2b:***

```{r}
data.grid$prob.3pt <- predict(glm.3pt, newdata=data.grid, type="response")
p3 + stat_contour(data=data.grid, aes(x=LOC_X, y=LOC_Y, z=prob.3pt), breaks=c(0.5))
```

----



### Q3. Analysis on **SHOT_RESULT** of 2-pointers (10 points)
In the following, we will try to predict the outcome of 2-point shots. We need to subset the data that only consists of 2-point shots.
```{r}
## split 3pt and 2pt
data2 <- subset(data, SHOT_TYPE=="2PT Field Goal")
```
In the rest of this question, we shall use **data2** as the dataset for analysis. Let's visualize **SHOT_RESULT** by X, Y locations.
```{r}
ggplot(data=data2) + geom_point(aes(x=LOC_X, y=LOC_Y, color=factor(SHOT_RESULT))) + theme_bw()
```

#### a. Build a simple Logistic Regression model to investigate the effect of **SHOT_DISTANCE**, and briefly interpret the sign of the coefficient for SHOT_DISTANCE that you obtain.

----

***ANSWER of Q3a:***

```{r}
glm1 <- glm(SHOT_RESULT ~ SHOT_DISTANCE, family=binomial, data=data2)
summary(glm1)
```
The coefficient **`r coef(glm1)[2]`** is significant and negative, meaning that the large the shot distance, the smaller the change of making a shot in.

----


#### b. Build a simple Logistic Regression model to investigate the effect of defender distance (**CLOSE_DEF_DIST**). According to the model, what is the probability of scoring if the closest defender is 3 feet away?

----

***ANSWER of Q3b:***

```{r}
glm2 <- glm(SHOT_RESULT ~ CLOSE_DEF_DIST, family=binomial, data=data2)
summary(glm2)
```

The probability of scoring with *CLOSE_DEF_DIST=3* can be predicted as below. 
```{r}
predict(glm2, newdata=data.frame(CLOSE_DEF_DIST=3), type="response")
```

----

#### c. Build a simple Logistic Regression model to investigate the effect of **ACTION_TYPE**. Among the 3 models built so far in Q3a, Q3b and Q3c, which model better predicts SHOT_RESULT?

----

***ANSWER of Q3c:***

```{r}
glm3 <- glm(SHOT_RESULT ~ ACTION_TYPE, family=binomial, data=data2)
summary(glm3)
```
The model in Q3c does the best because it has the lowest AIC and Residual Deviance.

----


#### d. Use the *stepAIC( )* function in the "MASS" package to do an automatic selection of the best Logistic Regression model that predicts **SHOT_RESULT**. Print a summary of the best model you have found at the end.
Note: You need to decide which variables should or should not be taken into consideration by stepAIC( ). For the sake of the test, let's not consider polynomials and transformations. Only choose the variables in their original forms.

----

***ANSWER of Q3d:***

```{r}
# First construct the maximal model
glm.all <- glm(SHOT_RESULT ~ PERIOD + MINUTES_REMAINING + SECONDS_REMAINING + ACTION_TYPE + SHOT_ZONE_BASIC + SHOT_ZONE_AREA + SHOT_DISTANCE + LOCATION + SHOT_CLOCK + DRIBBLES + TOUCH_TIME + CLOSE_DEF_DIST, family=binomial, data=data2)
library("MASS")
glm.best <- stepAIC(glm.all, scope=list(upper = glm.all, lower = glm1))
summary(glm.best)
```


----


Finally, we will do some cross validation. But before we start, we need to tackle one problem. If you check the frequency of variable **ACTION_TYPE**,
```{r}
table(data2$ACTION_TYPE)
```
there are a lot of action types that only have several data points. They will become big trouble if we do cross validation. Let's get rid of them by constructing a new variable **ACTION_TYPE2**, which only keeps those action types with at least 50 data points. All other types will be combined into one category called "others." The following code will get the job done.
```{r}
levels.old <- levels(data2$ACTION_TYPE)
levels.new <- ifelse(table(data2$ACTION_TYPE) > 50, levels.old, "others")
data2$ACTION_TYPE2 <- data2$ACTION_TYPE
levels(data2$ACTION_TYPE2) <- levels.new
table(data2$ACTION_TYPE2)
```


#### e. Consider the following two Logistic Regression models.
```{r}
glm.fit1 <- glm(SHOT_RESULT ~ ACTION_TYPE2 + SHOT_DISTANCE + CLOSE_DEF_DIST, family=binomial, data=data2)
glm.fit2 <- glm(SHOT_RESULT ~ MINUTES_REMAINING + ACTION_TYPE2 + SHOT_ZONE_BASIC + SHOT_DISTANCE + LOCATION + TOUCH_TIME + CLOSE_DEF_DIST, family=binomial, data=data2)
```
#### Run a 10-fold cross validation (once is enough, no need to repeat) of the models on **data2**, use the "ROCR" package to plot the two cross-validated misclassification rates (Error rate in ROCR) against the cutoff probability, and find the optimal cutoffs that gives you the lowest cross-validated misclassification error in both models.


----

***ANSWER of Q3e:***

In order to plot against the cutoff probability, we need to do k-fold cross validation manually. We will save the predicted probabilities into a new column **cv.prob** back in **data2**.
```{r}
K <- 10
set.seed(54321)
folds <- sample(1:K, nrow(data2), replace=TRUE)  # random split
# start the k-fold CV
for (k in 1:K) {
    # fit the two models
    glm.fit1 <- glm(SHOT_RESULT ~ ACTION_TYPE2 + SHOT_DISTANCE + CLOSE_DEF_DIST, family=binomial, data=data2[folds != k, ])
    glm.fit2 <- glm(SHOT_RESULT ~ MINUTES_REMAINING + ACTION_TYPE2 + SHOT_ZONE_BASIC + SHOT_DISTANCE + LOCATION + TOUCH_TIME + CLOSE_DEF_DIST, family=binomial, data=data2[folds != k, ])
    # prediction on the validation data
    data2[folds == k, "cv.prob1"] <- predict(glm.fit1, newdata=data2[folds == k, ], type="response")
    data2[folds == k, "cv.prob2"] <- predict(glm.fit2, newdata=data2[folds == k, ], type="response")
}
```

With the predicted probabilities **cv.prob**, we can plot the misclassification error against the cutoff.
```{r}
library("ROCR")
pred1 <- prediction(data2$cv.prob1, data2$SHOT_RESULT)
pred2 <- prediction(data2$cv.prob2, data2$SHOT_RESULT)
perf1 <- performance(pred1, measure="err")
perf2 <- performance(pred2, measure="err")
plot(perf2, col="red")
plot(perf1, col="blue", add=TRUE)
```

The optimal cutoff for model 1 is **`r perf1@x.values[[1]][which.min(perf1@y.values[[1]])]`**, and the optimal misclassification rate is **`r min(perf1@y.values[[1]])`**; for model 2, it is **`r perf2@x.values[[1]][which.min(perf2@y.values[[1]])]`**, and the optimal misclassification rate is **`r min(perf2@y.values[[1]])`**.

----


***[THE END]***
