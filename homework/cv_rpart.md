---
title: "Cross-Validation"
output: html_document
---

In this lab, we will carry out cross-validation to select the complexity parameter for a classification tree. We will use the variables that we created in our homework on tweets. In order for us all to be working with the same data frame, load the rda file from the Web which contains a data frame of the 19 features and observations for Android and iPhone. 

```{r}
load(url("http://www.stat.berkeley.edu/users/nolan/data/trumpTweets.rda"))
```

## Test and Train Data

Before we create our predictor, we set aside a test set of data.  These data will not be used in the cross-validation or building of the predictor. We reserve it for the final evaluation of the predictor.

We begin by setting a random seed, and then sampling the rows (observations) for the test set.  Let's call the test set `tweetTest` and the train set `tweetTrain`.

```{r}
set.seed(24687531)
nTotal = nrow(trumpDF2dev)
chooseTest = sample(nTotal, size = 564, replace = FALSE)
tweetTest = trumpDF2dev[chooseTest, ]
tweetTrain = trumpDF2dev[ -chooseTest, ]
```

Note that we chose the test size to be 564, which leaves 2400 observations in our train set. This is convenient for 10-fold cross-validation.

## Creating the Folds

For cross-validation, we need to partition our data into random non-overlapping pieces. An easy way to do this is to work through the indices of the data, and not the data itself. The `sample` function can give us a random permutation of all the indices.

Call the `sample` function with the following inputs to see what I mean

```{r}
sample(20)
sample(20)
sample(20)
```

If we want to create 5 folds, we can use a random permutation of the indices and reshape the vector of indices into a matrix with one column for each fold. 

```{r}
set.seed(2468111)
permuteIndices = sample(20)
folds = matrix(permuteIndices, ncol = 5)
folds
```

The `folds` matrix should appear as

```
     [,1] [,2] [,3] [,4] [,5]
[1,]    5    3    1   13   16
[2,]    4   17   15   18   19
[3,]   11   12   20    6    2
[4,]    8    7   10    9   14
```

Now it's your turn. Create a 10-column matrix called `folds` that contains indices for partitioning the `tweetTrain` data frame into 10 folds.

```{r, error=TRUE}
nTrain = nrow(tweetTrain)
# Set the seed so we all get the same results
set.seed(12344321)
permuteIndices = sample(...)

v = 10
folds = matrix( )
```


## Building the Tree 

Now that we have our folds, for each fold we:

* build the tree based on 9/10 of `tweetTrain`
* predict the device using this tree for the remaining 1/10 of the data

We want to build a tree for several values of the complexity parameter `cp`. 

Then, we will have predictions for all observations in `tweetTrain` for several valus of `cp`. 

We use a double loop, over the folds and the complexity parameter values to get our predictions


```{r, error=TRUE}
cps = c(seq(0.0001, 0.001, by = 0.0001), 
       seq(0.001, 0.01, by = 0.001),
       seq(0.01, 0.1, by = 0.01))

preds = matrix(nrow = nTrain, ncol = length(cps))

for (i in 1:10) {
  trainFold = as.integer(folds[, -i])
  testFold = folds[, i]
  
  for (j in 1:length(cps)) {
    tree = rpart(dev ~ .,
            data = tweetTrain[FILL IN], 
            method = "class",
            control = rpart.control(cp = FILL IN))
    preds[ FILL IN ] = 
      predict(tree, 
              newdata = tweetTrain[FILL IN],
              type = "class")
  }
}
```

Note that the data frame passed in the `newdata` parameter in the call to `predict` SHOULD NOT contain `dev`.

## Assess the Error in the Cross-Validated Predictions 

We want to compare the prediction in each column of `preds` to the truth, which is in `tweetTrain$dev`. We need to settle on the kind of error calculation, e.g., a simple misclassification rate, a weighted misclassification weight, or separate type I and II  errors. Let's find the simple misclassification rate.

Calculate the proportion of correct predictions for each value of the complexity parameter values.

```{r, error = TRUE}
 
cvRates = apply(preds, FILL IN, function(oneSet) {
  YOUR CODE HERE 
})
```

## Choose the Value for `cp`

From our plot and the following statistics, choose a value for `cp`.  You may not want to choose the `cp` with the smallest error, but choose a slightly larger `cp` that has nearly the same error rate.

```{r, error=TRUE}
which.max(cvErrs)

cvRes = data.frame(cps, cvRates)
ggplot(data = cvRes, aes(x = cps, y = cvRates)) +
  geom_line() + 
  labs(x = "Complexity Parameter", y = "Classification Rate")
```

## Final Assessment of the Classification Tree Predictor

Now that you have selected `cp` using cross-validation on your training data, we can 

* Build the predictor with the chosen `cp` on all of `tweetTrain`

* Predict the device for the observtions in `tweetTest`

* Calculate the classification rate for `tweetTest`

```{r, error=TRUE}
cpChoice = FILL IN

finalTree = rpart(dev ~ .,
                  data = FILL IN, 
                  method = "class",
                  control = rpart.control(cp = FILL IN))
   
testPreds = predict(finalTree, 
              newdata = FILL IN,
              type = "class")

classRate = sum(testPreds == tweetTest$dev) / 
  nrow(tweetTest)

classRate
```

How well does your choice of `cp` do with the test data?
Does it do as well as with the cross-validated train data?
