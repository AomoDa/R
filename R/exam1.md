---
title: "Predicting the Tweeting Device"
output: html_document
author: XXX and YYY
---

This assignment can be done in pairs. If you are doing the assignment alone then you need not create the two variables `numW` and `numAt`, and you need only create 2 of the 4 exploratory plots.


## Introduction

In this homework, you will extract features from a sample of tweets from @RealDonaldTrump. These tweets cover the period from 21 Jan through 11 Oct 2016.  In addition to the tweet itself, twitter also provides meta information about the tweet. This information includes the date of the tweet and the type of device on which the tweet was made.

There is a conjecture that @RealDonaldTrump tweets from an Android are different from tweets from an iPhone. We will investigate this conjecture in this homework asssignment. And some say that Donald Trump himself uses only one of these devices.

To study this conjecture, we will create the following variables from the tweets and their meta data:

* device on which the tweet was sent -- `dev`, factor with levels, Android, iPhone, and Other.
* number of characters in the tweet -- `numC`, numeric
* number of words in the treat (ok to count a url as a word) -- `numW`, numeric
* indicator for whether the tweet begins with a quotation mark -- `beginQ`, factor
* indicator for whether the tweet begins with RT (meaning a retweet) -- `rt`, factor
* indicator for whether the tweet contains a link -- `link`, factor
* number of hashtags in the tweet -- `numHT`, numeric
* number of twitter usernames in the tweet -- `numAt`, numeric
* hour of the day the tweet was sent -- `hr`, numeric

In addition, we take the 5 most popular hashtags and the 5 most popular usernames and create factors for each that indicate whether that hashtag or username appears in the tweet. I have provided these for you in a data frame at
"http://www.stat.berkeley.edu/users/nolan/data/topHashAndAt.rda"

## The Data

The data are in a json file and can be read into R with the `RJSONIO` package. After you install and load the package, read in the data with

```{r load}
library(RJSONIO)
tweetLines = readLines(url("http://www.stat.berkeley.edu/users/nolan/data/realDonaldTrump.json"))
tweetList = fromJSON(paste(tweetLines, collapse = " "))
names(tweetList) = NULL
load(url("http://www.stat.berkeley.edu/users/nolan/data/topHashAndAt.rda"))
```

Explore the list strucuture. Examine the top of each element to find where the tweet text, time, and device
appear. Extract the text of the tweets and place them in a character vector called `tweets`

```{r tweets, error=TRUE}
tweets = sapply(tweetList,FUN = function(x) x$text)
head(tweets)
```

## Derive features 

Next derive the 9 variables listed above, i.e., `dev`, `numC`, `numW`, `beginQ`, `rt`, `link`, `numHT`, `numAt`, and `hr`.  In lab, you wrote code to create `rt`, and `hr` and you wrote code that will help you create `numHT` and `numAt`. 

### Create `dev`

This variable is a factor with 3 levels. We lump together all devices other than the Android and iPhone. You may find it easiest to use `grepl` to create two logical vectors for whether the tweet was from an Android or an iPhone and combine these into a factor with the levels Android, iPhone, and Other. 

```{r dev, error=TRUE}
get_dev <- function(x){
 ifelse((grepl(pattern ="android",x$source)),"Android",
        ifelse((grepl(pattern = "iphone", x$source)), "iPhone", 
        "Other")) 
}
dev <-  as.factor(sapply(tweetList, get_dev))
summary(dev)
```

### Create `numC` and `numW`

Remember how to count characters in a string with `nchar`.  Also, it may be easiest to count the words by splitting each text string up into words. It's OK for a url to be a word. Careful not to split words like don't into don and t. 

```{r num, error = TRUE}
numC <- sapply(tweetList,FUN = function(x){
                nchar(x$text)
                 } 
               )
summary(numC)
numW <- sapply(tweetList,FUN = function(x){
                      length(unlist((
                            strsplit(x$text,
                               '[ ]'))))} 
               )
summary(numW)
```

### Create `beginQ`, `rt`, and `link`

The variables `beginQ` and `rt` are similar to create because they both look at the start of the string for a particular pattern. Also, the `grepl` function may be useful here. You can convert a logical into a factor after extracting the desired information as a logical.

```{r begin, error=TRUE}
rt = grepl(pattern = "^RT", tweets)
rt = factor(rt, levels = c(TRUE, FALSE), 
            labels = c("yes", "no"))
summary(rt)
beginQ = grepl(pattern = '^["]', tweets)
beginQ = factor(beginQ, levels = c(TRUE, FALSE), 
                labels = c("yes", "no"))
summary(beginQ)
```

In addition, search the tweet strings for a link(s) and create a factor variable, `link` that indicates whether or not there is a link in the tweet

```{r link, error=TRUE}
link = grepl(pattern = "http[s]?\\:\\/\\/", tweets)

link = factor(link, levels = c(TRUE, FALSE), 
                labels = c("yes", "no"))  
summary(link)
```

### Create `numHT` and `numAt`

Recall that hashtags begin with # and do not include punctuation or blanks. Usernames have similar rules: they begin with an @ and do not include punctuation or blanks. 
Use the `gregexpr` call that you developed in the lab to find the locations of the nodes. Recall that you named this list `hashLocs`. Write a short function to sapply over the numeric vectors in `hashLocs` and return the number of hashtags in the tweet. Note that if the vector is -1 then the corresponding tweet has no hashtags and you want to return 0. Otherwise, return the length of the vector. 


```{r numHash, error=TRUE}
hashLocs = gregexpr(pattern = "#[0-9A-Za-z]*", 
                    tweets)
# there is '#',but  not '#+'.
# because I want to get the 
# length of vector which does not include '#'
numHT = sapply(hashLocs, function(x) {
  ifelse((x[[1]]==-1), 0, attr(x,'match.length')-1 ) 
#length of vector which does not include '#'
})
summary(numHT)
```

Very similar code can be used to find and count the number of usernames in the tweets. 

```{r numAt, error = TRUE}
atLocs = gregexpr(pattern = "@[0-9A-Za-z]*",
                  tweets)
# there is '@',but not  '@+'.
# because I want to get the 
# length of vector which does not include '@' 
numAt = sapply(atLocs, function(x) {
  ifelse((x[[1]]==-1), 0, attr(x,'match.length')-1 )
# length of vector which does not include '@'
})
summary(numAt)
```

### Create `hr`

Finally create the variable `hr`, which is the hour of the day that the tweet was sent. Remember that you wrote code to do this in the lab assignment.

```{r hr, error=TRUE}
hr =  sapply(tweetList,function(x){
  as.numeric(unlist(
    strsplit(unlist(x$created_at),
             '[ ]|[:]'))[4])
})

summary(hr)
```

## Putting it all together

Create a data frame containing all 9 variables.
Merge this data frame with the one available on the web that contains ten variables -- factors indicating whether a particular hashtag or username appears in the tweet.
Use `cbind` to perform the merge. Call this data frame `trumpDF`

```{r, error=TRUE}

trumpDF = data.frame(dev, numC, numW, beginQ, rt, link, numHT, numAt, hr)
trumpDF = cbind(trumpDF, tagsats)
str(trumpDF)
```

## Explore the variables

Before we proceed with developing a predictor, make a few (~4) simple visualizations (e.g., boxplots, barplots, mosaicplots) of the variables that you created.  We are particularly interested in which variables, if any, might distinguish between tweets sent from an Android and tweets sent from an iPhone. Make visualizations that will help uncover possible relationships.  In a few sentences, describe what you see in each plot.

```{r plot1}
trumpDF2dev = trumpDF[trumpDF$dev != "Other",]
library(ggplot2)
mosaicplot(with(trumpDF2dev,table(dev,link)),shade = T)
ggplot(data = trumpDF2dev) +
  geom_bar(aes(dev, fill=link))+
  scale_x_discrete(name = "link")
```

plot1 findings:



```{r plot2}
ggplot(data = trumpDF2dev) +
  geom_boxplot(aes(dev, numW,fill=dev)) +
  scale_x_discrete(name = "devices") +
  scale_y_continuous(name = "number of words")
```

plot2 findings:



```{r plot3}
mosaicplot(with(trumpDF2dev,table(dev,beginQ)),shade = T)
ggplot(data = trumpDF2dev) +
  geom_bar(aes(dev, fill=beginQ))+
  scale_x_discrete(name = "beginQ")
```

plot3 findings:



```{r plot4}
ggplot(data = trumpDF2dev) +
  geom_bar(aes(dev, fill=rt)) +
  scale_x_discrete(name = "retweet")
```

plot4 findings:



## Classification Tree

Finally, create a classification tree with the goal of prediciting the device (Android or iPhone) from the remaining 18 variables. 

First, drop all of the rows in the data frame that correspond to tweets from other devices. Call this new data frame, `trumpDF2dev`

```{r, error = TRUE}
trumpDF2dev = trumpDF[trumpDF$dev != "Other",]
```

Now, use the `rpart` function in the `rpart` package to build a classification tree. 

```{r, error = TRUE}
library(rpart)

trumpTree = rpart(dev ~ ., 
                  data = trumpDF2dev, method = "class",  
                  control = rpart.control(cp = 0.01))
```


Plot the tree using the `prp` function in the `rpart.plot` package. 
```{r, error = TRUE}
library(rpart.plot)
prp(trumpTree, extra = 2)
```

Recreate the classification tree using different values for the complexity parameter to see if how the tree varies. 

```{r, error = TRUE}
trumpTree = rpart(dev ~ ., 
                  data = trumpDF2dev, method = "class",  
                  control = rpart.control(cp = 0.03))

prp(trumpTree, extra = 2)

```


Which tree did you settle on? Describe in words the basics of the tree that you chose. 

Ideally, at this point we would now check how well the classification performs on a separate set of data.  
