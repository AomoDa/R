---
title: "XML Extraction Lab"
author: AOMODA
output: html_document
---

## Introduction

In this lab, we will use the `XML` package to read exchange rates (against the euro) from the European Central Bank and create a time series plot showing how the rates for four different currencies--the British pound (GBP), the US dollar (USD), the Canadian dollar (CAD), and the Japanese yen (JPY)--have changed over time. 

Before jumping to the code portions, open a browser and visit this URL:

(http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml?167971e0f5d2192a5dc29404b0261986 "European Central Bank XML Format")

This URL provides an example of the strucutre of the  XML document with the exchange rates. Where do you see the currency and rate? What about the time?
How deep is the tree? 

## Preparation

Before you continue, please install the `XML` package.
Then load the library and read the data into R with:

```{r}
library(XML)

er = xmlParse("http://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.xml")
```

If for any reason, you are having trouble with the Web site responding, you can read a version of the data from

(http://www.stat.berkeley.edu/~nolan/data/ECB2016.xml) 


Now that we have the XML document read, locate the root node with a call to `xmlRoot`

```{r}
erRoot = xmlRoot(er)
```

Use `xmlSize` and `names` to find the number of child nodes that the root node has and their names

```{r}
xmlSize(erRoot)
names(erRoot)
```

The call to `xmlSize` should return 3 and the call to names should return subject, Sender, and Cube.

## Exploring the Data Structure

We first extract the exchange rates for the British pound (GBP) as an example. From the example on the ECB site, we see that all of the rate data are descendents of a "Cube" node.  If you are using the data extracted on 2016-10-31, then you will find  that it has 4567 children and they are all named Cube!

```{r}
xmlSize(erRoot[["Cube"]])

all(names(erRoot[["Cube"]]) == "Cube")
```

Let's contiue to use the list-like structure to examine the first child of this top-level Cube node. If you were to draw a diagram for this tree, with the following expression, we would be looking at the following node:

Envelope > Cube > Cube

```{r, error=TRUE}
erRoot[['Cube']][1]
```

As we can see, the Cube child of the root node has as its first child a Cube node with a "time" attribute. In turn, the Cube node with the "time" attribute is the parent of several other Cube nodes with "currency" and "rate" attributes. 

## Extracting the GBP Exchange rates

We will show you two ways to pull out the GBP exchange rate. 

### First Approach

The first way is a two-step process:

1. Obtain as a list all of the nodes that have the name "Cube" and an attribute "currency" with value "GBP". We use the `getNodeSet` function to do this.

2. Use xmlSApply() to get the value of the "rate" 
attribute on those nodes. We use the `xmlSApply` function to carry out this step.

To call `getNodeSet`, we provide the root node and the XPath expression that identifies the nodes we're interested in. Provide the predicate in the XPath expression below that filters the nodes to the subset that we want. (We are interested in Cube with a currency attribute value of "GBP").

```{r, error=TRUE}
gbp_nodes = getNodeSet(erRoot, '//x:Cube[@currency="GBP"]', 
                       namespaces = "x")
head(gbp_nodes)
```

Note we have briefly covered name spaces. The ECB combines two grammars so we specify "x" as a dummy name for the default name space.

Now for the second step. Use `xmlSApply` to apply the `xmlGetAttr` function to each of the nodes we identified above.  We need to pass `rate` as a parameter to `xmlGetAttr` to indicate that we want the value of this attribute.  

```{r, error=TRUE}
gbp1 = xmlSApply(gbp_nodes, xmlGetAttr, "rate")
```

Let's check it out. If you used the stored file from 2016-10-31, then you should have 4567 values for gbp1. What is the class? 

```{r, error=TRUE}
head(gbp1)
length(gbp1)
class(gbp1)
```


### Second Approach

The second approach does this all in one step with the
`xPathSApply` function. Note that this function is similarly spelled, but `xmlSApply` and `xpathSApply` are not the same. The latter provides an XPath expression to specify the nodes to which the function is applied.

```{r, error=TRUE}
gbp2 = xpathSApply(erRoot, '//x:Cube[@currency="GBP"]', 
                  xmlGetAttr, "rate", namespaces = "x")
xmlValue()
```

It is easy to check that these two methods 
produce the same result!

```{r, error=TRUE}
identical(gbp1, gbp2)
```

## Extracting Dates

Now that we've extracted the GBP exchange rate, let's get the dates associated with each of those values. Fill in the XPath expression in the code chunk below to locate all Cube nodes with a time attribute. Remember to use x: in front of the Cube tag name.

```{r, error=TRUE}
days = xpathSApply(erRoot, '//x:Cube[@time]', 
                   xmlGetAttr, "time", namespaces = "x")
```

Let's take a look at our work and reformat the return
object so that it's a Date object:

```{r, error=TRUE}
head(days)
dayz = as.Date(days)
```

For a quick sanity check: did we pull out as many days as there are exchange rates?

```{r, error=TRUE}
length(dayz) == length(gbp1)
```

This should be TRUE.


Now we've successfully extracted the exchange rates and dates for the British pound. We can create our first simple plot.

```{r, echo=FALSE, error=TRUE}
library(ggplot)
require(ggplot2)
Rates = data.frame(gbp = as.numeric(gbp1), dayz)

ggplot(data = Rates, aes(x = dayz, y = gbp)) +
  geom_line() +
  labs(title = "GBP Daily Exchange Rates (against Euro)",
       x = "Date",
       y = "Exchange Rate")
```


## Extract rates for three other currencies

We went through one currency example with the British pound. Next, we extract the exchange rates for other currencies, add them to the data frame we just created, and make a plot with all the currencies.

Rather than repeating code, we write a helper function to do the extraction. This function takes 2 inputs:

* the root node of the XML document
* the three letter currency abbreviation as a character string

It returns a numeric vector with the daily exchange rates for the associated currency.

```{r, error=TRUE}
getExchangeRates = function(abbrev, root) {
  # create the expression 
  currency_path =
    sprintf('//x:Cube[@currency = "%s"]', abbrev)
  
  # extract the rates
  rates = xpathSApply(root, currency_path,
                  xmlGetAttr, "rate", namespaces = "x")
  
  return(as.numeric(rates))
}
```

Apply your function to a vector of exchange rates abbreviations to obtain the exchange rates for 
the GBP, USD, CAD, and JPY.  Fill in the follwing
call to lapply:

```{r, error=TRUE}
dailyRates = sapply(c("GBP","USD","CAD","JPY"), getExchangeRates,
                    root = erRoot)
head(dailyRates)
```

Now we are ready to make our plot! The first task is to create a data frame to pass to `ggplot`. Check the class of `dailyRates`. It should be a matrix. Let's create a data frame called `dailyRatesDF` that contains 3 variables:

* `rate` - the rates of the 4 currencies stacked (recall that we can treat a matrix as a vector)
* `day` - repeat `dayz` so that each currency runs through all of the days.
* `currency` - use the 3-letter abbreviation for the currencies

```{r}
numDays = nrow(dailyRates)
dailyRatesDF = data.frame(
  rate = dailyRates,
  day = ,
  currency = c("GBP","USD","CAD","JPY")
)
```

Now that the data are in a format suitable for plotting with `ggplot`, we can create our line plot as follows

```{r, error=TRUE}

dailyRatesDF$ratenorm = 
  dailyRatesDF$rate / rep(dailyRates[numDays,], each = numDays)

ggplot(data = dailyRatesDF, 
       aes(x = day, y = ratenorm, color = currency)) +
  geom_line() +
  labs(title = "Daily Exchange Rates (against Euro,)",
       x = "Date",
       y = "Exchange Rate (normalized to rate on 1/4/1999)")
```

Since the rates for each country can be quite different, we probably want to normalize them to the rate on the first day for each currency. 

## OPTIONAL FOR FUN:

The currency for China (CNY) isn't available for all the dates in the file. Use the getExchangeRates function to 
extract the CNY rates. How many rates are in this vector?

Write another extraction to get the dates of those Cube nodes where the CNY rate is present. One way to do this is to locate the CNY Cube nodes and  then add one more step to the path to travel back up the tree one step to the parent Cube. To do this, remember that the step consists of an axis followed by :: followed by a node name and then an optional predicate. The axis "parent" is a valid axis.

```{r, error=TRUE}
cny = getExchangeRates(root = erRoot, abbrev = "CNY")

cnyDays = xpathSApply(erRoot, 'FILL IN', 
                      xmlGetAttr, "time", 
                      namespaces = "x")
```

