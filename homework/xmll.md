---
title: "2016 Presidential Election Debrief Project"
output: html_document
arthur: Shengyao Yang, Shuaya Zhan, Zhaoye Tang, Xinyi Luan, Yike Dong
---

## DATA WRANGLING

Your goal here is to create one comprehensive data frame that consists of data from six sources.

1. 2016 Presidential Election results reported at the county level.
These are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/2016_US_County_Level_Presidential_Results.csv

```{r 1-1}
eResult2016 = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/2016_US_County_Level_Presidential_Results.csv"), header = TRUE)
```

2. 2012 Presidential Election results reported at the county level. These data are now available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/xxx.xml
Where the xxx.xml will be replaced by state names.
These state names are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/stateNames.txt

```{r 1-2}
stateNames = read.table(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/stateNames.txt"), header = TRUE)
stateNames = stateNames[-c(2),] ## why ?
require(XML)

eResult2012 = list()
for (i in (1:length(stateNames))) {
  eResult2012List = paste("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/", stateNames[i], ".xml", sep = "")
  eResult2012tmp = try(xmlParse(eResult2012List))
  eResult2012tmp = try(xmlRoot(eResult2012tmp))
  eResult2012[[i]] = eResult2012tmp
}

infDoc = xmlParse(eResult2012List)
infRoot = xmlRoot(infDoc)
county=xpathSApply(infRoot, "//table/tbody/tr/th[@class='results-county']/text()",xmlValue)
# county = xpathSApply(infRoot, "//table/tbody/tr/th[@class='results-county']", xmlValue)
# county = substr(county, 1, nchar(county)-17)
head(county)

name <- xpathSApply(infRoot, '//table/tbody/tr/th[@class="results-candidate"]/text()',xmlValue)
vote <- xpathSApply(infRoot, '//table/tbody/tr/td[@class="results-percentage"]/text()',xmlValue)

# RomneyVote=xpathSApply(infRoot, '//table/tbody[./th[@class="results-candidate"]/text() = "M. Romney"]/td[@class="results-popular"]')
# RomneyVote
# ObamaVote= xpathSApply(infRoot, '//table/tbody[./th[@class="results-candidate"]/text() = "B. Obama(i)"]/td[@class="results-popular"]')

```

3. 2008 Presidential Election results (county level) are available from The Guardian.This sheet has been uploaded as an xlsx spreadsheet at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx
Note that the spreadsheet has tabs for each state. You will need to export these data as CSV files (or some other delimited file) 
in order to merge them.

```{r 1-3}
library(gdata)
eResult2008Total = read.xls("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx", sheet = 4)
eResult2008 = list()
for (i in (2:51)) {
  eResult2008[i] = read.xls("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx", sheet = i)
}

read.xlsx2('countyVotes2008.xlsx')


```

4. 2004 Presidential Election results (county level) are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2004.txt

```{r 1-4}
eResult2004 = read.table(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2004.txt"), header = TRUE)
```

5. Census data from the 2010 census. These data are available in three CSV files: B01003.csv DP02.csv DP03.csv These files each have an accompanying TXT file that describes the variables.
B01_metadata.txt DP02_metadata.txt DP03_metadata.txt
Not all variables described in the meta data files are available. The DP02 file contains socio-data, DP03 contains economic data, and B01 contains race information. Be careful with the B01 file as the data are organized differently than with DP02 and DP03.
All six of these files are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/xxx.csv

```{r 1-5}
census2010Race = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/B01003.csv"), header = TRUE)
census2010Socio = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP02.csv"), header = TRUE)
census2010Econ = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP03.csv"), header = TRUE)

census2010RaceMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/B01_metadata.txt"), header = TRUE)
census2010SocioMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP02_metadata.txt"), header = TRUE)
census2010EconMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP03_metadata.txt"), header = TRUE)
```

6. GML (Geographic Markup Language) data that contains the latitude and longitude for each county. These are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/counties.gml

```{r 1-6}
require(XML)
gmlData = "http://www.stat.berkeley.edu/users/nolan/data/voteProject/counties.gml"
gmlDataDoc = xmlParse(gmlData)
gmlDataRoot = xmlRoot(gmlDataDoc)
```

Your data frame should contain one row per county. It should have data from all files. This means it should have at a minimum the following variables from the election results and the county locations:
??? State
??? County
??? Trump votes and Clinton votes from 2016
??? Obama votes and Romney votes from 2012
??? Obama votes and McCain votes from 2008
??? Bush votes and Kerry votes from 2004
??? Latitude
??? Longitude
In addition, select several variables from each of the three census files. For example Total Population and White alone from B01, Percent unemployed and Employed in service industry from DP03, etc. You will want 30-40 variables from these three files.

```{r DATA WRANGLING}

```

## EXPLORATION

Your goal here is to carry out preliminary explorations that can help you in the further stages of analysis. Make several plots and describe your findings. 

```{r EXPLORATION}

```

## MAP MAKING

Your goal here is to make an informative map describing the election results. These may be put in the context of previous elections. To help you, consider the following maps. One shows the change in votes from 2004 to 2008, where the length of the arrow is proportional to the vote shift. Another shows the total vote in each county for the candidates. Feel free to gain inspiration from maps that you find online, but be sure to acknowledge your sources.

```{r MAP MAKING}

```

## MODELING

Your goal here is to create two predictors for the 2016 election results using the variables you create in the merge. One predictor is for the 2016 election results and one is for the change from 2012 to 2016. Assess the accuracy of your predictors. Compare the predictors. Did they do well in the same places? Explore where each did well and where it did poorly. Use different methods for the two predictors. For example, K-NN, Classification trees, Naïve Bayes. If you are familiar with other methods such as logistic regression or SVM, you may use them.

```{r MODELING}

```
