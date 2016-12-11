---
title: "project6"
output: html_document
arthur: amd
---

## DATA WRANGLING

Your goal here is to create one comprehensive data frame that consists of data from six sources.

1. 2016 Presidential Election results reported at the county level.
These are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/2016_US_County_Level_Presidential_Results.csv

```{r 1-1}
#load source data
eResult2016 = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/2016_US_County_Level_Presidential_Results.csv"), header = TRUE)

#load aux data (state names and abbr)
stateAbbr = read.csv("statesAbbr.csv", header = TRUE)
#merge state names and their abbr with source data
eResult2016 = merge(eResult2016, stateAbbr, by.x = "state_abbr", by.y = "Abbreviation", all = TRUE)
#remove all unwanted words
County=sub(" County","",eResult2016$county_name)
#clean data by choosing wanted columns
eResult2016 = data.frame(eResult2016$State, County, eResult2016$votes_gop, eResult2016$votes_dem)

#renaming columns for later merge
colnames(eResult2016) = c("State", "County", "Trump votes from 2016", "Clinton votes from 2016")

#Team member: Shengyao Yang. I first import all the data I need, then I merge the state names with corresponding abbreviation. Then I clean up the data frame according to what we need for later merge (state and county names).#
```

2. 2012 Presidential Election results reported at the county level. These data are now available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/xxx.xml
Where the xxx.xml will be replaced by state names.
These state names are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/stateNames.txt

```{r 1-2}
stateNames = read.table(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/stateNames.txt"), header = TRUE)
stateNames = stateNames[-c(2),]
stateNames1 = gsub("-", " ", stateNames)
stateNames1=capitalize(stateNames1)
stateNames2=gsub("^[[:alpha:]]+ *","", stateNames1)
stateNames1=gsub(" .*","",stateNames1)
stateNames2=capitalize(stateNames2)
#remove Alaska，split statesnames by " " and capitalized them for same format as my teammate.
require(XML)
install.packages(Hmisc)
library(Hmisc)
#package(Hmisc) for capitalizing
num_value <- function(x){ 
  require(stringr)
  if(!is.na(x)){
    a = xmlValue(x)
    rt =  as.numeric(str_replace(a,'[,]',''))
  } else{
    rt = NA
  }
  return(rt)
}
#create num_value function for further use(change votes to numeric)
eResult2012 = data.frame()
for (i in (1:length(stateNames1))){
  eResult2012List = paste("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/", stateNames[i], ".xml", sep = "")
  infDoc = xmlParse(eResult2012List)
  infRoot = xmlRoot(infDoc)
  county=xpathSApply(infRoot, "//table/tbody/tr/th[@class='results-county']/text()",xmlValue)
  #select county names by xpath
  RomneyVote=xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text()="M. Romney"]/td[@class="results-popular"]',num_value)
  #select Romney's popular vote by locating the nodes containing"M.Romney", and then extract the popular vote.
  ObamaVote=xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text() = "B. Obama (i)"]/td[@class="results-popular"]',num_value)
   #select Romney's popular vote by locating the nodes containing"B. Obama (i)", and then extract the popular vote.
  eResult2012tmp = data.frame(State = stateNames1[i], State2=stateNames2[i], County=county, RomneyVotes = RomneyVote, ObamaVotes = ObamaVote, stringsAsFactors = FALSE)
  State=paste(eResult2012tmp$State,eResult2012tmp$State2,sep=" ")
  eResult2012tmp=data.frame(State, eResult2012tmp$County,eResult2012tmp$RomneyVotes,eResult2012tmp$ObamaVotes,stringsAsFactors = FALSE)
  #paste back splited state names.
  eResult2012 = rbind(eResult2012, eResult2012tmp)
}
#merge State, County, RomneyVotes and ObamaVotes
colnames(eResult2012) = c("State", "County", "Romney votes from 2012", 'Obama votes from 2012')
#change colunm names
eResult2012$State = capitalize(as.character(eResult2012$State))
eResult2012[1119,2]="St. John the Baptist Parish"
eResult2012$State[283]="District of Columbia"

eResult2012$County=gsub(" *$","",eResult2012$County)
eResult2012$State=gsub(" *$","",eResult2012$State)
#capitalize state names
#Team member: Shuya Zhan, I first load statenames.txt(remove alaska) and load all xml file in R. Then I examine the alabama.xml file on web browser. I started by writing a for loop which runs all xml file by statenames. Inside the loop, I first extract county name by xpath and then extract romney and obama's votes by locating the nodes containing "M.Romney" and "B. Obama (i)", and then extract the popular vote. Then I merge statenames, county, romneyvote and obamavote and change colnames. Lastly I modified the data frame for further merge in part 6.
```

3. 2008 Presidential Election results (county level) are available from The Guardian.This sheet has been uploaded as an xlsx spreadsheet at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx
Note that the spreadsheet has tabs for each state. You will need to export these data as CSV files (or some other delimited file) in order to merge them.

```{r 1-3}
library(gdata)
library(plyr)
eResult2008 = list()
stateNames = sheetNames("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx")
for (i in (1:51)) {
  eResult2008[[i]] = read.xls("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2008.xlsx", sheet = i)
}
#Load data from the website.
eResult2008test = eResult2008
for (i in (2:51)) {
  eResult2008[[i]] = eResult2008[[i]][,-c(2,3,6,7)]
  eResult2008[[i]] = rename(eResult2008[[i]], c("County."="County","Obama."="Obama votes from 2008", "McCain."="McCain votes from 2008"))
  states = rep(stateNames[i],nrow(eResult2008[[i]]))
  eResult2008[[i]] = cbind(states, eResult2008[[i]])
}
#Remove the unnecessary columns in the data frame and create a column countaining state name.

eResult2008tmp = data.frame(eResult2008[[2]])
for (i in (3:51)) {
  eResult2008tmp = rbind(eResult2008tmp, data.frame(eResult2008[[i]]))
}
#Merge the data from all states.
eResult2008 = eResult2008tmp
colnames(eResult2008) = c("State", "County", "Obama votes from 2008", "McCain."="McCain votes from 2008")
eResult2008$County = as.character(eResult2008$County)
eResult2008$County = substr(eResult2008$County, 1, nchar(eResult2008$County)-1)
#Change column names.

#Team member: Claire Dong. For this part I firstly examined the data online and found that it is a excel file that contains multiple sheets. I used a package to read the files in r. Then I removed the unnecessary columns in the data frame and add the colunm of state name. Finally I combined all the states data frame together and changed the column names #
```

4. 2004 Presidential Election results (county level) are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2004.txt

```{r 1-4}
install.packages(Hmisc)
library(Hmisc)
eResult2004 = read.table(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2004.txt"), header = TRUE,stringsAsFactors = FALSE)
#Load election result from 2004 into R. Install Hmisc packagess#

Countyname = eResult2004$county
County=gsub("^[[:alpha:]]+ *[[:alpha:]]+,","",Countyname)
County=capitalize(County)
#Subset the county by selecting the Countyname column and separate it into State and County separately. Using the regular expression, we gave R a pattern to look for. Using the capitalize function in Hmisc package, we can capitalzie the first letter of Counties. 
County=sub(",washington","",County)
County=sub("St","St. ",County)

County1=gsub("^[[:alpha:]|//.]+ *", "", County)
County2=gsub("^[[:alpha:]]+ *", "", County1)
County=gsub(" .*","",County)
County1=gsub(" .*","",County1)
County3=gsub("^[[:alpha:]]+ *","",County2)
County2=gsub(" .*","",County2)

County1=capitalize(County1)
County2=capitalize(County2)
County3=capitalize(County3)

State=gsub(",.+","",Countyname)
State=capitalize(State)

State2=gsub("^[[:alpha:]]+ *","", State)
State=gsub(" .*","",State)
State2=capitalize(State2)
#Get states as a column, and capitalize the first letter of every states to be consistent and easier for later merging. 

eResult2004=data.frame(State,State2,County,County1,County2,County3,eResult2004$bushVote,eResult2004$kerryVote,stringsAsFactors = FALSE)

eResult2004=setNames(eResult2004,c("State","State2","County","County1","County2","County3","bushVote","kerryVote"))

eResult2004$County1[283]="of"
eResult2004$County1[1281]="of"
eResult2004$County2[1119]="the"
eResult2004$County2[1281]="the"

County=paste(eResult2004$County,eResult2004$County1,eResult2004$County2,eResult2004$County3,sep=" ")

State=paste(eResult2004$State,eResult2004$State2,sep=" ")
eResult2004=data.frame(State,County,eResult2004$bushVote,eResult2004$kerryVote,stringsAsFactors = FALSE)

eResult2004=setNames(eResult2004,c("State","County","Bush votes from 2004","Kerry votes from 2004"))

eResult2004[1119,2]="St. John the Baptist Parish"
eResult2004$State[283]="District of Columbia"

eResult2004$County=gsub(" *$","",eResult2004$County)
eResult2004$State=gsub(" *$","",eResult2004$State)



#Put states, counties, bush vote for 2004, kerry vote for 2004 together into a data frame, and change each column name to the column names we want.

#Team member: Xinyi, Luan. I took a look at the election reslut of 2004, and splitted the first column into states and counties separately. To make sure the format of our column names aligns, I changed the first letter of both columns to capital letters using the package Hmsic and capitalize function, and also changed the name of bush and kerry's vote into "Bush votes from 2004", and "Kerry Votes from 2004 to be consistent".#
```

5. Census data from the 2010 census. These data are available in three CSV files: B01003.csv DP02.csv DP03.csv These files each have an accompanying TXT file that describes the variables.
B01_metadata.txt DP02_metadata.txt DP03_metadata.txt
Not all variables described in the meta data files are available. The DP02 file contains socio-data, DP03 contains economic data, and B01 contains race information. Be careful with the B01 file as the data are organized differently than with DP02 and DP03.
All six of these files are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/xxx.csv

```{r 1-5}
#load source data
census2010Race = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/B01003.csv"), header = TRUE)
census2010Socio = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP02.csv"), header = TRUE)
census2010Econ = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP03.csv"), header = TRUE)

census2010RaceMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/B01_metadata.txt"), header = FALSE)
census2010SocioMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP02_metadata.txt"), header = FALSE)
census2010EconMeta = read.csv(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/census2010/DP03_metadata.txt"), header = FALSE)
#replace column names with interpretation indicated in metadata file.
colnames(census2010Econ) = census2010EconMeta$V2
colnames(census2010Socio) = census2010SocioMeta$V2
colnames(census2010Race) = census2010RaceMeta$V2
#create data frame of variables chosen from dp03 file
V = c(colnames(census2010Econ))
SelectedV = c(V[c(6:25)])
census2010EconVariables = c(as.character(SelectedV))
census2010Econvar = census2010Econ[,grep(paste(SelectedV, collapse = "|"), colnames(census2010Econ))]
census2010Econvar_data = data.frame(census2010Econ$Geography, census2010Econvar)
census2010Econvar_data$County <- lapply(strsplit(as.character(census2010Econ$Geography), ","), "[", 1)
census2010Econvar_data$State <- lapply(strsplit(as.character(census2010Econ$Geography), ","), "[", 2)
census2010Econvar_data = census2010Econvar_data[,-1]
#create the data frame for variable chose from dp02 file
V1 = c(colnames(census2010Socio))
SelectedV1 = c(V1[c(6:10)])
census2010SocioVariables = c(as.character(SelectedV))
census2010Sociovar = census2010Socio[,grep(paste(SelectedV1, collapse = "|"), colnames(census2010Socio))]
census2010Sociovar_data = data.frame(census2010Socio$Geography, census2010Sociovar)
census2010Sociovar_data$County <- lapply(strsplit(as.character(census2010Socio$Geography),  ","), "[", 1)
census2010Sociovar_data$State <- lapply(strsplit(as.character(census2010Socio$Geography), ","), "[", 2)
census2010Sociovar_data = census2010Sociovar_data[,-1]
census2010Sociovar_data = census2010Sociovar_data[,-4]
#create data frame with stats, county, Estimate total, Popgroup and Margin of Error
census2010Race$County <- lapply(strsplit(as.character(census2010Race$Geography),  ","), "[", 1)
census2010Race$State <- lapply(strsplit(as.character(census2010Race$Geography), ","), "[", 2)
census2010Race = census2010Race[,-c(1,2,3,4)]
#merge three data frames into one that indicates the general information provided in the three files
census2010EconScio = merge(census2010Sociovar_data, census2010Econvar_data, by = c("County", "State"))
census2010 = merge(census2010Race, census2010EconScio, by = c("County", "State"))
#Team member: Zhaoye Tang. I first load all the data into R, and examined and compared the six files.To make collumn names available for human understanding, I replaced the collumn names with their interpretations indicated in their according meta file. Then I trim and split data with geomgraphical information for further merging. In order to avoid meaningless data, I chose variables with meaningful information contended and merged them into a big data frame#
```

6. GML (Geographic Markup Language) data that contains the latitude and longitude for each county. These are available at
http://www.stat.berkeley.edu/users/nolan/data/voteProject/counties.gml

```{r 1-6}
require(XML)
#load source data
gmlData = "http://www.stat.berkeley.edu/users/nolan/data/voteProject/counties.gml"
gmlDataDoc = xmlParse(gmlData)
gmlDataRoot = xmlRoot(gmlDataDoc)
#parse xml source data  and get county names
countyNames = xpathSApply(gmlDataDoc, "//county/gml:name", xmlValue)
#trim county names accordingly
countyNames = data.frame(substr(countyNames, 6, nchar(countyNames)-12))
#get county x coordinates
countyX = xpathSApply(gmlDataDoc, "//gml:coord/gml:X", xmlValue)
countyX = data.frame(countyX)
#get county y coordinates
countyY = xpathSApply(gmlDataDoc, "//gml:coord/gml:Y", xmlValue)
countyY = data.frame(countyY)
#get state name for corresponding counties accordingly
stateNumOfCounty = c()
for (i in (1:length(stateAbbr$State))) {
  stateNumOfCounty[i] = length(xpathSApply(gmlDataDoc,paste( "//county[..//gml:name[@abbreviation='", as.character(stateAbbr$Abbreviation[i]),"']]", sep = ""), xmlValue))
}
stateNumOfCounty = data.frame(stateAbbr$State, stateNumOfCounty)
statesRep = rep(stateNumOfCounty$stateAbbr.State,stateNumOfCounty$stateNumOfCounty)
#create data frame with given data and rename for later merging
gml = data.frame(statesRep, countyNames, countyX, countyY)
colnames(gml) = c("State", "County", "countyX", "countyY")

#Team member: Shengyao Yang. I first load the source xml data and then extract location information accordingly. Then I merge them and rename the data frame. It will be merge with other data frames by state and county.#

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
eResult2016$County = as.character(eResult2016$County)
eResult2016$State = as.character(eResult2016$State)
eResult2012$County = as.character(eResult2012$County)
eResult2012$State = as.character(eResult2012$State)
eResult2008$County = as.character(eResult2008$County)
eResult2008$State = as.character(eResult2008$State)
eResult2008$`Obama votes from 2008` = as.numeric(eResult2008$`Obama votes from 2008`)
eResult2008$`McCain votes from 2008` = as.numeric(eResult2008$`Obama votes from 2008`)
eResult2004$County = as.character(eResult2004$County)
eResult2004$State = as.character(eResult2004$State)
eResult = NULL
eResult = merge(eResult2016, eResult2012, by = c("State", "County"), all = TRUE)
eResult = merge(eResult, eResult2008, by = c("State", "County"), all = TRUE)
eResult = merge(eResult, eResult2004, by = c("State", "County"), all = TRUE)
eResult = merge(eResult, gml, by = c("State", "County"), all = TRUE)
census2010$County=sub(" County","",census2010$County)
census2010$State = substr(census2010$State, 2, nchar(census2010$State))
eResultWithInfo = merge(eResult, census2010, all = TRUE)
```

## EXPLORATION

Your goal here is to carry out preliminary explorations that can help you in the further stages of analysis. Make several plots and describe your findings. 

```{r}
library(ggplot2)
```

```{r EXPLORATION}
# Bar plot for Election 2004-2016 at State Level.
# P1.2004 Bush vs. Kerry
eResult2004$`Bush votes from 2004`<-factor(eResult2004$`Bush votes from 2004`)
eResult2004$`Bush votes from 2004`<- sapply(eResult2004$`Bush votes from 2004`, as.numeric)
eResult2004$State<-as.data.frame(eResult2004$State)
Bus<-c(rep(1,2975))
Bus<-cbind(Bus,eResult2004$State,eResult2004$`Bush votes from 2004`)
Ker<-c(rep(0,2975))
Ker<-cbind(Ker,eResult2004$State,eResult2004$`Kerry votes from 2004`)
colnames(Bus)=c("1","2","3")
colnames(Ker)=c("1","2","3")
Re2004<-rbind(Bus,Ker)
Re2004<-as.data.frame(Re2004)
colnames(Re2004)=c("Category","State","Votes")
eResult2004$`Kerry Votes from 2004`<- factor(eResult2004$`Kerry Votes from 2004`)
eResult2004$`Kerry Votes from 2004`<- sapply(eResult2004$`Kerry Votes from 2004`, as.numeric)
p2004 <- ggplot(Re2004,aes(x=factor(Re2004$State),y=factor(Re2004$Votes), fill=factor(Re2004$Category)))+
  stat_summary(fun.y=mean,position="stack",geom="bar")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_fill_discrete(name="Candidates",labels=c("Bush", "Kerry"))+
  xlab("States")+ylab("Votes")
p2004
```

```{r}
# P2.2008 Obama vs. McCain
eResult2008$`Obama votes from 2008`<-factor(eResult2008$`Obama votes from 2008`)
eResult2008$`Obama votes from 2008`<- sapply(eResult2008$`Obama votes from 2008`, as.numeric)
eResult2008$State<-as.data.frame(eResult2008$State)
Oba2<-c(rep(1,3114))
Oba2<-cbind(Oba2,eResult2008$State,eResult2008$`Obama votes from 2008`)
Mc<-c(rep(0,3114))
Mc<-cbind(Mc,eResult2008$State,eResult2008$`McCain votes from 2008`)
colnames(Oba2)=c("1","2","3")
colnames(Mc)=c("1","2","3")
Re2008<-rbind(Oba2,Mc)
Re2008<-as.data.frame(Re2008)
colnames(Re2008)=c("Category","State","Votes")
eResult2008$`McCain votes from 2008`<- factor(eResult2008$`McCain votes from 2008`)
eResult2008$`McCain votes from 2008`<- sapply(eResult2008$`McCain votes from 2008`, as.numeric)
p2008 <- ggplot(Re2008,aes(x=factor(Re2008$State),y=factor(Re2008$Votes), fill=factor(Re2008$Category)))+
  stat_summary(fun.y=mean,position="stack",geom="bar")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_fill_discrete(name="Candidates",labels=c("Obama", "McCain"))+
  xlab("States")+ylab("Votes")
p2008
```

```{r}
# P3. 2012 Obama vs. Romney
eResult2012$`Romney votes from 2012`<-factor(eResult2012$`Romney votes from 2012`)
eResult2012$`Romney votes from 2012`<- sapply(eResult2012$`Romney votes from 2012`, as.numeric)
Rom<-c(rep(1,3113))
Rom<-cbind(Rom,eResult2012$State,eResult2012$`Romney votes from 2012`)
Oba<-c(rep(0,3113))
Oba<-cbind(Oba,eResult2012$State,eResult2012$`Obama votes from 2012`)
Re2008<-rbind(Rom,Oba)
Re2008<-as.data.frame(Re2008)
colnames(Re2008)=c("Category","State","Votes")
eResult2012$`Obama votes from 2012`<- factor(eResult2012$`Obama votes from 2012`)
eResult2012$`Obama votes from 2012`<- sapply(eResult2012$`Obama votes from 2012`, as.numeric)
p2012 <- ggplot(Re2008,aes(x=factor(Re2008$State),y=factor(Re2008$Votes), fill=factor(Re2008$Category)))+
  stat_summary(position="stack",geom="bar")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_fill_discrete(name="Candidates",labels=c("Obama", "Romney"))+
  xlab("States")+ylab("Votes")
p2012
```

```{r}
# P4.2016 Clinton vs. Trump
eResult2016$`Trump votes from 2016`<-factor(eResult2016$`Trump votes from 2016`)
eResult2016$`Trump votes from 2016`<- sapply(eResult2016$`Trump votes from 2016`, as.numeric)
eResult2016$State<-as.data.frame(eResult2016$State)
Tru<-c(rep(1,3141))
Tru<-cbind(Tru,eResult2016$State,eResult2016$`Trump votes from 2016`)
Cli<-c(rep(0,3141))
Cli<-cbind(Cli,eResult2016$State,eResult2016$`Clinton votes from 2016`)
colnames(Tru)=c("1","2","3")
colnames(Cli)=c("1","2","3")
Re2016<-rbind(Tru,Cli)
Re2016<-as.data.frame(Re2016)
colnames(Re2016)=c("Category","State","Votes")
eResult2016$`Clinton votes from 2016`<- factor(eResult2016$`Clinton votes from 2016`)
eResult2016$`Clinton votes from 2016`<- sapply(eResult2016$`Clinton votes from 2016`, as.numeric)
p2016 <- ggplot(Re2016,aes(x=factor(Re2016$State),y=factor(Re2016$Votes), fill=factor(Re2016$Category)))+
  stat_summary(fun.y=mean,position="stack",geom="bar")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_fill_discrete(name="Candidates",labels=c("Trump", "Clinton"))+
  xlab("States")+ylab("Votes")
p2016
```

```{r}
# Overview of 2004-2016 at State Level.
# P5. 2004-2016 Election: Democratic Party
#2004 Kerry
eResult2004$`Kerry votes from 2004`<-factor(eResult2004$`Kerry votes from 2004`)
eResult2004$`Kerry votes from 2004`<-sapply(eResult2004$`Kerry votes from 2004`, as.numeric)
Ker2004<-aggregate(eResult2004$`Kerry votes from 2004`, by=list(Category=eResult2004$State), FUN=sum)
Ker<-c(rep(0,length(Ker2004)))
Ker<-cbind(Ker,Ker2004)
colnames(Ker)=c("1","2","3")
#2008 Obama
eResult2008$`Obama votes from 2008`<-factor(eResult2008$`Obama votes from 2008`)
eResult2008$`Obama votes from 2008`<- sapply(eResult2008$`Obama votes from 2008`, as.numeric)
Oba2008<- aggregate(eResult2008$`Obama votes from 2008`, by=list(Category=eResult2008$State), FUN=sum)
Oba<-c(rep(1,length(Oba2008)))
Oba<-cbind(Oba,Oba2008)
colnames(Oba)=c("1","2","3")
#2012 Obama
eResult2012$`Obama votes from 2012`<-factor(eResult2012$`Obama votes from 2012`)
eResult2012$`Obama votes from 2012`<- sapply(eResult2012$`Obama votes from 2012`, as.numeric)
Oba2012<- aggregate(eResult2012$`Obama votes from 2012`,by=list(Category=eResult2012$State), FUN=sum)
Oba3<-c(rep(2,51))
Oba3<-cbind(Oba3,Oba2012)
colnames(Oba3)=c("1","2","3")
#2016 Clinton
eResult2016$`Clinton votes from 2016`<-factor(eResult2016$`Clinton votes from 2016`)
eResult2016$`Clinton votes from 2016`<- sapply(eResult2016$`Clinton votes from 2016`, as.numeric)
Cli2016<- aggregate(eResult2016$`Clinton votes from 2016`, by=list(Category=eResult2016$State), FUN=sum)
Cli<-c(rep(3,51))
Cli<-cbind(Cli,Cli2016)
colnames(Cli)=c("1","2","3")
DemTotal<-rbind(Ker,Oba,Oba3,Cli)
DemTotal<-as.data.frame(DemTotal)
colnames(DemTotal)=c("Category","State","Votes")
#Plot
pDemTotal<-ggplot(DemTotal, aes(x=factor(DemTotal$State), y=factor(DemTotal$Votes)))+
geom_line(aes(col=DemTotal$Category, group=DemTotal$Category)) +        geom_point(aes(col=DemTotal$Category),size=1)+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_colour_continuous(name="Candidates",labels=c("2004Kerry","2008Obama","2012Obama","2016Clinton"))+
  xlab("States")+ylab("Votes")
pDemTotal  
```

```{r}
# P6. 2004-2016 Election : Republican Party
#2004 Bush
eResult2004$`Bush votes from 2004`<-factor(eResult2004$`Bush votes from 2004`)
eResult2004$`Bush votes from 2004`<- sapply(eResult2004$`Bush votes from 2004`, as.numeric)
Bus2004<- aggregate(eResult2004$`Bush votes from 2004`, by=list(Category=eResult2004$State), FUN=sum)
Bus<-c(rep(0,length(Bus2004)))
Bus<-cbind(Bus,Bus2004)
colnames(Bus)=c("1","2","3")
#2008 McCain
eResult2008$`McCain votes from 2008`<- factor(eResult2008$`McCain votes from 2008`)
eResult2008$`McCain votes from 2008`<- sapply(eResult2008$`McCain votes from 2008`, as.numeric)
Mc2008<-aggregate(eResult2008$`McCain votes from 2008`, by=list(Category=eResult2008$State), FUN=sum)
Mc<-c(rep(1,length(Mc2008)))
Mc<-cbind(Mc,Mc2008)
colnames(Mc)=c("1","2","3")
#2012 Romney
eResult2012$`Romney votes from 2012`<-factor(eResult2012$`Romney votes from 2012`)
eResult2012$`Romney votes from 2012`<- sapply(eResult2012$`Romney votes from 2012`, as.numeric)
Rom2012<- aggregate(eResult2012$`Romney votes from 2012`, by=list(Category=eResult2012$State), FUN=sum)
Rom<-c(rep(2,51))
Rom<-cbind(Rom,Rom2012)
colnames(Rom)=c("1","2","3")
#2016 Trump
eResult2016$`Trump votes from 2016`<-factor(eResult2016$`Trump votes from 2016`)
eResult2016$`Trump votes from 2016`<- sapply(eResult2016$`Trump votes from 2016`, as.numeric)
Tru2016<- aggregate(eResult2016$`Trump votes from 2016`, by=list(Category=eResult2016$State), FUN=sum)
Tru<-c(rep(3,51))
Tru<-cbind(Tru,Tru2016)
colnames(Tru)=c("1","2","3")
RepTotal<-rbind(Bus,Mc,Rom,Tru)
RepTotal<-as.data.frame(RepTotal)
colnames(RepTotal)=c("Category","State","Votes")
#Plot
pRepTotal<-ggplot(RepTotal, aes(x=factor(RepTotal$State), y=factor(RepTotal$Votes)))+
geom_line(aes(col=RepTotal$Category, group=RepTotal$Category)) +        geom_point(aes(col=RepTotal$Category),size=1)+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(
  axis.text.x = element_text(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())+
  scale_colour_continuous(name="Candidates",labels=c("2004Bush","2008McCain","2012 Romney","2016Trump"))+
  xlab("States")+ylab("Votes")
pRepTotal  
```

```{r}
#Change of Votes for each Party at State Level
#Democratic Party Votes Change
#P.7 2004-2008 (Kerry->Obama1)
colnames(Ker)<-c("Category","State","Votes")
Alaska<-data.frame(Category=0,State="Alaska",Votes="68")
Ker<-rbind(Ker[1,],Alaska,Ker[2:48,])
Hawaii<-data.frame(Category=0,State="Hawaii",Votes="22413")
Ker<-rbind(Ker[1:11,],Hawaii,Ker[12:49,])
Dem_04_08<-as.numeric(Oba$`3`)-as.numeric(Ker$Votes)
Dem_04_08<-as.data.frame(Dem_04_08)
Dem_04_08<-cbind(Ker$State,Dem_04_08)
colnames(Dem_04_08)<-c("State","Votes")
pd_04_08 <- ggplot(Dem_04_08,aes(x=Dem_04_08$State,y=Dem_04_08$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Democratic Party Votes Change", subtitle = "2004-2008 (Kerry->Obama)")
pd_04_08
```

```{r}
#P.8 2008-2012 (Obama1->Obama2)
colnames(Oba)<-c("Category","State","Votes")
colnames(Oba3)<-c("Category","State","Votes")
Oba3<-Oba3[-c(4,9,14),]
California<-data.frame(Category=2,State="California",Votes="10817")
Illinois<-data.frame(Category=2,State="Illinois",Votes="63485")
Oba3<-rbind(Oba3[1:4,],California,Oba3[5:48,])
Oba3<-rbind(Oba3[1:13,],Illinois,Oba3[14:49,])
Dem_08_12<-as.numeric(Oba3$Votes)-as.numeric(Oba$Votes)
Dem_08_12<-as.data.frame(Dem_08_12)
Dem_08_12<-cbind(Oba$State,Dem_08_12)
colnames(Dem_08_12)<-c("State","Votes")
pd_08_12 <- ggplot(Dem_08_12,aes(x=Dem_08_12$State,y=Dem_08_12$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Democratic Party Votes Change", subtitle = "2008-2012 (Obama 1st->Obama 2nd)")
pd_08_12
```

```{r}
#P.9 2012-2016 (Obama2->Clinton)
colnames(Cli)<-c("Category","State","Votes")
Alaska<-data.frame(Category=2,State="Alaska",Votes="73312")
Oba3<-rbind(Oba3[1,],Alaska,Ker[2:50,])

Dem_12_16<-as.numeric(Cli$Votes)-as.numeric(Oba3$Votes)
Dem_12_16<-as.data.frame(Dem_12_16)
Dem_12_16<-cbind(Oba3$State,Dem_12_16)
colnames(Dem_12_16)<-c("State","Votes")
pd_12_16 <- ggplot(Dem_12_16,aes(x=Dem_12_16$State,y=Dem_12_16$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Democratic Party Votes Change", subtitle = "2012-2016 (Obama 2nd->Clinton)")
pd_12_16
```

```{r}
#Republican Party Votes Change
#P.10 2004-2008 (Bush->McCain)
colnames(Bus)<-c("Category","State","Votes")
Alaska<-data.frame(Category=0,State="Alaska",Votes="68")
Bus<-rbind(Bus[1,],Alaska,Bus[2:48,])
Hawaii<-data.frame(Category=0,State="Hawaii",Votes="23782")
Bus<-rbind(Bus[1:11,],Hawaii,Bus[12:49,])
Rep_04_08<-as.numeric(Mc$`3`)-as.numeric(Bus$Votes)
Rep_04_08<-as.data.frame(Rep_04_08)
Rep_04_08<-cbind(Ker$State,Rep_04_08)
colnames(Rep_04_08)<-c("State","Votes")
pr_04_08 <- ggplot(Rep_04_08,aes(x=Rep_04_08$State,y=Rep_04_08$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Republican Party Votes Change", subtitle = "2004-2008 (Bush->McCain)")
pr_04_08
```

```{r}
#P.11 2008-2012 (McCain->Romney)
colnames(Mc)<-c("Category","State","Votes")
colnames(Rom)<-c("Category","State","Votes")
Rom<-Rom[-c(8),]
Rep_08_12<-as.numeric(Rom$Votes)-as.numeric(Mc$Votes)
Rep_08_12<-as.data.frame(Rep_08_12)
Rep_08_12<-cbind(Oba$State,Rep_08_12)
colnames(Rep_08_12)<-c("State","Votes")
pr_08_12 <- ggplot(Rep_08_12,aes(x=Rep_08_12$State,y=Rep_08_12$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Democratic Party Votes Change", subtitle = "2008-2012 (McCain->Romney)")
pr_08_12
```

```{r}
#P.12 2012-2016 (Romney->Trump)
colnames(Tru)<-c("Category","State","Votes")
Alaska<-data.frame(Category=2,State="Alaska",Votes="82128")
Rom<-rbind(Rom[1,],Alaska,Ker[2:50,])
Rep_12_16<-as.numeric(Rom$Votes)-as.numeric(Tru$Votes)
Rep_12_16<-as.data.frame(Rep_12_16)
Rep_12_16<-cbind(Oba3$State,Rep_12_16)
colnames(Rep_12_16)<-c("State","Votes")
pr_12_16 <- ggplot(Rep_12_16,aes(x=Rep_12_16$State,y=Rep_12_16$Votes))+
  geom_bar(stat = "identity",position="identity")+
  scale_x_discrete(labels=stateAbbr$Abbreviation)+
  theme(axis.text.x = element_text())+
  xlab("States")+ylab("Votes")+
  labs(title = "Democratic Party Votes Change", subtitle = "2012-2016 (Romney 2nd->Trump)")
pr_12_16

```

## MAP MAKING

Your goal here is to make an informative map describing the election results. These may be put in the context of previous elections. To help you, consider the following maps. One shows the change in votes from 2004 to 2008, where the length of the arrow is proportional to the vote shift. Another shows the total vote in each county for the candidates. Feel free to gain inspiration from maps that you find online, but be sure to acknowledge your sources.

```{r MAP MAKING}

```

## MODELING

Your goal here is to create two predictors for the 2016 election results using the variables you create in the merge. One predictor is for the 2016 election results and one is for the change from 2012 to 2016. Assess the accuracy of your predictors. Compare the predictors. Did they do well in the same places? Explore where each did well and where it did poorly. Use different methods for the two predictors. For example, K-NN, Classification trees, Naïve Bayes. If you are familiar with other methods such as logistic regression or SVM, you may use them.

```{r MODELING-1}
eResultWithInfo$`Margin of Error; Total`=NULL
eResultWithInfo$Percent.Margin.of.Error..EMPLOYMENT.STATUS...Population.16.years.and.over = NULL
#prepare test data
winner=factor((eResultWithInfo$`Trump votes from 2016`- eResultWithInfo$`Clinton votes from 2016`>0))
levels(winner) = c("Dem", "Rep")
eResultTest = data.frame(winner, eResultWithInfo[,-c(3,4,7,8,9,10)])
colnames(eResultTest)[4] = "rep vote"
colnames(eResultTest)[5] = "dem vote"
eResultTest = eResultTest[is.na(eResultTest$winner) == FALSE,]
eResultTest = eResultTest[complete.cases(eResultTest),]
#prepare train data
winner=factor((eResultWithInfo$`Romney votes from 2012`- eResultWithInfo$`Obama votes from 2012`>0))
levels(winner) = c("Dem", "Rep")
eResultTrain = data.frame(winner, eResultWithInfo[,-c(3,4,5,6,9,10)])
colnames(eResultTrain)[5] = "rep vote"
colnames(eResultTrain)[4] = "dem vote"
eResultTrain = eResultTrain[is.na(eResultTrain$winner) == FALSE,]
eResultTrain = eResultTrain[complete.cases(eResultTrain),]
#set seed so the result is constant
set.seed(24687531)
#create permute folds
nTrain = nrow(eResultTrain)
nTest = nrow(eResultTest)
permuteIndices = sample(nTrain)
v = 3
folds = matrix(permuteIndices, ncol = v)
library(rpart)
#choose the best cp
cps = c(seq(0.0001, 0.001, by = 0.0001), 
       seq(0.001, 0.01, by = 0.001),
       seq(0.01, 0.1, by = 0.01))
preds = matrix(nrow = nTrain, ncol = length(cps))
for (i in 1:v) {
  trainFold = as.integer(folds[, -i])
  testFold = folds[, i]
  
  for (j in 1:length(cps)) {
    tree = rpart(`winner` ~ .,
            data = eResultTrain[trainFold, -c(2,3)], 
            method = "class",
            control = rpart.control(cp = cps[j]))
    preds[ testFold,j ] = 
      predict(tree, 
              newdata = eResultTest[testFold,-c(1,2,3)],
              type = "class")
  }
}

cvRates = apply(preds, 2, function(oneSet) {
  sum(oneSet==as.numeric(eResultTest$winner))/nTest
})

require(ggplot2)
which.max(cvRates)

cvRes = data.frame(cps, cvRates)
ggplot(data = cvRes, aes(x = cps, y = cvRates)) +
  geom_line() + 
  labs(x = "Complexity Parameter", y = "Classification Rate") +
  scale_y_continuous(limits = c(0.91, 0.92))
#final prediction using preferred cp
finalTree = rpart(winner ~ .,
                  data = eResultTrain[,-c(2,3)], 
                  method = "class",
                  control = rpart.control(cp = 0.1))
finalPreds = predict(finalTree, 
              newdata = eResultTest[,-c(1,2,3)],
              type = "class")
classRate = sum(finalPreds == eResultTest$winner) / nTest
classRate
#the prediction is very accurate(about 92% accuracy)
eResultChange = eResultTest[which(finalPreds!=eResultTest$winner),]
ggplot() +
  geom_bar(data = eResultChange[eResultChange$POPGROUP!="Total population",], aes(POPGROUP, fill = winner)) +
  scale_x_discrete("population group")
#from the bar plot its obvious that counties with white majority have changed their vote, and most of people changed their votes from democratic to republican
ggplot() +
  geom_bar(data = eResultChange, aes(floor(Percent..EMPLOYMENT.STATUS...In.labor.force...Civilian.labor.force...Employed.1), fill = winner), alpha=0.8) +
    scale_x_continuous(limits = c(30, 70), "employment rate %")
mean(eResultTest$Percent..EMPLOYMENT.STATUS...In.labor.force...Civilian.labor.force...Employed.1)
#from this plot we can see that those counties that changed their votes to republican have a employment rate concentrate on 40-65%(higher than average), and they tend to have a higher employment rate than those counties that changed to democratic
#so in conclusion, the predicition works very well; population group and employment rate tend to give the prediction some uncertainty; and those counties that the mis predicted have higher than average employment rate and are mainly white
```

```{r MODELING-2}
#the other predictor is for 2016 result from previous years result: 2004, 2008, and 2012
install.packages("class")
library(class)
#download class package to use knn function 

eResultWithInfo=na.omit(eResultWithInfo)
#get rid of NA values 
Party=factor((eResultWithInfo$`Trump votes from 2016`- eResultWithInfo$`Clinton votes from 2016`>0))
levels(Party)=c("Dem","Rep")
#identify democratic counties and republican counties

e2016=data.frame(eResultWithInfo$`Clinton votes from 2016`,eResultWithInfo$`Trump votes from 2016`)

#Now we want to use all three years' election result to predict, so we add the columns of Democratic/Republican candidates up
Demo3years=eResultWithInfo$`Kerry votes from 2004`+eResultWithInfo$`Obama votes from 2008`+eResultWithInfo$`Obama votes from 2012`
Repu3years=eResultWithInfo$`Bush votes from 2004`+eResultWithInfo$`McCain votes from 2008`+eResultWithInfo$`Romney votes from 2012`
eResult3years=data.frame(Demo3years,Repu3years)

prediction=c()
for(i in 1:30){
prediction[i]=length(Party[Party!=knn(train=eResult3years, test=e2016, cl=Party, k=i,l=0,prob = TRUE)])
error3=c(prediction[1:30])/length(Party)}

error3years=data.frame(k=1:30,y=error3)
ggplot(error3years)+geom_line(mapping=aes(x=k,y=error3))
which.min(error3)
error3[5]
#this gives us the k value that has the smallest error using the three years election data to predict, also the graph of the error rate for different k-value, so we decide to use k=5 using the previous three years election result as a training set. Also we get an error rate of around 5 percent. In general, this predictor is pretty accurate in predicting the 2016 election Result

eResultWithInfo$Party=Party
eResultWithInfo$prediction=knn(train=eResult3years, test=e2016, cl=Party, k=5,l=0,prob = TRUE)
#we add the prediction of democratic and republican status back into the original dataframe 

Rep_as_Dem=eResultWithInfo[eResultWithInfo$prediction=="Dem"&eResultWithInfo$Party=="Rep",]
Dem_as_Rep=eResultWithInfo[eResultWithInfo$prediction=="Rep"&eResultWithInfo$Party=="Dem",]
nrow(Rep_as_Dem)
nrow(Dem_as_Rep)
#we want to take a more specific look of the error rate: the two different kinds of error--Democratic wrongly predicted as Republican and Republican County wrongly predicted as Democratic. According to the variables found at each dataframe, we found out that there's a much larger error in a Democratic being wrongly predicted as Republican: around 170 observations found in Dem_as_Rep, while only around 30 for Rep_as_Dem.

#Now we want to use the k we have found and test how different variables in the Census data and geograpy information do in predicting the result

mean(Dem_as_Rep$Estimate..EMPLOYMENT.STATUS...In.labor.force)
mean(Rep_as_Dem$Estimate..EMPLOYMENT.STATUS...In.labor.force)
mean(eResultWithInfo$Estimate..EMPLOYMENT.STATUS...In.labor.force)
#Comparing the result, we found that there is not much difference in mean employment status between every county and the Dem wrongly predicted as the Republican one, but there is really significant differences in employment status between Rep_as_Dem and Dem_as_Rep, roughly 24,000.So we decided to check if this will give good prediction of result and we used K=5 as we have identified above.

employment=data.frame(eResultWithInfo$Estimate..EMPLOYMENT.STATUS...In.labor.force,eResultWithInfo$Estimate..EMPLOYMENT.STATUS...Population.16.years.and.over)
length(Party[Party!=knn(train=employment, test=e2016, cl=Party, k=5,l=0,prob = TRUE)])/length(Party)
#21% error

employmentpercent=data.frame(eResultWithInfo$Percent..EMPLOYMENT.STATUS...In.labor.force,eResultWithInfo$Percent..EMPLOYMENT.STATUS...Population.16.years.and.over)
length(Party[Party!=knn(train=employmentpercent, test=e2016, cl=Party, k=5,l=0,prob = TRUE)])/length(Party)
#17% error

employment16=data.frame(eResultWithInfo$Estimate..EMPLOYMENT.STATUS...Population.16.years.and.over,eResultWithInfo$Percent..EMPLOYMENT.STATUS...Population.16.years.and.over)
length(Party[Party!=knn(train=employment16, test=e2016, cl=Party, k=5,l=0,prob = TRUE)])/length(Party)
#15.7% error

Geography=data.frame(as.numeric(eResultWithInfo$POPGROUP),eResultWithInfo$Percent..HOUSEHOLDS.BY.TYPE...Total.households)
length(Party[Party!=knn(train=Geography, test=e2016, cl=Party, k=5,l=0,prob = TRUE)])/length(Party)
#15.3% error

#We picked several pairs of variables, and we compared the percentage of error, and decided that the Geography comparison (population subgroup and percent of housetype) did the best job in predicting the result. The employment information for age 16 and over did roughly as good. However, compared to the general 3 years election result from 2004, 2008, and 2012, these predictors have larger error rate. 

#Based on these data, we believe that geography information predict the election result pretty well probably because different population subgroups have different political belief. However, the past results most acurately predict the result, maybe because a state or person's political belief won't change a lot. Also we found out that Democratic being predicted as Republican has much larger error rate than Republican counties being predicted as Democratic. Potential reasons maybe either more counties have changed to be Democratic, or there are just more counties in the state that flipped their preferrence.
```
