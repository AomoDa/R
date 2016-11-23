---
title: "IMDB - Getting Started"
output: html_document
---

To connect to the database, 

```{r}
library(RMySQL)
drv = dbDriver("MySQL")
con = dbConnect(drv, dbname = 'imdb', user = 'imdb', password = 'imdb',
                host = 'radagast.berkeley.edu')
dbListTables(con)
```

Answer the following 2 questions. 

## How many records are in the `kind_type` table? 

Put your answer in `numKind`. Retrieve all of the records and display print them to the console.

```{r}
res <- dbSendQuery(con, "SELECT count(1) as numKind FROM kind_type")
numKind <- dbFetch(res, n = 2)
numKind
```

## How many movies and TV series are in imdb?

Use the information in `kind_type` to determine how many television series are in the data base and how many movies are in the data base. Put your answers in `numTV` and `numMov`, respectively

```{r}
res_tv <- dbSendQuery(con, "select count(1) as numTV from kind_type where kind like '%tv%' ")
numTV <- dbFetch(res_tv, n = 2)
numTV
res_mov <- dbSendQuery(con, "select count(1) as numMov from kind_type where kind like '%movie%' ")
numMov <- dbFetch(res_mov, n = 2)
numMov
```

## What is Brad Pitt's id?

Pull all of the names of actors and their ids  into R. Use regular expressions to find the identification number for Brad Pitt.  Place this id number in `PittID`

```{r}
library(stringr)
all_actor_names <- dbFetch(dbSendQuery(con,"select id,name from name "),n=-1)
a <- str_match(all_actor_names$name,'^Pitt[^a-zA-Z]*Brad$')
id <- 1:length(a)
PittID <- all_actor_names$id[id[!is.na(a)]]
PittID
```

## Additional Information

Examine all of the fields for the first 10 records in the `info_type` table.
If you wanted to find genre information out about a movie, which id would you use?  Find the title and production year for 10 movies in the comedy genre. 

```{r}
info_type<- dbFetch(dbSendQuery(con,"select * from info_type limit 10"))
info_type
Comedy_movie_id <- dbFetch(dbSendQuery(con,"select movie_id from movie_info where info_type_id=3  and info= 'Comedy' limit 10"))
Comedy_movie_id
Comedy <- dbFetch(dbSendQuery(con," select id, title,production_year from title where id in( 41, 77,63,65,66, 72,92,98,129,142 ) "))
Comedy
```

-----
-----

To connect to the database, 

```{r}
library(RSQLite)
drv = dbDriver("SQLite")
con = dbConnect(drv, dbname = "~/Downloads/lean_imdbpy_2010.db") #your path
dbListTables(con)
```

Answer the following 5 questions. In addition, there is 1 bonus question that you can use to improve one of your HW scores or lab scores by 1 point. 


## What proportion of the actors are female? 

Assign your answer to 'propF'

```{r, error=TRUE}
dbListFields(con,"name2")
all <- dbGetQuery(con, "SELECT count(*) as numProp FROM name2;" )
female <- dbGetQuery(con, "SELECT count(*) as numPropF FROM name2 WHERE gender = 'f'")
propF=female / all

```

## How many movies are there in the database? 

Assign your answer to 'numMovies'. Note that by movies, we mean that they are actual movies and not television series, etc.

```{r, error=TRUE}

```


## Long-Running TV Series

List the 5 longest running television series (number of seasons). Include the number of episodes as `numEp`, number of seasons as `numSe`, and name of each series as `title`. Your answer should be in a data frame called `longRun`

```{r, error=TRUE}

```


## Has the number of movies in the horror genre changed over time? 

Plot the overall number of movies in each year over time. Be sure to make your plot relative to the number movies produced each year.

```{r, error=TRUE}

```


##  Who are the actors that have been in the most movies? 

List the top 20 actors with their names and the count of movies. 
Store this information in the data frame called `top20` with  the actor's name in `name` and the count in `numM`. 

```{r, error=TRUE}

```


## Challenge Networks

Pick a (lead) actor who has been in at least 20 movies. Find all of the other actors that have appeared in a movie with that person. For each of these, find all the people they have appeared in a move with him/her. 

Use this to create a network/graph of who has appeared with who. Use the `igraph` or `statnet` packages to display this network. 

Try doing this with individual SQL commands and the process the results in R to generate new SQL queries. In other words, don't spend too much time trying to create clever SQL queries as there is a more direct way to do this in R.


```{r, error=TRUE}

```

