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

