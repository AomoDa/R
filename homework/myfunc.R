


##Extract Height
##input: a column in the data
##output : a numeric vector with the values for
##the records (corresponding to meters).
get_height <- function(x){
  x <- as.character(x)
  y <- as.numeric(unlist(str_extract_all(x, "^[0-9][.][0-9]+")))
  return(y)
}


##Extract Athlete’s Name
##input: a column in the data
##output : a character vector with 
##just the first and last names of athletes
get_athlete_name <- function(x){
 y <- as.character(unlist(str_extract_all(x, "[a-zA-Z]+[ ][a-zA-Z]+")))
  return(y)
}

###Extract Country
##input: a column in the data
##output : a character vector containing 
##the initials of the countries (without no parenthesis)
get_athlete_country <- function(x){
  x <- as.character(unlist(str_extract_all(x, "\\([a-zA-Z]+\\)")))
  y <- str_replace_all(x,pattern = '\\(|\\)',replacement = '')
  return(y)
}


##Remove Brackets:
##input: a column in the data
##output : return a “clean”" character vector 
##with no brackets (and no numbers inside the brackets).
remove_brackets <- function(x){
 x <- as.character(x)
 x <- str_replace(x,pattern = '(\\[[0-9]\\])+',replacement = '')
 return(x)
}



##day
##input: a column in the data
##output : return a numeric vector with such day numbers
get_day <-function(x){
 x <- remove_brackets(x)
 y <- as.numeric(unlist(str_extract(x, "[0-9]+")))
 return(y)
}


##mon
##input: a column in the data
##output : return a character vector with the names of the months
get_mon <-function(x){
 x <- remove_brackets(x)
 y <- as.character(unlist(str_extract(x, "[a-zA-Z]+")))
 return(y)
}

##year
##input: a column in the data
##output : return a numeric vector with  year numbers
get_year <-function(x){
 x <- remove_brackets(x)
 y <- as.numeric(unlist(str_extract(x, "[0-9]+$")))
 return(y)
}


##date
##input: a column in the data
##output : return a vector of class "Date"
get_date <- function(x){
 num_m <- match(get_mon(x),month.name)
 date_char <- paste(get_year(x),num_m,get_day(x),sep='-')
 y <- as.Date(date_char)
 return(y)
}



##Extract City
##input: a column in the data
##output : return a character vector with just the name fo the city
get_city <- function(x){
 y <- as.character(unlist(str_extract(x, "[a-zA-Z]+")))
 return(y)
}



## Extra Credit
##input: a column in the data
##output : returns a character vector in US customary units
get_height_extra <- function(x){
  x <- as.character(x)
  char <-c()
  for (i in 1:length(x)) {
  y <- unlist(str_extract_all(x[i], 
    "[0-9]([.])?[0-9]*"))[-1]
  if( (!is.na(y[3])) & (!is.na(y[4])) ){
    char[i] <- paste(y[1],'ft',y[2],
      paste(y[3],'/',y[4],sep = ''),
      'in',
      sep = ' ') } else if ( (!is.na(y[3])) &(is.na(y[4]))){
    char[i] <-    paste(y[1],'ft',
      paste(y[2],'/',y[3],sep = ''),
      'in',
      sep = ' ') } else{
    char[i]<-paste(y[1],'ft',y[2],
      'in',
      sep = ' ')
    }
  }  
  return(char)
}


