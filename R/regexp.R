


# define   get_mydata function

get_mydata <- function(data_file) {
if(file.exists(data_file)) {
require('stringr')
a <- read.delim('sessions.htm',header = F,encoding = 'UTF-8',stringsAsFactors = F,skip=2)
id <- 0
a$sessionid <- NA
a$speakerNumber <- NA
a$speakerLastName <- NA
a$speakerFirstName <- NA
a$speakerAffiliation <- NA
a$sessionOrganizer <- NA
a$organizerAffiliation <-NA
a$organizerEmail <- NA
a$sessionChair <-
a$chairAffiliation <-NA
a$chairEmail <- NA
a$discussant <- NA
a$speakerEmail <-NA
a$presentationTitle <-NA
a_ema <- as.character(str_extract(a[,1],'[[:alnum:]]+[@][[:alnum:]]+[.][[:alnum:]]+'))
a_title <- str_replace(as.character(str_extract(a[,1],'Title[:][^<]+')),'Title[:]','')
a_dis <- str_replace(as.character(str_extract(a[,1],'Discussant[:][^<]+')),'Discussant[:]','')
for (i in 1:nrow(a)) {

# sessionid 
 if(!is.na(str_match(a[i,1],'<strong>[^:]*<[/]strong>')) ){
    id <- id + 1 
    a$sessionid[i] <- id 
  }
 a$sessionid[i] <- id

#sessionName
a$sessionName[i] <- str_extract(as.character(str_extract(a[i,1],
             '<strong>[^:]*<[/]strong>')),'([^<|>]*[ ])+')

## speaker
## there are some error word "Spekaer" ?= "Speaker"
a_num <- as.character(str_extract(a[i,1],'(Speaker|Spekaer)[ ]*[1-4][^<]+'))

if(!is.na(a_num)){
 a$speakerNumber[i] <- str_extract(a_num,'[1-4]')
 a$speakerFirstName[i] <- str_split(str_split(a_num,'[:]|[,]')[[1]][2],'[ ]')[[1]][2]
 a$speakerLastName[i] <- str_split(str_split(a_num,'[:]|[,]')[[1]][2],'[ ]')[[1]][3]
 a$speakerAffiliation[i] <- str_replace(a_num,'.*[:][^,]*[,]','')
 a$speakerEmail[i] <-a_ema[i+1]
 a$presentationTitle[i] <- a_title[i+2]
 a$discussant[i] <- a_dis[i+3]
}


##Organizer and Chair
a_org_cha <- as.character(str_extract(a[i,1],'Organizer[ ]and[ ]Chair[:][^<]+'))
a_org<- as.character(str_extract(a[i,1],'Organizer[^<]+'))
a_cha<- as.character(str_extract(a[i,1],'Chair[^<]+'))

if(!is.na(a_org_cha)){
 a$sessionOrganizer[i] <- str_split(a_org_cha,'[:]|[,]')[[1]][2]
 a$sessionChair[i] <- str_split(a_org_cha,'[:]|[,]')[[1]][2]
 a$organizerAffiliation[i] <- str_replace(a_org_cha,'.*[:][^,]+[,]','')
 a$chairAffiliation[i] <- str_replace(a_org_cha,'.*[:][^,]+[,]','')
 a$organizerEmail[i] <- ifelse(is.na(a_ema[i+1]),a_ema[i],a_ema[i+1])
 a$chairEmail[i] <-ifelse(is.na(a_ema[i+1]),a_ema[i],a_ema[i+1])
}
if(!is.na(a_org)){
  a$sessionOrganizer[i] <- str_split(a_org,'[:]|[,]')[[1]][2]
  a$organizerAffiliation[i] <- str_replace(a_org,'.*[:][^,]+[,]','')
  a$organizerEmail[i] <- ifelse(is.na(a_ema[i+1]),a_ema[i],a_ema[i+1])
}
if(!is.na(a_cha)){
  a$sessionChair[i] <- str_split(a_cha,'[:]|[,]')[[1]][2]
  a$chairAffiliation[i] <- str_replace(a_cha,'.*[:][^,]+[,]','')
  a$chairEmail[i] <-ifelse(is.na(a_ema[i+1]),a_ema[i],a_ema[i+1])
}
}
 
sess_info  <- na.omit(a[,c('sessionid','sessionName')])
org_info <- na.omit(a[,c('sessionid','sessionOrganizer','organizerAffiliation','organizerEmail')])
chair_info  <- na.omit(a[,c('sessionid','sessionChair','chairAffiliation','chairEmail')])
spea_info <- spea_info <- a[!is.na(a$speakerNumber),c('sessionid','speakerNumber','speakerLastName','speakerFirstName',
                                    'speakerAffiliation','discussant','presentationTitle')]
xx <- merge(merge(merge(sess_info,org_info,all.x = T),chair_info),spea_info)[,-1]

return(xx)
}

}


# run  get_mydata function
mydata <- get_mydata(data_file = 'sessions.htm')
