

#---------------------------------
# Part 1
#---------------------------------


library(xlsx)
sheets <- getSheets(loadWorkbook(file = 'treerings.xls'))

# 利用循环将这八张表读入R并建立相应的数据框，分别命名为tr1-tr8
for (i in 1:length(sheets)) {
  x <- read.xlsx2('treerings.xls',
  	               sheetIndex = i,
  	               as.data.frame = T,
  	               header = F,
  	               stringsAsFactors=F,
  	               colClasses=c('character',rep('numeric',10)),
  	               colIndex=2:12
  	               )
  x <- data.frame(x[,-1],row.names = x[,1])
  x <- as.data.frame(t(x))
  x[is.na(x)] <- NA
  assign(paste('tr',i,sep=''),x)
}

ls()


#---------------------------------
# Part 2
#---------------------------------


# 自定义一个判断数据框是否完全一样的function
compare_dt <- function(dat1,dat2) {
 dat1[is.na(dat1)] <- Inf
 dat2[is.na(dat2)] <- Inf
 sum(all(dat1==dat2))
}

compart_mt <- matrix(NA,ncol = 8,nrow = 8)

for (i in 1:7) {
	for (j in (i+1):8) {
        x <- get(paste('tr',i,sep=''))
        y <- get(paste('tr',j,sep=''))
		compart_mt[i,j] <- eval(call('compare_dt',x,y))
		compart_mt[j,i] <- compart_mt[i,j] 
	}	
}
# tr3 和tr5 是完全相同。
compart_mt

summary(tr3)
summary(tr5)


#---------------------------------
# Part 3
#---------------------------------

#将检查后的数据合并成一个数据集
# 删除tr3,保留tr5
x <- data.frame()
for (i in c(1:2,4:8)) {
  a <- get(paste('tr',i,sep=''))
  a$tableid <- i
  x <- rbind(a,x)
}


#---------------------------------
# Part 4
#---------------------------------

library(reshape)

mydata <- melt(data = x,
	id.vars = c('tableid','DISC','H',  
		'Ring','DOBNS','DOBWE','MDOB',
		'DUBNS','DUBWE','MDUB'),
	na.rm=T,variable_name='year')

mydata$year <- stringr::str_extract_all(mydata$year,'[0-9]+')
head(mydata)

