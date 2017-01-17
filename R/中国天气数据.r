


#1
library(xlsx)
# 分别读取4个文件的数据
dat1 <- read.xlsx2(file = 'bj51-69.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
dat2 <- read.xlsx2(file = 'bj70-89.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
dat3 <- read.xlsx2(file = 'bj90-99.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
dat4 <- read.xlsx2(file = 'bj00-09.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
dat5 <- read.xlsx2(file = 'bj10-14.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
bjcma <- rbind(dat1,dat2,dat3,dat4,dat5)

#保留需要的11个变量
bjcma <- bjcma[,c(1:5,11,13,14,16,18,22)]
# 重命名字段名称
names(bjcma) <- c('区站点','年','月','日',
	'降水量','平均气温','平均相对湿度','日照时数',
	'最低气温','最高气温','最小相对湿度')
str(bjcma)



#2 and 4


# 特征值处理
##所有要素
bjcma[,-1][bjcma[,-1]==32744] <- NA
bjcma[,-1][bjcma[,-1]==32766] <- NA
bjcma[,-1][bjcma[,-1]==32700] <- 0
# 单位修改。将温度相关的单位统一成摄氏度℃，将降水量转换成毫米mm，日照时数单位改成小时
bjcma[,c(5,6,8,9:10)] <- bjcma[,c(5,6,8,9:10)] / 10
summary(bjcma)


#3
library(ggplot2)

summary(bjcma)
##平均气温直方图
par(mfrow=c(1,3))
hist(bjcma$平均气温,main='平均气温直方图',xlab='平均气温')
hist(bjcma$最低气温,main='最低气温直方图',xlab='最低气温')
hist(bjcma$最高气温,main='最高气温直方图',xlab='最高气温')
par(mfrow=c(1,1))


#5

library(reshape)
temp_data <- melt(data = bjcma,id.vars = c("区站点","年" ,"月" ,"日"))
temp_data <- na.omit(temp_data)

# mbjcma
rt_fun <- function(x) {
	return(data.frame(avg=mean(x,na.rm=T),
		              max=max(x,na.rm=T),
		              min=min(x,na.rm=T),
		              mid=median(x,na.rm=T)
		              )
	)
}
mbjcma <- cast(data=temp_data,年+月~variable,rt_fun )

# extrabj
extrabj <- data.frame()
for (i in unique(bjcma[,2])) {
	a <- bjcma[bjcma[,2]==i,]
	b <- data.frame(year=i,
		            max_temperature=max(a[,10]),
		            max_temperature_mon=a[which.max(a[,10]),3],
		            max_temperature_day=a[which.max(a[,10]),4],	
		            min_temperature=min(a[,9]),
		            min_temperature_mon=a[which.min(a[,9]),3],
		            min_temperature_day=a[which.min(a[,9]),4]
		            )
	extrabj <- rbind(b,extrabj)
}

# 历史最高温/最低温及其出现的日期
bjcma[which.max(bjcma[,10]),c(2:4,10)]
bjcma[which.min(bjcma[,9]),c(2:4,9)]


#6

# write file
write.xlsx2(bjcma,'bjcma.xlsx')
write.xlsx2(mbjcma,'mbjcma.xlsx')
write.xlsx2(extrabj,'extrabj.xlsx')


#7

bjspi <- aggregate(降水量~区站点+月+年,data=bjcma,sum)
write.table(bjspi,file = 'bjspi.txt',sep='\t')

#8
bjspi_ts <- ts(bjspi[,4],frequency = 12,start = c(1951,1))
plot.ts(bjspi_ts)




# Part 1
#---------------------------------

rm(list = ls())

library(xlsx)
sheets <- getSheets(loadWorkbook(file = 'treerings.xls'))
# 利用循环将这八张表读入R并建立相应的数据框，分别命名为tr1-tr8
for (i in 1:length(sheets)) {
  x <- read.xlsx2('treerings.xls',sheetIndex = i,as.data.frame = T,header = F,stringsAsFactors=F,colClasses=c('character',rep('numeric',10)),colIndex=2:12)
  x <- data.frame(x[,-1],row.names = x[,1])
  x <- as.data.frame(t(x))
  x[is.na(x)] <- NA
  assign(paste('tr',i,sep=''),x)
}

ls()




#2
# 自定义一个判断数据框是否完全一样的function
myfunc1 <- function(dat1,dat2) {
 dat1[is.na(dat1)] <- Inf
 dat2[is.na(dat2)] <- Inf
 sum(all(dat1==dat2))
}

rlt <- matrix(0,ncol = 8,nrow = 8)

for (i in 1:7) {
	for (j in (i+1):8) {
        x <- get(paste('tr',i,sep=''))
        y <- get(paste('tr',j,sep=''))
		rlt[i,j] <- eval(call('myfunc1',x,y))
		rlt[j,i] <- rlt[i,j] 
	}	
}
# tr3 和tr5 是完全相同。
rlt


#3
#将检查后的数据合并成一个数据集
# 删除tr3,保留tr5
x <- data.frame()
for (i in c(1:2,4:8)) {
  a <- get(paste('tr',i,sep=''))
  a$tableid <- i
  x <- rbind(a,x)
}



#4
library(reshape)
library(stringr)

mydata <- melt(data = x,
	id.vars = c('tableid','DISC','H',  
		'Ring','DOBNS','DOBWE','MDOB',
		'DUBNS','DUBWE','MDUB'),
	na.rm=T,variable_name='year')

mydata$year <- str_extract_all(mydata$year,'[0-9]+')
head(mydata)
write.xlsx2(mydata,'mydata.xlsx')



