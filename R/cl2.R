


#--------------------------------------------------------
# Part 1
#--------------------------------------------------------

library(xlsx)
# 分别读取4个文件的数据
x51 <- read.xlsx2(file = 'bj51-69.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
x70 <- read.xlsx2(file = 'bj70-89.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
x90 <- read.xlsx2(file = 'bj90-99.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))
x00 <- read.xlsx2(file = 'bj00-09.xlsx',sheetIndex = 1,stringsAsFactors=F,colClasses = rep('numeric',22))

#将这些数据合并成一个数据集bjcma
bjcma <- rbind(x51,x70,x90,x00)

#保留需要的11个变量
bjcma <- bjcma[,c(1:5,11,13,14,16,18,22)]
# 重命名字段名称
names(bjcma) <- c('区站点','年','月','日',
	'降水量','平均气温','平均相对湿度','日照时数',
	'最低气温','最高气温','最小相对湿度')
str(bjcma)


#--------------------------------------------------------
# Part 2 & 4 
# 单位不统一应该是说特征值，我这合并数据集做单位统一的时候
# 需要做这些处理，因此把 4 提前一下。。。
#--------------------------------------------------------

# 特征值处理
##所有要素
bjcma[,-1][bjcma[,-1]==32744] <- NA
bjcma[,-1][bjcma[,-1]==32766] <- NA
bjcma[,-1][bjcma[,-1]==32700] <- 0
## 降水量
bjcma[,5][bjcma[,5]>=30000] <- bjcma[,5][bjcma[,5]>=30000] %% 1000
##相对湿度 无特征值，因此不需要处理

# 单位修改。将温度相关的单位统一成摄氏度℃，将降水量转换成毫米mm，日照时数单位改成小时
bjcma[,c(5,6,8,9:10)] <- bjcma[,c(5,6,8,9:10)] / 10

#--------------------------------------------------------
# Part 3
#--------------------------------------------------------

library(ggplot2)
#描述分析
summary(bjcma)

##平均气温直方图
ggplot(data=bjcma,aes(x=`平均气温`)) + 
       geom_histogram(bins = 30,col=I('white')) +
       labs(title='平均气温直方图',y='天数')

##日照时数直方图 &密度图
ggplot(data=bjcma,aes(x=`日照时数`)) + 
       geom_histogram(bins = 30,col=I('white')) +
       labs(title='日照时数直方图',y='天数')

ggplot(data=bjcma,aes(x=`日照时数`,col=as.factor(年))) + 
       geom_density(alpha=0.5) +
       labs(title='日照时数密度图',y='天数')

#--------------------------------------------------------
# Part 5
#--------------------------------------------------------

library(reshape)
temp_data <- melt(data = bjcma,id.vars = c("区站点","年" ,"月" ,"日"))

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

#--------------------------------------------------------
# Part 6
#--------------------------------------------------------

# write file
write.xlsx2(bjcma,'bjcma.xlsx')
write.xlsx2(mbjcma,'mbjcma.xlsx')
write.xlsx2(extrabj,'extrabj.xlsx')

#--------------------------------------------------------
# Part 7
#--------------------------------------------------------

bjspi <- aggregate(降水量~区站点+月+年,data=bjcma,sum)
write.table(bjspi,file = 'bjspi.txt',sep='\t')


#--------------------------------------------------------
# Part 8
#--------------------------------------------------------

bjspi_ts <- ts(bjspi[,4],frequency = 12,start = c(1951,1))
plot.ts(bjspi_ts)
