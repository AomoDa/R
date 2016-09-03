# Part 1 : Loading Data

data_get_odbc <- function() 
{
require(RODBC) #加载RODBC包，用于载入Excel数据
data_channel <- odbcConnectExcel('2011-2014国控日均.xls') #建立ODBC源链接
data_table <- sqlTables(channel = data_channel) #读取Excel源的表结构
data_sheet <- unique(sub(pattern='\\$\\_?',replacement='\\',x=data_table$TABLE_NAME)) #取出Excel中所有的Sheet名称
data_sheet <- data_sheet[c(1:5,8,10:11)]# 选取需要载入的Sheet名称
data_all <- data.frame(stringsAsFactors=F) #创建空的数据框，用于加载所需要的数据
for (i in 1:length(data_sheet)) 
    {data_all=rbind(data_all,data.frame(area=data_sheet[i],as.data.frame(sqlFetch(channel = data_channel,sqtable = data_sheet[i])) ,stringsAsFactors=F))}
close(data_channel)
return(data_all)
}

data_all <- data_get_odbc() # 调用数据获取函数

#使用循环依次载入所有需要的Sheet的数据，并且将所有的数据合并成为一张表格

###################################################################################################################################
###################################################################################################################################

# Part 2 : Processing Data

data_proc <- function() 
{
data_all$date <- as.Date(data_all$date) # 将data转化为日期格式
data_new <- data_all[data_all$date >='2012-01-01',] # 筛选出2012年以后的数据。
data_new$date_num<-as.numeric(substr(data_new$date,1,4))*100+as.numeric(substr(data_new$date,6,7))#创建年月
data_new$date_year<-as.numeric(substr(data_new$date,1,4))#创建年
data_new$date_mon<-as.numeric(substr(data_new$date,6,7))#创建月
data_new$date_quart<-quarters(data_new$date)#创建季度
data_new$area <- as.factor(data_new$area)
data_new$season[data_new$date_mon %in% c(2,3,4)] <- 'Spring'
data_new$season[data_new$date_mon %in% c(5,6,7)] <- 'Summer'
data_new$season [data_new$date_mon %in% c(8,9,10)]<- 'Autumn'
data_new$season [data_new$date_mon %in% c(11,12,1)]<- 'Winter'
data_new$AQI_type[data_new$AQI >=100 & data_new$AQI <=150] <-'轻度污染'
data_new$AQI_type[data_new$AQI >=151 & data_new$AQI <=200] <-'中度污染'
data_new$AQI_type[data_new$AQI >=201 & data_new$AQI <=300] <-'重度污染'
return(data_new)
}

data_new <- data_proc() # 调用数据处理函数

###################################################################################################################################
###################################################################################################################################

# Part 3 : Exploring  Data

library(plyr)  #加载plyr包，用于分组计算数据
library(ggplot2)  #加载ggplot2包，用于绘图可视化


data_mean_mon<-ddply(.data = data_new,.variables = .(area,date_mon),.fun=summarise,
      SO2=round(mean(SO2,na.rm=T),1),NO2=round(mean(NO2,na.rm=T),1),PM10=round(mean(PM10,na.rm=T),1),CO=round(mean(CO,na.rm=T),1),
      O38h=round(mean(O38h,na.rm=T),1),PM2.5=round(mean(PM2.5,na.rm=T),1), AQI=round(mean( AQI,na.rm=T),1))

data_mon<-ddply(.data = data_new,.variables = .(date_mon),.fun=summarise,
      SO2=round(mean(SO2,na.rm=T),1),NO2=round(mean(NO2,na.rm=T),1),PM10=round(mean(PM10,na.rm=T),1),CO=round(mean(CO,na.rm=T),1),
      O38h=round(mean(O38h,na.rm=T),1),PM2.5=round(mean(PM2.5,na.rm=T),1), AQI=round(mean( AQI,na.rm=T),1))
data_mon$area<-'佛山市'

data_mean_year<-ddply(.data = data_new,.variables = .(area,date_year),.fun=summarise,
      SO2=round(mean(SO2,na.rm=T),1),NO2=round(mean(NO2,na.rm=T),1),PM10=round(mean(PM10,na.rm=T),1),CO=round(mean(CO,na.rm=T),1),
      O38h=round(mean(O38h,na.rm=T),1),PM2.5=round(mean(PM2.5,na.rm=T),1), AQI=round(mean( AQI,na.rm=T),1))

data_year<-ddply(.data = data_new,.variables = .(date_year),.fun=summarise,
      SO2=round(mean(SO2,na.rm=T),1),NO2=round(mean(NO2,na.rm=T),1),PM10=round(mean(PM10,na.rm=T),1),CO=round(mean(CO,na.rm=T),1),
      O38h=round(mean(O38h,na.rm=T),1),PM2.5=round(mean(PM2.5,na.rm=T),1), AQI=round(mean( AQI,na.rm=T),1))

data_year$area<-'佛山市'

data_mean_every_mon<-ddply(.data = data_new,.variables = .(area,date_mon,date_year),.fun=summarise,
      SO2=round(mean(SO2,na.rm=T),1),NO2=round(mean(NO2,na.rm=T),1),PM10=round(mean(PM10,na.rm=T),1),CO=round(mean(CO,na.rm=T),1),
      O38h=round(mean(O38h,na.rm=T),1),PM2.5=round(mean(PM2.5,na.rm=T),1), AQI=round(mean( AQI,na.rm=T),1))



# 8个国控点及佛山市  AQI值月平均折线图
ggplot(data=rbind(data_mon,data_mean_mon), aes(x=as.factor(date_mon),y=AQI,group=area,col=area))+geom_path()+
      labs(x='月份',y='月均AQI',title='2012-2014年佛山市月均AQI趋势') 




# 8个国控点及佛山市 AQI年均折线图
ggplot(data=rbind(data_year,data_mean_year), aes(x=as.factor(date_year),y=AQI,group=area,col=area))+geom_path()+
      labs(x='年',y='年均AQI',title='2012-2014年佛山市年均AQI趋势') 




###################################################################################################################################

# 要绘制其他污染物，把下面的AQI全部替换成某个污染物
# 其他自己添加吧

# 8个国控点及佛山市 SO2值月平均折线图
ggplot(data=data_mean_mon, aes(x=as.factor(date_mon),y=SO2,group=area,col=area))+geom_path()+
      labs(x='月份',y='月均AQI',title='2012-2014年佛山市月均SO2趋势') 



# 8个国控点及佛山市 SO2年均折线图
ggplot(data=rbind(data_year,data_mean_year), aes(x=as.factor(date_year),y=SO2,group=area,col=area))+geom_path()+
      labs(x='年',y='年均AQI',title='2012-2014年佛山市年均SO2趋势') 


###################################################################################################################################






#先分别筛选八个国控点每一个月的首要污染物，用柱状图表示。横坐标为时间序列，纵坐标为首要污染物次数，柱状图颜色代表首要污染物种类 每一年一张图

ggplot(data=data_new[data_new$date_year==2012 & !is.na(data_new$首要污染物),] ,aes(x=as.factor(date_mon)))+geom_bar(aes(fill=首要污染物),position='stack') +
      labs(x='月份',y='天数',title='2012年佛山市首要污染物分布') +facet_wrap(~area)

ggplot(data=data_new[data_new$date_year==2013 & !is.na(data_new$首要污染物),] ,aes(x=as.factor(date_mon)))+geom_bar(aes(fill=首要污染物),position='stack') +
      labs(x='月份',y='天数',title='2013年佛山市首要污染物分布') +facet_wrap(~area)

ggplot(data=data_new[data_new$date_year==2014 & !is.na(data_new$首要污染物) ,] ,aes(x=as.factor(date_mon)))+geom_bar(aes(fill=首要污染物),position='stack') +
      labs(x='月份',y='天数',title='2014年佛山市首要污染物分布') +facet_wrap(~area)



# 然后绘制饼图，按季度和年度分类，年度：2012/2013/2014季度2012春夏秋冬/2013春夏秋冬/2014春夏秋冬




ggplot(data=data_new[data_new$date_year==2012 & !is.na(data_new$首要污染物) ,] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) +
   coord_polar(theta = "y")+labs(x=NULL,y=NULL,title='2012年佛山市首要污染物饼图')

ggplot(data=data_new[data_new$date_year==2013 & !is.na(data_new$首要污染物) ,] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) +
  coord_polar(theta = "y") +labs(x=NULL,y=NULL,title='2013年佛山市首要污染物饼图')

ggplot(data=data_new[data_new$date_year==2014 & !is.na(data_new$首要污染物) ,] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) + 
coord_polar(theta = "y") +labs(x=NULL,y=NULL,title='2014年佛山市首要污染物饼图')





ggplot(data=data_new[data_new$date_year==2012  & !is.na(data_new$首要污染物),] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) +
   coord_polar(theta = "y")+labs(x=NULL,y=NULL,title='2012年佛山市首要污染物饼图') +facet_wrap(~season)

ggplot(data=data_new[data_new$date_year==2013  & !is.na(data_new$首要污染物),] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) +
  coord_polar(theta = "y") +labs(x=NULL,y=NULL,title='2013年佛山市首要污染物饼图') +facet_wrap(~season)

ggplot(data=data_new[data_new$date_year==2014  & !is.na(data_new$首要污染物),] ,aes(x=as.factor(1)))+geom_bar(aes(fill=首要污染物),position='fill',width = 1) + 
coord_polar(theta = "y") +labs(x=NULL,y=NULL,title='2014年佛山市首要污染物饼图')+facet_wrap(~season)

###################################################################################################################################

ggplot(data=data_new[!is.na(data_new$AQI_type),] ,aes(x=as.factor(date_year),fill=AQI_type) ) +geom_bar(position='dodge') +facet_wrap(~area)


ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2012,] ,aes(x=as.factor(season),fill=AQI_type) ) +geom_bar(position='dodge') +
      facet_wrap(~area) +labs(x='季节',y='天数',title='2012年佛山市AQI分布') 

ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2013,] ,aes(x=as.factor(season),fill=AQI_type) ) +geom_bar(position='dodge') +
      facet_wrap(~area) +labs(x='季节',y='天数',title='2013年佛山市AQI分布') 

ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2014,] ,aes(x=as.factor(season),fill=AQI_type) ) +geom_bar(position='dodge') +
       facet_wrap(~area) +labs(x='季节',y='天数',title='2014年佛山市AQI分布') 


ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2012,] ,aes(x=as.factor(season),fill=首要污染物) ) +geom_bar(position='stack') +
       facet_wrap(~area) +labs(x='季节',y='天数',title='2012年佛山市污染天气首要污染物分布') 

ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2013,] ,aes(x=as.factor(season),fill=首要污染物) ) +geom_bar(position='stack') +
       facet_wrap(~area) +labs(x='季节',y='天数',title='2013年佛山市污染天气首要污染物分布') 

ggplot(data=data_new[!is.na(data_new$AQI_type) &data_new$date_year==2014,] ,aes(x=as.factor(season),fill=首要污染物) ) +geom_bar(position='stack') +
       facet_wrap(~area) +labs(x='季节',y='天数',title='2014年佛山市污染天气首要污染物分布') 

durative_pollution <- function(data)  # 定义污染天气持续次数计算函数
{
data_new <- as.data.frame(data)
x<-data.frame()
for (i in unique(data_new$area))  { 
z <- data_new[data_new$area==i,]
z <- z[!is.na(z$AQI),]
z <- z[order(z$date),]
z$durative_pollution <-0
for ( j in 2: (nrow(z)-1)   ) { if( z$AQI[j-1] <100 & z$AQI[j] >=100 &z$AQI[j+1] >=100 ) {z$durative_pollution[j] <-1}   }
if(z$AQI[1] >=100 &z$AQI[2] >=100) z$durative_pollution[1] <-1
x<-rbind(x,z)
}
return(x)
}


z<-durative_pollution(data_new)

ggplot(data=z  ,aes(x=as.factor(date_year) ,weight=durative_pollution,fill=as.factor(date_year))) + geom_bar(position='dodge') +facet_wrap(~area) +
       labs(x='年',y='天数',title='佛山市污染天气持续次数分布')  +guides(fill = guide_legend(title = "年", title.position = "top"))


ggplot(data=z[z$date_year==2012,]  ,aes(x=as.factor(season) ,weight=durative_pollution,fill=as.factor(season))) + geom_bar(position='dodge') +facet_wrap(~area) +
       labs(x='季节',y='天数',title='2012年佛山市污染天气持续次数分布')  +guides(fill = guide_legend(title = "季节", title.position = "top"))

ggplot(data=z[z$date_year==2013,]  ,aes(x=as.factor(season) ,weight=durative_pollution,fill=as.factor(season))) + geom_bar(position='dodge') +facet_wrap(~area) +
       labs(x='季节',y='天数',title='2013年佛山市污染天气持续次数分布')  +guides(fill = guide_legend(title = "季节", title.position = "top"))

ggplot(data=z[z$date_year==2014,]  ,aes(x=as.factor(season) ,weight=durative_pollution,fill=as.factor(season))) + geom_bar(position='dodge') +facet_wrap(~area) +
       labs(x='季节',y='天数',title='2014年佛山市污染天气持续次数分布')  +guides(fill = guide_legend(title = "季节", title.position = "top"))




###################################################################################################################################
###################################################################################################################################

#PM 2.5 部分

# 年 -- PM2.5时空分布特征，时间序列图空间分布图（8个国控点）

ggplot(data=rbind(data_year,data_mean_year), aes(x=as.factor(date_year),y=PM2.5,group=area,col=area))+geom_path()+
      labs(x='年',y='年均PM2.5',title='2012-2014年佛山市年均PM2.5趋势') 

# 月-- PM2.5时空分布特征，时间序列图空间分布图（8个国控点）

ggplot(data=rbind(data_mon,data_mean_mon), aes(x=as.factor(date_mon),y=PM2.5,group=area,col=area))+geom_path()+
      labs(x='月份',y='月均PM2.5',title='2012-2014年佛山市月均PM2.5趋势') 




# PM2.5 持续污染过程处理

data_new_hczz <-data_new[data_new$area=='华材职中',]

pm2.5_durative_pollution <-function(data) { # 定义个函数求PM2.5持续污染的次数，和AQI的算法一样
z <-as.data.frame(data)
z <- z[order(z$date),]
z <-z[!is.na(z$PM2.5),]
z$pm2.5_ty<-0
for (i in 2: (nrow(z)-2)) { z$pm2.5_ty[i] <- ifelse( z$PM2.5[i-1] <=75 & z$PM2.5[i]>75 &z$PM2.5[i+1]>75 &z$PM2.5[i+2] >75,1,0)     }
z$pm2.5_ty[1] <- ifelse( z$PM2.5[1]>75 &  z$PM2.5[2]>75& z$PM2.5[3]>75 ,1,0 )
return(z)
}

data_new_hczz_pm2.5_durative_pollution <-pm2.5_durative_pollution(data=data_new_hczz)


#PM2.5持续污染次数年度分布
ggplot(data=data_new_hczz_pm2.5_durative_pollution  ,aes(x=as.factor(date_year) ,weight=pm2.5_ty,fill=as.factor(date_year))) + geom_bar(position='stack')  +
       labs(x='年',y='次数',title='佛山市PM2.5持续污染次数年度分布')  +guides(fill = guide_legend(title = "年", title.position = "top"))
# PM2.5持续污染次数季度分布
ggplot(data=data_new_hczz_pm2.5_durative_pollution  ,aes(x=as.factor(season) ,weight=pm2.5_ty,fill=as.factor(season))) + geom_bar(position='stack')  +
       labs(x='季度',y='次数',title='佛山市PM2.5持续污染次数季度分布')  +guides(fill = guide_legend(title = "季度", title.position = "top"))+facet_wrap(~date_year,ncol=1)

#PM2.5持续污染次数月度分布
ggplot(data=data_new_hczz_pm2.5_durative_pollution  ,aes(x=as.factor(date_mon) ,weight=pm2.5_ty,fill=as.factor(date_mon))) + geom_bar(position='stack')  +
       labs(x='月',y='次数',title='佛山市PM2.5持续污染次数月度分布')  +guides(fill = guide_legend(title = "月", title.position = "top")) +facet_wrap(~date_year,ncol=1)
 


# Part 1 : Loading Data

stations_info <- read.csv('get.csv',header = T,sep=',',stringsAsFactors = F)

get_data <- function (data)  # 定义一个导入数据的程序
{
 require(xlsx) # 加载所需要的包
 require(rJava)# 加载所需要的包
 require(xlsxjars) # 加载所需要的包
 stations_info <- as.data.frame(data) #格式化数据
 x<-data.frame() ##初始化数据框
 xx<-data.frame() ##初始化数据框
 n.succ <-0 #初始化成功导入表格数量
 n.fail <-0 #初始化由于文件不存在而无法导入表格数量
 fail.name <-vector()   #初始化由于文件不存在而无法导入表格名称
 fail.index <- 1 #初始化由于文件不存在而无法导入表格索引
 a<-paste(stations_info[,1],stations_info[,2],'-2013','.xls',sep='') # 提取出需要导入的2013年的文件名称
 b<-paste(stations_info[,1],stations_info[,2],'-2014','.xls',sep='') # 提取出需要导入的2014年的文件名称
 c<-paste(stations_info[,1],stations_info[,2],'-2009-2012','.xls',sep='') # 提取出需要导入的2009-2012年的文件名称

 for (i in 1:nrow(stations_info)) #创建循环读取文件
  {  
    if(file.exists(a[i])) #判断文件是否存在
    {
      x_temp <- read.xlsx2(file = a[i],sheetIndex = 1,colClasses = c(rep('character',2),'Date',rep('numeric',28)),stringsAsFactors=F) 
       # 读取2013年的数据
      x_temp <- data.frame(area=stations_info[i,6],x_temp)
      x <- as.data.frame(rbind(x,x_temp))  #合并数据
      n.succ<-n.succ+1 # 成功数量加1
     } else {n.fail<-n.fail+1 ; fail.name[fail.index] <- a[i]; fail.index <- fail.index+1} # 保存由于文件缺失而无法导入的数据的信息
    if(file.exists(b[i])) #判断文件是否存在
   {
      x_temp <- read.xlsx2(file = b[i],sheetIndex = 1,colClasses = c(rep('character',2),'Date',rep('numeric',28)),stringsAsFactors=F) 
      # 读取2014年的数据
      x_temp <- data.frame(area=stations_info[i,6],x_temp)
      x <- as.data.frame(rbind(x,x_temp)) #合并数据
      n.succ<-n.succ+1 # 成功数量加1
     } else { n.fail<-n.fail+1 ; fail.name[fail.index] <- b[i]; fail.index <- fail.index+1 }  # 保存由于文件缺失而无法导入的数据的信息

    if(file.exists(c[i])) #判断文件是否存在
    {
      xx_temp <- read.xlsx2(file = c[i],sheetIndex = 1,colClasses = rep('numeric',12),stringsAsFactors=F) 
      xx_temp <- data.frame(area=stations_info[i,6],xx_temp)
      xx <- as.data.frame(rbind(xx,xx_temp))  #合并数据
      n.succ<-n.succ+1 # 成功数量加1
     } else {n.fail<-n.fail+1 ; fail.name[fail.index] <- c[i]; fail.index <- fail.index+1} # 保存由于文件缺失而无法导入的数据的信息
  } 
return( list(data_13_14=x,data_09_12=xx,info=print( c(paste('成功导入',n.succ,'张表格',sep='' ) 
     ,paste('由于文件不存在，无法导入数据，共缺失',n.fail,'张表格，缺失的数据名单在fail_table中有详细记录。',sep='')) ) ,fail_table= fail.name) )  
}

x <- get_data(stations_info)
x_data <-x$data_13_14
x_data$DDATETIME <- as.Date(as.character(x_data$DDATETIME)) # 这里需要特别特别的注意，导入的数据的日期格式并不是真正的格式。原因是Excel中的数据不规范。R语言识别不好。
xx_data <- x$data_09_12
rm(x) # 释放数据，防止内存不够用。
###################################################################################################################################


#处理09-12年的数据使得和13-14年的数据格式一致。

xx_data$date <-  as.Date(paste(xx_data$YY,'-',xx_data$MM,'-',xx_data$DD,sep = ''))

data_09_12 <- ddply()



###################################################################################################################################
###################################################################################################################################


data_match <- function(x,y)  #创建数据匹配函数，用于将颗粒物的数据和气象数据匹配到一起。
{
 require(plyr) # 载入所需要的包
 x <- as.data.frame(x) #格式化数据
 y <- as.data.frame(y[,c(1,2,5,8)]) # 选取颗粒物的详
 names(y)[2] <- 'DDATETIME'  # 将date命名为 DDATETIME  
 y$DDATETIME <- as.Date(as.character(y$DDATETIME))
 x_summary <- ddply(.data=x,.variables=.(DDATETIME),.fun=summarise,
          DF=round(mean(WD2DF,na.rm=T)), T=round(mean(T,na.rm=T)), WD=mean(WD2DD,na.rm=T),
          RH=round(mean(RH,na.rm=T)), RF=round(sum(HOURRF,na.rm=T)) )
 # x_summary 是将所有的数据每个时段的数据汇总成每天，求平均风速、风向、
 y_summary <- ddply(.data=y,.variables=.(DDATETIME),.fun=summarise,PM10=round(mean(PM10,na.rm=T )),PM2.5=round(mean(PM2.5,na.rm=T)))
 x_output<-join(x_summary,y_summary,type = "left",match = "first",by='DDATETIME')
 for (i in 1:nrow(x_output))  # 划分季节 分向
 {
x_output$season[i] <- switch(as.numeric(substr(x_output$DDATETIME,6,7))[i],'Winter','Spring','Spring','Spring',
                           'Summer','Summer','Summer','Autumn','Autumn','Autumn','Winter','Winter')
x_output$WD_scale[i] <-switch(ceiling(x_output$WD/22.5)[i],
                     '0-22.5','22.5-45','45-67.5','67.5-90','90-112.5','112.5-135','135-157.5','157.5-180', '180-202.5','202.5-225',
                      '225-247.5','247.5-270','270-292.5','292.5-315','315-337.5','337.5-360') }
x_output$season <- as.factor(x_output$season) # 水平化
x_output$WD_scale <- as.factor(x_output$WD_scale )
 return(x_output)
}


data_ok <- data_match(x = x_data,y=data_new)
data_ok$year <- substr(data_ok$DDATETIME,1,4)
data_ok$mon <- as.numeric(substr(data_ok$DDATETIME,6,7))
###################################################################################################################################
###################################################################################################################################

# Part 3 : Analysing Data

mean_data_13_14 <- ddply(.data = data_ok,.variables = .(mon,year),.fun = summarise,DF=mean(DF,na.rm =T) *0.1,
                 T=mean(T,na.rm =T)*0.1,WD=mean(WD,na.rm = T),RH=mean(RH,na.rm =T),RF=sum(RF,na.rm =T))

mean_data_09_12<- ddply(.data=xx_data,.variables = .(MM,YY),.fun = summarise,T=mean(T,na.rm = T),WD=mean(WD2,na.rm=T),DF=mean (WF2,na.rm=T),RH=mean(R1H,na.rm=T)*10,RF=NA   )
names(mean_data_09_12)[c(1,2)] <- c('mon','year')


# 风、温、湿、压 变化

# 风速 / 密度图

# 2
ggplot(data=rbind(mean_data_09_12[,c(1,2,5)] ,mean_data_13_14[,c(1,2,3)] )
,aes(x=as.factor(mon),y=DF ,group=as.factor(year),col=year)) +geom_path() +
        labs(x='月份',y='平均风速',title='2009-2014年佛山市月度平均风速') +facet_wrap(~year,ncol=2)



# 温度  月均变化
ggplot(data=rbind(mean_data_09_12[,c(1,2,3)] ,mean_data_13_14[,c(1,2,4)] ),aes(x=as.factor(mon),y=T ,group=as.factor(year),col=year)) +geom_path() +
        labs(x='月份',y='平均温度',title='2009-2014年佛山市月度平均温度')+facet_wrap(~year,ncol=2)


# 温度  年均变化

data_year_mean <- ddply(.data=rbind(mean_data_09_12[,c(1,2,3)] ,mean_data_13_14[,c(1,2,4)] ),.variables=.(year),.fun=summarise,T=mean(T))

ggplot(data=data_year_mean,aes(x=year,weight=T,fill=T )) +geom_bar() +
        labs(x='年份',y='平均温度',title='2009-2014年佛山市年均平均温度')


# 相对湿度
ggplot(data=rbind(mean_data_09_12[,c(1,2,6)] ,mean_data_13_14[,c(1,2,6)] ),aes(x=as.factor(mon),y=RH ,group=as.factor(year),col=year)) +geom_path() +
        labs(x='月份',y='平均相对湿度',title='2009-2014年佛山市月度平均相对湿度')+facet_wrap(~year,ncol=2)


# 降雨量 (选一种) # 只有13-14年的
#图1
ggplot(data=mean_data_ok) + geom_bar(aes(x=as.factor(mon),weight=RF ,fill=as.factor(year)),position='dodge' ) +
        labs(x='月份',y='月总降雨量',title='2013-2014年佛山市月度月降雨量') 

#图2
ggplot(data=mean_data_ok) +geom_path(aes(x=as.factor(mon),y=RF ,group=as.factor(year) ,col=year)) +
        labs(x='月份',y='月总降雨量',title='2013-2014年佛山市月度月降雨量') 

#图3
ggplot(data=mean_data_ok) + geom_bar(aes(x=as.factor(mon),weight=RF ,fill=as.factor(year) ))+
         geom_path(aes(x=as.factor(mon),y=RF ,group=as.factor(year) ,col=I('BLUE')) ,lwd=1 ) +
        labs(x='月份',y='月总降雨量',title='2013-2014年佛山市月度月降雨量') + facet_wrap(~year,ncol=1)

###################################################################################################################################
###################################################################################################################################



###################################################################################################################################
###################################################################################################################################

# 最后一部分在这里

#持续污染日，pm2.5每小时浓度数据

data_cor_hour <- read.csv('data_cor_hour.csv',header=T,stringsAsFactors = F)
data_cor_hour$DDATETIME <- as.POSIXlt(data_cor_hour$DDATETIME)
data_cor_hour$season <- as.factor(data_cor_hour$season)
data_cor_hour$type <- as.factor(data_cor_hour$type)
data_cor_hour$pm_type <- as.factor(data_cor_hour$pm_type)


ggplot(data=data_cor_hour[data_cor_hour$season=='Spring' &data_cor_hour$type=='污染日',],aes(x=DDATETIME,y=pm2.5,group=1,col=I('red') ))+
       geom_path(na.rm = T)+labs(x='时间',y='PM2.5小时浓度',title='Spring')

ggplot(data=data_cor_hour[data_cor_hour$season=='Summer' &data_cor_hour$type=='污染日',],aes(x=DDATETIME,y=pm2.5,group=1,col=I('red') ))+
       geom_path(na.rm = T)+labs(x='时间',y='PM2.5小时浓度',title='Summer')

ggplot(data=data_cor_hour[data_cor_hour$season=='Autumn' &data_cor_hour$type=='污染日',],aes(x=DDATETIME,y=pm2.5,group=1,col=I('red') ))+
       geom_path(na.rm = T)+labs(x='时间',y='PM2.5小时浓度',title='Autumn')

ggplot(data=data_cor_hour[data_cor_hour$season=='Winter' &data_cor_hour$type=='污染日',],aes(x=DDATETIME,y=pm2.5,group=1,col=I('red') ))+
       geom_path(na.rm = T)+labs(x='时间',y='PM2.5小时浓度',title='Winter')



###################################################################################################################################

# 污染前后三天温度变化 ,日均数据变化

data_cor_day <- read.csv('data_cor_day.csv',header=T,stringsAsFactors = F)
data_cor_day$date <- as.Date(data_cor_day$date)
data_cor_day$season <- as.factor(data_cor_day$season)
data_cor_day$type <- as.factor(data_cor_day$type)
data_cor_day$pm_type <- as.factor(data_cor_day$pm_type)



ggplot(data=data_cor_day[data_cor_day$season=='Spring' & !is.na(data_cor_day$type),],aes(x=date,y=T*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='T',title='Spring') 

ggplot(data=data_cor_day[data_cor_day$season=='Summer' & !is.na(data_cor_day$type),],aes(x=date,y=T*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='T',title='Summer') 

ggplot(data=data_cor_day[data_cor_day$season=='Autumn' & !is.na(data_cor_day$type),],aes(x=date,y=T*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='T',title='Autumn') 

ggplot(data=data_cor_day[data_cor_day$season=='Winter' & !is.na(data_cor_day$type),],aes(x=date,y=T*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='T',title='Winter') 

###################################################################################################################################

# 污染前后三天风速变化  日均数据变化

ggplot(data=data_cor_day[data_cor_day$season=='Spring' & !is.na(data_cor_day$type),],aes(x=date,y=WD10DF*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='WD10DF',title='Spring') 

# 夏季的数据是缺失的。画出来为0
ggplot(data=data_cor_day[data_cor_day$season=='Summer' & !is.na(data_cor_day$type),],aes(x=date,y=WD10DF*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='WD10DF',title='Summer') 

ggplot(data=data_cor_day[data_cor_day$season=='Autumn' & !is.na(data_cor_day$type),],aes(x=date,y=WD10DF*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='WD10DF',title='Autumn') 
       date
ggplot(data=data_cor_day[data_cor_day$season=='Winter' & !is.na(data_cor_day$type),],aes(x=date,y=WD10DF*0.1,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='WD10DF',title='Winter') 

###################################################################################################################################

# 污染前后三天相对湿度变化 日均数据变化

ggplot(data=data_cor_day[data_cor_day$season=='Spring' & !is.na(data_cor_day$type),],aes(x=date,y=RH,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RH',title='Spring') 

ggplot(data=data_cor_day[data_cor_day$season=='Summer' & !is.na(data_cor_day$type),],aes(x=date,y=RH,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RH',title='Summer') 

ggplot(data=data_cor_day[data_cor_day$season=='Autumn' & !is.na(data_cor_day$type),],aes(x=date,y=RH,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RH',title='Autumn') 

ggplot(data=data_cor_day[data_cor_day$season=='Winter' & !is.na(data_cor_day$type),],aes(x=date,y=RH,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RH',title='Winter') 


###################################################################################################################################

# 污染前后三天降雨量变化 日均数据变化

ggplot(data=data_cor_day[data_cor_day$season=='Spring' & !is.na(data_cor_day$type),],aes(x=date,y=RF,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RF',title='Spring') 

ggplot(data=data_cor_day[data_cor_day$season=='Summer' & !is.na(data_cor_day$type),],aes(x=date,y=RF,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RF',title='Summer') 

ggplot(data=data_cor_day[data_cor_day$season=='Autumn' & !is.na(data_cor_day$type),],aes(x=date,y=RF,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RF',title='Autumn') 

ggplot(data=data_cor_day[data_cor_day$season=='Winter' & !is.na(data_cor_day$type),],aes(x=date,y=RF,colour=type,group=1))+
       geom_path(na.rm = T,lwd=1)+labs(x='时间',y='RF',title='Winter') 


###################################################################################################################################
###################################################################################################################################

# 污染日和清洁日的相关系数和线性模型 取日数据计算

library(psych)


# 你的要求

pairs.panels(data_cor_day[data_cor_day$pm_type=='持续污染日' & data_cor_day$season=='Spring'  ,c(5,6,8,9,10,11)] ,method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='持续污染日' & data_cor_day$season=='Summer'  ,c(5,6,8,9,10,11)],method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='持续污染日' & data_cor_day$season=='Autumn'  ,c(5,6,8,9,10,11)],method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='持续污染日' & data_cor_day$season=='Winter'  ,c(5,6,8,9,10,11)],method="spearman")

pairs.panels(data_cor_day[data_cor_day$pm_type=='清洁日' & data_cor_day$season=='Spring'  ,c(5,6,8,9,10,11)],method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='清洁日' & data_cor_day$season=='Summer'  ,c(5,6,8,9,10,11)],method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='清洁日' & data_cor_day$season=='Autumn'  ,c(5,6,8,9,10,11)],method="spearman")
pairs.panels(data_cor_day[data_cor_day$pm_type=='清洁日' & data_cor_day$season=='Winter'  ,c(5,6,8,9,10,11)],method="spearman")


###################################################################################################################################

# 我认为应该这样做

# 整体
pairs.panels(data_cor_day [,c(5,6,8,9,10,11)] ,method="spearman")

# 只区分季节
pairs.panels(data_cor_day [data_cor_day$season=='Spring',c(5,6,8,9,10,11)] ,method="spearman")
pairs.panels(data_cor_day [data_cor_day$season=='Summer',c(5,6,8,9,10,11)] ,method="spearman")
pairs.panels(data_cor_day [data_cor_day$season=='Autumn',c(5,6,8,9,10,11)] ,method="spearman")
pairs.panels(data_cor_day [data_cor_day$season=='Winter',c(5,6,8,9,10,11)] ,method="spearman")




