


# 平均绩点达到2就及格
# 只要带文字的删除掉
# 没学分的数据也删掉了吧

j1 <- read.csv('J1.csv',header = T,stringsAsFactors = F)
j2 <- read.csv('J2.csv',header = T,stringsAsFactors = F)
# 合并数据
x <- rbind(j1,j2)
x <- x[x$课程性质!=''&!is.na(x$绩点),]
# 期末成绩和实验成绩整合
x$期末成绩 <- ifelse( (is.na(x$期末成绩) | x$期末成绩==0) & (x$实验成绩!=0 & !is.na(x$实验成绩)),x$实验成绩,x$期末成绩)
x <- x[,c(1:6,8,14:15,19)]


# 绩点分布
hist(x$绩点,breaks=30,probability=T,xlab='绩点区间',main='学生绩点分布',ylab='频率')
abline(v=2,col='red',lty=2,lwd=2)
lines(density(x$绩点,from=0,to=5),col='orange')

# 预警 标准
x$是否预警 <- ifelse(x$绩点>=2,'否','是')
table(x$是否预警)


## 预警与学年
ggplot(data=x,aes(x=学年,y=绩点,fill=学年))+geom_boxplot(na.rm = T,show.legend = F)

ggplot(data=x,aes(x=学年,fill=是否预警))+
      geom_bar(na.rm = T,show.legend = T,position = 'fill')+
      coord_polar(theta = "y") + 
      labs(x='',y='百分比',title='预警与学年')


## 预警与课程性质
ggplot(data=x,aes(x=课程性质,y=绩点,fill=课程性质))+geom_boxplot(na.rm = T,show.legend = F)

ggplot(data=x,aes(x=课程性质,fill=是否预警))+
      geom_bar(na.rm = T,show.legend = T,position = 'fill')+
      coord_polar(theta = "y") + 
      labs(x='',y='百分比',title='预警与课程性质')




