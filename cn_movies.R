library(ggplot2)
library(car)
library(psych)
library(forecast)
library(MASS)
library(gplots)

# Part 1 描述性数据分析
# 下载数据
load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))
# 提取我们关系的数据指标部分
movies <- as.data.frame(movies[,c(3,5,7,13:18)],row.names = movies$title) 
# 查看数据结构
str(movies)
summary(movies)



# 2
ggplot(data=movies,aes(x=thtr_rel_year,fill=as.factor(genre)))+geom_bar(position='stack',na.rm = FALSE)+
      labs(x='year',y='movies num',title='年度电影数量')+
      guides(fill = guide_legend(title = "movie genre", title.position = "top"))

ggplot(data=movies,aes(x=thtr_rel_year,fill=as.factor(genre)))+geom_density(position='stack',na.rm = FALSE,alpha=0.7)+
      labs(x='year',y='movies num precent',title='年度电影占比趋势分布')+
      guides(fill = guide_legend(title = "movie genre", title.position = "top"))+facet_wrap(~genre,ncol=4)

# years change in the mean of different type movies audience_score
# From the years change in the number of different type movies plot, 
# we can clearly find that TV movies are extremely unstable and the mean of Documentary is more than Feature film every year. 
xx<-aggregate(audience_score~genre,data=movies,FUN = mean)
xx<-xx[order(-xx[,2]),]
head(xx,3)
ggplot(data=xx,aes(x=genre,weight=audience_score,group=genre,fill=genre))+geom_bar()+
       guides(fill = guide_legend(title = "movie type", title.position = "top"))+
       labs(x='year',y='movies num',title='不同电影平均评分比较')


# Part 2 

shapiro.test(movies$audience_score)
hist(movies$audience_score,probability = T,xlab = 'score')
lines(density(movies$audience_score),col='red')


# Q2:
# T test
# null hypothesis :   mean is equal
# alternative hypothesis: not equal
# The p value is 0, so the movies nominated for a best picture Oscar will get higher score.

t.test(audience_score~audience_rating,data=movies)
aggregate(audience_score~audience_rating,data=movies,mean)

# Q3 :
# is the mean audience_score of different critics_rating is equal ?
# one-way ANOVA
# null hypothesis :  the mean audience_score of different critics_rating is equal
# alternative hypothesis: not equal
# The p value is smaller than 0. So we can consider that the mean audience_score of different critics_rating is not equal. 
# By aggregate function we can find that the audience_score of  Certified Fresh  critics_rating will get higher score than Fresh and Rotten. 

summary(aov(audience_score~critics_rating,data=movies))
aggregate(audience_score~critics_rating,data=movies,FUN = mean)
plotmeans(audience_score~critics_rating,data=movies)


# Q4:
qplot(data=movies,x=mpaa_rating,y=audience_score,geom='boxplot',fill=mpaa_rating)
summary(aov(audience_score~mpaa_rating,data=movies))
aggregate(audience_score~mpaa_rating,data=movies,FUN = mean)

# Q5:
# spearman correlation coefficient and correlation diagram
corr.test(movies[c('audience_score','imdb_rating','imdb_num_votes','critics_score')],use = 'complete',method='spearman')
pairs.panels(movies[c('audience_score','imdb_rating','imdb_num_votes','critics_score')],method = 'spearman')

# Part 3 Modeling
#  In this part, we will find the best multiple linear regression model to explain audience movie scores. 
# We used the data before 2015 to fit model, and use the data in 2015 to verify our prediction.

movies_train<- movies[movies$thtr_rel_year!=2014 ,-3]

movies_test <- movies[movies$thtr_rel_year==2014 & (!is.na(movies$thtr_rel_year==2014)),-3]


lm1<-lm(formula=audience_score~.,data=movies_train ,na.action = na.omit)
stepAIC(object = lm1,direction = 'backward')
lm2<- lm(formula = audience_score ~ audience_rating + critics_score + 
    imdb_rating + genre, data = movies_train, na.action = na.omit)
summary(lm2)
lm3<- lm(formula = audience_score ~ audience_rating  + 
    imdb_rating + genre, data = movies_train, na.action = na.omit)
summary(lm3)
anova(lm3,lm2,lm1)

# F test

# Linear regression composite hypothesis test
par(mfrow=c(2,2))
plot(lm3)
par(mfrow=c(1,1))

crPlots(lm3)


# Part 4 Prediction

# the prediction with 95% confidence interval of 2015 movies audience_score
lm.p<-forecast.lm(object = lm3,newdata = movies_test,level = 0.95)

plot(movies_test$audience_score,type='b',ylim=c(10,90),xlab=NA,ylab='电影评分',main='2014年电影评分预测值和实际数值比较')
points(lm.p$mean,col='red',type='b')
lines(lm.p$lower,col='blue',lty=8)
lines(lm.p$upper,col='blue',lty=8)
legend('bottomleft',legend=c('实际值','预测值','95%预测区间') ,col=c('black','red','blue'),lty=c(1,1,5))   

# END



