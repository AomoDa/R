library(caret)
library(plyr)
library(mice)


movie_metadata_url <- 'https://raw.githubusercontent.com/sundeepblue/movie_rating_prediction/master/movie_metadata.csv'
x <- read.csv(movie_metadata_url,
	header = T,
	stringsAsFactors = F,
	na.strings='')

#-----------------------------------
# 电影评分与导演、演员的关系，
# 谁是烂片之王？谁是高分之王？
# 谁的电影更受欢迎？
#-----------------------------------

director <- na.omit(x[x$director_name!='',c('director_name','imdb_score')])
director$tmp <- 1 

actor <- na.omit(rbind(data.frame(actor_name=x$actor_1_name,imdb_score=x$imdb_score,tmp=1),
	                   data.frame(actor_name=x$actor_2_name,imdb_score=x$imdb_score,tmp=1),
	                   data.frame(actor_name=x$actor_3_name,imdb_score=x$imdb_score,tmp=1)))

director_summ <- ddply(.data =director,
	.variables = .(director_name),
	.fun = summarise,
	avg_imdb_score=mean(imdb_score,na.rm = T),
	cnt=sum(tmp))
actor_summ <- ddply(.data =actor,
	.variables = .(actor_name),
	.fun = summarise,
	avg_imdb_score=mean(imdb_score,na.rm = T),
	cnt=sum(tmp))


# 导演平均分，【高分段、低分段】
director_summ <- director_summ[director_summ$cnt>=2,]
par(mfrow=c(1,2))
barplot(height=director_summ[order(-director_summ$avg_imdb_score),2][1:10],
	names.arg =director_summ[order(-director_summ$avg_imdb_score),1][1:10],
	las=2,
	horiz = F,
	cex.names = 0.7,
	col=rainbow(n=10,alpha = 0.7),
	main='TOP 10 \n High Score of Director',ylim=c(0,10))
barplot(height=director_summ[order(director_summ$avg_imdb_score),2][1:10],
	names.arg =director_summ[order(director_summ$avg_imdb_score),1][1:10],
	las=2,
	horiz = F,
	cex.names = 0.7,
	col=rainbow(n=10,alpha = 0.7),
	main='TOP 10 \n Low Score of Director',ylim=c(0,10))
par(mfrow=c(1,2))


# 演员平均分，【高分段、低分段】
actor_summ <- actor_summ[actor_summ$cnt>=2,]
par(mfrow=c(1,2))
barplot(height=actor_summ[order(-actor_summ$avg_imdb_score),2][1:10],
	names.arg =as.character(actor_summ[order(-actor_summ$avg_imdb_score),1][1:10]),
	las=2,
	horiz = F,
	cex.names = 0.7,
	col=rainbow(n=10,alpha = 0.7),
	main='TOP 10 \n High Score of Actor',
	ylim=c(0,10))
barplot(height=actor_summ[order(actor_summ$avg_imdb_score),2][1:10],
	names.arg =actor_summ[order(actor_summ$avg_imdb_score),1][1:10],
	las=2,
	horiz = F,
	cex.names = 0.7,
	col=rainbow(n=10,alpha = 0.7),
	main='TOP 10 \n Low Score of Actor',
	ylim=c(0,10))
par(mfrow=c(1,2))


#-----------------------------------
# 电影评分的影响因素有哪些？
#-----------------------------------

xx <- x[,-c(2,7,10:12,15,17,18)]
newx <- na.omit(xx)


# 数据单位修改，金额按百万
newx$gross <- round(newx$gross / 1e6,2)
newx$budget <- round(newx$budget / 1e6,2)



# 电影评分数据分布情况
par(mfrow=c(1,3))
hist(newx$imdb_score,
	probability = T,
	main='Histogram')
lines(density(newx$imdb_score),
	col='orange',
	lty=2,
	lwd=2)
boxplot(newx$imdb_score,
	main='BoxPlot')
qqPlot(newx$imdb_score,
	distribution = 'norm',
	main='Q-Q Plot')
par(mfrow=c(1,1))


#------------------------------------
# EDA
#------------------------------------

# year
# 与年份没有太大关系。
boxplot(imdb_score ~ title_year, 
	data=newx[newx$title_year >= 1970,], 
	col="bisque", 
	las=2,
	main='IMDB score vs movie year', lwd=0.5)w

#  country
# 与国家的关系也不是很大
reordered_country <- with(newx, reorder(country, -imdb_score, median))
boxplot(imdb_score ~ reordered_country, 
	data=newx, 
	lwd=0.5, 
	col="bisque", 
	las=2,
	main='IMDB score vs country')

# language
# 与语言的关系也不是很大
reordered_language  <- with(newx, reorder(language, -imdb_score, median))
boxplot(imdb_score ~ reordered_language, 
	data=newx,
	lwd=0.5,
	col="bisque",
	las=2,
	main='IMDB score vs language')

# content_rating
# 与电影级的关系也不是很大。
reordered_content_rating <- with(newx, reorder(content_rating, -imdb_score, median))
boxplot(imdb_score ~ reordered_content_rating, 
	data=newx,
	lwd=0.5, 
	col="bisque", 
	las=2,
	main='IMDB score vs content rating')



#------------------------------------
# Feature Selection
#------------------------------------

mydf <- newx[,-c(1,9,12:14)]
findCorrelation(cor(mydf))
nearZeroVar(mydf)

#-------------
# Backwards Feature Selection
#--------------

set.seed(2018)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   number=5,
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=model.matrix(imdb_score~.,data=mydf)[,-1],
	             y=mydf$imdb_score,
                 sizes = c(1:(ncol(mydf)-1)),
                 rfeControl = ctrl,
                 metric='Rsquared')
# Best Features
best_features <- lmProfile$optVariables


#---------
# Best Subsets Regression
#-----------

reg1 <- regsubsets(imdb_score~.,data=mydf,nvmax = ncol(mydf)-1)
reg1.summ <- summary(reg1)

par(mfrow=c(1,3))
plot(reg1.summ$cp,type='b',col='red',pch=16,main='CP')
abline(v=which.min(reg1.summ$cp),lty=2,col=gray(0.8),lwd=1)
abline(h=min(reg1.summ$cp),lty=2,col=gray(0.8),lwd=1)
plot(reg1.summ$bic,type='b',col='blue',pch=16,main='BIC')
abline(v=which.min(reg1.summ$bic),lty=2,col=gray(0.8),lwd=1)
abline(h=min(reg1.summ$bic),lty=2,col=gray(0.8),lwd=1)
plot(reg1.summ$adjr2,type='b',col='orange',pch=16,main='ADJR^2')
abline(v=which.max(reg1.summ$adjr2),lty=2,col=gray(0.8),lwd=1)
abline(h=max(reg1.summ$adjr2),lty=2,col=gray(0.8),lwd=1)
par(mfrow=c(1,1))

# Best Features
# the same as Backwards Feature Selection
best_variable <- names(coef(reg1,id=which.min(reg1.summ$bic)))[-1]

best_variable  %in% best_features



#------------------------------------
# Build Models
#------------------------------------

set.seed(1)
ind <- sample(x = 1:2,size = nrow(mydf),replace = T,prob = c(0.8,0.2))
train.data <- subset(mydf[ind==1,],select=c(best_variable,'imdb_score'))
test.data <- subset(mydf[ind==2,],select=c(best_variable,'imdb_score'))

