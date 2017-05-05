---
title: "Untitled"
author: "Your Nmae"
date: "2017年5月5日"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Part 1 

##Q1

```{r, message=FALSE, warning=FALSE}
library(mlbench)
library(caret)
library(randomForest)
library(tree)
data('Vehicle')

#Part 1 

# 10-fold cross validation.
K <- 10
set.seed(2017)
ind <- sample(x = 1:K,
              size = nrow(Vehicle),
              replace = T)

## Q1
# building tree model
tree.accuracy <- c()
set.seed(2018)
for (i in 1:K) {
	# split data
	train.data <- Vehicle[ind!=i,]
	test.data <- Vehicle[ind==i,]
	# building tree model using train data set
	t2 <- tree(Class~.,data=train.data)
	# predict class using test data set 
	t2.pred <- predict(t2,
	                   newdata = test.data,
	                   type = 'class')
	# compute confusion matrix
	cf <- confusionMatrix(table(t2.pred,test.data$Class))
	# accuracy rate
    tree.accuracy[i] <-as.numeric(round(cf$overall,4)[1])
}

boxplot(tree.accuracy,
        horizontal = T,
	main='Tree model prediction accuracy \n with 10-fold cross validation ',xlab='accuracy rate ')
mean(tree.accuracy)
quantile(tree.accuracy,c(0.025,0.975))
```

##Q2

```{r}
# use PCA to find the principal components of 
# the matrix of predictors 
# scaling these predict
newdata <- cbind( as.data.frame(
                  predict(prcomp(Vehicle[,-19],
                                  scale. = TRUE))
                          ),
                  Vehicle[,19]
                  )
names(newdata)[19] <- 'Class'
mis.mt <- matrix(data = NA,nrow = 10,ncol = 18)

for (k_pc in 1:(ncol(newdata)-1)) {
	for (i in 1:K) {
		# split data
		train.data <- newdata[ind!=i,c(1:k_pc,19)]
		test.data <- newdata[ind==i,c(1:k_pc,19)]

		# building tree model using train data set
		t2 <- tree(Class~.,data=train.data)
		# predict class using test data set 
		t2.pred <- predict(t2,newdata = test.data,type = 'class')
		# compute confusion matrix
		cf <- confusionMatrix(table(t2.pred,test.data$Class))
		# misclass rate
	    misclass.rate <- 1-as.numeric(round(cf$overall,4)[1])
	    # print 
	    mis.mt[i,k_pc] <- misclass.rate
	    # debug
	    # print(paste0(Sys.time() ,'-----',k_pc,':',i))
	}
}

par(mfrow=c(1,2))
boxplot(mis.mt,xlab='k principal components',ylab='Misclassification Rate',
	main='Misclassification Rate Vs \n K principal components')
plot(colMeans(mis.mt),type='b',xlab='k principal components',ylab='Misclassification Rate',
	main='Misclassification Rate Vs \n K principal components',pch=16)
abline(v=12,col='red',lty=2,lwd=1)
par(mfrow=c(1,1))

which.min(colMeans(mis.mt))
# first 12 principal components to 
# predict the vehicle class with a tree

mean(mis.mt[,12])
#  95% confidence interval
quantile(mis.mt[,12],c(0.025,0.975))
boxplot(mis.mt[,12],
        horizontal = T,
	main='Tree model with PCA  misclass rate \n with 10-fold cross validation ',xlab='misclass rate ')
```

##Q3

```{r, message=FALSE, warning=FALSE}
rf.mis.mt <- matrix(data = NA,nrow = 10,ncol = 30)
set.seed(2017)

for (m in 2:30) {
	for (i in 1:K) {
		# split data
		train.data <- Vehicle[ind!=i,]
		test.data <- Vehicle[ind==i,]
		# building tree model using train data set
		rf1 <- randomForest(Class~.,data=train.data,
		                    maxnodes=m)
		# predict class using test data set 
		rf1.pred <- predict(rf1,newdata = test.data,type = 'class')
		# compute confusion matrix
		cf <- confusionMatrix(table(rf1.pred,test.data$Class))
		# misclass rate
	    misclass.rate <- 1-as.numeric(round(cf$overall,4)[1])
	    # print 
	    rf.mis.mt[i,m] <- misclass.rate
	    # debug
	    # print(paste0(Sys.time() ,'-----',m,':',i))
	}
}

par(mfrow=c(1,2))
boxplot(rf.mis.mt,xlab='maxnodes',ylab='Misclassification Rate',
	main='Misclassification Rate Vs maxnodes')
plot(1:30,colMeans(rf.mis.mt),type='b',xlab='maxnodes',ylab='Misclassification Rate',
	main='Misclassification Rate Vs \n maxnodes',pch=16)
abline(v=which.min(colMeans(rf.mis.mt)),col='red',lty=2,lwd=1)
par(mfrow=c(1,1))
# mean
mean(rf.mis.mt[,which.min(colMeans(rf.mis.mt))])
# 95 % confidence interval
quantile(rf.mis.mt[,which.min(colMeans(rf.mis.mt))],c(0.025,0.975))

boxplot(rf.mis.mt[,which.min(colMeans(rf.mis.mt))],
        horizontal = T,
	main='random Forest model misclass rate \n with 10-fold cross validation ',xlab='misclass rate ')

```

##Q4

```{r, message=FALSE, warning=FALSE}
library(psych)
library(MASS)
newdf <- subset(Vehicle,subset = Class %in% c('saab','opel'))
newdf$is_saab <- ifelse(newdf$Class=='saab',1,0)
newdf <- newdf[,-19]
pairs.panels(newdf[,-19])
# Determine highly correlated variables
high_cor <- findCorrelation(cor(newdf[,-19]))
#  highly correlated variables
names(newdf)[high_cor]
# Delete highly correlated variables
newdf <- newdf[,-high_cor]
# building a logistic regression model
glm1 <- glm(is_saab~.,data=newdf,family = binomial(link = "logit"))
# Choose a model by AIC in a Stepwise Algorithm
glm2 <- stepAIC(glm1,direction = 'backward')
library(effects)
plot(allEffects(glm2))

glm.mis <- c()
for (i in 1:K) {
	# split data
	train.data <- newdf[ind!=i,]
	test.data <- newdf[ind==i,]
	# building tree model using train data set
	glm0 <- glm(formula = is_saab ~ Comp + Rad.Ra + Pr.Axis.Ra + Max.L.Rect + Ra.Gyr, 
		family = binomial(link = "logit"), 
		data = newdf)
	# predict class using test data set 
	glm0.pred.prob <- predict(glm0,newdata = test.data,type = 'response')
    glm0.pred <- ifelse(glm0.pred.prob>0.5,1,0)
	# compute confusion matrix
	cf <- confusionMatrix(table(glm0.pred,test.data$is_saab))
	# misclass rate
    misclass.rate <- 1-as.numeric(round(cf$overall,4)[1])
    # print 
    glm.mis[i] <- misclass.rate
}

boxplot(glm.mis,
        horizontal = T,
	main='Logistic Regression model misclass rate \n with 10-fold cross validation ',xlab='misclass rate ')
mean(glm.mis)
quantile(glm.mis,c(0.025,0.975))
```

#Part 2

##Q5

```{r}
set.seed(2017)
plot(mlbench.smiley(n = 500,sd1 = 0.2,sd2 = 0.1))
```

##Q6

```{r}
set.seed(1)
mydf <- as.data.frame(mlbench.smiley(n = 500,sd1 = 0.1,sd2 = 0.3))
clu1 <- kmeans(mydf[,-3],centers = 4,nstart = 50)
h1 <- hclust(dist(mydf[,-3]),method = 'complete')
h1.clu <- cutree(h1,k = 4)

par(mfrow=c(1,2))
plot(NA,xlim=c(-2,2),ylim=c(-2,2),xlab='',ylab='',main='target class labels')
text(mydf$x.x4,mydf$x.V2,labels = mydf$classes,cex=0.7)
plot(NA,xlim=c(-2,2),ylim=c(-2,2),xlab='',ylab='',main='kmeans class labels')
text(mydf$x.x4,mydf$x.V2,labels = clu1$cluster,cex=0.7)
par(mfrow=c(1,1))


# compute which values of sd2 the clusters 
# coincide (more or less) with the target class labels
coincide <- function(x,y,print=FALSE) {
	stopifnot(length(x)==length(y))
	accuracy <- c()
	for (i in unique(x)) {
		v <- as.numeric(i)
		real <- as.numeric(names(table(y[x==v]))[which.max(table(y[x==v]))])
		if(print) cat('cluster ', real, '==', v, 'target class labels \n')
		accuracy <- c(accuracy,y[x==v]==real)
	}
	if(!print) return(sum(accuracy) / length(x))
}

# test 
coincide(x=mydf$classes,y=clu1$cluster,print = TRUE)



sd2 <- seq(from=0.05,to = 0.5,by = 0.05)
rt <- matrix(NA,nrow = 20,ncol = 10)
colnames(rt) <- as.character(sd2)
h.rt <- matrix(NA,nrow = 20,ncol = 10)
colnames(h.rt) <- as.character(sd2)

set.seed(2017)
for (i in 1:length(sd2)) {
	for (j in 1:20) {
		mydf <- as.data.frame(mlbench.smiley(n = 500,sd1 = 0.1,sd2 = sd2[i]))
		# kmeans
	    clu1 <- kmeans(mydf[,-3],centers = 4,nstart = 50)
	    #####################################################
	    # debug
	    # cat(i,'----',j,'\n')
	    # coincide(x=mydf$classes,y=clu1$cluster,print = TRUE)
	    # cat('-------------------------------\n')
	    #####################################################
        # hierarchical
	    h1 <- hclust(dist(mydf[,-3]),method = 'complete')
        h1.clu <- cutree(h1,k = 4)
	    rt[j,i] <- coincide(x=mydf$classes,y=clu1$cluster)
	    h.rt[j,i] <- coincide(x=mydf$classes,y=h1.clu)
	    }   
}

par(mfrow=c(1,2))
plot(sd2,colMeans(rt),type='b',xlab='sd2',ylab='coincide',main='concordance rate \n kmeans')
boxplot(rt,main='coincide with \n 20 simulations kmeans',xlab='sd2',ylab='coincide')
par(mfrow=c(1,1))

```

##Q7

```{r}
par(mfrow=c(1,2))
plot(NA,xlim=c(-2,2),ylim=c(-2,2),xlab='',ylab='',main='target class labels')
text(mydf$x.x4,mydf$x.V2,labels = mydf$classes,cex=0.7)
plot(NA,xlim=c(-2,2),ylim=c(-2,2),xlab='',ylab='',main='hierarchical class labels')
text(mydf$x.x4,mydf$x.V2,labels = h1.clu,cex=0.7)
par(mfrow=c(1,1))


par(mfrow=c(1,2))
plot(sd2,colMeans(h.rt),type='b',xlab='sd2',ylab='coincide',main='concordance rate \n hierarchical')
boxplot(h.rt,main='coincide with \n 20 simulations hierarchical',xlab='sd2',ylab='coincide')
par(mfrow=c(1,1))
```

