library(psych)
library(car)
library(effects)

uk <- read.csv('ReferendumResults.csv',header = TRUE,stringsAsFactors = FALSE)
# the proportion of Leave votes
uk$LP <- ifelse(uk$Leave!=-1,uk$Leave/uk$NVotes,NA)

# Age 
uk$Age_young <- uk$Age_18to19+uk$Age_20to24+uk$Age_25to29
uk$Age_working <- uk$Age_30to44 + uk$Age_45to59 + uk$Age_60to64
uk$Age_retirement <- uk$Age_65to74+uk$Age_75to84+uk$Age_85to89+uk$Age_90plus

#social grades
uk$C2 <- uk$C2DE - uk$DE
uk$C1 <- uk$C1C2DE - uk$C2DE
uk$DE <- uk$DE
uk$Region <- as.factor(ifelse(uk$RegionName=='London','London','NO London'))
uk$Area <- as.factor(ifelse(uk$AreaType=='E09','E09','NO E09'))

# new data set
uk_new <- uk[,c(1:10,27:46,49:57)]


###########################################
##              PCA                      ##
###########################################

library(psych)
# Exclude ID,AreaType,RegionName,NVotes,Leave,Postals,LP
pca_data <- uk_new[,-c(1:6,32,38:39)]
names(pca_data)

#Scree plots 
fa.parallel(pca_data,fa = 'pc')
# Parallel analysis suggests that  the number of components =  5 

pc1 <- principal(pca_data,nfactors = 5,scores = T,rotate = 'varimax')

#SS loadings
pc1$loadings

# scores
pca_final_data <- as.data.frame(pc1$scores)
data_ok <- cbind(uk_new[,c(1:6,32,38:39)],pca_final_data)

names(data_ok)


###########################################
##              DATA SPLIT               ##
###########################################

train.data <- data_ok[!is.na(data_ok$LP),]
pred.data <- data_ok[is.na(data_ok$LP),]
set.seed(2017)
row.index <- sample(x = 1:nrow(train.data),size = 700)
model.data <- train.data[row.index,]
test.data <- train.data[-row.index,]


#EDA
par(mfrow=c(2,2))
boxplot(train.data$LP,col=gray(0.8),main='BoxPlot of LP',xlab='',ylab='LP')
hist(train.data$LP,probability = T,main='Histogram of LP',xlab='')
lines(density(train.data$LP),lty=2,col='orange',lwd=2)
qqPlot(train.data$LP,main='QQPlot of LP',xlab='',ylab='LP')
plot(train.data$LP,ylim=c(0,1),main='Outline Plot of LP',xlab='',ylab='LP')
abline(h = mean(train.data$LP),lty=2,lwd=2,col='red')
abline(h = mean(train.data$LP)+ 3*sd(train.data$LP),lty=2,lwd=2,col='blue')
abline(h = mean(train.data$LP)- 3*sd(train.data$LP),lty=2,lwd=2,col='blue')
par(mfrow=c(1,1))


# drop Postals
bartlett.test(LP~Postals,data = train.data)
t.test(LP~Postals,data = train.data)

# 
bartlett.test(LP~AreaType,data = train.data)
bartlett.test(LP~RegionName,data = train.data)
summary(aov(LP~RegionName,data = train.data))
summary(aov(LP~AreaType,data = train.data))
TukeyHSD(aov(LP~RegionName,data = train.data))
TukeyHSD(aov(LP~AreaType,data = train.data))

pairwise.t.test(x = train.data$LP,g=train.data$RegionName)
pairwise.t.test(x = train.data$LP,g=train.data$AreaType)


#----
#ggplot(data=train.data,aes(x=RC1,y=LP,group=Region))+geom_point()+geom_smooth(se=F,aes(col=Region))
#ggplot(data=train.data,aes(x=RC2,y=LP,group=Region))+geom_point()+geom_smooth(se=F,aes(col=Region))
#ggplot(data=train.data,aes(x=RC3,y=LP,group=Region))+geom_point()+geom_smooth(se=F,aes(col=Region))
#ggplot(data=train.data,aes(x=RC4,y=LP,group=Region))+geom_point()+geom_smooth(se=F,aes(col=Region))
#ggplot(data=train.data,aes(x=RC5,y=LP,group=Region))+geom_point()+geom_smooth(se=F,aes(col=Region))
#----

###########################################
##              LM                       ##
###########################################

lm1 <- lm(LP~RegionName+AreaType+RC1+RC2+RC3+RC4+RC5,data=model.data)
lm2 <- lm(LP~Region*(RC1+RC2+RC3+RC4+RC5),data=model.data)
step(lm2)
lm3 <- lm(formula = LP ~ Region + RC1 + RC2 + RC3 + RC4 + RC5 + Region:RC1 + 
              Region:RC2 + Region:RC3 + Region:RC4, data = model.data)

lm4 <- lm(formula = LP ~ Region + RC1 + RC2 + RC3  + RC5 + Region:RC1 + 
                Region:RC3 + Region:RC4, data = model.data)

lm4 <- lm(formula = LP ~ Region + RC1 + RC2 + RC3  + RC5 + Region:RC1 + 
                Region:RC3 + Region:RC4, data = model.data)

anova(lm1,lm2,lm3,lm4)

# choose lm4

plot(allEffects(lm4))

# Durbin-Watson Test
dwtest(lm4)



###########################################
##              GLM                      ##
###########################################

glm1 <- glm(cbind(Leave,NVotes-Leave)~Region + RC1 + RC2 + RC3  + RC5 + Region:RC1 + Region:RC3 + Region:RC4,
	data=model.data,family = quasibinomial(link='logit'))


plot(allEffects(glm1))


s <- function(model,data){
	dispersion <- summary(model)$dispersion
	pred <- predict(model,newdata = data,se.fit = TRUE,type = 'response')
	p <- pred$fit
	if(is.null(dispersion)){
      sigma <- sqrt(pred$se.fit^2 + pred$residual.scale^2)} else{
		sigma <- sqrt((pred$se.fit^2 + dispersion* p* (1-p)/data$NVotes))
	}
    s_value <- sum( log(sigma) + (data$LP-pred$fit)^2 / (2*sigma^2) )
    return(s_value)
}

s(model = lm4,data = model.data)
s(model = lm4,data = test.data)
s(model = glm1,data = model.data)
s(model = glm1,data = test.data)


cv.s <- function(data,k=5,seeds=2017){
    set.seed(seeds)
    ind <- sample(x=k,size = nrow(data),replace = T)
    ans <- data.frame(lm.train.s=numeric(k),lm.test.s=numeric(k),
    	              glm.train.s=numeric(k),glm.test.s=numeric(k)) 
    for (i in 1:k) {
        a <- data[ind!=i,]
        b <- data[ind==i,]
        lm.test <- lm(formula = LP ~ Region + RC1 + RC2 + RC3  + RC5 + Region:RC1 + 
                                Region:RC3 + Region:RC4, data = a)
        glm.test <- glm(cbind(Leave,NVotes-Leave)~Region + RC1 + RC2 + RC3  + RC5 +
                        Region:RC1 + Region:RC3 + Region:RC4,
	                    data=a,family = quasibinomial())
        ans[i,1] <- s(lm4,a)
        ans[i,2] <- s(lm4,b)
        ans[i,3] <- s(glm1,a)
        ans[i,4] <- s(glm1,b)
    }
 return(ans)
}

cv.s(data = train.data,k = 5,seeds = 100)


