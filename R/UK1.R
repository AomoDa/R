

library(psych)

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
uk$Region <- ifelse(uk$RegionName=='London','London','NO London')
uk$Area <- ifelse(uk$AreaType=='E09','E09','NO E09')

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
lines(density(train.data$LP),lty=2,col='orange')
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


lm1 <- lm(LP~AreaType+RegionName+RC1+RC2+RC3+RC4+RC5,data=model.data)

lm2 <- lm(LP~RegionName+RC1+RC2+RC3+RC4+RC5,data=model.data)


lm3 <- lm(LP~Area+Region+RC1+RC2+RC3+RC4+RC5,data=model.data)
