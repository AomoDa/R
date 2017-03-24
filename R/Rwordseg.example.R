library(rJava)
library(Rwordseg)
library (tm)
library(slam)

#1.读取数据
# allfdata<-read.csv('F:/BiaoZhu3.csv',header=T,sep=',')
# modelky1<-read.csv('F:/作业资料/实验/数据处理/modelkyword1.csv',header=T,sep=',')
# yucedata1<-read.csv("F:\\作业资料\\实验\\数据处理\\data1.csv",header=T,sep=",")

allfdata <- read.csv('biaozhu3.csv',header = T,sep = ',')
modelky1 <- read.csv('modelkyword1.csv',header = T,sep = ',')
yucedata1 <- read.csv('data1.csv',header = T,sep = ',')





#2.分词
segword<-function(unf){
pp<-as.character(unf$title)
#insertWords(kw)
rf<-segmentCN(pp,nature=TRUE)
rf
}

#3.提取名词与动词
nvword<-function(wf){
wf.n=wf
for(i in 1:length(wf)){
wf.n[[i]]<-wf[[i]][which(names(wf[[i]])=='n'|names(wf[[i]])=='v'|names(wf[[i]])=='nr'|names(wf[[i]])=='nz'|names(wf[[i]])=='a'|names(wf[[i]])=='ad'|names(wf[[i]])=='an')]
wf.n[[i]]<-wf.n[[i]][which(is.na(match(wf.n[[i]],c("是","有","可以"))))]
}
ovid<-Corpus(VectorSource(wf.n))
ovid
}



#4、建模数据准备
prepword<-function(modelky,ovid,unf){
  pdtm<-DocumentTermMatrix(ovid,list(wordLengths = c(2,Inf)))
  pbin<-weightBin(pdtm)
  pbintx<-as.matrix(pbin)
  pdoc<-as.integer(dimnames(pdtm)$Docs)
  pnb<-as.matrix(unf$score[pdoc])
  pmodeldata<-data.frame(pnb,pbintx)
  prtn<-list(pmodeldata,pdoc)
}
 

#数据处理

wf1<-segword(allfdata)
ovid1<-nvword(wf1)
prtn1<-prepword(modelky1,ovid1,allfdata)  #特征选择数据
mydata<-prtn1[[1]]

# 删除无效的变量
zerovar  <- nearZeroVar(mydata,freqCut = 200/5, uniqueCut = 30)
mydata <- mydata[,-zerovar]
mydata <- na.omit(mydata)

#训练 和 测试集
set.seed(1)
sp<-sample(c(0,1),size=length(allfdata[,1]),prob=c(0.7,0.3),replace=TRUE)
traindata <- na.omit(mydata[sp==0,])
testdata <- na.omit(mydata[sp==1,])


#7.建模与评估：SVM
library(kernlab)
sample_ksvm  <-  ksvm(as.factor(pnb) ~ ., data=traindata,type ="C-svc",kernel="rbfdot")



#训练结果
svmCl  <-  predict(sample_ksvm)
trainscore<-traindata$pnb
svmTable <-table(SVM=svmCl, sample=trainscore)
library(caret)
# 混淆矩阵结果
confusionMatrix(svmTable)



#测试结果
svmT <-  predict(sample_ksvm,testdata)
testscore<-testdata$pnb
svmTable_test <-table(SVM=svmT, sample=testscore)
#测试集合准确率，覆盖率
confusionMatrix(svmTable_test)




#预测数据预处理
wf1<-segword(yucedata1)
ovid1<-nvword(wf1)
prtn1<-prepword(modelky1,ovid1,yucedata1)  #特征选择数据
pred_data<-prtn1[[1]]
pred_data <- pred_data[,c(names(mydata))]

# 使用选择的SVM，进行预测，
svmres  <-  predict(sample_ksvm,pred_data)
#保存预测结果
yucedata1$score <- svmres


# 写出文件
write.csv(yucedata1,'final.csv')



