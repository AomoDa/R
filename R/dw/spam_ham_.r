setwd("/Volumes/CHRISTY/530") #set work directory
rm(list = ls()) #清除右侧global environment里之前残留的数据集
if(require(tm) == FALSE) {  #install text mining package
  install.packages("tm")  
  library(tm)  
}  


spam.path <- "./spam/"             #垃圾邮件1  
ham.path <- "./ham/"      #容易与垃圾邮件区分的非垃圾邮件1  

###### function:get.msg ###########################  
# target:邮件中抽取正文  
# method:每个邮件正文和头部信息是通过一个空白行分割  
# arguments:character 邮件文件路径  
# return:character 邮件正文内容  
get.msg <- function(path) {  
  con <- file(path, open = "rt", encoding = "latin1")  
  texts <- readLines(con)  
  breakLineNum <- which(texts == "")[1]  
  
  if( is.na(breakLineNum) || is.nan(breakLineNum) || is.infinite(breakLineNum)) {  
    cat(paste(path, breakLineNum,  "", sep=" "))  
    breakLineNum <- 0  
  }    
  msg <- texts[seq(breakLineNum, length(texts), 1)]  
  close(con)  
  return(paste(msg, collapse = "\n"))  
}  
###################################################  
spam.docs <- dir(spam.path) #获取所有的垃圾邮件  
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep=""))) #处理所有的邮件，提取正文部分的内容  
ham.docs <- dir(ham.path)  
all.ham <- sapply(ham.docs, function(p) get.msg(paste(ham.path, p, sep="")))  

#####Check empty file####
####Get ride some garage files such as HTML ####
###如果文件里面是字符长度小于5 判定为空，
#里面同时包含</body>等字符则判定为html

#然后将处理好的文件输入新的data文件夹里面
dir.create("./data")
k=0
for(text in spam.docs){
  te=get.msg(paste(spam.path, text, sep=""))
  if(nchar(te)>5&length(grep("</body>",te))<1)
  {
    write.table(te,file=paste("./data/spam",text,sep=""))
    k=k+1
  }
}
kh=0
for(text in ham.docs){
  te=get.msg(paste(ham.path, text, sep=""))
  if(nchar(te)>5&length(grep("</body>",te))<1)
  {
    write.table(te,file=paste("./data/ham",text,sep=""))
    kh=kh+1
  }
}
############################################################
###3  7  分，实验集和训练集
set.seed(100)
x=sample(1:(k+kh),round(0.7*(k+kh)))

all.docs <- dir("./data") #获取所有的垃圾邮件  
train_ham=all.docs[x[x<=kh]]
train_spam=all.docs[x[x>kh]]
train_ham.text <- sapply(train_ham, function(p) get.msg(paste("./data/", p, sep="")))  
train_spam.text <- sapply(train_spam, function(p) get.msg(paste("./data/", p, sep="")))  


############# funciont get.tdm  ###################  
# target: 获取词频矩阵  
# method: 通过tm包中携带的函数获取一系列文本的词频矩阵  
# arguments: vector 文本数组  
# return: matrix 词频数组  
get.tdm <- function(doc.vec) {  
  doc.corpus <- Corpus(VectorSource(doc.vec))  
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, minDocFreq = 2)  
  doc.tdm <- TermDocumentMatrix(doc.corpus, control)  
  return(doc.tdm)  
}  


###################################################  
tdm <- get.tdm(train_ham.text)  
spam.matrix <- as.matrix(tdm)       #词文本矩阵，即这个词在某个文本中出现的次数 稀疏矩阵  
spam.counts <- rowSums(spam.matrix) #每一行和，即单词在所有文本中出现的总次数  
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors = FALSE)  
names(spam.df) <- c('term','frequency')  
spam.df$frequency <- as.numeric(spam.df$frequency)  
spam.occurrence <- sapply(1:nrow(spam.matrix),  
                          function(i) {length(which(spam.matrix[i,] > 0)) / ncol(spam.matrix)})  
spam.density <- spam.df$frequency/sum(spam.df$frequency)  
spam.df <- transform(spam.df, density=spam.density,  
                     occurrence=spam.occurrence)  


tdm <- get.tdm(train_spam.text)  
ham.matrix <- as.matrix(tdm)  
ham.counts <- rowSums(ham.matrix)  
ham.df <- data.frame(cbind(names(ham.counts), as.numeric(ham.counts)), stringsAsFactors = FALSE)  
names(ham.df) <- c('term','frequency')  
ham.df$frequency <- as.numeric(ham.df$frequency)  
ham.df$density <- ham.df$frequency/sum(ham.df$frequency)  

ham.occurrence <- sapply(1:nrow(ham.matrix), function(i) {length(which(ham.matrix[i,] > 0)) / ncol(spam.matrix)})  
ham.df$occurrence <- ham.occurrence


head(spam.df[with(spam.df, order(-occurrence)), ])  

head(ham.df[with(ham.df, order(-occurrence)), ])  


###分类函数###  
classify.email <- function(path, traning.df, prior=0.5, c=1e-6) {  
  msg <- get.msg(path)  
  msg.tdm <- get.tdm(msg)  
  msg.freq <- rowSums(as.matrix(msg.tdm))  
  msg.match <- intersect(names(msg.freq), traning.df$term)  
  
  if (length(msg.match) < 1) {  
    #如果一个相同的单词都没有出现，那么每个单词的概率就以默认概率的乘积  
    return(prior*c^(length(msg.freq)))  
  } else {  
    #否则出现的以出现的概率计算，没有出现的以默认概率计算，最后求乘积  
    match.probs <- traning.df$occurrence[match(msg.match, traning.df$term)]  
    return(prior * prod(match.probs) * c^(length(msg.freq) - length(msg.match)))  
  }  
  
}  



###########预测###############################
y=1:(k+kh)

test=all.docs[!y%in%x]


spamprob <- sapply(test, function(p) {classify.email(paste("./data/", p, sep=""), spam.df)})  
hamprob <- sapply(test, function(p) {classify.email(paste("./data/", p, sep=""), ham.df)})  

hardham.class <- ifelse(spamprob <=hamprob, 'spam', 'ham')  

pre=table(hardham.class,c(rep("ham",64),rep("spam",78)))


##%ham
pre[1,1]/sum(pre[,1])
####spam
pre[2,2]/sum(pre[,2])
