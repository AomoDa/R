#关联分析代码
```R
setwd("D:/R/table")  #设置工作目录
library(Matrix)  #加载Matrix包，因为此R版本为3.1.0，加载arules包时，需要先加载此包
library(arules)  #加载arules包

# 产生合适的交易流数据，以下四种数据格式，进行分析和转化
#------------第一种数据格式------------
a_list<-list(c("a","b","c"),c("a","b"),c("a","b","d"),c("c","e"),c("a","b","d","e")) #设置一个列表
names(a_list)<-paste("Tr",c(1:5),sep="")  #对列表进行赋予列名，其中用paste函数进行合并作为列名
trans<-as(a_list,"transactions") #将数据列表转化为,关联规则方法apriori可以处理的数据形式
inspect(trans) #查看数据列表
# 输出形式：
# items     transactionID
# 1 {a,b,c}   Tr1          
# 2 {a,b}     Tr2          
# 3 {a,b,d}   Tr3          
# 4 {c,e}     Tr4          
# 5 {a,b,d,e} Tr5 

#------------第二种数据格式------------
a_matrix<-matrix(c(1,1,1,0,0,1,1,0,0,0,1,1,0,1,0,0,0,1,0,1,1,1,0,1,1),ncol=5) #创建一个 5*5 的矩阵
# a_matrix 数据显示类型
#    Tr1 Tr2 Tr3 Tr4 Tr5
# a   1   1   1   0   1
# b   1   1   1   0   1
# c   1   0   0   1   0
# d   0   0   1   0   1
# e   0   0   0   1   1
dimnames(a_matrix)<-list(c("Tr1","Tr2","Tr3","Tr4","Tr5"),c("a","b","c","d","e")) #将一个列表中的数值，设置为矩阵的行名和列名
# dimnames(a_matrix)<-list(c("a","b","c","d","e"),paste("Tr",c(1:5),sep="")) #将一个列表中的数值，设置为矩阵的行名和列名
trans2<-as(a_matrix,"transactions") #将数据列表转化为,关联规则方法apriori可以处理的数据形式
inspect(trans2) #查看数据列表
#   items     transactionID
# 1 {a,b,c,e} Tr1          
# 2 {a,b,c,e} Tr2          
# 3 {a,d}     Tr3          
# 4 {c,e}     Tr4          
# 5 {d,e}     Tr5 

#------------第三种数据格式------------
a_df<-data.frame(age = as.factor(c(6,8,7,6,9,5)),grade = as.factor(c(1,3,1,1,4,1))) #创建一个2（age,grade）*6的数据块
trans3<-as(a_df, "transactions") #将数据列表转化为,关联规则方法apriori可以处理的数据形式
inspect(trans3) #查看数据列表
# items           transactionID
# 1 {age=6,grade=1} 1            
# 2 {age=8,grade=3} 2            
# 3 {age=7,grade=1} 3            
# 4 {age=6,grade=1} 4            
# 5 {age=9,grade=4} 5            
# 6 {age=5,grade=1} 6

#------------第四种数据格式------------
# LETTERS[1:5] #显示几个字母
a_df2<-sample(c(LETTERS[1:5], NA),10,TRUE)  #有缺失值
a_df2<-data.frame(X = a_df2, Y = sample(a_df2))  #转化为数据框
trans4<-as(a_df2, "transactions") #将数据列表转化为,关联规则方法apriori可以处理的数据形式
as(trans4, "data.frame")
inspect(trans4)

#—----处理-包含交易ID和交易物品-理------
a_df3<-data.frame(TID = c(1,1,2,2,2,3),item=c("a","b","a","b","c", "b")) # 字段里包含ID的数据源
trans5<-as(split(a_df3[,"item"], a_df3[,"TID"]),"transactions") #对数据源进行改正
# a<-split(a_df3[,"item"],a_df3[,"TID"])  #对数据进行重组和
inspect(trans5)

##### 实战案例 1 #####
# 使用第一种数据类型进行关联规则的频繁项集的分析：
frequentsets<-eclat(trans,parameter=list(support=0.5,maxlen=10)) #使用Arules包中的eclat算法进行查看频繁项集,默认支持度的0.05，置信度为10
inspect(frequentsets)    #察看求得的频繁项集
inspect(frequentsets[1:3])    #察看求得的频繁项集，显示如下：
#  items support
# 1 {a,b} 0.8    
# 2 {a}   0.8    
# 3 {b}   0.8 
inspect(sort(frequentsets[1:3])) #查看求得的频繁项集,排序查看（倒序）
x<-subset(rules,subset=rhs%in%”whole milk”&lift>=1.2) #求所需要的关联规则子集

##### 实战案例#####
#案例数据为Dataset.data （共2201条数据）
readLines("Dataset.data",n=5)  #查看一下数据,参数n为要查看的数据条数
titanic<-read.table("Dataset.data",header=F) #读取数据源，其中参数header为是否将第一行设定为表头，F为不设置，T为设置
names(titanic)<-c("Class","Age","Sex","Survived") # 通过names函数，给数据块设定一个自定义的表头 
rules.all<-apriori(titanic)  #使用Arules包中的apriori算法进行关联规则的挖掘计算，显示全部的关联规则
rules.all  # 查看计算之后的攻产生多少相关规则
inspect(rules.all) #查看求得的相关规则的具体相关信息，支持度、置信度、提升度

# apriori算法，自定义的一些参数，可显示一些自定义的项目集
# apriori算法中的，参数:data(数据源),control(控制算法),parameter(参数设置,如:长度、支持度、置信度),appearance(显示的形式,设置左右关联)
rules.all<-apriori(titanic,
                   control=list(verbose=F),
                   parameter=list(minlen=2,supp=0.005,conf=0.8),
                   appearance=list(rhs=c("Survived=no","Survived=yes"),default="lhs")) #default，设置显示左右全部的关联项
quality(rules.all)<-round(quality(rules.all),digits=3)  #只显示支持度、置信度、提升度,且四舍五入，保留三位小数
rules.all.sorted<-sort(rules.all,by="lift")  #对于产出的数据进行按照“lift”倒序排序
inspect(rules.all.sorted)  #显示排序之后的数据 

#消除冗余规则1,先找出那些是冗余规则
subset.matrix<-is.subset(rules.all.sorted,rules.all.sorted) #检查rules.all.sorted是否为其本身的子集
subset.matrix[lower.tri(subset.matrix, diag=T)]<-NA #返回一个逻辑矩阵，将下三角,包括对角线为NA
redundant<-colSums(subset.matrix,na.rm=T) >=1 # 对矩阵的列求和，na.rm=T,为排除NA，不参与计算
which(redundant) #确定出冗余规则的下标索引

#消除冗余规则2,删除那些是冗余规则
rules.all.pruned<-rules.all.sorted[!redundant] #排除列和数大于等于1 的数据元素
inspect(rules.all.pruned) #输出新的矩阵



```
