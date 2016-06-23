#关联分析代码
这是别人的代码，我拿来只是看看格式怎么写。。。。

```R

library(Matrix)  #加载Matrix包，因为此R版本为3.1.0，加载arules包时，需要先加载此包
library(arules)  #加载arules包
```
实战案例
```r
readLines("Dataset.data",n=5)  #查看一下数据,参数n为要查看的数据条数
titanic<-read.table("Dataset.data",header=F) #读取数据源，其中参数header为是否将第一行设定为表头，F为不设置，T为设置
names(titanic)<-c("Class","Age","Sex","Survived") # 通过names函数，给数据块设定一个自定义的表头 
rules.all<-apriori(titanic)  #使用Arules包中的apriori算法进行关联规则的挖掘计算，显示全部的关联规则
rules.all  # 查看计算之后的攻产生多少相关规则
inspect(rules.all) #查看求得的相关规则的具体相关信息，支持度、置信度、提升度
```

apriori算法，自定义的一些参数，可显示一些自定义的项目集
apriori算法中的，参数:data(数据源),control(控制算法),parameter(参数设置,如:长度、支持度、置信度),appearance(显示的形式,设置左右关联)
```r
rules.all<-apriori(titanic,
                   control=list(verbose=F),
                   parameter=list(minlen=2,supp=0.005,conf=0.8),
                   appearance=list(rhs=c("Survived=no","Survived=yes"),default="lhs")) 
#default，设置显示左右全部的关联项
quality(rules.all)<-round(quality(rules.all),digits=3)  #只显示支持度、置信度、提升度,且四舍五入，保留三位小数
rules.all.sorted<-sort(rules.all,by="lift")  #对于产出的数据进行按照“lift”倒序排序
inspect(rules.all.sorted)  #显示排序之后的数据 
```

消除冗余规则,先找出那些是冗余规则
```r
rules.all.pruned<-rules.all.sorted[!redundant] #排除列和数大于等于1 的数据元素
inspect(rules.all.pruned) #输出新的矩阵

```
