
#homework

##1.	背景介绍及说明
我们小组收集到了机械专业的71个同学的学业成绩，并且我们对学科之间成绩的关系非常感兴趣，尤其是毕业设计成绩和平时学科成绩之间的联系。我们小组将通过以下的数据分析来探索学生学科成绩之间的某种联系。
我们小组将使用R语言作为主要的分析工具，为了完成以下的分析，我们需要首先安装、加载以下几个算法包。
```r
library(xlsx)
library(aplpack)
library(arules)
library(arulesViz)
library(party)
 ```
 
##2.	数据预处理
###2.1	读取数据
我们使用 xlsx包中的read.xlsx2函数将Excel文件读取到R语言中，R语言代码如下：
```r
> x <- read.xlsx2(file = '201109.xls',sheetIndex = 1,colClasses= c(rep('character',9),rep('numeric',5) ,rep('character',3)),stringsAsFactors=F )
> str(x)
'data.frame':	5614 obs. of  17 variables:
 $ 学年度: chr  "2014-2015" "2014-2015" "2014-2015" "2014-2015" ...
 $ 学期: chr  "2" "2" "2" "2" ...
 $ 学号: chr  "20112758" "20112758" "20112760" "20112760" ...
 $ 姓名: chr  "南茜" "南茜" "袁怡欣" "袁怡欣" ...
 $ 班级: chr  "机械11171" "机械11171" "机械11171" "机械11171" ...
 $ 课程序: chr  "211703303" "211703602" "211703302" "211703602" ...
 $ 课程代码: chr  "2117033" "2117036" "2117033" "2117036" ...
 $ 课程名称: chr  "机械制造技术课程设计" "毕业设计"  ...
 $ 授课教师: chr  "王道累" "纪冬梅" "杨峰" "纪冬梅" ...
 $ 课程学分: num  2 16 2 16 2 16 2 16 2 16 ...
 $ 分数: num  84 84 84 94 84 64 84 74 74 74 ...
 $ 实得学分: num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
 $ 绩点: num  3 3 3 4 3 1 3 2 2 2 ...
 $ 课程类别代码: num  98 98 98 98 98 98 98 98 98 98 ...
 $ 课程类别: chr  "实践课程(必修)" "实践课程(必修)" ...
 $ 修读类别: chr  "正常" "正常" "正常" "正常" ...
 $ 学生类别: chr  "本科" "本科" "民族生" "民族生" ...
 ```
 
###2.2	创建二维表
为了方便后续的分析，我们需要将原始的事务数据转换为稀疏矩阵，因此创建姓名和课程的二维表，也就是课程的稀疏矩阵。代码如下：
```r
tx<- t(na.omit(with(x,tapply(绩点,list(课程名称,姓名),max))))
xx <- as.data.frame(tx,row.names = row.names(tx))
head(xx[,1:5])
C语言程序设计 毕业设计 材料成型技术基础 材料力学(1) 测试技术
蔡睿          1.5        4              2.0         1.0      1.5
陈冰吉        1.5        2              1.0         1.0      2.0
陈起          1.5        2              2.5         1.0      2.0
陈英超        4.0        3              2.5         1.5      2.0
陈忠喜        3.5        2              1.0         1.0      1.0
崔哲          2.5        2              1.0         1.0      1.0
```

##3.	关联分析
###3.1	数据预处理
由于原始数据的绩点为数值，我们需要将它转换为分类变量。转换之后容易统计出学生毕业设计成绩为B的共5位同学，占比7%，成绩为C的为29位，占比41%，成绩为D的共21位，占比34%，成绩为E的共13位u，占比18%。
```r
xxx=as.data.frame(apply(xx,2,function(x)return(cut(x,breaks=c(0,1,2,3,4,5),include.lowest=T,labels=c("E","D","C","B","A")))),row.names=row.names(xx))
table(xxx$毕业设计)
 B  C  D  E 
 5 29 24 13
> round(prop.table(table(xxx$毕业设计)),2)
  B    C    D    E 
0.07 0.41 0.34 0.18
```
###3.2	关联模型建模
为了简化研究的目的，我们将建立一个关联模型，探索毕业设计成绩和平时学科成绩的联系。代码如下：
```r
>br=apriori(data=xxx,parameter=list(supp=0.2,conf=0.6,target='rules'),appearance = list(rhs=c('毕业设计=B','毕业设计=C','毕业设计=D' ,'毕业设计=E') ,default='lhs'))
Apriori
Parameter specification:
confidence minval smax arem  aval originalSupport support minlen maxlen
0.6    0.1    1 none FALSE            TRUE     0.2      1     10
 target   ext
  rules FALSE
Algorithmic control:
filter tree heap memopt load sort verbose
0.1 TRUE TRUE  FALSE TRUE    2    TRUE
Absolute minimum support count: 14 
set item appearances ...[4 item(s)] done [0.00s].
set transactions ...[234 item(s), 71 transaction(s)] done [0.00s].
sorting and recoding items ... [148 item(s)] done [0.00s].
creating transaction tree ... done [0.00s].
checking subsets of size 1 2 3 4 5 done [0.00s].
writing ... [9 rule(s)] done [0.00s].
creating S4 object  ... done [0.00s].
> summary(br)
set of 9 rules
rule length distribution (lhs + rhs):sizes
2 3 
3 6 
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   3.000   2.667   3.000   3.000 
summary of quality measures:
    support         confidence          lift      
 Min.   :0.2113   Min.   :0.6000   Min.   :1.469  
 1st Qu.:0.2113   1st Qu.:0.6000   1st Qu.:1.669  
 Median :0.2113   Median :0.6296   Median :1.775  
 Mean   :0.2160   Mean   :0.6587   Mean   :1.716  
 3rd Qu.:0.2113   3rd Qu.:0.7143   3rd Qu.:1.781  
 Max.   :0.2394   Max.   :0.7500   Max.   :1.849  
mining info:
 data ntransactions support confidence
  xxx            71     0.2        0.6
> inspect(br)
 lhs         rhs    
1 {大学物理C(2)=B} => {毕业设计=C}
2 {电厂动力工程B=B} => {毕业设计=C}
3 {机械制造技术课程设计=D} => {毕业设计=D}
4 {大学物理C(2)=B,毛泽东思想和中国特色社会主义理论体系概论=B}  => {毕业设计=C}
5 {机械制造技术=E,形势与政策(3)=C} => {毕业设计=D}
6 {形势与政策(2)=D,形势与政策(3)=C} => {毕业设计=D}
7 {毛泽东思想和中国特色社会主义理论体系概论=B,形势与政策(2)=C} => {毕业设计=C}
8 {电力生产概论B=C,毛泽东思想和中国特色社会主义理论体系概论=B} => {毕业设计=C}
9 {毛泽东思想和中国特色社会主义理论体系概论=B,体育(2)=B} => {毕业设计=C}
  support   confidence lift    
1 0.2253521 0.7272727  1.780564
2 0.2112676 0.6818182  1.669279
3 0.2112676 0.6000000  1.775000
4 0.2112676 0.7500000  1.836207
5 0.2112676 0.6250000  1.848958
6 0.2112676 0.6000000  1.775000
7 0.2112676 0.7142857  1.748768
8 0.2112676 0.6000000  1.468966
9 0.2394366 0.6296296  1.541507
```
3.3	关联规则可视化
```r
> plot(br)
> plot(br, method="matrix", measure=c("lift", "confidence"))
Itemsets in Antecedent (LHS)
[1] "{大学物理C(2)=B}" 
[2] "{电厂动力工程B=B}" 
[3] "{机械制造技术课程设计=D}" 
[4] "{大学物理C(2)=B,毛泽东思想和中国特色社会主义理论体系概论=B}" 
[5] "{机械制造技术=E,形势与政策(3)=C}"                            
[6] "{形势与政策(2)=D,形势与政策(3)=C}"                           
[7] "{毛泽东思想和中国特色社会主义理论体系概论=B,形势与政策(2)=C}"
[8] "{电力生产概论B=C,毛泽东思想和中国特色社会主义理论体系概论=B}"
[9] "{毛泽东思想和中国特色社会主义理论体系概论=B,体育(2)=B}"      
Itemsets in Consequent (RHS)
[1] "{毕业设计=C}" "{毕业设计=D}"
> plot(br, method="grouped")
> plot(br, method="graph")
 
 ```
 
 
 
##4.	决策树
###4.1	数据处理
首先将数据随机抽取一部分作为训练样本，另外一部分作为测试样本。代码如下
```r
> set.seed(1)
> rd <- sample(x = 1:2,size = nrow(xxx),replace = T,prob = c(0.8,0.2))
> xxx.train <- xxx[rd==1,]
> xxx.test <- xxx[rd==2,]
```
###4.2	决策树建模
我们将建立一个决策树模型，探索毕业设计成绩和平时学科成绩的联系。代码如下：
```r
> myct <- ctree(formula = 毕业设计~.,data=xxx.train,controls = ctree_control(mincriterion = 0.9, minsplit = 20, minbucket = 4))
> myct
	 Conditional inference tree with 4 terminal nodes
Response:  毕业设计 
Inputs:  C语言程序设计, 材料成型技术基础, 材料力学(1), 测试技术, 大学物理C(1), 大学物理C(2), 大学英语(1), 大学英语(2), 大学英语(3), 大学英语(4), 电厂动力工程B, 电工电子技术(1), 电工电子技术(2), 电力生产概论B, 复变函数与积分变换B, 概率论与数理统计, 高等数学A(1), 高等数学A(2), 工程材料, 工程化学, 工程流体力学B, 公差与技术测量, 画法几何与机械制图(1), 画法几何与机械制图(2), 机电一体化原理, 机械设计, 机械设计课程设计, 机械原理, 机械原理课程设计, 机械制造工艺学, 机械制造技术, 机械制造技术课程设计, 机械专业英语, 计算机辅助设计与制造, 计算机应用基础, 金工实习(1), 金工实习(2), 控制工程基础, 理论力学A, 马克思主义基本原理, 毛泽东思想和中国特色社会主义理论体系概论, 认识实习, 数控技术, 思想道德修养与法律基础, 体育(1), 体育(2), 体育(3), 体育(4), 微机原理与接口技术, 物理实验(1), 物理实验(2), 线性代数B, 形势与政策(1), 形势与政策(2), 形势与政策(3), 液压传动, 制图测绘, 中国近现代史纲要, 专业实习 
Number of observations:  60 
1) 画法几何与机械制图(2) == {B, C, D}; criterion = 0.999, statistic = 39.423
  2) 概率论与数理统计 == {B, C, D}; criterion = 0.97, statistic = 29.626
    3) 工程材料 == {C}; criterion = 0.938, statistic = 27.668
      4)*  weights = 7 
    3) 工程材料 == {B, D, E}
      5)*  weights = 41 
  2) 概率论与数理统计 == {E}
    6)*  weights = 8 
1) 画法几何与机械制图(2) == {E}
  7)*  weights = 4 
> plot(myct)
 ```

###4.3	决策树预测
将建立好的模型进行预测，预测结果和预测代码如下，模型能够准确预测出4+2=6个，因此模型的准确率为6/11=54.5%，准确率并不是很高，因此模型还有待优化。
```r
> predict(myct,newdata=xxx.test)
 [1] C E E E C C C C C E E
Levels: B C D E
> table(predict(myct,newdata=xxx.test),xxx.test$毕业设计)
    B C D E
  B 0 0 0 0
  C 1 4 0 1
  D 0 0 0 0
  E 0 0 3 2
 ```
##5.	聚类
###5.1	K-means聚类
我们将根据学生的所有成绩将这些学生分为三类，代码如下：
```r
> mycl <- kmeans(x = xx,centers = 3)
> mycl$cluster
  蔡睿 陈冰吉   陈起 陈英超 陈忠喜   崔哲 范黎洁   高磊 高仕君 高原雷   何彪 
  1      2      2      3      2      2      1      3      3      2      3 
胡甘成 胡晓桦 华逸飞 季登辉 江震旭   姜毅 蒋霄铭 冷亚东   黎明 李玲颖 李天成 
   1      2      1      3      2      1      2      2      3      1      3 
  李伟   刘广 刘维勤 刘禺杜 陆一鸣   毛韡 孟志豪 闵剑峰   南茜 欧德飞 潘晓磊 
   3      3      2      2      2      1      2      3      3      3      3 
  庞龙   乔黛   阮帆 沈隽晟   孙昊   孙津 王惠雨 王君宇 王俊凯 王祉琛 文一舒 
   2      1      1      3      2      1      3      3      3      2      3 
  吴疆   吴晏 谢生源 熊琪睿 杨佳文 杨珮瑶 杨胜录 杨兴梓   叶飞   叶靖 余林根 
   2      1      1      1      2      1      3      1      1      1      1 
俞佳诚 袁怡欣 岳思行 詹榕晖 张华培   张鹏 张瑞强 张亚东 张志行 赵梦瀛 赵孙诗 
    1      3      1      1      2      2      1      2      2      2      2 
郑宝国   钟炎 朱嘉伟 朱良杰 朱志兵 
     3      2      3      2      2 
> mycl$centers[,1:5]
  C语言程序设计 毕业设计 材料成型技术基础 材料力学(1) 测试技术
1      1.795455 2.545455         1.772727    2.204545 2.159091
2      1.703704 1.777778         1.259259    1.611111 1.518519
3      2.681818 2.909091         2.636364    2.795455 2.954545
```
###5.2	K-means聚类可视化
```r
> plot(xx[,c(2,5)],col=mycl$cluster)
> points(mycl$centers[,c(2,5)],pch=8,col=1:3,cex=2)

``` 
###5.3	K-means聚类聚类结果分析
```r
> table(mycl$cluster)
 1  2  3 
22 27 22 
> table(mycl$cluster,xxx$毕业设计)
     B  C  D  E
  1  3  9  7  3
  2  0  3 15  9
  3  2 17  2  1
```



