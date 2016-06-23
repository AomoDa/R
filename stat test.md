#统计软件实验
##第一题（基本统计分析，共30分）
针对基础包vcd中的Arthritis数据集，请完成如下问题：
（1）给出安装vcd包的R代码或操作步骤；并给出调用vcd包的命令；
（2）给出Arthritis数据集中的变量有哪些，并给出变量Age的描述统计分析代码和结果；
（3）给出变量Improved对应直方图和饼图所对应的R代码和实现结果；
（4）给出变量Treatment和变量Improved的二维列联表对应的R代码和实现结果；
（5）对变量Treatment和变量Improved进行卡方对立性检验，给出对应的R代码和实现结果的解释。
（6）计算变量Treatment和变量Improved的pearson相关系数并进行相关系数的限制性检验，给出给出对应的R代码和实现结果的解释。
###（1）给出安装vcd包的R代码或操作步骤；并给出调用vcd包的命令；
```r
install.packages("vcd")
library(vcd)
## Loading required package: grid
```
###（2）给出Arthritis数据集中的变量有哪些，并给出变量Age的描述统计分析代码和结果；
```r
str(Arthritis)
## 'data.frame':    84 obs. of  5 variables:
##  $ ID       : int  57 46 77 17 36 23 75 39 33 55 ...
##  $ Treatment: Factor w/ 2 levels "Placebo","Treated": 2 2 2 2 2 2 2 2 2 2 ...
##  $ Sex      : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 2 2 2 ...
##  $ Age      : int  27 29 30 32 46 58 59 59 63 63 ...
##  $ Improved : Ord.factor w/ 3 levels "None"<"Some"<..: 2 1 1 3 3 3 1 3 1 1 ...
summary(Arthritis)
##        ID          Treatment      Sex          Age          Improved 
##  Min.   : 1.00   Placebo:43   Female:59   Min.   :23.00   None  :42  
##  1st Qu.:21.75   Treated:41   Male  :25   1st Qu.:46.00   Some  :14  
##  Median :42.50                            Median :57.00   Marked:28  
##  Mean   :42.50                            Mean   :53.36              
##  3rd Qu.:63.25                            3rd Qu.:63.00              
##  Max.   :84.00                            Max.   :74.00
```
###（3）给出变量Improved对应直方图和饼图所对应的R代码和实现结果；
```r
barplot(table(Arthritis$Improved),main = 'Improved直方图')
 
pie(table(Arthritis$Improved),main = 'Improved饼图')
 ```
###（4）给出变量Treatment和变量Improved的二维列联表对应的R代码和实现结果；
```r
with(Arthritis,table(Treatment,Improved))
##          Improved
## Treatment None Some Marked
##   Placebo   29    7      7
##   Treated   13    7     21
```
###（5）对变量Treatment和变量Improved进行卡方对立性检验，给出对应的R代码和实现结果的解释。
可以使用以下两种方法进行卡方独立性检验。由于P=0.0015 <0.05,因此拒绝原假设，即Treatment和Improved之间不相互独立。
```r
chisq.test(with(Arthritis,table(Treatment,Improved)))
## 
##  Pearson's Chi-squared test
## 
## data:  with(Arthritis, table(Treatment, Improved))
## X-squared = 13.055, df = 2, p-value = 0.001463
summary(with(Arthritis,table(Treatment,Improved)))
## Number of cases in table: 84 
## Number of factors: 2 
## Test for independence of all factors:
##  Chisq = 13.055, df = 2, p-value = 0.001463
```
###（6）计算变量Treatment和变量Improved的pearson相关系数并进行相关系数的限制性检验，给出给出对应的R代码和实现结果的解释。
```r
#分类变量无法计算积差相关系数
#cor()和cor.test()只能计算数值型变量的相关系数和相关性检验。 
```
##第二题（回归分析，共30分）

下表是教育学家测试的21个儿童的记录，其中 是儿童的年龄（以月为单位）， 表示某种智力指标。

请完成以下问题（给出相应的R代码和实现结果）：
（1）输入数据 和 ；
（2）给出 和 的散点图；
（3）建立被解释变量 关于解释变量 的回归模型并解释（包括拟合程度、t检验、F检验和系数的经济含义解释）。
（4）利用QQ图给出正态性检验；
（5）当解释变量 为30时，给出被解释变量 的点预测值。

###（1）输入数据
```r
iq <- data.frame(x=c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,42,17,11,10),y=c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,57,121,86,100))
head(iq)
##    x   y
## 1 15  95
## 2 26  71
## 3 10  83
## 4  9  91
## 5 15 102
## 6 20  87
```
###（2）给出 和 的散点图；
```r
plot(iq,main='XY散点图')
 ```
###（3）建立被解释变量关于解释变量的回归模型并解释（包括拟合程度、t检验、F检验和系数的经济含义解释）。
由以下代码可以得出几个结论： 方程的截距、X的系数T检验的P值均小于0.05，系数全部显著； 方差整体的F检验，F值为13.2，自由度为1和19，p值小于0.05，p值显著，因此方程整体显著。 方程调整R方为0.3789，拟合效果并是很理想。 通过以上分析，可以确定，回归方程为 Y = 109.87-1.13*X
```r
fit1 <- lm(y~x,data=iq)
summary(fit1)
## 
## Call:
## lm(formula = y ~ x, data = iq)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.604  -8.731   1.396   4.523  30.285 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 109.8738     5.0678  21.681 7.31e-15 ***
## x            -1.1270     0.3102  -3.633  0.00177 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.02 on 19 degrees of freedom
## Multiple R-squared:   0.41,  Adjusted R-squared:  0.3789 
## F-statistic:  13.2 on 1 and 19 DF,  p-value: 0.001769
```
###（4）利用QQ图给出正态性检验；
通过QQ图，判断得出残差服从正态分布。
```r
library(car)
qqPlot(fit1)
 ```
###（5）当解释变量 为30时，给出被解释变量Y的点预测值。
由以下代码得出当X=30时候，Y为76.06，Y的95%置信区间为50.37到101.76之间。
```r
predict(fit1,newdata = data.frame(x=30),level = 0.95,interval = 'prediction')
##        fit      lwr      upr
## 1 76.06417 50.36507 101.7633
 ```
##第三题（方差分析，共20分）

小白鼠在接种了3种不同菌型的伤寒杆菌后的存活天数如下表：
菌型	存活日数
A	2	4	3	2	4	7	7	2	2	5	4
B	5	6	8	5	10	7	12	12	6	6	
C	7	11	6	6	7	9	5	5	10	6	3
请完成以下问题（给出相应的R代码和实现结果）：
（1）输入数据；
（2）试分析接种不同菌型的小白鼠的平均存活天数有无显著差异；
（3）基于多重比较，问那种菌型对应的存活天数最长。

###（1）输入数据
```r
survival <- rbind(data.frame(type=rep('A',11),time=c(2,4,3,2,4,7,7,2,2,5,4)),data.frame(type=rep('B',10),time=c(5,6,8,5,10,7,12,12,6,6)),data.frame(type=rep('C',11),time=c(7,11,6,6,7,9,5,5,10,6,3)))
head(survival)
##   type time
## 1    A    2
## 2    A    4
## 3    A    3
## 4    A    2
## 5    A    4
## 6    A    7
```
###（2）试分析接种不同菌型的小白鼠的平均存活天数有无显著差异
通过以下代码，P值小于0.05，因此拒绝原假设，即三种均值至少有一组有显著差异。
```r
#单因素方差分析
m1 <- aov(time~type,data=survival)
summary(m1)
##             Df Sum Sq Mean Sq F value  Pr(>F)   
## type         2   88.5   44.25   8.154 0.00155 **
## Residuals   29  157.4    5.43                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#分组计算均值
aggregate(time~type,data=survival,mean)
##   type     time
## 1    A 3.818182
## 2    B 7.700000
## 3    C 6.818182
```
###（3）基于多重比较，问那种菌型对应的存活天数最长。
从多重比较的结果可以得出： B组与C组差距不显著，即B组与C组近似，无法判断哪组存活天数更长。 B组与A组差距显著，A组明显大于B组。 C组与A组差距显著，C组明显大于A组。
```r
TukeyHSD(m1)
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = time ~ type, data = survival)
## 
## $type
##           diff        lwr      upr     p adj
## B-A  3.8818182  1.3681150 6.395521 0.0018589
## C-A  3.0000000  0.5468768 5.453123 0.0140430
## C-B -0.8818182 -3.3955214 1.631885 0.6653994
plot(TukeyHSD(m1))
 ```
 
##第四题（因子分析）（共20分）

对305个女中学生测量8个体型指标：身高（ ）、手臂长（ ）、上肢长（ ）下肢长（ ）、体重（ ）、颈围（ ）、胸围（ ）、胸宽（ ）。以下给出了8个指标的相关矩阵：
请完成以下问题（给出相应的R代码和实现结果）：
（1）输入数据；
（2）确定公共因子的个数；
（3）进行因子分析（解释因子载荷、解释公共因子的含义和计算因子得分等）。

###（1）输入数据；
将数据文件保存到本地，通过如下命令将原始数据导入到R中。
```r
body_cor <- read.csv('body_cov.csv',header = T,row.names =paste(rep('X',8),1:8,sep = '') )
body_cor
##       X1    X2    X3    X4    X5    X6    X7    X8
## X1 1.000 0.846 0.805 0.859 0.473 0.398 0.301 0.382
## X2 0.846 1.000 0.881 0.826 0.376 0.326 0.277 0.277
## X3 0.805 0.881 1.000 0.801 0.380 0.319 0.237 0.345
## X4 0.859 0.826 0.801 1.000 0.436 0.329 0.327 0.365
## X5 0.473 0.376 0.380 0.436 1.000 0.762 0.730 0.629
## X6 0.398 0.326 0.319 0.329 0.762 1.000 0.583 0.577
## X7 0.301 0.277 0.237 0.327 0.730 0.583 1.000 0.539
## X8 0.382 0.277 0.345 0.365 0.629 0.577 0.539 1.000
```
###（2）确定公共因子的个数；
根据特征值和平行碎石图可以得出公因子个数为2.
```r
library(psych)
fa.parallel(body_cor,n.obs = 305,fa='pc')
## Parallel analysis suggests that the number of factors =  NA  and the number of components =  2
 ```
###（3）进行因子分析
进行因子分析，使用最大方差旋转法。代码如下： 两个因子分别为pc1和pc2，对于自变量x1，2个公因子共解释88%的方差，对于自变量x2，两个因子共解释90%方差，以此类推等等。对于整体数据，pc1解释43%的方差，pc2解释37%的方差，两者累计解释81%方差。 根据因子得分系数，可以通过以下公式计算pc1和pc2，其中：
 pc1 = 0.27x1+0.3x2+0.3x3+0.28x4-0.06x5-x0.08x6-0.1x7-0.06x8
 pc2 = -0.04x1-0.09x2-0.08x3-0.05x4+0.33x5+0.32x6+0.33x7+0.29x8
 ```r
#进行因子分析
pc <- principal(body_cor,nfactors = 2,n.obs = 305,rotate = 'varimax')
pc
## Principal Components Analysis
## Call: principal(r = body_cor, nfactors = 2, rotate = "varimax", n.obs = 305)
## Standardized loadings (pattern matrix) based upon correlation matrix
##     PC1  PC2   h2    u2 com
## X1 0.90 0.27 0.88 0.120 1.2
## X2 0.93 0.17 0.90 0.097 1.1
## X3 0.92 0.18 0.87 0.129 1.1
## X4 0.90 0.24 0.86 0.137 1.1
## X5 0.25 0.89 0.85 0.151 1.2
## X6 0.18 0.84 0.74 0.264 1.1
## X7 0.11 0.84 0.71 0.289 1.0
## X8 0.20 0.77 0.63 0.370 1.1
## 
##                        PC1  PC2
## SS loadings           3.46 2.98
## Proportion Var        0.43 0.37
## Cumulative Var        0.43 0.81
## Proportion Explained  0.54 0.46
## Cumulative Proportion 0.54 1.00
## 
## Mean item complexity =  1.1
## Test of the hypothesis that 2 components are sufficient.
## 
## The root mean square of the residuals (RMSR) is  0.05 
##  with the empirical chi square  47.28  with prob <  8.7e-06 
## 
## Fit based upon off diagonal values = 0.99
#因子得分系数
round(pc$weights,2)
##      PC1   PC2
## X1  0.27 -0.04
## X2  0.30 -0.09
## X3  0.30 -0.08
## X4  0.28 -0.05
## X5 -0.06  0.33
## X6 -0.08  0.32
## X7 -0.10  0.33
## X8 -0.06  0.29
```
