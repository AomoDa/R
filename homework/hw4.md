---
title: "hw4"
author: "Your Name"
date: "2016年10月5日"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Q25


###解题思路：

1. 初始化设定x=0，时间T=0；

2. 利用sample抽样从{-1，1}抽样，每个数字被抽取的概率均为50%，抽取的数值记为a，同时时间T+1

3. 判断x+a是否小于10，如果小于10，则继续循环，当x+a等于10的时候，停止循环并输出时间T。

###结论

1. 将上诉过程模拟200次，产生200个时间T。其中最小的T为14，最大为649300。

2. 1st Qu 为80，中位数为263，3rd Qu为926.

3. 为了合适的展示boxplot，将纵坐标限制在0~3000之间。

```{r}
# 定义q25的模拟过程。求出当X第一次等于10的时间T
s25 <- function(){
     T <-0 # 初始化时间T
     x <- 0 # 初始化x
     while(x<10){ # 设定循环过程，如果X小于10，则继续循环，直到当X第一次等于10的停止循环。
     	a <- sample(x = c(1,-1),size = 1,prob = c(0.5,0.5)) 
        x <- x + a # 按照transition probabilities，求出下一步的X值
        T <- T +1 # 时间T递增+1
     }
return(T)
}
# Use a simulation to generate a few hundred values of T
set.seed(25)
T <- replicate(n = 200,expr = s25())
#the box plot and  a description(max, min, quartiles, median).
summary(T)
boxplot(T,ylim=c(0,3000))
```


##Q26

这里不知道是什么意思。。。。。你自己写点吧。。

1. 自己写。。

2. 自己写。。

##Q27

当JOE betting $1 on red numbers成功的时候，记为```{R=T}```,失败时为```{R=F}```。当 betting $1 on odd numbers成功的时候，记为```{O=T}```,失败时记为```{O=F}```

因此，All possible outcomes of a single game and their probabilities：
$$
\begin{aligned}
P\{R=T,O=T\} = \frac{10}{38}
\end{aligned}
$$

$$
\begin{aligned}
P\{R=T,O=F\} = \frac{8}{38}
\end{aligned}
$$
$$
\begin{aligned}
P\{R=F,O=T\}= \frac{8}{38}
\end{aligned}
$$
$$
\begin{aligned}
P\{R=F,O=F\}= \frac{12}{38}
\end{aligned}
$$
its expected value:

$$
\begin{aligned}
E(X) = \sum_{R,O \in(T,F)} P\{R,O\} \cdot X = \frac{10}{38} * 3 + \frac{8}{38} * 1 + \frac{8}{38} * 2  +  \frac{12}{38} * 0 \approx 1.421
\end{aligned}
$$

##Q28

<font color=red size=3>**这里感觉题目有问题或者就是我理解不对，1<=j,k<=n感觉不对，应该是1<=j,k<=n^2 ,我解释的也不太好，你参考就好了**</font>


NOT TRUE:

假设当n=3的时候，一共会有3*3=9tiles，按照如下标记每个tile。

```{r}
matrix(1:9,nrow = 3)
```

当k=2，j=1的时候分别有：
$$
\begin{aligned}
P(X_{i+1}=2 |X_i = 1) = \frac{1}{2}
\end{aligned} 
$$
$$
\begin{aligned}
P(X_{i+1}=1 |X_i = 2) = \frac{1}{3}
\end{aligned} 
$$

因此可以得出：
$$
\begin{aligned}
P(X_{i+1}=1 |X_i = 2)\neq P(X_{i+1}=2 |X_i = 1)
\end{aligned} 
$$


##Q29

the joint probability mass function (pmf) for X and Y :

$$
\begin{aligned}
P\{X=k,Y=j\} = \frac{1}{n} \cdot C^j_k \cdot p^j \cdot(1-p)^{k-j} \ \  j \leq k \leq n 
\end{aligned} 
$$


##Q30

###loading transition matrix

将transition matrix写入CSV文件，然后使用R语言读取文件，这样做是方便操作。

```{r}

tm <- as.matrix(read.csv(file = 'C://Users//AomoDa//Documents//q30.csv',header = F))
colnames(tm) <- 1:9
round(tm,3)
```


###a

结算结果可以由以下代码计算出：

$$
\begin{aligned}
P(X_3 = 3 | X_0 = 1) = 0.0625
\end{aligned}
$$

```{r}
init <- matrix(data = c(1,rep(0,8)),nrow = 1)
round(init %*%tm %*%tm %*%tm ,4)
```

###b


<font color=red size=3>**这里感觉也是理解有问题，循环了好几百万次，找不到合适的T，也就说说明存活的概率始终是大于50%的，不论在这里面呆多久时间。**</font>

要么就是转移矩阵不对，到底可不可以斜着走？

```{r}
# 代码写出来了，但是求不出T
# T <- 0
# while(st[,9]<=0.5 ) {
# if(T==0){st <- init %*% tm } else{st <- st %*% tm}
# T <- T +1
# }
```



##Q31

###a

$$
\begin{aligned}
P\{N=n,X=x\} = \frac{\lambda^n}{n!} e^{-\lambda} \cdot C^x_n \cdot p^x \cdot (1-p)^{n-x}
\end{aligned}
$$

###b


```{r}
s31 <- function(lamda,p,k=1){
   if(lamda>0 & p >0  & p <1){
        return(replicate(n=k ,expr = rbinom(n = 1,size=rpois(n = 1,lamda),prob = p)))
    }
}
s31(lamda = 10,p = 0.2,k = 10)
```

###c

模拟100次，每次产生100个随机数然后求得X的期望值，通过模拟不同的$\lambda$ 和p，猜想得出X的期望值为：

$$
\begin{aligned}
E(x) = \lambda \cdot p
\end{aligned}
$$

```{r}
### when lamda = 5,p = 0.7,k=100,重复100次实验
set.seed(31)
mean(replicate(n = 100,expr =mean(s31(lamda = 5,p = 0.7,k = 100)) ))

### when lamda = 0.8,p = 0.5,k=100，重复100次实验
set.seed(31)
mean(replicate(n = 100,expr =mean(s31(lamda = 0.8,p = 0.5,k = 100)) ))

### when lamda = 20,p = 0.5,k=100，重复100次实验
set.seed(31)
mean(replicate(n = 100,expr =mean(s31(lamda = 20,p = 0.5,k = 100)) ))

```



##Q32


```{r}
set.seed(32)
X <- rbinom(n = 10000,size = 100,prob = 0.2)
Y <- rbinom(n = 10000,size = 100,prob = 0.8)
Z <- X+Y
```


##a

using R simulations,得出的结果为：

$$
\begin{aligned}
P(X <10 | X <15)=0.02648172
\end{aligned}
$$
$$
\begin{aligned}
E(X|X<15)=12.87137
\end{aligned}
$$

```{r}
sum(X<15 & X < 10) / sum(X <15)
mean(X[X<15])
```


##b
```{r}
plot.ecdf(X[X<30 & X>10])
```


###c
```{r}
plot.ecdf(X[Z==100],col='red')
```


###d

using R simulations,得出的结果为：
$$
\begin{aligned}
E(Z| X =15)=95.28509
\end{aligned}
$$
$$
\begin{aligned}
E(Z| X =20)=99.8714
\end{aligned}
$$
$$
\begin{aligned}
E(Z| X =25)=104.9007
\end{aligned}
$$

```{r}
mean(Z[X==15]);mean(Z[X==20]);mean(Z[X==25])
```

###e

using R simulations,得出的结果为：
$$
\begin{aligned}
E(X| Z =90)=15.52229
\end{aligned}
$$
$$
\begin{aligned}
E(X| Z =95)=17.5511
\end{aligned}
$$
$$
\begin{aligned}
E(X| Z =100)=19.98074
\end{aligned}
$$

```{r}
mean(X[Z==90]);mean(X[Z==95]);mean(X[Z==100])
```


