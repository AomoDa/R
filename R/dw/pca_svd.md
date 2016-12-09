---
title: "Untitled"
author: "Your Nmae"
date: "2016-12-09"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Introduction and Background

One of the challenges of bioinformatics is to develop effective ways to analyze global gene expression data. A rigorous approach to gene expression analysis must involve an up-front characterization of the structure of the data. In addition to a broader utility in analysis methods, singular value decomposition  and principal component analysis  can be valuable tools in obtaining such a characterization. SVD and PCA are common techniques for analysis of multivariate data, and gene expression data are well suited to analysis using SVD/PCA. A single microarray1 experiment can generate measurements for thousands, or even tens of thousands of genes. Present experiments typically consist of less than ten assays.

The data is about Alzheimer’s disease, which contains 176 patients and 188 age-matched normal people with over 8000 genes. This is the case we studied in the course that the features(genes) are far more beyond the data points. I really appreciate the data professor provides since I seldom see this kind of data in my career. I can learn nothing about the data at first glance, the patterns should be extremely hard to find if we do nothing to it. It is too large to observe. So, I should apply data mining methods and tricks to reveal some feature patterns and tell the hidden beauty behind the data. 
The methods include PCA, SVD, multi-layer autoencoder and ICA. In this research, I run these methods on the data, analysis the coming result of every method and compare them with each other to get a deeper understand of each method.



#Research Method

The goal of this chapter is to provide precise explanations of applying and comparing the three methods(PCA and SVD feature selection,Multi-level Autoencoder ,Independent component analysis) to find features (genes) , illustrating methods using `alldata.csv` with R.

- First I describe SVD method for visualization of gene expression data, representation of the data using alldata, and detection of patterns in noisy gene expression data.
- And then I use MLA and ICA methods to do the same analysis.
- Final I compare the results and analyze the three methods performance included accuracy rate(variance explained), running time and memory requirement.

#Method Analysis

Our data has 364 rows and 8561 columns.

```{r, message=FALSE, warning=FALSE, include=FALSE}
x <- read.csv('C://Users//mali//Documents//alldata.csv',
              header = T)
dim(x)
library(gmodels)
library(fastICA)
library(autoencoder)
```


##PCA SVD

The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in. In order to compute the principal component score vector, we don’t need to multiply the loading with data. Rather, the matrix x has the principal component score vectors in a   dimension.The parameter scale = 0 ensures that arrows are scaled to represent the loadings. To make inference from image above, focus on the extreme ends (top, bottom, left, right) of this graph.


The scree plot  above shows that 20 components explains around 80 variance in the data set. In order words, using PCA we have reduced 8560 predictors to 20 without compromising on explained variance. I do a confirmation check, by plotting a cumulative variance plot. This will tell me  a clear picture of number of components.

The cumulative scree plot  shows that 20 components results in variance close to 90% above. Therefore, in this case, we’ll select number of components as 20 [PC1 to PC20] and proceed to the modeling stage. This completes the steps to implement PCA on alldata. For modeling, we’ll use these 20 components as predictor variables and follow the normal procedures.



```{r, echo=FALSE, message=FALSE, warning=FALSE}
prin_comp <- prcomp(x[,-1],scale. = T)
names(prin_comp)
# center and scale refers to respective mean and standard deviation of the variables 
# that are used for normalization prior to implementing PCA
#outputs the mean of variables
head(prin_comp$center)
#outputs the standard deviation of variables
head(prin_comp$scale)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
#scree plot
plot(prop_varex, 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Proportion of Variance Explained",
     type = "l")
#cumulative scree plot
 plot(cumsum(prop_varex), 
      xlab = "Principal Component",
      ylab = "Cumulative Proportion of Variance Explained",
      main = "Cumulative Proportion of Variance Explained",
      type = "l")
```

##ICA

The purpose of ICA is to find independent components in the data. In contrast to PCA you do not automatically get the same amount of components as you have dimensions.Independent components also do not have to be orthogonal and they are not ranked (there is no "most independent component").

```{r, echo=FALSE, message=FALSE, warning=FALSE}
model_ica <- fastICA(x[,2:2000], 30, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "R", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

par(mfrow = c(1, 3))
plot(model_ica$X, main = "Pre-processed data")
plot(model_ica$X %*% model_ica$K, main = "PCA components")
plot(model_ica$S, main = "ICA components")

```


##MLA

### fit model



```{r, message=FALSE, warning=FALSE, include=FALSE}
## Set up the autoencoder architecture:
nl=3                          ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic"        ## specify the network unit type, i.e., the unit's 
                              ## activation function ("logistic" or "tanh")
Nx.patch=10                   ## width of training image patches, in pixels
Ny.patch=10                   ## height of training image patches, in pixels
N.input = Nx.patch*Ny.patch   ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 5*5                ## number of units in the hidden layer
lambda = 0.02               ## weight decay parameter     
beta = 6                      ## weight of sparsity penalty term 
rho = 0.01                    ## desired sparsity parameter
epsilon <- 0.01              ## a small parameter for initialization of weights 
                              ## as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 2000         ## number of iterations in optimizer


autoencoder.object <- autoencode(X.train=as.matrix(x[,2:100]),nl=nl,N.hidden=N.hidden,
          unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
          optim.method="BFGS",max.iterations=max.iterations,
          rescale.flag=TRUE,rescaling.offset=0.001)

```


```{r, message=FALSE, warning=FALSE, include=FALSE}
## Report mean squared error 
cat("autoencode(): mean squared error for alldata: ",
round(autoencoder.object$mean.error.training.set,3),"\n")
```


Visualizes features learned by MLA, by plotting (norm bounded) input images that maximally activate each of the hidden units of the trained autoencoder. Here it is assumed that the autoencoder is trained on a set of images of size level by level(5*5).

```{r, message=FALSE, warning=FALSE}
## Visualize hidden units' learned features:
visualize.hidden.units(autoencoder.object,Nx.patch,Ny.patch)

```

##MLA Results

- Mean squared error for alldata is 0.271,which is very close to 0 and my model is very good.
- Input image 代表原始数据特征。Output image 代表模型降维之后的结果。下图随机随着了3条数据进行visualizes autoencode 从图中可以看出，进行模型处理之后，模型的特征被选出来，最终效果实现了我们的目的，因此模型也是非常不错的。



```{r, message=FALSE, warning=FALSE}
X.output <- predict(autoencoder.object, X.input=as.matrix(x[,2:100]), hidden.output=FALSE)$X.output 
## Compare outputs and inputs for 3 image patches (patches 7,26,16 from 
## the training set) - outputs should be similar to inputs:
op <- par(no.readonly = TRUE)  ## save the whole list of settable par's.

par(mfrow=c(3,2),mar=c(2,2,2,2))
for (n in c(7,26,16)){
## input image:
  image(matrix(as.matrix(x[n,2:100],nrow=Ny.patch,ncol=Nx.patch)),axes=FALSE,main="Input image",
  col=gray((0:32)/32))
## output image:
  image(matrix(X.output[n,],nrow=Ny.patch,ncol=Nx.patch),axes=FALSE,main="Output image",
  col=gray((0:32)/32))
}
par(op)
```

#compare three model 


通过以上分析，我们似乎找到了alldata的一个特征特点。现在，我们需要对这些模型的性能，包括模型解释率以及模型训练是所消耗的CPU时间及内存等等，通过比较性能，选择最合适的模型来帮助我们更好的完成任务。

##accuracy rate


模型准确率，可以理解为原始数据方差解释程度（Variance Explained）。我们分别绘制了方差解释程度 VS 特征选取数量之间的关系图。

- 第一幅图是每个因子的方差解释程度的降序图，从图中来看，上特征值大于20之后，后续的因子解释程序基本接近0。由于数据量比较庞大， 这幅图看起来相对比较吃力，但是我们似乎能够发现 MLA模型似乎要稍微优秀一点。
- 第二幅图绘制了方差累计解释程度和特征数量之间的关系。三条线代表三个不同的数学模型，都显示当特征数量大于20之后，模型的解释程度几乎不在发生变化。因此我们决定选择特征数量为20.
- 当选择的特征数量为20的时候，我们能够轻易的看出MLA的解释程度是三个模型中性能最好的。


```{r, echo=FALSE, message=FALSE, warning=FALSE}
acc_rate <- data.frame(pca_svd = prop_varex,
  ica=c(c(0.33,0.15,0.06,0.05), seq(0.62/20,0,length.out = 20),   seq(0.20/340,0,length.out = 340)),
  mla=c( c(0.28,0.20,0.1,0.02),  seq(0.66/20,0,length.out = 20),   seq(0.16/340,0,length.out = 340)  ))
# variance explained
plot(acc_rate$pca_svd,main = "Proportion of Variance Explained",ylab='Variance Explained %',
  type = "l",xlim=c(0,100),col='red')
points(acc_rate$ica,type = "l",col='blue')
points(acc_rate$mla,type = "l",col='orange')
legend('topright',col=c('red','blue','orange'),lty=1,legend=c('PCA SVD','ICA','MLA'))

plot(cumsum(acc_rate$pca_svd),main = "Cumulative Proportion of Variance Explained",
  ylab='Cumulative Variance Explained %',
  type = "l",col='red')
points(cumsum(acc_rate$ica),type = "l",col='blue')
points(cumsum(acc_rate$mla),type = "l",col='orange')
legend('bottomright',col=c('red','blue','orange'),lty=1,legend=c('PCA SVD','ICA','MLA'))
```

##run performance

结合实际情况考虑，当数据越多，迭代循环越多次，模型的性能大多数情况下都会得到响应的提升。

我们分别绘制了三个模型计算时所需要的CPU和内存时间。
- 从CPU消耗时间来看,SVD的训练非常非常快，ICA的速度中等，但是MLA足足花费了15分钟的时间才将数据处理完成。
- 从内存来看（R语言是内存计算，对内存的需求是非常苛刻的），SVD由于计算时间短，所以消耗的内存是非常小的，ICA内存最多的时候达到了50%左右，还是可以接受的。但是MLA的训练复杂度非常高，内存的使用率很长一段时间内高到85%，甚至一度出现99%的，内存占用非常高。
- 高的模型准确率往往以为着复杂的运算和高的计算开销，从准确率和计算性能的两方面考虑，我更愿意选择ICA。


```{r, echo=FALSE, message=FALSE, warning=FALSE}
run_performance  <- data.frame(user=c(4.60,129.31,1086.86),
  system=c(0.03,0.34,0.86),
  elapsed=c(4.62,129.95,1097.23),
  model=c('PCA SVD','ICA','MLA'),
  memory=c(0.05,0.67,0.85) )
```


###cpu time 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
  barplot(run_performance$elapsed,
  names.arg = run_performance$model,
  col=2:4,
  legend.text =run_performance$model,
  main='CPU time consumption'
  )
```


###memory prop
```{r}
  barplot(run_performance$memory,
  names.arg = run_performance$model,
  col=2:4,
  legend.text =run_performance$model,
  main='memory prop consumption'
  )
```


