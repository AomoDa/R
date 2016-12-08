


主成分（Principal components）： 
stats包的prcomp()（基于svd()）和princomp()（基于eigen()）能计算主成分。sca包做单分量分析。
nFactors可评价碎石图(Scree plot)，paran包可评估主成分分析得到的主成分和因子分析得到的因子。
pcurve包做主曲线（Principal Curve）分析和可视化。
gmodels包提供适合大矩阵的fast.prcomp()和fast.svd()。
kernlab包里的kpca()用核方法做非线性的主成分分析。
pcaPP包用投影寻踪（projection pursuit）法计算稳健/鲁棒（robust）主成分。
amap包的acpgen()和acprob()函数分别针对广义（generalized）和稳健（robust）主成分分析。
主成分在很多方面也有相应的应用，如：涉及生态的ade4包，感官的SensoMinR包。
psy包里有用于心理学的各种程序，与主成分相关的有：sphpca()用球形直观表示相关矩阵，类似于3D的PCA；
fpca()图形展示主成分分析的结果，而且允许某些变量间有相关性；
scree.plot()图形展示相关或协方差矩阵的特征值。
PTAk包做主张量分析（Principal Tensor Analysis）。
smatr包提供关于异速生长（allometry）的函数。 



独立成分（Independent Components）： 

fastICA包用fastICA算法做独立成分分析（ICA）和投影寻踪分析（Projection Pursuit），mlica包提供独立成分分析的最大似然拟合，PearsonICA包用基于互信息的打分函数分离独立信号。
ICS包能执行不变坐标系（invariant coordinate system）和独立成分分析（independent components）。JADE包提供就JADE算法的接口，而且可做一些 ICA。 


autoencoder
‘SAENET’


http://jotterbach.github.io/2016/03/24/Principal_Component_Analysis/
https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

#------------------------------------------------

x <- read.csv('alldata.csv',header = T)

library(gmodels)
library(fastICA)
library(autoencoder)

# system.time
#Efficient computation singular value decompositions
biplot


#---------------------
# PCA SVD
#--------------------

# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
# https://datajobs.com/data-science-repo/SVD-Primer-[Evert-and-Lenci].pdf

# base svd method
prin_comp <- prcomp(x[,-1],scale. = T)
names(prin_comp)

# center and scale refers to respective mean and standard deviation of the variables 
# that are used for normalization prior to implementing PCA
#outputs the mean of variables
head(prin_comp$center)
#outputs the standard deviation of variables
head(prin_comp$scale)

#2. The rotation measure provides the principal component loading. 
# Each column of rotation matrix contains the principal component loading vector. 
# This is the most important measure we should be interested in.

prin_comp$rotation[1:5,1:4]

# In order to compute the principal component score vector, 
# we don’t need to multiply the loading with data. Rather, 
# the matrix x has the principal component score vectors in a  dimension.

dim(prin_comp$x)

# Let’s plot the resultant principal components.
biplot(prin_comp, scale = 0)

# The parameter scale = 0 ensures that arrows are scaled to represent the loadings. 
# To make inference from image above, focus on the extreme ends (top, bottom, left, right) of this graph.

# We infer than first principal component corresponds to a measure of Outlet_TypeSupermarket, 
# Outlet_Establishment_Year 2007. Similarly, it can be said that the second component corresponds to a 
# measure of Outlet_Location_TypeTier1, Outlet_Sizeother. 
# For exact measure of a variable in a component, you should look at rotation matrix(above) again.

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")

# The plot above shows that ~ 30 components explains around 98.4% variance in the data set. 
# In order words, using PCA we have reduced 44 predictors to 30 without compromising on explained variance. 
# This is the power of PCA> Let’s do a confirmation check, by plotting a cumulative variance plot. 
# This will give us a clear picture of number of components.


#cumulative scree plot
 plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")

# This plot shows that 30 components results in variance close to ~ 98%. 
# Therefore, in this case, we’ll select number of components as 30 [PC1 to PC30] and proceed to the modeling stage. 
# This completes the steps to implement PCA on train data. For modeling, 
# we’ll use these 30 components as predictor variables and follow the normal procedures.



#---------------------
# ICA
#--------------------

model_ica <- fastICA(x[,-1])

a <- fastICA(x[,2:2000], 30, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "R", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)


par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")



#---------------------
# autoencoder
#--------------------

library(autoencoder)

## Set up the autoencoder architecture:
nl=3                          ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic"        ## specify the network unit type, i.e., the unit's 
                              ## activation function ("logistic" or "tanh")
Nx.patch=10                   ## width of training image patches, in pixels
Ny.patch=10                   ## height of training image patches, in pixels
N.input = Nx.patch*Ny.patch   ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 5*5                ## number of units in the hidden layer
lambda = 0.0002               ## weight decay parameter     
beta = 6                      ## weight of sparsity penalty term 
rho = 0.01                    ## desired sparsity parameter
epsilon <- 0.001              ## a small parameter for initialization of weights 
                              ## as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 2000         ## number of iterations in optimizer


autoencoder.object <- autoencode(X.train=as.matrix(x[,2:50]),nl=nl,N.hidden=N.hidden,
          unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
          optim.method="BFGS",max.iterations=max.iterations,
          rescale.flag=TRUE,rescaling.offset=0.001)


## N.B.: Training this autoencoder takes a long time, so in this example we do not run the above 
## autoencode function, but instead load the corresponding pre-trained autoencoder.object.


## Report mean squared error for training and test sets:
cat("autoencode(): mean squared error for training set: ",
round(autoencoder.object$mean.error.training.set,3),"\n")

## Visualize hidden units' learned features:
visualize.hidden.units(autoencoder.object,Nx.patch,Ny.patch)

## Compare the output and input images (the autoencoder learns to approximate 
## inputs in outputs using features learned by the hidden layer):
## Evaluate the output matrix corresponding to the training matrix 
## (rows are examples, columns are input channels, i.e., pixels)
X.output <- predict(autoencoder.object, X.input=as.matrix(x[,2:50]), hidden.output=FALSE)$X.output 

## Compare outputs and inputs for 3 image patches (patches 7,26,16 from 
## the training set) - outputs should be similar to inputs:
op <- par(no.readonly = TRUE)  ## save the whole list of settable par's.
par(mfrow=c(3,2),mar=c(2,2,2,2))
for (n in c(7,26,16)){
## input image:
  image(matrix(training.matrix[n,],nrow=Ny.patch,ncol=Nx.patch),axes=FALSE,main="Input image",
  col=gray((0:32)/32))
## output image:
  image(matrix(X.output[n,],nrow=Ny.patch,ncol=Nx.patch),axes=FALSE,main="Output image",
  col=gray((0:32)/32))
}
par(op)  ## restore plotting par's
