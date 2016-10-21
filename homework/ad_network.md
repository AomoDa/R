---
title: "Roulette"
output: html_document
arthur: Shengyao Yang, Shuaya Zhan, Zhaoye Tang
---

## nodeDensity

This function takes two inputs: x and y, two numeric vectors of the same length. The function returns a numeric vector of values that are proportional to node density at the (x,y) pairs.

```{r nodeDensity}
require(ggplot2)

nodeDensity = function (x, y) {
# x is a numeric vector 
# y is a numeric vector the same length as x
# z is returned. 
# It is a numeric vector that provides the value of the node density function

# Check the inputs for the correct format
if (mode(x) != "numeric" | mode(y) != "numeric") stop("x and y must be numeric")
if (length(x) != length(y)) stop("x and y must be same length")

  a = 3
  b = 1
  band = 15 
  bank = 1
  inBoundary = (0 <= x & x <= 100) & 
             (0 <= y & y <= 100 & y < sqrt(110^2 - x^2))

  river = abs(sqrt(x^2 + y^2) - 60) 
  hiArea = river> bank & river < band & inBoundary

  hiDensity = a * cos(river[hiArea] * pi / (2 * band)) + b 

  z = b * inBoundary
  z[hiArea] = hiDensity
  z[river <= bank] = 0 
  z
}
x = seq(0, 100, by =0.5)
y = seq(0, 100, by =0.5)
zMax = max(nodeDensity(x, y))
```

## genNodes

(a) The name of the functions is genNodes()
(b) Input argument: n, the number of nodes to generate (required).
(c) Return value: an n by 2 vector with the first column representing the x coordinates and the second columns the y coordinates of the nodes.

```{r genNodes}
genNodes = function(n){
  current = n
  resultDim = 0
  while (resultDim < n) {
    x = runif(current, min = 0, max = 100)
    y = runif(current, min = 0, max = 100)
    z = runif(current, min = 0, max = zMax)
    xCurrent = x[z < nodeDensity(x, y)]
    yCurrent = y[z < nodeDensity(x, y)]
    result = matrix(c(xCurrent, yCurrent), length(xCurrent))
    resultDim = dim(result)[1]
    current = ceiling(current * (current/(resultDim+1)))
  }
  return(head(result, n))
}
```

## findTranMat

Write a helper function called findTranMat() to find the transition matrix based on a distance matrix and a value for R. That is, this function takes as an input a distance matrix called mat and a value for R, called R. Both of these are required arguments. The function returns the transition matrix P for these inputs.

```{r findTranMat}
findTranMat = function(mat, R) {
  mat = as.matrix(mat)
  mat[mat < R] = 1
  mat[mat > R] = 0
  rowSum = rowSums(mat)
  for (i in (1:length(rowSum))) {
    if (rowSum[i] != 0) {
      mat[i,] = mat[i,] / rowSum[i]
    }
  }
  return(mat)
}
```

## getEigen2

Write a second helper function called getEigen2() which returns the second largest eigenvalue of a matrix. The input to this function has one argument, which is required. The parameter is a matrix, called mat.

```{r getEigen2}
getEigen2 = function(mat) {
  sort(eigen(mat)$values, decreasing = TRUE)[2]
}
```

## findRange

Write a function called findRange(), which finds the range of Rs to search over based on the above observations. This function has one input: the distance matrix called mat. It is required. The function returns a numeric vector of length 2, with the minimum and maximum values of R
to search over.

```{r findRange}
findRange = function(mat){
  mat = as.matrix(mat)
  mat[mat == 0] = NaN
  rowSize = dim(mat)[1]
  lo = c()
  hi = c()
  for (i in (1:rowSize)) {
    lo[i] = min(mat[i,], na.rm = TRUE)
  }
  for (i in (1:rowSize)) {
    hi[i] = max(mat[i,], na.rm = TRUE)
  }
  return(c(max(lo), min(hi)))
}
```

## findRc

(a) The function is called findRc()
(b) The first input parameter is nodes. It is required. This input is a 2-column matrix of the x and y locations of the nodes
(c) The second input parameter is too. It has a default value of 0.05, which is the tolerance level for how close you need to get to the true value of Rc for the provided configuration.
(d) The return value is a numeric vector of length 1, that holds the value of Rc (or a value close to it).

```{r findRc}
findRc = function(nodes, tol = 0.05){
  if (length(nodes) < 4) {
    return(0)
  }
  distMat = dist(nodes)
  lo = findRange(distMat)[1]
  if (length(nodes) == 4) {
    return(lo)
  }
  hi = findRange(distMat)[2]
  mid = lo + (hi-lo)/2
  while((hi-lo)/2 > tol) {
    transMat = findTranMat(distMat, mid)
    if (getEigen2(transMat) == 1) {
      lo = mid
      mid = lo + (hi-lo)/2
    } else {
      hi = mid
      mid = lo + (hi-lo)/2
    }
  }
  return(mid)
}
```

## questions

Generate 1000 networks and for each find the value for Rc. Examine the distribution of these Rc values. Some questions for you to consider are the following:

(a) How does Rc, the smallest radius such that the network is connected, change with different node configurations?

as number of nodes increases, Rc decreases

```{r question1}
rcTrend = function(nodeMin = 1, nodeMax = 1000, nodeStep = 50, nodeSample = 10) {
  num = seq(nodeMin, nodeMax, nodeStep)
  rc = sapply(num, function(x) {findRc(genNodes(x))})
  nodesRc = data.frame(numNodes=num, Rc=rc)
  for (i in (1:nodeSample)) {
    num = seq(nodeMin, nodeMax, nodeStep)
    rc = sapply(num, function(x) {findRc(genNodes(x))})
    nodesRc$Rc = nodesRc$Rc + rc
  }
  nodesRc$Rc = nodesRc$Rc/nodeSample
  ggplot(data = nodesRc) +
    geom_line(aes(x=numNodes, y=Rc))
}
#rcTrend()
```

(b) Explore the distribution of Rc. Is it symmetric, skewed, long tailed, multimodal?

Rc is skewed to the right

```{r question2-1}
  rc = rep(10, 1000)
  rc = sapply(rc, function(x) {findRc(genNodes(x))})
  nodesRc = data.frame(Rc=rc)
  ggplot(data = nodesRc) +
    geom_histogram(aes(x=Rc))
```

```{r question2-2}
  rc = rep(50, 1000)
  rc = sapply(rc, function(x) {findRc(genNodes(x))})
  nodesRc = data.frame(Rc=rc)
  ggplot(data = nodesRc) +
    geom_histogram(aes(x=Rc))
```

```{r question2-3}
  rc = rep(100, 1000)
  rc = sapply(rc, function(x) {findRc(genNodes(x))})
  nodesRc = data.frame(Rc=rc)
  ggplot(data = nodesRc) +
    geom_histogram(aes(x=Rc))
```

```{r question2-4}
  rc = rep(200, 1000)
  rc = sapply(rc, function(x) {findRc(genNodes(x))})
  nodesRc = data.frame(Rc=rc)
  ggplot(data = nodesRc) +
    geom_histogram(aes(x=Rc))
```

(c) Plot the network of connected points for four of your 1000 node configurations corresponding roughly to the min, median, mean, and maximum values of Rc.

```{r question3}
  dots = list()
  for (i in (1:1000)) {
    dots[[i]] = genNodes(100)
  }
  rc = c()
  for (i in (1:1000)) {
    rc[i] = findRc(dots[[i]])
  }
```

```{r min}
  minData = data.frame(dots[[match(min(rc), rc)]])
  ggplot(minData) +
    geom_point(aes(x =  X1, y = X2))
```

```{r median}
  medianRc = min(rc[abs(rc-median(rc))==min(abs(rc-median(rc)))])
  medianData = data.frame(dots[[match(medianRc, rc)]])
  ggplot(medianData) +
    geom_point(aes(x =  X1, y = X2))
```

```{r mean}
  meanRc = min(rc[abs(rc-mean(rc))==min(abs(rc-mean(rc)))])
  meanData = data.frame(dots[[match(meanRc, rc)]])
  ggplot(meanData) +
    geom_point(aes(x =  X1, y = X2))
```

```{r max}
  maxData = data.frame(dots[[match(max(rc), rc)]])
  ggplot(maxData) +
    geom_point(aes(x =  X1, y = X2))
```
