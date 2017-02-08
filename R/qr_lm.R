######### HOMEWORK 1, STOR 665: COMPUTING PART, DUE FEB 8, 2017 #########

######### Student Name:                      #########

# INSTRUCTION: Edit this R file by adding your solution to the end of each 
# question. Upload the final file to Sakai.

# Your submitted file should run smoothly in R and provide all 
# required results. You are allowed to work with other students but the 
# homework should be in your own words. Identical solutions will receive a 0 
# in grade and will be investigated.

# This homework is an exercise in Least Squares computation.  We will
# build up R code that 1) computes a Q-R decomposition of a matrix X,
# 2) solves equations when the coefficients are upper triangular,
# 3) inverts an upper triangular matrix R, and finally
# 4) computes and returns most things we find useful in regression.
# We will be negligent in the sense that we will ignore precautions 
# that a numerical analyst would build into his code, and we will not
# consider measures to improve numerical conditioning such as pivoting
# (switching the order of the variables) either.


# BACKGROUND: Most LS solvers are based on decompositions of the
# design matrix of the form X = Q R, where R is square and triangular
# and Q has orthonormal columns.  The idea is that the LS criterion
# can be written as follows:
#      | y - X b |^2  =  | Q^T y - Q^T X b |^2  +  | r |^2
#                     =  | Q^T y - R b |^2      +  | r |^2
# where the first term can be forced to zero by solving  
#      Q^T y  =  R b
# This equation is easily solved when  R  is triangular.
# Furthermore, for inference we want the matrix (X^T X)^{-1},
# but with a Q-R decomposition this is easily gotten by
# inverting R because
#   (X^T X)^{-1}  =  R^{-1} R^{-1}^T


#----------------------------------------------------------------

# PROBLEM 1: Write a function 'gs(X)' to implement the so-called
# modified Gram-Schmidt procedure for orthnormalizing an nxp matrix.
# This procedure consists of orthnormalizing the columns of X to form
# the matrix Q of the same size, and storing the coefficients to
# reconstitute X from Q in an upper triangular matrix R.

# Here is the algorithm:
#   Initialize Q with a copy of X
#   Loop over the columns of Q and do the following:
#     At stage j,
#       normalize Q[,j] in place
#       adjust the columns of Q[,(j+1):p] for Q[,j]
#       store the original length of Q[,j] in R[j,j]
#         and the coefficients from adjustment properly in row R[j,].
# If you do this right, it should hold that  X = Q R.
# Finish the function gs() with  return(list(Q=Q, R=R))

# In this algorithm, use loops and sum() to calculate inner products
# and sqrt() for lengths, but not lm() or any other high-level function.

# Test your function on these data:
#   X <- cbind(rep(1,10), 1:10, (1:10)^2)
#   sol <- gs(X)
# Show these and comment on them:
#   sol$Q %*% sol$R
#   t(sol$Q) %*% sol$Q

# SOLUTION: 

gs <- function(X){
    Q <- X 
	Q <- as.matrix(Q)
    p <- ncol(Q) #The Number of Columns
    R <- diag(p) 
    #Loop over the columns of Q
    for (j in 1:(p-1) ){
      for (k in  (j+1):p ){
          R[j,k] <-  sum(X[,k]*Q[,j])/sum(Q[,j]*Q[,j])        
          Q[,k] <-  Q[,k] - R[j,k] * Q[,j]
        }
    }   
    #normalize Q and R
    for (j in 1:p){
      norm <-  sqrt(sum(Q[,j]^2))
      R[j,] <-  R[j,] * norm
      Q[,j] <-  Q[,j]/norm
    }    
    return(list(Q=Q,R=R))
}


# Test your function on these data:
X <- cbind(rep(1,10), 1:10, (1:10)^2)
sol <- gs(X)

# Show these and comment on them:
sol$Q %*% sol$R
##       [,1] [,2] [,3]
##  [1,]    1    1    1
##  [2,]    1    2    4
##  [3,]    1    3    9
##  [4,]    1    4   16
##  [5,]    1    5   25
##  [6,]    1    6   36
##  [7,]    1    7   49
##  [8,]    1    8   64
##  [9,]    1    9   81
## [10,]    1   10  100

# t(sol$Q) %*% sol$Q
round(t(sol$Q) %*% sol$Q,2)
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

#----------------------------------------------------------------

# PROBLEM 2: Write function 'tri.solve(R,z)' that accepts an upper
# triangular matrix R and a colum z and returns the solution b of z =
# R b.  Show the results for these inputs:

#   z <- 10:1
#   R <- (row(diag(10)) <= col(diag(10)))

# You may use loops and the sum() function inside tri.solve() to spare
# yourself an inner loop, but you must not use canned solvers such as
# solve() or ginv() (the latter from the MASS package).

# SOLUTION:

tri.solve <- function(R,z){
    p <- length(z)
    b <- numeric(length = p)
    for (j in p:1){
      b[j] <- z[j] / R[j,j]
      for (k in 1:(p-1)) {
      	  z[k] <- z[k] - R[k,j]*b[j]
        }
    }
  return(b)
}

#Show the results for these inputs:
z <- 10:1
R <- (row(diag(10)) <= col(diag(10)))
tri.solve(R,z)
##  [1] 1 1 1 1 1 1 1 1 1 1

#----------------------------------------------------------------

# PROBLEM 3: No programming, just thinking.
# Assume R and R1 are upper triangular square matrices.
# Questions:
# a) If the vector b is has non-zero values only in the first k entries,
#    what can one say about z=Rb?
# b) When is R invertible?
# c) If the vector z has non-zero values only in the first k entries,
#    and if R is invertible, what can one say about the vector b?
# d) What can one say about R1%*%R?
# e) What can one say about R^{-1}?

# SOLUTION:

# a) z is a linear combination of the first k columns of R
#    z lives in a k-dimensional subspace of the original p-dimensional space.  
# 
# b) none of the diagnoral items are 0.
# 
# c) the bottom right submatrix of R with (p-k)*(p-k) dimension (give a name to this matrix R'), 
#    multiples the last (p-k) elements of b (give a name b'), is 0. i.e. R'b'=0. i.e. 
#    b' lives in the left null space of R'
#
# d) upper triangular.
#
# e) upper triangular.

#----------------------------------------------------------------

# PROBLEM 4: For inference about the regression coefficients, we need
# the matrix (X^T X)^{-1}.  In preparation for it, we need the
# inversion of an upper triangular matrix R because from a Q-R
# decomposition X=QR we can easily (X^T X)^{-1} if we have R^{-1}.
# Package the inversion in a function inv(R).  Try it out on the R
# matrix of the above 'sol':
#   inv(sol$R)
#   inv(sol$R) %*% sol$R
# Show both and comment on both.

# SOLUTION:

inv <- function(R){
  p <- nrow(R)
  z <- diag(p)
  r_inv <-matrix(0,nrow = p,ncol = p)
  for (j in 1:p){ 
  	  r_inv[,j] <-  tri.solve(R,z[,j])
    }
  return(r_inv)
}

# TEST
inv(sol$R)
##           [,1]       [,2]        [,3]
## [1,] 0.3162278 -0.6055301  0.95742711
## [2,] 0.0000000  0.1100964 -0.47871355
## [3,] 0.0000000  0.0000000  0.04351941

# inv(sol$R) %*% sol$R
round(inv(sol$R) %*% sol$R,2)

##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

#----------------------------------------------------------------

# PROBLEM 5: Using tri.solve(R,z), inv(R) and gs(X,y), write a
# function reg(X,y) that computes a list with the following named
# elements:

# a) "Coeffs": a matrix with one row per regression coefficient and columns
#    named "Coefficient", "Std.Err.Est", "t-Statistic", "P-Value"
# b) "Var": the coefficient variance matrix  s^2*(X^T X)^{-1}
# c) "RMSE": a number, s
# d) "R2": a number, R2
# e) "Residuals": the vector of residuals

# Assume that X does not have a column of 1's but an intercept is desired,
# hence first thing in the function do  X <- cbind(Icept=1,X) .

# You may use matrix multiplication if convenient, but no high-level
# functions other than things like sqrt() and pt().

# Try your solution on the following data and show the full result:
#   X <- cbind(Pred1=1:10, Pred2=(1:10)^2)
#   y <- rep(1,10) + 2*X[,1] + 3*X[,2] + resid(lm(rep(0:1,5)~X))
#   reg(X,y)
# Compare with results from the canned regression function
#   summary(lm(y~X))
# No need to comment, but you need to get the exact numbers.

# Compare the function execution time. Your function should be much faster.
#   system.time(for (i in 1:100){reg(X,y)})
#   system.time(for (i in 1:100){lm(y~X)})
 
# SOLUTION:

reg <- function(X,y){
  X <- cbind(Icept=1,X)
  n <- nrow(X) # ROWS
  p <- ncol(X) # COLS
  sol <- gs(X)
  z <-  t(sol$Q) %*% y 
  beta_hat <- tri.solve(sol$R,z)  
  # inv 
  r_inv <- inv(sol$R)
  xinv <- r_inv %*% t(r_inv)
  RSS <- sum((y - X %*% beta_hat)^2)
  sigma2 <- RSS/(n-p)
  sig <- sqrt(sigma2)
  sd_beta_hat <- sig * sqrt(diag(xinv)) 
  t_stat <- beta_hat/sd_beta_hat 
  p_value <- 2 * (1- pt(t_stat, n-p))   
  mt <- cbind(beta_hat, sd_beta_hat, t_stat, p_value)
  colnames(mt) <- c("Coefficient", "Std.Err.Est","t-Statistic", "P-Value")
  xvar <- sig^2 * xinv
  s <- sqrt(RSS/n)
  tss <- sum((y-mean(y))^2)
  R2 <- (tss-RSS)/tss 
  res <- y - X %*% beta_hat
 return (list(Coeffs = mt, Var=xvar, RMSE=s, R2=R2, Residuals=res))
}


# Try your solution on the following data and show the full result:
X <- cbind(Pred1=1:10, Pred2=(1:10)^2)
y <- rep(1,10) + 2*X[,1] + 3*X[,2] + resid(lm(rep(0:1,5)~X))
reg(X,y)

## $Coeffs
##      Coefficient Std.Err.Est t-Statistic      P-Value
## [1,]           1  0.69215351    1.444766 1.917550e-01
## [2,]           2  0.28907249    6.918680 2.274824e-04
## [3,]           3  0.02561073  117.138380 8.713030e-13
## 
## $Var
##             [,1]         [,2]          [,3]
## [1,]  0.47907648 -0.181818182  0.0144300144
## [2,] -0.18181818  0.083562902 -0.0072150072
## [3,]  0.01443001 -0.007215007  0.0006559097
## 
## $RMSE
## [1] 0.492366
## 
## $R2
## [1] 0.9999771
## 
## $Residuals
##             [,1]
##  [1,] -0.3636364
##  [2,]  0.6060606
##  [3,] -0.4242424
##  [4,]  0.5454545
##  [5,] -0.4848485
##  [6,]  0.4848485
##  [7,] -0.5454545
##  [8,]  0.4242424
##  [9,] -0.6060606
## [10,]  0.3636364

# Compare with results from the canned regression function
summary(lm(y~X))

## 
## Call:
## lm(formula = y ~ X)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.6061 -0.4697  0.0000  0.4697  0.6061 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.00000    0.69215   1.445 0.191755    
## XPred1       2.00000    0.28907   6.919 0.000227 ***
## XPred2       3.00000    0.02561 117.138 8.71e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5885 on 7 degrees of freedom
## Multiple R-squared:      1,  Adjusted R-squared:      1 
## F-statistic: 1.528e+05 on 2 and 7 DF,  p-value: < 2.2e-16

# Compare the function execution time. Your function should be much faster.
system.time(for (i in 1:100){reg(X,y)})

## user system elapsed  
## 0.03 0.00 0.03 
system.time(for (i in 1:100){lm(y~X)})
## user system elapsed 
## 0.10 0.00 0.09 

#----------------------------------------------------------------
