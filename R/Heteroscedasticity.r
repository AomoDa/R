lmHetero <- function(X, ...){UseMethod("lmHetero")}                  # Declare function as S3 object

lmHetero.default <- function(formula, hetero=~1, data, subset, na.action, 
                             contrasts = NULL, iter=FALSE, ...) {
  ################################################################################################
  ##
  ## Purpose: Calculate multiplicately weighted regression models with respect to externally
  ##          given exogenous variables
  ##
  ## Source: The ML estimation procedure for multiplicately weighted regression is given
  ##         in Greene W. H. (2000). Econometric Analysis. 4th edition. Upper Saddle River:
  ##         Prentice Hall. pp 516-521 (Note: page numbers will differ for other editions)
  ##
  ## Objective: estimate gamma values for mulitiplicative heteroscedasticity models
  ##
  ## Syntax:
  ## [1] lmHetero(y~x1+x2, hetero= ~z1+z2, data=mydata)
  ## [2] lmHetero(y~x1+x2 | z1+z2, data=mydata)
  ## Note: An expression for Z must be present.
  ## y : Dependent variable 
  ## X : Independent variable(s) with added intercept
  ## Z : Weight variable(s) with intercept added.
  ##
  ##     !!! Important user input: weighte variables Z must be enter log-transformed !!!
  ##
  ## Authors: Michael Tiefelsdorf (tiefelsdorf@utd.edu) & Yongwan Chun
  ############################################################################################### 
  
  ## Parseing the function call
  require(Formula) 
  cl <- match.call()
  if (missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (!missing(hetero)) {
    formula <- as.Formula(formula, hetero)
    cl$hetero <- NULL
    cl$formula <- formula(formula)
  }
  else {
    formula <- as.Formula(formula)
  }
  stopifnot(length(formula)[1] == 1L, length(formula)[2] %in% 1:2)
  has_dot <- function(formula) inherits(try(terms(formula), silent = TRUE), "try-error")
  if (has_dot(formula)) {
    f1 <- formula(formula, rhs = 1)
    f2 <- formula(formula, lhs = 0, rhs = 2)
    if (!has_dot(f1) & has_dot(f2))
      formula <- as.Formula(f1, update(formula(formula, lhs = 0, rhs = 1), f2))
  }
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- model.response(mf, "numeric")
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf, contrasts)
  if (length(formula)[2] < 2L) {
    #stop("Set of weights variables is missing in function call!")
    #mtZ <- NULL
    #Z <- NULL
    zNames <- "(Intercept)"
    Z <- matrix(1, nrow=length(y), ncol=1)
  }
  else {
    mtZ <- delete.response(terms(formula, data = data, rhs = 2))
    Z <- model.matrix(mtZ, mf, contrasts)
    zNames <- colnames(Z)
    if (!all(exp(Z) > 0)) stop("All weight variables must be positive!")
    #Z[,-1] <- log(Z[,-1])
  }
  ## done::parsing and starting the estimation
  
  ##
  ## Calculate start values for Beta and Gamma
  ##
  nofreg <- length(y)
  Beta <- qr.solve(crossprod(X),crossprod(X,y))
  res <- y - X %*% Beta
  res <- log(res^2)
  
  Gamma <- qr.solve(crossprod(Z),crossprod(Z,res))
  Gamma[1] <- Gamma[1]+1.2704    # Harvey (1976) correction
  if (iter==TRUE) { cat("Beta:",Beta);cat("\n");cat("Gamma:",Gamma);cat("\n") }
  
  qrcz <- qr(crossprod(Z))
  
  ## Start interative estimation procedure
  MaxDiff <- Inf
  while (MaxDiff > 0.001) {
    Sigma2 <- exp(Z %*% Gamma) 
    W <- diag(1 / Sigma2[,1])
    
    ## Calculate the values at the next step
    #  newBeta <- solve(t(x) %*% W %*% x, t(x) %*% W %*% y)
    xW <- crossprod(X,W)
    newBeta <- qr.solve(xW %*% X, xW %*% y)
    res2 <- (y - X %*% newBeta)^2
    newGamma <- Gamma + qr.coef(qrcz,crossprod(Z, res2 / Sigma2 - 1))        
    MaxDiff <- max(abs(newGamma - Gamma))
    if (iter==TRUE) {cat("Beta:",newBeta);cat("\n");cat("Gamma:",newGamma);cat("\n")}
    Beta <- newBeta
    Gamma <- newGamma
  }  # end while
  
  Sigma2 <- exp(Z %*% newGamma) 
  cBeta <- qr.solve(xW %*% X)      # The global covariance matrix is block diagonal
  cGamma <- 2*qr.solve(crossprod(Z))
  logLikeH1 <- -(nofreg/2)*log(2*pi) - 0.5*sum(log(Sigma2[,1])) - 0.5* sum(res2/Sigma2[,1])
  logLikeH0 <- logLik(lm(y~X))[1]
  ##
  ## Report results. As ML-estimates the values are slightly biased
  ##
  rval <- list()
  rval$sigma2 <- exp(newGamma[1]) 
  rval$gamma <- newGamma
  rval$namesGamma <- zNames
  rval$beta <- newBeta 
  rval$weights <- 1/Sigma2[,1]
  rval$covBeta <- cBeta
  rval$covGamma <- cGamma
  rval$logLikeH1 <- logLikeH1
  rval$logLikeH0 <- logLikeH0
#   rval$call <- cl
#   rval$formula <- formula(formula)
#   rval$terms <- list(regressors = mtX, hetero = mtZ, full = mt)
#   rval$na.action <- attr(mf, "na.action")
#   rval$levels <- .getXlevels(mt, mf)
#   rval$contrasts <- list(regressors = attr(X, "contrasts"),
#                        hetero = attr(Z, "contrasts"))
  class(rval) <- "lmHetero"
return(rval)
} # end::lmHetero

summary.lmHetero <- function(object, ...){
  ## Regression coefficent info
  b <- object$beta
  se <- sqrt(diag(object$covBeta))
  z <- b/se
  table1 <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
  colnames(table1) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
  rownames(table1) <- rownames(object$beta)
  
  ## Gamma weights coefficient infor
  g <- object$gamma
  seg <- sqrt(diag(object$covGamma))
  zg <- g/seg
  table2 <- cbind(g, seg, zg, 2*(1-pnorm(abs(zg))))
  colnames(table2) <- c("Gamma", "Std.Err", "z-value", "Pr(>|z|)")
  rownames(table2) <- object$namesGamma
  
  result <- list(coef=table1, gamma=table2, 
                 logLikeH1=object$logLikeH1,logLikeH0=object$logLikeH0)
  class(result) <- "summary.lmHetero" 
  result
} # end::summary.lmHetero

print.summary.lmHetero <- function(X, ...){
  cat("\n===============================================================")
  cat("\nMultiplicatively Weighted Heteroscedasticity ML-Regrssion Model")
  cat("\n===============================================================\n\n")
  cat("Regression Coefficients:\n")
  printCoefmat(X$coef)
  cat("\nGamma Coefficients:\n")
  printCoefmat(X$gamma)
  cat("\nlog-likelihood =", X$logLikeH1,"\n")
  
  ## Likelihood ratio test for heteroscedasticity
  df <- nrow(X$gamma)-1                      # with df=dim(Gamma[-Intercept])
  if (df > 0L)
  {
    LR <- 2*abs(X$logLikeH1 - X$logLikeH0)     # chi^2 distributed 
    table <- cbind(LR,df,pchisq(LR,df,lower.tail=FALSE))
    colnames(table) <- c("LR","  df"," Pr(Chi > LR)")
    rownames(table) <- ""
    LRTest <- c("LR"=round(LR,2),"df"=round(df,0),"Pr(Chi > LR)"=round(pchisq(LR,df,lower.tail=FALSE),5))
    cat("\nHeteroscedasticity likelihood ratio test:\n")
    print(table)
  }
  cat("\n")
  invisible(X)
} # end::print.summary.lmHetero



library(foreign)
setwd("M:\\Lectures2016\\ECON6306\\MaxLikWithGLS")
Bladder <- read.spss("bladder_wmp2.sav",to.data.frame=TRUE)

##
## OLS model with heteroscedasticity investigation
##
hist(log(Bladder$pop))
lm1 <- lm(lnbladd~lnlung1+lnpopden, data=Bladder)
summary(lm1)
## Breusch-Pagan (library car) heteroscedasticy test with H0:homoscedasticity
car::ncvTest(lm1, var.formula=~log(pop), data=Bladder)     # Specific variable log(pop)
car::ncvTest(lm1, data=Bladder)                            # General model y_hat

##
## See Kleiber & Zeileis pp76-78 "Weighted Least Squares"
##
## Estimation of Gamma
auxreg1 <- lm(log(residuals(lm1)^2)~log(pop), data=Bladder) 
summary(auxreg1)

plot(log(residuals(lm1)^2)~log(Bladder$pop))
abline(auxreg1)
title("Heteroscedastic lm-Residuals")

## Weighted regression with inverse variance: 1/exp(fitted(auxreg1))
lm2 <- lm(lnbladd~lnlung1+lnpopden, weight=1/exp(fitted(auxreg1)), data=Bladder)
summary(lm2)

## weigthed.residuals() are scaled by the square root of the weights used in fitting
plot(log(weighted.residuals(lm2)^2)~log(Bladder$pop))     
abline(lm(log(weighted.residuals(lm2)^2)~log(Bladder$pop)))
title("Homoscedastic Weighted Residuals")

##
## Maximum Likelihood Approach 
##

## Procedure test with no hetero-part: lmH0$weights=constant and 1/lmH0$weights = lmH0$sigma2

lmH0 <- lmHetero(lnbladd~lnlung1+lnpopden, data=Bladder)
summary(lmH0)
all.equal(1/lmH0$weights[1], lmH0$sigma2)

##
## Simple weighted model with keyword syntax
##
lmH1 <- lmHetero(lnbladd~lnlung1+lnpopden, hetero= ~log(pop), data=Bladder)
summary(lmH1)
wlm1 <- lm(lnbladd~lnlung1+lnpopden, data=Bladder, weights=lmH1$weights)
summary(wlm1)

##
## Complexer model for weights with alternative syntax
##
lmH2 <- lmHetero(lnbladd~lnlung1+lnpopden | log(pop)+I(log(pop)^2), data=Bladder)
summary(lmH2)
wlm2 <- lm(lnbladd~lnlung1+lnpopden, data=Bladder, weights=lmH2$weights)
summary(wlm2)

chi <- -2*(lmH1$logLikeH1-lmH2$logLikeH1)
pchisq(chi,df=1, lower.tail=F)
