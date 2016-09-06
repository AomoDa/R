WARNING: ignoring environment value of R_HOME

R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> caret::getModelInfo()
$ada
$ada$label
[1] "Boosted Classification Trees"

$ada$library
[1] "ada"  "plyr"

$ada$loop
function (grid) 
{
    loop <- ddply(grid, c("nu", "maxdepth"), function(x) c(iter = max(x$iter)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$iter)) {
        index <- which(grid$maxdepth == loop$maxdepth[i] & grid$nu == 
            loop$nu[i])
        trees <- grid[index, "iter"]
        submodels[[i]] <- data.frame(iter = trees[trees != loop$iter[i]])
    }
    list(loop = loop, submodels = submodels)
}

$ada$type
[1] "Classification"

$ada$parameters
  parameter   class          label
1      iter numeric         #Trees
2  maxdepth numeric Max Tree Depth
3        nu numeric  Learning Rate

$ada$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out = expand.grid(iter = floor((1:len) * 50), maxdepth = seq(1, 
            len), nu = 0.1)
    }
    else {
        out <- data.frame(iter = sample(1:1000, replace = TRUE, 
            size = len), maxdepth = sample(1:10, replace = TRUE, 
            size = len), nu = runif(len, min = 0.001, max = 0.5))
        out <- out[!duplicated(out), ]
    }
    out
}

$ada$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$maxdepth <- param$maxdepth
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(maxdepth = param$maxdepth, cp = -1, 
        minsplit = 0, xval = 0)
    modelArgs <- c(list(x = x, y = y, iter = param$iter, nu = param$nu, 
        control = ctl), theDots)
    out <- do.call("ada", modelArgs)
    out
}

$ada$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, n.iter = modelFit$tuneValue$iter)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$iter) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$iter)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, n.iter = submodels$iter[[i]])
        }
        out <- lapply(tmp, as.character)
    }
    out
}

$ada$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "prob", n.iter = modelFit$tuneValue$iter)
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$iter) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$iter)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, type = "prob", 
                n.iter = submodels$iter[[i]])
            colnames(tmp[[i + 1]]) <- modelFit$obsLevels
        }
        out <- lapply(tmp, as.data.frame)
    }
    out
}

$ada$levels
function (x) 
x$obsLevels

$ada$tags
[1] "Tree-Based Model"              "Ensemble Model"               
[3] "Boosting"                      "Implicit Feature Selection"   
[5] "Two Class Only"                "Handle Missing Predictor Data"

$ada$sort
function (x) 
x[order(x$iter, x$maxdepth, x$nu), ]


$AdaBag
$AdaBag$label
[1] "Bagged AdaBoost"

$AdaBag$library
[1] "adabag" "plyr"  

$AdaBag$loop
function (grid) 
{
    loop <- ddply(grid, c("maxdepth"), function(x) c(mfinal = max(x$mfinal)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mfinal)) {
        index <- which(grid$maxdepth == loop$maxdepth[i])
        trees <- grid[index, "mfinal", drop = FALSE]
        submodels[[i]] <- data.frame(mfinal = trees[trees != 
            loop$mfinal[i]])
    }
    list(loop = loop, submodels = submodels)
}

$AdaBag$type
[1] "Classification"

$AdaBag$parameters
  parameter   class          label
1    mfinal numeric         #Trees
2  maxdepth numeric Max Tree Depth

$AdaBag$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mfinal = floor((1:len) * 50), maxdepth = seq(1, 
            len))
    }
    else {
        out <- data.frame(mfinal = sample(1:1000, replace = TRUE, 
            size = len), maxdepth = sample(1:30, replace = TRUE, 
            size = len))
    }
    out
}

$AdaBag$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$maxdepth <- param$maxdepth
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(maxdepth = param$maxdepth, cp = -1, 
        minsplit = 0, xval = 0)
    modelArgs <- c(list(formula = as.formula(.outcome ~ .), data = if (is.data.frame(x)) x else as.data.frame(x), 
        mfinal = param$mfinal, control = ctl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("bagging", modelArgs)
    out
}

$AdaBag$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    out <- predict(modelFit, newdata, newmfinal = modelFit$tuneValue$mfinal)$class
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$mfinal) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$mfinal)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, newmfinal = submodels$mfinal[[i]])$class
        }
        out <- tmp
    }
    out
}

$AdaBag$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    out <- predict(modelFit, newdata)$prob
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$mfinal) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$mfinal)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, newmfinal = submodels$mfinal[[i]])$prob
            colnames(tmp[[i + 1]]) <- modelFit$obsLevels
        }
        out <- lapply(tmp, as.data.frame)
    }
    out
}

$AdaBag$varImp
function (object, ...) 
{
    imps <- data.frame(Overall = object$importance)
    rownames(imps) <- names(object$importance)
    imps
}

$AdaBag$levels
function (x) 
x$obsLevels

$AdaBag$predictors
function (x, ...) 
names(x$importance)[x$importance != 0]

$AdaBag$tags
[1] "Tree-Based Model"              "Ensemble Model"               
[3] "Boosting"                      "Bagging"                      
[5] "Implicit Feature Selection"    "Handle Missing Predictor Data"

$AdaBag$sort
function (x) 
x[order(x$mfinal, x$maxdepth), ]


$AdaBoost.M1
$AdaBoost.M1$label
[1] "AdaBoost.M1"

$AdaBoost.M1$library
[1] "adabag" "plyr"  

$AdaBoost.M1$loop
function (grid) 
{
    loop <- ddply(grid, c("coeflearn", "maxdepth"), function(x) c(mfinal = max(x$mfinal)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mfinal)) {
        index <- which(grid$maxdepth == loop$maxdepth[i] & grid$coeflearn == 
            loop$coeflearn[i])
        trees <- grid[index, "mfinal"]
        submodels[[i]] <- data.frame(mfinal = trees[trees != 
            loop$mfinal[i]])
    }
    list(loop = loop, submodels = submodels)
}

$AdaBoost.M1$type
[1] "Classification"

$AdaBoost.M1$parameters
  parameter     class            label
1    mfinal   numeric           #Trees
2  maxdepth   numeric   Max Tree Depth
3 coeflearn character Coefficient Type

$AdaBoost.M1$grid
function (x, y, len = NULL, search = "grid") 
{
    types <- c("Breiman", "Freund", "Zhu")
    if (search == "grid") {
        out <- expand.grid(mfinal = floor((1:len) * 50), maxdepth = seq(1, 
            len), coeflearn = types)
    }
    else {
        out <- data.frame(mfinal = sample(1:1000, replace = TRUE, 
            size = len), maxdepth = sample(1:30, replace = TRUE, 
            size = len), coeflearn = sample(types, replace = TRUE, 
            size = len))
    }
    out
}

$AdaBoost.M1$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$maxdepth <- param$maxdepth
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(maxdepth = param$maxdepth, cp = -1, 
        minsplit = 0, xval = 0)
    modelArgs <- c(list(formula = as.formula(.outcome ~ .), data = if (is.data.frame(x)) x else as.data.frame(x), 
        mfinal = param$mfinal, coeflearn = as.character(param$coeflearn), 
        control = ctl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("boosting", modelArgs)
    out
}

$AdaBoost.M1$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    out <- predict(modelFit, newdata, newmfinal = modelFit$tuneValue$mfinal)$class
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$mfinal) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$mfinal)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, newmfinal = submodels$mfinal[[i]])$class
        }
        out <- tmp
    }
    out
}

$AdaBoost.M1$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    out <- predict(modelFit, newdata)$prob
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = length(submodels$mfinal) + 
            1)
        tmp[[1]] <- out
        for (i in seq(along = submodels$mfinal)) {
            tmp[[i + 1]] <- predict(modelFit, newdata, newmfinal = submodels$mfinal[[i]])$prob
            colnames(tmp[[i + 1]]) <- modelFit$obsLevels
        }
        out <- lapply(tmp, as.data.frame)
    }
    out
}

$AdaBoost.M1$levels
function (x) 
x$obsLevels

$AdaBoost.M1$varImp
function (object, ...) 
{
    imps <- data.frame(Overall = object$importance)
    rownames(imps) <- names(object$importance)
    imps
}

$AdaBoost.M1$predictors
function (x, ...) 
names(x$importance)[x$importance != 0]

$AdaBoost.M1$tags
[1] "Tree-Based Model"              "Ensemble Model"               
[3] "Boosting"                      "Implicit Feature Selection"   
[5] "Handle Missing Predictor Data"

$AdaBoost.M1$sort
function (x) 
x[order(x$mfinal, x$maxdepth), ]


$adaboost
$adaboost$label
[1] "AdaBoost Classification Trees"

$adaboost$library
[1] "fastAdaboost"

$adaboost$loop
NULL

$adaboost$type
[1] "Classification"

$adaboost$parameters
  parameter     class  label
1     nIter   numeric #Trees
2    method character Method

$adaboost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out = expand.grid(nIter = floor((1:len) * 50), method = c("Adaboost.M1", 
            "Real adaboost"))
    }
    else {
        out <- data.frame(nIter = sample(1:1000, replace = TRUE, 
            size = len), method = sample(c("Adaboost.M1", "Real adaboost"), 
            replace = TRUE, size = len))
    }
    out
}

$adaboost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    out <- if (param$method == "Adaboost.M1") 
        adaboost(.outcome ~ ., data = dat, nIter = param$nIter, 
            ...)
    else real_adaboost(.outcome ~ ., data = dat, nIter = param$nIter, 
        ...)
    out
}

$adaboost$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)$class
}

$adaboost$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)$prob
    out <- t(apply(out, 1, function(x) ifelse(x == Inf, 1, x)))
    out <- t(apply(out, 1, function(x) ifelse(x == -Inf, 0, x)))
    out <- as.data.frame(out)
    colnames(out) <- as.vector(modelFit$classnames)
    out
}

$adaboost$levels
function (x) 
as.vector(x$classnames)

$adaboost$predictors
function (x, ...) 
unique(unlist(lapply(x$trees, predictors)))

$adaboost$tags
[1] "Tree-Based Model"           "Ensemble Model"            
[3] "Boosting"                   "Implicit Feature Selection"
[5] "Two Class Only"            

$adaboost$sort
function (x) 
x[order(x$nIter), ]


$amdai
$amdai$label
[1] "Adaptive Mixture Discriminant Analysis"

$amdai$library
[1] "adaptDA"

$amdai$loop
NULL

$amdai$type
[1] "Classification"

$amdai$parameters
  parameter     class      label
1     model character Model Type

$amdai$grid
function (x, y, len = NULL, search = "grid") 
data.frame(model = "lda")

$amdai$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    mod <- amdai(x, as.numeric(y), model = as.character(param$model), 
        ...)
    mod$levels <- levels(y)
    mod
}

$amdai$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, K = length(modelFit$levels))$cls
    factor(modelFit$levels[out], levels = modelFit$levels)
}

$amdai$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, K = length(modelFit$levels))$P
    factor(modelFit$levels[out], levels = modelFit$levels)
    colnames(out) <- modelFit$obsLevels
    out
}

$amdai$varImp
NULL

$amdai$predictors
function (x, ...) 
predictors(x$terms)

$amdai$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$amdai$tags
[1] "Discriminant Analysis" "Mixture Model"        

$amdai$sort
function (x) 
x


$ANFIS
$ANFIS$label
[1] "Adaptive-Network-Based Fuzzy Inference System"

$ANFIS$library
[1] "frbs"

$ANFIS$type
[1] "Regression"

$ANFIS$parameters
   parameter   class           label
1 num.labels numeric    #Fuzzy Terms
2   max.iter numeric Max. Iterations

$ANFIS$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, max.iter = 10)
    }
    else {
        out <- data.frame(num.labels = sample(1:10, replace = TRUE, 
            size = len), max.iter = sample(1:20, replace = TRUE, 
            size = len))
    }
    out
}

$ANFIS$loop
NULL

$ANFIS$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "ANFIS")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.iter <- param$max.iter
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        max.iter = param$max.iter, max.iter = 10, step.size = 0.01, 
        type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$ANFIS$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$ANFIS$prob
NULL

$ANFIS$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$ANFIS$tags
[1] "Rule-Based Model"

$ANFIS$levels
NULL

$ANFIS$sort
function (x) 
x[order(x$num.labels), ]


$avNNet
$avNNet$label
[1] "Model Averaged Neural Network"

$avNNet$library
[1] "nnet"

$avNNet$loop
NULL

$avNNet$type
[1] "Classification" "Regression"    

$avNNet$parameters
  parameter   class         label
1      size numeric #Hidden Units
2     decay numeric  Weight Decay
3       bag logical       Bagging

$avNNet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(size = ((1:len) * 2) - 1, decay = c(0, 
            10^seq(-1, -4, length = len - 1)), bag = FALSE)
    }
    else {
        out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
            decay = 10^runif(len, min = -5, 1), bag = sample(c(TRUE, 
                FALSE), size = len, replace = TRUE))
    }
    out
}

$avNNet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    library(nnet)
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- avNNet(.outcome ~ ., data = dat, weights = wts, 
            size = param$size, decay = param$decay, bag = param$bag, 
            ...)
    }
    else out <- avNNet(.outcome ~ ., data = dat, size = param$size, 
        decay = param$decay, bag = param$bag, ...)
    out
}

$avNNet$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata, type = "raw")
    }
    out
}

$avNNet$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "prob")
    if (ncol(as.data.frame(out)) == 1) {
        out <- cbind(out, 1 - out)
        dimnames(out)[[2]] <- rev(modelFit$obsLevels)
    }
    out
}

$avNNet$predictors
function (x, ...) 
x$names

$avNNet$levels
function (x) 
x$model[[1]]$lev

$avNNet$tags
[1] "Neural Network"       "Ensemble Model"       "Bagging"             
[4] "L2 Regularization"    "Accepts Case Weights"

$avNNet$sort
function (x) 
x[order(x$size, -x$decay), ]


$awnb
$awnb$label
[1] "Naive Bayes Classifier with Attribute Weighting"

$awnb$library
[1] "bnclassify"

$awnb$type
[1] "Classification"

$awnb$parameters
  parameter   class               label
1    smooth numeric Smoothing Parameter

$awnb$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(smooth = 0:(len - 1))
    }
    else {
        out <- data.frame(smooth = runif(len, min = 0, max = 10))
    }
    out
}

$awnb$loop
NULL

$awnb$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    struct <- nb(class = ".outcome", dataset = dat)
    lpawnb(struct, dat, smooth = param$smooth, trees = 10, bootstrap_size = 0.5, 
        ...)
}

$awnb$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$awnb$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$awnb$levels
function (x) 
x$obsLevels

$awnb$predictors
function (x, s = NULL, ...) 
x$xNames

$awnb$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$awnb$sort
function (x) 
x[order(x[, 1]), ]


$awtan
$awtan$label
[1] "Tree Augmented Naive Bayes Classifier with Attribute Weighting"

$awtan$library
[1] "bnclassify"

$awtan$type
[1] "Classification"

$awtan$parameters
  parameter     class               label
1     score character      Score Function
2    smooth   numeric Smoothing Parameter

$awtan$grid
function (x, y, len = NULL, search = "grid") 
{
    out <- expand.grid(score = c("loglik", "bic", "aic"), smooth = 1:2)
}

$awtan$loop
NULL

$awtan$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    struct <- tan_cl(class = ".outcome", dataset = dat, score = as.character(param$score))
    lpawnb(struct, dat, smooth = param$smooth, trees = 10, bootstrap_size = 0.5, 
        ...)
}

$awtan$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$awtan$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$awtan$levels
function (x) 
x$obsLevels

$awtan$predictors
function (x, s = NULL, ...) 
x$xNames

$awtan$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$awtan$sort
function (x) 
x[order(x[, 1]), ]


$bag
$bag$label
[1] "Bagged Model"

$bag$library
[1] "caret"

$bag$loop
NULL

$bag$type
[1] "Regression"     "Classification"

$bag$parameters
  parameter   class                         label
1      vars numeric #Randomly Selected Predictors

$bag$grid
function (x, y, len = NULL, search = "grid") 
data.frame(vars = ncol(x))

$bag$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- bag(x, y, vars = param$vars, ...)
    out$xNames <- colnames(x)
    out
}

$bag$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$bag$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$bag$predictors
function (x, ...) 
x$xNames

$bag$levels
function (x) 
x$obsLevels

$bag$varImp
NULL

$bag$tags
[1] "Bagging"        "Ensemble Model"

$bag$sort
function (x) 
x


$bagEarth
$bagEarth$label
[1] "Bagged MARS"

$bagEarth$library
[1] "earth"

$bagEarth$type
[1] "Regression"     "Classification"

$bagEarth$parameters
  parameter   class          label
1    nprune numeric         #Terms
2    degree numeric Product Degree

$bagEarth$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    mod <- earth(.outcome ~ ., data = dat, pmethod = "none")
    maxTerms <- nrow(mod$dirs)
    maxTerms <- min(200, floor(maxTerms * 0.75) + 2)
    if (search == "grid") {
        out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, 
            length = len))), degree = 1)
    }
    else {
        out <- data.frame(nprune = sample(2:maxTerms, size = len, 
            replace = TRUE), degree = sample(1:2, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$bagEarth$loop
function (grid) 
{
    deg <- unique(grid$degree)
    loop <- data.frame(degree = deg)
    loop$nprune <- NA
    submodels <- vector(mode = "list", length = length(deg))
    for (i in seq(along = deg)) {
        np <- grid[grid$degree == deg[i], "nprune"]
        loop$nprune[loop$degree == deg[i]] <- np[which.max(np)]
        submodels[[i]] <- data.frame(nprune = np[-which.max(np)])
    }
    list(loop = loop, submodels = submodels)
}

$bagEarth$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots$keepxy <- TRUE
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(x = x, y = y, degree = param$degree, 
        nprune = param$nprune), theDots)
    if (is.factor(y)) 
        modelArgs$glm <- list(family = binomial)
    tmp <- do.call("bagEarth", modelArgs)
    tmp$call["nprune"] <- param$nprune
    tmp$call["degree"] <- param$degree
    tmp
}

$bagEarth$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata)
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- if (is.matrix(out)) 
            out[, 1]
        else out
        for (j in seq(along = submodels$nprune)) {
            prunedFit <- update(modelFit, nprune = submodels$nprune[j])
            if (modelFit$problemType == "Classification") {
                tmp[[j + 1]] <- predict(prunedFit, newdata, type = "class")
            }
            else {
                tmp[[j + 1]] <- predict(prunedFit, newdata)
            }
        }
        out <- tmp
    }
    out
}

$bagEarth$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nprune)) {
            prunedFit <- update(modelFit, nprune = submodels$nprune[j])
            tmp2 <- predict(prunedFit, newdata, type = "response")
            tmp2 <- cbind(1 - tmp2, tmp2)
            colnames(tmp2) <- modelFit$obsLevels
            tmp[[j + 1]] <- tmp2
        }
        out <- tmp
    }
    out
}

$bagEarth$predictors
function (x, ...) 
{
    predEarth <- function(x) {
        vi <- varImp(x)
        notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 
            0)))))
        if (length(notZero) > 0) 
            rownames(vi)[notZero]
        else NULL
    }
    eachFit <- lapply(x$fit, predEarth)
    unique(unlist(eachFit))
}

$bagEarth$varImp
function (object, ...) 
{
    allImp <- lapply(object$fit, varImp, ...)
    impDF <- as.data.frame(allImp)
    meanImp <- apply(impDF, 1, mean)
    out <- data.frame(Overall = meanImp)
    rownames(out) <- names(meanImp)
    out
}

$bagEarth$levels
function (x) 
x$levels

$bagEarth$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Ensemble Model"                          
[3] "Implicit Feature Selection"              
[4] "Bagging"                                 
[5] "Accepts Case Weights"                    

$bagEarth$sort
function (x) 
x[order(x$degree, x$nprune), ]

$bagEarth$oob
function (x) 
apply(x$oob, 2, function(x) quantile(x, probs = 0.5))


$bagEarthGCV
$bagEarthGCV$label
[1] "Bagged MARS using gCV Pruning"

$bagEarthGCV$library
[1] "earth"

$bagEarthGCV$type
[1] "Regression"     "Classification"

$bagEarthGCV$parameters
  parameter   class          label
1    degree numeric Product Degree

$bagEarthGCV$grid
function (x, y, len = NULL, search = "grid") 
data.frame(degree = 1)

$bagEarthGCV$loop
NULL

$bagEarthGCV$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        mod <- bagEarth(x, y, degree = param$degree, glm = list(family = binomial, 
            maxit = 100), weights = wts, ...)
    }
    else {
        mod <- bagEarth(x, y, degree = param$degree, weights = wts, 
            ...)
    }
    mod
}

$bagEarthGCV$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata)
    }
    out
}

$bagEarthGCV$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$bagEarthGCV$predictors
function (x, ...) 
{
    predEarth <- function(x) {
        vi <- varImp(x)
        notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 
            0)))))
        if (length(notZero) > 0) 
            rownames(vi)[notZero]
        else NULL
    }
    eachFit <- lapply(x$fit, predEarth)
    unique(unlist(eachFit))
}

$bagEarthGCV$varImp
function (object, ...) 
{
    allImp <- lapply(object$fit, varImp, ...)
    impDF <- as.data.frame(allImp)
    meanImp <- apply(impDF, 1, mean)
    out <- data.frame(Overall = meanImp)
    rownames(out) <- names(meanImp)
    out
}

$bagEarthGCV$levels
function (x) 
x$levels

$bagEarthGCV$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Ensemble Model"                          
[3] "Implicit Feature Selection"              
[4] "Bagging"                                 
[5] "Accepts Case Weights"                    

$bagEarthGCV$sort
function (x) 
x[order(x$degree), ]

$bagEarthGCV$oob
function (x) 
apply(x$oob, 2, function(x) quantile(x, probs = 0.5))


$bagFDA
$bagFDA$label
[1] "Bagged Flexible Discriminant Analysis"

$bagFDA$library
[1] "earth" "mda"  

$bagFDA$loop
NULL

$bagFDA$type
[1] "Classification"

$bagFDA$parameters
  parameter   class          label
1    degree numeric Product Degree
2    nprune numeric         #Terms

$bagFDA$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (!is.data.frame(x)) 
        as.data.frame(x)
    else x
    dat$.outcome <- y
    mod <- fda(.outcome ~ ., data = dat, method = earth, pmethod = "none")
    maxTerms <- nrow(mod$fit$dirs) - 1
    maxTerms <- min(200, floor(maxTerms * 0.75) + 2)
    if (search == "grid") {
        out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, 
            length = len))), degree = 1)
    }
    else {
        out <- data.frame(nprune = sample(2:maxTerms, size = len, 
            replace = TRUE), degree = sample(1:2, size = len, 
            replace = TRUE))
    }
    out
}

$bagFDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    bagFDA(.outcome ~ ., data = dat, degree = param$degree, nprune = param$nprune, 
        weights = wts, ...)
}

$bagFDA$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Ensemble Model"                          
[3] "Implicit Feature Selection"              
[4] "Bagging"                                 
[5] "Accepts Case Weights"                    

$bagFDA$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$bagFDA$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "probs")

$bagFDA$predictors
function (x, ...) 
{
    fdaPreds <- function(x) {
        code <- getModelInfo("earth", regex = FALSE)[[1]]$predictors
        tmp <- predictors(x$terms)
        out <- if (class(x$fit) == "earth") 
            code(x$fit)
        else tmp
        out
    }
    eachFit <- lapply(x$fit, fdaPreds)
    unique(unlist(eachFit))
}

$bagFDA$varImp
function (object, ...) 
{
    allImp <- lapply(object$fit, varImp, ...)
    impDF <- as.data.frame(allImp)
    meanImp <- apply(impDF, 1, mean)
    out <- data.frame(Overall = meanImp)
    rownames(out) <- names(meanImp)
    out
}

$bagFDA$levels
function (x) 
x$levels

$bagFDA$sort
function (x) 
x[order(x$degree, x$nprune), ]

$bagFDA$oob
function (x) 
apply(x$oob, 2, function(x) quantile(x, probs = 0.5))


$bagFDAGCV
$bagFDAGCV$label
[1] "Bagged FDA using gCV Pruning"

$bagFDAGCV$library
[1] "earth"

$bagFDAGCV$type
[1] "Classification"

$bagFDAGCV$parameters
  parameter   class          label
1    degree numeric Product Degree

$bagFDAGCV$grid
function (x, y, len = NULL, search = "grid") 
data.frame(degree = 1)

$bagFDAGCV$loop
NULL

$bagFDAGCV$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    bagFDA(.outcome ~ ., data = dat, degree = param$degree, weights = wts, 
        ...)
}

$bagFDAGCV$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Ensemble Model"                          
[3] "Implicit Feature Selection"              
[4] "Bagging"                                 

$bagFDAGCV$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$bagFDAGCV$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "probs")

$bagFDAGCV$predictors
function (x, ...) 
{
    fdaPreds <- function(x) {
        code <- getModelInfo("earth", regex = FALSE)[[1]]$predictors
        tmp <- predictors(x$terms)
        out <- if (class(x$fit) == "earth") 
            code(x$fit)
        else tmp
        out
    }
    eachFit <- lapply(x$fit, fdaPreds)
    unique(unlist(eachFit))
}

$bagFDAGCV$varImp
function (object, ...) 
{
    allImp <- lapply(object$fit, varImp, ...)
    impDF <- as.data.frame(allImp)
    meanImp <- apply(impDF, 1, mean)
    out <- data.frame(Overall = meanImp)
    rownames(out) <- names(meanImp)
    out
}

$bagFDAGCV$levels
function (x) 
x$levels

$bagFDAGCV$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Ensemble Model"                          
[3] "Implicit Feature Selection"              
[4] "Bagging"                                 
[5] "Accepts Case Weights"                    

$bagFDAGCV$sort
function (x) 
x[order(x$degree), ]

$bagFDAGCV$oob
function (x) 
apply(x$oob, 2, function(x) quantile(x, probs = 0.5))


$bam
$bam$label
[1] "Generalized Additive Model using Splines"

$bam$library
[1] "mgcv"

$bam$loop
NULL

$bam$type
[1] "Regression"     "Classification"

$bam$parameters
  parameter     class             label
1    select   logical Feature Selection
2    method character            Method

$bam$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(select = c(TRUE, FALSE), method = "GCV.Cp")
    }
    else {
        out <- data.frame(select = sample(c(TRUE, FALSE), size = len, 
            replace = TRUE), method = sample(c("GCV.Cp", "ML", 
            "REML"), size = len, replace = TRUE))
    }
    out[!duplicated(out), ]
}

$bam$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    modForm <- caret:::smootherFormula(x)
    if (is.factor(y)) {
        dat$.outcome <- ifelse(y == lev[1], 0, 1)
        dist <- binomial()
    }
    else {
        dat$.outcome <- y
        dist <- gaussian()
    }
    modelArgs <- list(formula = modForm, data = dat, select = param$select, 
        method = as.character(param$method))
    theDots <- list(...)
    if (!any(names(theDots) == "family")) 
        modelArgs$family <- dist
    modelArgs <- c(modelArgs, theDots)
    out <- do.call(getFromNamespace("bam", "mgcv"), modelArgs)
    out
}

$bam$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- predict(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$bam$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$bam$predictors
function (x, ...) 
{
    predictors(x$terms)
}

$bam$levels
function (x) 
x$obsLevels

$bam$varImp
function (object, ...) 
{
    smoothed <- summary(object)$s.table[, "p-value", drop = FALSE]
    linear <- summary(object)$p.table
    linear <- linear[, grepl("^Pr", colnames(linear)), drop = FALSE]
    gams <- rbind(smoothed, linear)
    gams <- gams[rownames(gams) != "(Intercept)", , drop = FALSE]
    rownames(gams) <- gsub("^s\\(", "", rownames(gams))
    rownames(gams) <- gsub("\\)$", "", rownames(gams))
    colnames(gams)[1] <- "Overall"
    gams <- as.data.frame(gams)
    gams$Overall <- -log10(gams$Overall)
    allPreds <- colnames(attr(object$terms, "factors"))
    extras <- allPreds[!(allPreds %in% rownames(gams))]
    if (any(extras)) {
        tmp <- data.frame(Overall = rep(NA, length(extras)))
        rownames(tmp) <- extras
        gams <- rbind(gams, tmp)
    }
    gams
}

$bam$notes
[1] "Which terms enter the model in a nonlinear manner is determined by the number of unique values for the predictor. For example, if a predictor only has four unique values, most basis expansion method will fail because there are not enough granularity in the data. By default, a predictor must have at least 10 unique values to be used in a nonlinear basis expansion."

$bam$tags
[1] "Generalized Linear Model"   "Generalized Additive Model"

$bam$sort
function (x) 
x


$bartMachine
$bartMachine$label
[1] "Bayesian Additive Regression Trees"

$bartMachine$library
[1] "bartMachine"

$bartMachine$loop
NULL

$bartMachine$type
[1] "Classification" "Regression"    

$bartMachine$parameters
  parameter   class                              label
1 num_trees numeric                             #Trees
2         k numeric                     Prior Boundary
3     alpha numeric  Base Terminal Node Hyperparameter
4      beta numeric Power Terminal Node Hyperparameter
5        nu numeric                 Degrees of Freedom

$bartMachine$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num_trees = 50, k = (1:len) + 1, alpha = seq(0.9, 
            0.99, length = len), beta = seq(1, 3, length = len), 
            nu = (1:len) + 1)
    }
    else {
        out <- data.frame(num_trees = sample(10:100, replace = TRUE, 
            size = len), k = runif(len, min = 0, max = 5), alpha = runif(len, 
            min = 0.9, max = 1), beta = runif(len, min = 0, max = 4), 
            nu = runif(len, min = 0, max = 5))
    }
    if (is.factor(y)) {
        out$k <- NA
        out$nu <- NA
    }
    out <- out[!duplicated(out), ]
}

$bartMachine$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    out <- if (is.factor(y)) {
        bartMachine(X = x, y = y, num_trees = param$num_trees, 
            alpha = param$alpha, beta = param$beta, ...)
    }
    else {
        bartMachine(X = x, y = y, num_trees = param$num_trees, 
            k = param$k, alpha = param$alpha, beta = param$beta, 
            nu = param$nu, ...)
    }
    out
}

$bartMachine$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- if (is.factor(modelFit$y)) 
        predict(modelFit, newdata, type = "class")
    else predict(modelFit, newdata)
}

$bartMachine$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "prob")
    out <- data.frame(y1 = 1 - out, y2 = out)
    colnames(out) <- modelFit$y_levels
    out
}

$bartMachine$predictors
function (x, ...) 
colnames(x$X)

$bartMachine$varImp
function (object, ...) 
{
    imps <- investigate_var_importance(object, plot = FALSE)
    imps <- imps$avg_var_props - 1.96 * imps$sd_var_props
    missing_x <- !(colnames(object$X) %in% names(imps))
    if (any(missing_x)) {
        imps2 <- rep(0, sum(missing_x))
        names(imps2) <- colnames(object$X)[missing_x]
        imps <- c(imps, imps2)
    }
    out <- data.frame(Overall = as.vector(imps))
    rownames(out) <- names(imps)
    out
}

$bartMachine$levels
function (x) 
x$y_levels

$bartMachine$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Bayesian Model"             "Two Class Only"            

$bartMachine$sort
function (x) 
x[order(-x[, "num_trees"]), ]


$bayesglm
$bayesglm$label
[1] "Bayesian Generalized Linear Model"

$bayesglm$library
[1] "arm"

$bayesglm$loop
NULL

$bayesglm$type
[1] "Regression"     "Classification"

$bayesglm$parameters
  parameter     class     label
1 parameter character parameter

$bayesglm$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$bayesglm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (!any(names(theDots) == "family")) 
        theDots$family <- if (is.factor(dat$.outcome)) 
            binomial()
        else gaussian()
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat), theDots)
    out <- do.call("bayesglm", modelArgs)
    out$call <- NULL
    out
}

$bayesglm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- predict(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$bayesglm$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$bayesglm$levels
function (x) 
x$obsLevels

$bayesglm$trim
function (x) 
{
    x$y = c()
    x$model = c()
    x$residuals = c()
    x$fitted.values = c()
    x$effects = c()
    x$qr$qr = c()
    x$linear.predictors = c()
    x$weights = c()
    x$prior.weights = c()
    x$data = c()
    x$family$variance = c()
    x$family$dev.resids = c()
    x$family$aic = c()
    x$family$validmu = c()
    x$family$simulate = c()
    attr(x$terms, ".Environment") = c()
    attr(x$formula, ".Environment") = c()
    x$R <- c()
    x$xNames <- c()
    x$xlevels <- c()
    x
}

$bayesglm$tags
[1] "Generalized Linear Model" "Logistic Regression"     
[3] "Linear Classifier"        "Bayesian Model"          
[5] "Accepts Case Weights"    

$bayesglm$sort
function (x) 
x


$bdk
$bdk$label
[1] "Self-Organizing Map"

$bdk$library
[1] "kohonen"

$bdk$loop
NULL

$bdk$type
[1] "Classification" "Regression"    

$bdk$parameters
  parameter     class    label
1      xdim   numeric      Row
2      ydim   numeric  Columns
3   xweight   numeric X Weight
4      topo character Topology

$bdk$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(xdim = 1:len, ydim = 2:(len + 1), 
            xweight = seq(0.5, 0.9, length = len), topo = "hexagonal")
        out <- subset(out, xdim <= ydim)
    }
    else {
        out <- data.frame(xdim = sample(1:len, size = len * 10, 
            replace = TRUE), ydim = sample(1:len, size = len * 
            10, replace = TRUE), topo = sample(c("rectangular", 
            "hexagonal"), size = len * 10, replace = TRUE), xweight = runif(len * 
            10, min = 0.5, max = 1))
        out <- subset(out, xdim <= ydim & xdim * ydim < nrow(x))
        out <- out[1:max(nrow(out), len), ]
    }
    out
}

$bdk$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
bdk(as.matrix(x), Y = if (is.factor(y)) classvec2classmat(y) else y, 
    xweight = param$xweight, contin = !is.factor(y), grid = somgrid(param$xdim, 
        param$ydim, as.character(param$topo)), ...)

$bdk$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))$prediction
    if (modelFit$problemType == "Classification") {
        out <- as.character(out)
    }
    else {
        if (is.matrix(out)) 
            out <- out[, 1]
    }
    out
}

$bdk$prob
function (modelFit, newdata, submodels = NULL) 
{
    preds <- predict(modelFit, as.matrix(newdata))
    preds$unit.predictions[preds$unit.classif, , drop = FALSE]
}

$bdk$levels
function (x) 
x$obsLevels

$bdk$tags
[1] "Self-Organising Maps"

$bdk$sort
function (x) 
x[order(x$xdim, x$ydim), ]


$binda
$binda$label
[1] "Binary Discriminant Analysis"

$binda$library
[1] "binda"

$binda$loop
NULL

$binda$type
[1] "Classification"

$binda$parameters
     parameter   class               label
1 lambda.freqs numeric Shrinkage Intensity

$binda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(lambda.freqs = seq(0, 1, length = len))
    }
    else {
        out <- data.frame(lambda.freqs = runif(len, min = 0, 
            max = 1))
    }
    out
}

$binda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    binda(as.matrix(x), y, lambda.freqs = param$lambda.freqs, 
        ...)
}

$binda$predict
function (modelFit, newdata, submodels = NULL) 
{
    as.character(predict(modelFit, as.matrix(newdata))$class)
}

$binda$prob
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, as.matrix(newdata))$posterior
}

$binda$varImp
NULL

$binda$predictors
function (x, ...) 
rownames(x$logp0)

$binda$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else names(x$logfreqs)

$binda$tags
[1] "Discriminant Analysis"  "Two Class Only"         "Binary Predictors Only"

$binda$sort
function (x) 
x


$blackboost
$blackboost$label
[1] "Boosted Tree"

$blackboost$library
[1] "party"  "mboost" "plyr"  

$blackboost$type
[1] "Regression"     "Classification"

$blackboost$parameters
  parameter   class          label
1     mstop numeric         #Trees
2  maxdepth numeric Max Tree Depth

$blackboost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(maxdepth = seq(1, len), mstop = floor((1:len) * 
            50))
    }
    else {
        out <- data.frame(mstop = sample(1:1000, replace = TRUE, 
            size = len), maxdepth = sample(1:10, replace = TRUE, 
            size = len))
    }
    out
}

$blackboost$loop
function (grid) 
{
    loop <- ddply(grid, .(maxdepth), function(x) c(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        index <- which(grid$maxdepth == loop$maxdepth[i])
        subStops <- grid[index, "mstop"]
        submodels[[i]] <- data.frame(mstop = subStops[subStops != 
            loop$mstop[i]])
    }
    list(loop = loop, submodels = submodels)
}

$blackboost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "tree_controls")) {
        theDots$tree_controls$maxdepth <- param$maxdepth
        treeCtl <- theDots$tree_controls
        theDots$tree_controls <- NULL
    }
    else treeCtl <- ctree_control(maxdepth = param$maxdepth)
    if (any(names(theDots) == "control")) {
        theDots$control$mstop <- param$mstop
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- boost_control(mstop = param$mstop)
    if (!any(names(theDots) == "family")) {
        if (is.factor(y)) {
            theDots$family <- if (length(lev) == 2) 
                Binomial()
            else Multinomial()
        }
        else theDots$family <- GaussReg()
    }
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = if (!is.data.frame(x)) as.data.frame(x) else x, 
        control = ctl, tree_controls = treeCtl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("blackboost", modelArgs)
    out$call["data"] <- "data"
    out
}

$blackboost$predict
function (modelFit, newdata, submodels = NULL) 
{
    predType <- ifelse(modelFit$problemType == "Classification", 
        "class", "response")
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = predType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- as.vector(out)
        for (j in seq(along = submodels$mstop)) {
            tmp[[j + 1]] <- as.vector(predict(modelFit[submodels$mstop[j]], 
                newdata, type = predType))
        }
        out <- tmp
    }
    out
}

$blackboost$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    lp <- predict(modelFit, newdata)
    out <- cbind(binomial()$linkinv(-lp), 1 - binomial()$linkinv(-lp))
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            tmpProb <- predict(modelFit[submodels$mstop[j]], 
                newdata)
            tmpProb <- cbind(binomial()$linkinv(-tmpProb), 1 - 
                binomial()$linkinv(-tmpProb))
            colnames(tmpProb) <- modelFit$obsLevels
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$blackboost$predictors
function (x, ...) 
{
    strsplit(variable.names(x), ", ")[[1]]
}

$blackboost$levels
function (x) 
levels(x$response)

$blackboost$tags
[1] "Tree-Based Model"     "Ensemble Model"       "Boosting"            
[4] "Accepts Case Weights"

$blackboost$sort
function (x) 
x[order(x$mstop, x$maxdepth), ]


$blasso
$blasso$label
[1] "The Bayesian lasso"

$blasso$library
[1] "monomvn"

$blasso$type
[1] "Regression"

$blasso$parameters
  parameter   class              label
1  sparsity numeric Sparsity Threshold

$blasso$grid
function (x, y, len = NULL, search = "grid") 
{
    if (len == 1) 
        return(data.frame(sparsity = 0.5))
    if (search == "grid") {
        out <- expand.grid(sparsity = seq(0.3, 0.7, length = len))
    }
    else {
        out <- data.frame(sparsity = runif(len, min = 0, max = 1))
    }
    out
}

$blasso$loop
function (grid) 
{
    grid <- grid[order(grid$sparsity, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$blasso$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    mod <- blasso(as.matrix(x), y, ...)
    mod$.percent <- apply(mod$beta, 2, function(x) mean(x != 
        0))
    mod$.sparsity <- param$sparsity
    mod$.betas <- colMeans(mod$beta)
    mod
}

$blasso$predict
function (modelFit, newdata, submodels = NULL) 
{
    betas <- modelFit$.betas
    betas[modelFit$.percent <= modelFit$.sparsity] <- 0
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- (newdata %*% betas)[, 1]
    if (modelFit$icept) 
        out <- out + mean(modelFit$mu)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels))
        for (i in 1:nrow(submodels)) {
            betas <- modelFit$.betas
            betas[modelFit$.percent <= submodels$sparsity[i]] <- 0
            tmp[[i]] <- (newdata %*% betas)[, 1]
            if (modelFit$icept) 
                tmp[[i]] <- tmp[[i]] + mean(modelFit$mu)
        }
        out <- c(list(out), tmp)
    }
    out
}

$blasso$predictors
function (x, s = NULL, ...) 
{
    x$xNames[x$.percent <= x$.sparsity]
}

$blasso$notes
[1] "This model creates predictions using the mean of the posterior distributions but sets some parameters specifically to zero based on the tuning parameter `sparsity`. For example, when `sparsity = .5`, only coefficients where at least half the posterior estimates are nonzero are used."

$blasso$tags
[1] "Linear Regression"          "Bayesian Model"            
[3] "Implicit Feature Selection" "L1 Regularization"         

$blasso$prob
NULL

$blasso$sort
function (x) 
x[order(-x$sparsity), ]


$blassoAveraged
$blassoAveraged$label
[1] "Bayesian Ridge Regression (Model Averaged)"

$blassoAveraged$library
[1] "monomvn"

$blassoAveraged$type
[1] "Regression"

$blassoAveraged$parameters
  parameter     class     label
1 parameter character parameter

$blassoAveraged$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$blassoAveraged$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- blasso(as.matrix(x), y, ...)
    out
}

$blassoAveraged$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- modelFit$beta %*% t(newdata)
    if (modelFit$icept) 
        out <- out + (matrix(1, ncol = ncol(out), nrow = nrow(out)) * 
            modelFit$mu)
    apply(out, 2, mean)
}

$blassoAveraged$predictors
function (x, s = NULL, ...) 
{
    x$xNames[apply(x$beta, 2, function(x) any(x != 0))]
}

$blassoAveraged$notes
[1] "This model makes predictions by averaging the predictions based on the posterior estimates of the regression coefficients. While it is possible that some of these posterior estimates are zero for non-informative predictors, the final predicted value may be a function of many (or even all) predictors. "

$blassoAveraged$tags
[1] "Linear Regression" "Bayesian Model"    "L1 Regularization"

$blassoAveraged$prob
NULL

$blassoAveraged$sort
function (x) 
x


$Boruta
$Boruta$label
[1] "Random Forest with Additional Feature Selection"

$Boruta$library
[1] "Boruta"       "randomForest"

$Boruta$type
[1] "Regression"     "Classification"

$Boruta$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$Boruta$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), replace = TRUE, 
            size = len)))
    }
    out
}

$Boruta$loop
NULL

$Boruta$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    fs <- Boruta(x, y, mtry = param$mtry, ...)
    keepers <- as.character(names(fs$finalDecision)[fs$finalDecision == 
        "Confirmed"])
    out <- randomForest(x[, keepers, drop = FALSE], y, mtry = param$mtry, 
        ...)
    out$Boruta <- fs
    out
}

$Boruta$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$Boruta$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$Boruta$tags
[1] "Tree-Based Model"          "Ensemble Model"           
[3] "Feature Selection Wrapper" "Random Forest"            

$Boruta$levels
function (x) 
x$classes

$Boruta$sort
function (x) 
x[order(x[, 1]), ]


$bridge
$bridge$label
[1] "Bayesian Ridge Regression"

$bridge$library
[1] "monomvn"

$bridge$type
[1] "Regression"

$bridge$parameters
  parameter     class     label
1 parameter character parameter

$bridge$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$bridge$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- bridge(as.matrix(x), y, ...)
    out
}

$bridge$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- modelFit$beta %*% t(newdata)
    if (modelFit$icept) 
        out <- out + (matrix(1, ncol = ncol(out), nrow = nrow(out)) * 
            modelFit$mu)
    apply(out, 2, mean)
}

$bridge$predictors
function (x, s = NULL, ...) 
{
    x$xNames
}

$bridge$tags
[1] "Linear Regression" "Bayesian Model"    "L2 Regularization"

$bridge$prob
NULL

$bridge$sort
function (x) 
x


$brnn
$brnn$label
[1] "Bayesian Regularized Neural Networks"

$brnn$library
[1] "brnn"

$brnn$type
[1] "Regression"

$brnn$parameters
  parameter   class     label
1   neurons numeric # Neurons

$brnn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(neurons = 1:len)
    }
    else {
        out <- data.frame(neurons = sample(1:20, replace = TRUE, 
            size = len))
    }
    out
}

$brnn$loop
NULL

$brnn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
brnn(as.matrix(x), y, neurons = param$neurons, ...)

$brnn$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata))

$brnn$prob
NULL

$brnn$predictors
function (x, s = NULL, ...) 
names(x$x_spread)

$brnn$tags
[1] "Bayesian Model" "Neural Network" "Regularization"

$brnn$prob
NULL

$brnn$sort
function (x) 
x[order(x[, 1]), ]


$BstLm
$BstLm$label
[1] "Boosted Linear Model"

$BstLm$library
[1] "bst"  "plyr"

$BstLm$type
[1] "Regression"     "Classification"

$BstLm$parameters
  parameter   class                 label
1     mstop numeric # Boosting Iterations
2        nu numeric             Shrinkage

$BstLm$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mstop = floor((1:len) * 50), nu = 0.1)
    }
    else {
        out <- data.frame(mstop = floor(runif(len, min = 1, max = 500)), 
            nu = runif(len, min = 0.001, max = 0.6))
    }
    out
}

$BstLm$loop
function (grid) 
{
    loop <- ddply(grid, .(nu), function(x) c(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        index <- which(grid$nu == loop$nu[i])
        subTrees <- grid[index, "mstop"]
        submodels[[i]] <- data.frame(mstop = subTrees[subTrees != 
            loop$mstop[i]])
    }
    list(loop = loop, submodels = submodels)
}

$BstLm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    modDist <- if (is.factor(y)) 
        "hinge"
    else "gaussian"
    y <- if (is.factor(y)) 
        ifelse(y == lev[1], 1, -1)
    else y
    if (any(names(theDots) == "ctrl")) {
        theDots$ctrl$mstop <- param$mstop
        theDots$ctrl$nu <- param$nu
    }
    else {
        theDots$ctrl <- bst_control(mstop = param$mstop, nu = param$nu)
    }
    modArgs <- list(x = x, y = y, family = modDist, learner = "ls")
    modArgs <- c(modArgs, theDots)
    do.call("bst", modArgs)
}

$BstLm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class", mstop = modelFit$submodels$mstop)
        out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response", 
            mstop = modelFit$submodels$mstop)
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            if (modelFit$problemType == "Classification") {
                bstPred <- predict(modelFit, newdata, type = "class", 
                  mstop = submodels$mstop[j])
                tmp[[j + 1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], 
                  modelFit$obsLevels[2])
            }
            else {
                tmp[[j + 1]] <- predict(modelFit, newdata, type = "response", 
                  mstop = submodels$mstop[j])
            }
        }
        out <- tmp
    }
    out
}

$BstLm$levels
function (x) 
x$obsLevels

$BstLm$tags
[1] "Linear Regression"          "Ensemble Model"            
[3] "Boosting"                   "Implicit Feature Selection"

$BstLm$prob
NULL

$BstLm$sort
function (x) 
x[order(x$mstop, x$nu), ]


$bstSm
$bstSm$label
[1] "Boosted Smoothing Spline"

$bstSm$library
[1] "bst"  "plyr"

$bstSm$type
[1] "Regression"     "Classification"

$bstSm$parameters
  parameter   class                 label
1     mstop numeric # Boosting Iterations
2        nu numeric             Shrinkage

$bstSm$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mstop = floor((1:len) * 50), nu = 0.1)
    }
    else {
        out <- data.frame(mstop = sample(1:500, replace = TRUE, 
            size = len), nu = runif(len, min = 0.001, max = 0.6))
    }
    out
}

$bstSm$loop
function (grid) 
{
    loop <- ddply(grid, .(nu), function(x) c(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        index <- which(grid$nu == loop$nu[i])
        subTrees <- grid[index, "mstop"]
        submodels[[i]] <- data.frame(mstop = subTrees[subTrees != 
            loop$mstop[i]])
    }
    list(loop = loop, submodels = submodels)
}

$bstSm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    modDist <- if (is.factor(y)) 
        "hinge"
    else "gaussian"
    y <- if (is.factor(y)) 
        ifelse(y == lev[1], 1, -1)
    else y
    if (any(names(theDots) == "ctrl")) {
        theDots$ctrl$mstop <- param$mstop
        theDots$ctrl$nu <- param$nu
    }
    else {
        theDots$ctrl <- bst_control(mstop = param$mstop, nu = param$nu)
    }
    modArgs <- list(x = x, y = y, family = modDist, learner = "sm")
    modArgs <- c(modArgs, theDots)
    do.call("bst", modArgs)
}

$bstSm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class", mstop = modelFit$submodels$mstop)
        out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response", 
            mstop = modelFit$submodels$mstop)
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            if (modelFit$problemType == "Classification") {
                bstPred <- predict(modelFit, newdata, type = "class", 
                  mstop = submodels$mstop[j])
                tmp[[j + 1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], 
                  modelFit$obsLevels[2])
            }
            else {
                tmp[[j + 1]] <- predict(modelFit, newdata, type = "response", 
                  mstop = submodels$mstop[j])
            }
        }
        out <- tmp
    }
    out
}

$bstSm$levels
function (x) 
x$obsLevels

$bstSm$tags
[1] "Ensemble Model"             "Boosting"                  
[3] "Implicit Feature Selection"

$bstSm$prob
NULL

$bstSm$sort
function (x) 
x[order(x$mstop, x$nu), ]


$bstTree
$bstTree$label
[1] "Boosted Tree"

$bstTree$library
[1] "bst"  "plyr"

$bstTree$type
[1] "Regression"     "Classification"

$bstTree$parameters
  parameter   class                 label
1     mstop numeric # Boosting Iterations
2  maxdepth numeric        Max Tree Depth
3        nu numeric             Shrinkage

$bstTree$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mstop = floor((1:len) * 50), maxdepth = seq(1, 
            len), nu = 0.1)
    }
    else {
        out <- data.frame(mstop = sample(1:500, replace = TRUE, 
            size = len), maxdepth = sample(1:10, replace = TRUE, 
            size = len), nu = runif(len, min = 0.001, max = 0.6))
    }
    out
}

$bstTree$loop
function (grid) 
{
    loop <- ddply(grid, .(maxdepth, nu), function(x) c(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        index <- which(grid$maxdepth == loop$maxdepth[i] & grid$nu == 
            loop$nu[i])
        subTrees <- grid[index, "mstop"]
        submodels[[i]] <- data.frame(mstop = subTrees[subTrees != 
            loop$mstop[i]])
    }
    list(loop = loop, submodels = submodels)
}

$bstTree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    modDist <- if (is.factor(y)) 
        "hinge"
    else "gaussian"
    y <- if (is.factor(y)) 
        ifelse(y == lev[1], 1, -1)
    else y
    if (any(names(theDots) == "ctrl")) {
        theDots$ctrl$mstop <- param$mstop
        theDots$ctrl$nu <- param$nu
    }
    else {
        theDots$ctrl <- bst_control(mstop = param$mstop, nu = param$nu)
    }
    if (any(names(theDots) == "control.tree")) {
        theDots$control.tree$maxdepth <- param$maxdepth
    }
    else {
        theDots$control.tree <- list(maxdepth = param$maxdepth)
    }
    modArgs <- list(x = x, y = y, family = modDist, learner = "tree")
    modArgs <- c(modArgs, theDots)
    do.call("bst", modArgs)
}

$bstTree$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class", mstop = modelFit$submodels$mstop)
        out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response", 
            mstop = modelFit$submodels$mstop)
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            if (modelFit$problemType == "Classification") {
                bstPred <- predict(modelFit, newdata, type = "class", 
                  mstop = submodels$mstop[j])
                tmp[[j + 1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], 
                  modelFit$obsLevels[2])
            }
            else {
                tmp[[j + 1]] <- predict(modelFit, newdata, type = "response", 
                  mstop = submodels$mstop[j])
            }
        }
        out <- tmp
    }
    out
}

$bstTree$levels
function (x) 
x$obsLevels

$bstTree$tags
[1] "Tree-Based Model" "Ensemble Model"   "Boosting"        

$bstTree$prob
NULL

$bstTree$sort
function (x) 
x[order(x$mstop, x$maxdepth, x$nu), ]


$C5.0
$C5.0$label
[1] "C5.0"

$C5.0$library
[1] "C50"  "plyr"

$C5.0$loop
function (grid) 
{
    loop <- ddply(grid, c("model", "winnow"), function(x) c(trials = max(x$trials)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$trials)) {
        index <- which(grid$model == loop$model[i] & grid$winnow == 
            loop$winnow[i])
        trials <- grid[index, "trials"]
        submodels[[i]] <- data.frame(trials = trials[trials != 
            loop$trials[i]])
    }
    list(loop = loop, submodels = submodels)
}

$C5.0$type
[1] "Classification"

$C5.0$parameters
  parameter     class                 label
1    trials   numeric # Boosting Iterations
2     model character            Model Type
3    winnow   logical                Winnow

$C5.0$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        c5seq <- if (len == 1) 
            1
        else c(1, 10 * ((2:min(len, 11)) - 1))
        out <- expand.grid(trials = c5seq, model = c("tree", 
            "rules"), winnow = c(TRUE, FALSE))
    }
    else {
        out <- data.frame(trials = sample(1:100, replace = TRUE, 
            size = len), model = sample(c("tree", "rules"), replace = TRUE, 
            size = len), winnow = sample(c(TRUE, FALSE), replace = TRUE, 
            size = len))
    }
    out
}

$C5.0$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$winnow <- param$winnow
    }
    else theDots$control <- C5.0Control(winnow = param$winnow)
    argList <- list(x = x, y = y, weights = wts, trials = param$trials, 
        rules = param$model == "rules")
    argList <- c(argList, theDots)
    do.call("C5.0.default", argList)
}

$C5.0$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (!is.null(submodels)) {
        tmp <- out
        out <- vector(mode = "list", length = nrow(submodels) + 
            1)
        out[[1]] <- tmp
        for (j in seq(along = submodels$trials)) out[[j + 1]] <- as.character(predict(modelFit, 
            newdata, trial = submodels$trials[j]))
    }
    out
}

$C5.0$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "prob")
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$trials)) {
            tmp[[j + 1]] <- predict(modelFit, newdata, type = "prob", 
                trials = submodels$trials[j])
        }
        out <- tmp
    }
    out
}

$C5.0$levels
function (x) 
x$obsLevels

$C5.0$predictors
function (x, ...) 
{
    vars <- C5imp(x, metric = "splits")
    rownames(vars)[vars$Overall > 0]
}

$C5.0$varImp
function (object, ...) 
C5imp(object, ...)

$C5.0$tags
[1] "Tree-Based Model"              "Rule-Based Model"             
[3] "Implicit Feature Selection"    "Boosting"                     
[5] "Ensemble Model"                "Handle Missing Predictor Data"
[7] "Accepts Case Weights"         

$C5.0$sort
function (x) 
{
    x$model <- factor(as.character(x$model), levels = c("rules", 
        "tree"))
    x[order(x$trials, x$model, !x$winnow), ]
}

$C5.0$trim
function (x) 
{
    x$boostResults <- NULL
    x$size <- NULL
    x$call <- NULL
    x$output <- NULL
    x
}


$C5.0Cost
$C5.0Cost$label
[1] "Cost-Sensitive C5.0"

$C5.0Cost$library
[1] "C50"  "plyr"

$C5.0Cost$loop
function (grid) 
{
    loop <- ddply(grid, c("model", "winnow", "cost"), function(x) c(trials = max(x$trials)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$trials)) {
        index <- which(grid$model == loop$model[i] & grid$winnow == 
            loop$winnow[i], grid$cost == loop$cost[i])
        trials <- grid[index, "trials"]
        submodels[[i]] <- data.frame(trials = trials[trials != 
            loop$trials[i]])
    }
    list(loop = loop, submodels = submodels)
}

$C5.0Cost$type
[1] "Classification"

$C5.0Cost$parameters
  parameter     class                 label
1    trials   numeric # Boosting Iterations
2     model character            Model Type
3    winnow   logical                Winnow
4      cost   numeric                  Cost

$C5.0Cost$grid
function (x, y, len = NULL, search = "grid") 
{
    c5seq <- if (len == 1) 
        1
    else c(1, 10 * ((2:min(len, 11)) - 1))
    expand.grid(trials = c5seq, model = c("tree", "rules"), winnow = c(TRUE, 
        FALSE), cost = 1:2)
    if (search == "grid") {
        c5seq <- if (len == 1) 
            1
        else c(1, 10 * ((2:min(len, 11)) - 1))
        out <- expand.grid(trials = c5seq, model = c("tree", 
            "rules"), winnow = c(TRUE, FALSE), cost = 1:2)
    }
    else {
        out <- data.frame(trials = sample(1:100, replace = TRUE, 
            size = len), model = sample(c("tree", "rules"), replace = TRUE, 
            size = len), winnow = sample(c(TRUE, FALSE), replace = TRUE, 
            size = len), cost = runif(len, min = 1, max = 20))
    }
    out
}

$C5.0Cost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$winnow <- param$winnow
    }
    else theDots$control <- C5.0Control(winnow = param$winnow)
    argList <- list(x = x, y = y, weights = wts, trials = param$trials, 
        rules = param$model == "rules")
    cmat <- matrix(c(0, param$cost, 1, 0), ncol = 2)
    rownames(cmat) <- colnames(cmat) <- levels(y)
    if (any(names(theDots) == "cost")) {
        warning("For 'C5.0Cost', the costs are a tuning parameter")
        theDots$costs <- cmat
    }
    else argList$costs <- cmat
    argList <- c(argList, theDots)
    do.call("C5.0.default", argList)
}

$C5.0Cost$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (!is.null(submodels)) {
        tmp <- out
        out <- vector(mode = "list", length = nrow(submodels) + 
            1)
        out[[1]] <- tmp
        for (j in seq(along = submodels$trials)) out[[j + 1]] <- as.character(predict(modelFit, 
            newdata, trial = submodels$trials[j]))
    }
    out
}

$C5.0Cost$prob
NULL

$C5.0Cost$predictors
function (x, ...) 
{
    vars <- C5imp(x, metric = "splits")
    rownames(vars)[vars$Overall > 0]
}

$C5.0Cost$levels
function (x) 
x$obsLevels

$C5.0Cost$varImp
function (object, ...) 
C5imp(object, ...)

$C5.0Cost$tags
[1] "Tree-Based Model"              "Rule-Based Model"             
[3] "Implicit Feature Selection"    "Boosting"                     
[5] "Ensemble Model"                "Cost Sensitive Learning"      
[7] "Two Class Only"                "Handle Missing Predictor Data"
[9] "Accepts Case Weights"         

$C5.0Cost$sort
function (x) 
{
    x$model <- factor(as.character(x$model), levels = c("rules", 
        "tree"))
    x[order(x$trials, x$model, !x$winnow, x$cost), ]
}

$C5.0Cost$trim
function (x) 
{
    x$boostResults <- NULL
    x$size <- NULL
    x$call <- NULL
    x$output <- NULL
    x
}


$C5.0Rules
$C5.0Rules$label
[1] "Single C5.0 Ruleset"

$C5.0Rules$library
[1] "C50"

$C5.0Rules$loop
NULL

$C5.0Rules$type
[1] "Classification"

$C5.0Rules$parameters
  parameter     class label
1 parameter character  none

$C5.0Rules$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$C5.0Rules$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
C5.0(x = x, y = y, weights = wts, rules = TRUE, ...)

$C5.0Rules$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$C5.0Rules$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$C5.0Rules$predictors
function (x, ...) 
{
    vars <- C5imp(x, metric = "splits")
    rownames(vars)[vars$Overall > 0]
}

$C5.0Rules$levels
function (x) 
x$obsLevels

$C5.0Rules$varImp
function (object, ...) 
C5imp(object, ...)

$C5.0Rules$tags
[1] "Rule-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$C5.0Rules$trim
function (x) 
{
    x$boostResults <- NULL
    x$size <- NULL
    x$call <- NULL
    x$output <- NULL
    x
}

$C5.0Rules$sort
function (x) 
x[order(x[, 1]), ]


$C5.0Tree
$C5.0Tree$label
[1] "Single C5.0 Tree"

$C5.0Tree$library
[1] "C50"

$C5.0Tree$loop
NULL

$C5.0Tree$type
[1] "Classification"

$C5.0Tree$parameters
  parameter     class label
1 parameter character  none

$C5.0Tree$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$C5.0Tree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
C5.0(x = x, y = y, weights = wts, ...)

$C5.0Tree$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$C5.0Tree$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$C5.0Tree$predictors
function (x, ...) 
{
    vars <- C5imp(x, metric = "splits")
    rownames(vars)[vars$Overall > 0]
}

$C5.0Tree$levels
function (x) 
x$obsLevels

$C5.0Tree$varImp
function (object, ...) 
C5imp(object, ...)

$C5.0Tree$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$C5.0Tree$sort
function (x) 
x[order(x[, 1]), ]

$C5.0Tree$trim
function (x) 
{
    x$boostResults <- NULL
    x$size <- NULL
    x$call <- NULL
    x$output <- NULL
    x
}


$cforest
$cforest$label
[1] "Conditional Inference Random Forest"

$cforest$library
[1] "party"

$cforest$loop
NULL

$cforest$type
[1] "Classification" "Regression"    

$cforest$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$cforest$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), replace = TRUE, 
            size = len)))
    }
    out
}

$cforest$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "controls")) {
        theDots$controls@gtctrl@mtry <- as.integer(param$mtry)
        ctl <- theDots$controls
        theDots$controls <- NULL
    }
    else ctl <- cforest_control(mtry = param$mtry)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(.outcome ~ .), data = dat, 
        controls = ctl), theDots)
    out <- do.call(getFromNamespace("cforest", "party"), modelArgs)
    out
}

$cforest$predict
function (modelFit, newdata = NULL, submodels = NULL) 
{
    if (!is.null(newdata) && !is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, OOB = TRUE)
    if (is.matrix(out)) 
        out <- out[, 1]
    if (!is.null(modelFit@responses@levels$.outcome)) 
        out <- as.character(out)
    out
}

$cforest$prob
function (modelFit, newdata = NULL, submodels = NULL) 
{
    if (!is.null(newdata) && !is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    obsLevels <- levels(modelFit@data@get("response")[, 1])
    rawProbs <- treeresponse(modelFit, newdata, OOB = TRUE)
    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
        byrow = TRUE)
    out <- data.frame(probMatrix)
    colnames(out) <- obsLevels
    rownames(out) <- NULL
    out
}

$cforest$predictors
function (x, ...) 
{
    vi <- varimp(x, ...)
    names(vi)[vi != 0]
}

$cforest$varImp
function (object, ...) 
{
    variableImp <- varimp(object, ...)
    out <- data.frame(Overall = variableImp)
    out
}

$cforest$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Accepts Case Weights"      

$cforest$levels
function (x) 
levels(x@data@get("response")[, 1])

$cforest$sort
function (x) 
x[order(x[, 1]), ]

$cforest$oob
function (x) 
{
    obs <- x@data@get("response")[, 1]
    pred <- predict(x, OOB = TRUE)
    postResample(pred, obs)
}


$chaid
$chaid$label
[1] "CHi-squared Automated Interaction Detection"

$chaid$library
[1] "CHAID"

$chaid$loop
NULL

$chaid$type
[1] "Classification"

$chaid$parameters
  parameter   class
1    alpha2 numeric
2    alpha3 numeric
3    alpha4 numeric
                                                                                    label
1                                                                       Merging Threshold
2                                                       Splitting former Merged Threshold
3 \n                                                    Splitting former Merged Threshold

$chaid$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(alpha2 = seq(from = 0.05, to = 0.01, 
            length = len), alpha3 = -1, alpha4 = seq(from = 0.05, 
            to = 0.01, length = len))
    }
    else {
        out <- data.frame(alpha2 = runif(len, min = 1e-06, max = 0.1), 
            alpha3 = runif(len, min = -0.1, max = 0.1), alpha4 = runif(len, 
                min = 1e-06, max = 0.1))
    }
    out
}

$chaid$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$alpha2 <- param$alpha2
        theDots$control$alpha3 <- param$alpha3
        theDots$control$alpha4 <- param$alpha4
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- chaid_control(alpha2 = param$alpha2, alpha3 = param$alpha3, 
        alpha4 = param$alpha4)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("chaid", modelArgs)
    out
}

$chaid$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$chaid$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
}

$chaid$levels
function (x) 
x$obsLevels

$chaid$predictors
function (x, surrogate = TRUE, ...) 
{
    predictors(terms(x))
}

$chaid$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Two Class Only"             "Accepts Case Weights"      

$chaid$sort
function (x) 
x[order(-x$alpha2, -x$alpha4, -x$alpha3), ]


$CSimca
$CSimca$label
[1] "SIMCA"

$CSimca$library
[1] "rrcovHD"

$CSimca$loop
NULL

$CSimca$type
[1] "Classification"

$CSimca$parameters
  parameter     class     label
1 parameter character parameter

$CSimca$grid
function (x, y, len = NULL, search = "grid") 
{
    data.frame(parameter = "none")
}

$CSimca$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
CSimca(x, y, ...)

$CSimca$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)@classification

$CSimca$prob
NULL

$CSimca$tags
[1] "Robust Model"

$CSimca$levels
function (x) 
names(x@prior)

$CSimca$sort
function (x) 
x


$ctree
$ctree$label
[1] "Conditional Inference Tree"

$ctree$library
[1] "party"

$ctree$loop
NULL

$ctree$type
[1] "Classification" "Regression"    

$ctree$parameters
     parameter   class                 label
1 mincriterion numeric 1 - P-Value Threshold

$ctree$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mincriterion = seq(from = 0.99, to = 0.01, 
            length = len))
    }
    else {
        out <- data.frame(mincriterion = runif(len, min = 0, 
            max = 1))
    }
    out
}

$ctree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "controls")) {
        theDots$controls@gtctrl@mincriterion <- param$mincriterion
        ctl <- theDots$controls
        theDots$controls <- NULL
    }
    else ctl <- do.call(getFromNamespace("ctree_control", "party"), 
        list(mincriterion = param$mincriterion))
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, controls = ctl), theDots)
    out <- do.call(getFromNamespace("ctree", "party"), modelArgs)
    out
}

$ctree$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)
    if (!is.null(modelFit@responses@levels$.outcome)) 
        out <- as.character(out)
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$ctree$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    obsLevels <- levels(modelFit@data@get("response")[, 1])
    rawProbs <- treeresponse(modelFit, newdata)
    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
        byrow = TRUE)
    out <- data.frame(probMatrix)
    colnames(out) <- obsLevels
    rownames(out) <- NULL
    out
}

$ctree$predictors
function (x, surrogate = TRUE, ...) 
{
    treeObj <- unlist(nodes(x, 1))
    target <- "psplit\\.variableName"
    vars <- treeObj[grep(target, names(treeObj))]
    if (surrogate) {
        target2 <- "ssplits\\.variableName"
        svars <- treeObj[grep(target, names(treeObj))]
        vars <- c(vars, svars)
    }
    unique(vars)
}

$ctree$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Accepts Case Weights"      

$ctree$levels
function (x) 
levels(x@data@get("response")[, 1])

$ctree$sort
function (x) 
x[order(-x$mincriterion), ]


$ctree2
$ctree2$label
[1] "Conditional Inference Tree"

$ctree2$library
[1] "party"

$ctree2$loop
NULL

$ctree2$type
[1] "Regression"     "Classification"

$ctree2$parameters
     parameter   class                 label
1     maxdepth numeric        Max Tree Depth
2 mincriterion numeric 1 - P-Value Threshold

$ctree2$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(maxdepth = 1:len, mincriterion = seq(from = 0.99, 
            to = 0.01, length = len))
    }
    else {
        out <- data.frame(maxdepth = sample(1:15, replace = TRUE, 
            size = len), mincriterion = runif(len, min = 0, max = 1))
    }
    out
}

$ctree2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "controls")) {
        theDots$controls@tgctrl@maxdepth <- param$maxdepth
        theDots$controls@gtctrl@mincriterion <- param$mincriterion
        ctl <- theDots$controls
        theDots$controls <- NULL
    }
    else ctl <- do.call(getFromNamespace("ctree_control", "party"), 
        list(maxdepth = param$maxdepth, mincriterion = param$mincriterion))
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, controls = ctl), theDots)
    out <- do.call(getFromNamespace("ctree", "party"), modelArgs)
    out
}

$ctree2$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)
    if (!is.null(modelFit@responses@levels$.outcome)) 
        out <- as.character(out)
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$ctree2$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    obsLevels <- levels(modelFit@data@get("response")[, 1])
    rawProbs <- treeresponse(modelFit, newdata)
    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
        byrow = TRUE)
    out <- data.frame(probMatrix)
    colnames(out) <- obsLevels
    rownames(out) <- NULL
    out
}

$ctree2$predictors
function (x, surrogate = TRUE, ...) 
{
    treeObj <- unlist(nodes(x, 1))
    target <- "psplit\\.variableName"
    vars <- treeObj[grep(target, names(treeObj))]
    if (surrogate) {
        target2 <- "ssplits\\.variableName"
        svars <- treeObj[grep(target, names(treeObj))]
        vars <- c(vars, svars)
    }
    unique(vars)
}

$ctree2$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Accepts Case Weights"      

$ctree2$levels
function (x) 
levels(x@data@get("response")[, 1])

$ctree2$sort
function (x) 
x[order(x[, 1]), ]


$cubist
$cubist$label
[1] "Cubist"

$cubist$library
[1] "Cubist"

$cubist$loop
function (grid) 
{
    grid <- grid[order(-grid$committees, grid$neighbors, decreasing = TRUE), 
        , drop = FALSE]
    uniqueCom <- unique(grid$committees)
    loop <- data.frame(committees = uniqueCom)
    loop$neighbors <- NA
    submodels <- vector(mode = "list", length = length(uniqueCom))
    for (i in seq(along = uniqueCom)) {
        subK <- grid[grid$committees == uniqueCom[i], "neighbors"]
        loop$neighbors[loop$committees == uniqueCom[i]] <- subK[which.max(subK)]
        submodels[[i]] <- data.frame(neighbors = subK[-which.max(subK)])
    }
    list(loop = loop, submodels = submodels)
}

$cubist$type
[1] "Regression"

$cubist$parameters
   parameter   class       label
1 committees numeric #Committees
2  neighbors numeric  #Instances

$cubist$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(neighbors = c(0, 5, 9), committees = c(1, 
            10, 20))
    }
    else {
        out <- data.frame(neighbors = sample(0:9, replace = TRUE, 
            size = len), committees = sample(1:100, replace = TRUE, 
            size = len))
    }
    out[!duplicated(out), ]
}

$cubist$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- cubist(x, y, committees = param$committees, ...)
    if (last) 
        out$tuneValue$neighbors <- param$neighbors
    out
}

$cubist$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, neighbors = modelFit$tuneValue$neighbors)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$neighbors)) tmp[[j + 
            1]] <- predict(modelFit, newdata, neighbors = submodels$neighbors[j])
        out <- tmp
    }
    out
}

$cubist$varImp
function (object, weights = c(0.5, 0.5), ...) 
{
    if (length(weights) != 2) 
        stop("two weights must be given")
    weights <- weights/sum(weights)
    out <- data.frame(Overall = object$usage$Conditions * weights[1] + 
        object$usage$Model * weights[2])
    rownames(out) <- object$usage$Variable
    out
}

$cubist$tags
[1] "Rule-Based Model"           "Boosting"                  
[3] "Ensemble Model"             "Prototype Models"          
[5] "Model Tree"                 "Linear Regression"         
[7] "Implicit Feature Selection"

$cubist$prob
NULL

$cubist$sort
function (x) 
x[order(x$committees, x$neighbors), ]


$dda
$dda$label
[1] "Diagonal Discriminant Analysis"

$dda$library
[1] "sparsediscrim"

$dda$loop
NULL

$dda$type
[1] "Classification"

$dda$parameters
  parameter     class          label
1     model character          Model
2 shrinkage character Shrinkage Type

$dda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(model = rep(c("Linear", "Quadratic"), each = 3), shrinkage = rep(c("None", 
    "Variance", "Mean"), 2))

$dda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (param$model == "Linear") {
        if (param$shrinkage == "None") {
            out <- dlda(x, y, ...)
        }
        else {
            if (param$shrinkage == "Variance") {
                out <- sdlda(x, y, ...)
            }
            else out <- smdlda(x, y, ...)
        }
    }
    else {
        if (param$shrinkage == "None") {
            out <- dqda(x, y, ...)
        }
        else {
            if (param$shrinkage == "Variance") {
                out <- sdqda(x, y, ...)
            }
            else out <- smdqda(x, y, ...)
        }
    }
    out
}

$dda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$dda$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$scores
    as.data.frame(t(apply(out, 2, function(x) exp(x)/sum(exp(x)))))
}

$dda$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else colnames(x$means)

$dda$tags
[1] "Discriminant Analysis" "Linear Classifier"     "Polynomial Model"     
[4] "Regularization"       

$dda$levels
function (x) 
names(x$prior)

$dda$sort
function (x) 
x


$deepboost
$deepboost$label
[1] "DeepBoost"

$deepboost$library
[1] "deepboost"

$deepboost$loop
NULL

$deepboost$type
[1] "Classification"

$deepboost$parameters
   parameter     class                     label
1   num_iter   numeric     # Boosting Iterations
2 tree_depth   numeric                Tree Depth
3       beta   numeric         L1 Regularization
4     lambda   numeric Tree Depth Regularization
5  loss_type character                      Loss

$deepboost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(tree_depth = seq(1, len), num_iter = floor((1:len) * 
            50), beta = 2^seq(-8, -4, length = len), lambda = 2^seq(-6, 
            -2, length = len), loss_type = "l")
    }
    else {
        out <- data.frame(num_iter = floor(runif(len, min = 1, 
            max = 500)), tree_depth = sample(1:20, replace = TRUE, 
            size = len), beta = runif(len, max = 0.25), lambda = runif(len, 
            max = 0.25), loss_type = sample(c("l"), replace = TRUE, 
            size = len))
    }
    out
}

$deepboost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    if (is.null(wts)) {
        dat <- x
        dat$.outcome <- y
        out <- deepboost(.outcome ~ ., data = dat, tree_depth = param$tree_depth, 
            num_iter = param$num_iter, beta = param$beta, lambda = param$lambda, 
            loss_type = as.character(param$loss_type), ...)
    }
    else {
        out <- deepboost(.outcome ~ ., data = dat, tree_depth = param$tree_depth, 
            instance_weights = wts, num_iter = param$num_iter, 
            beta = param$beta, lambda = param$lambda, loss_type = as.character(param$loss_type), 
            ...)
    }
    out
}

$deepboost$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$deepboost$levels
function (x) 
x@classes

$deepboost$prob
NULL

$deepboost$tags
[1] "Tree-Based Model"           "Boosting"                  
[3] "Ensemble Model"             "Implicit Feature Selection"
[5] "Accepts Case Weights"       "L1 Regularization"         
[7] "Two Class Only"            

$deepboost$sort
function (x) 
x[order(x$num_iter, x$tree_depth, x$beta), ]


$DENFIS
$DENFIS$label
[1] "Dynamic Evolving Neural-Fuzzy Inference System "

$DENFIS$library
[1] "frbs"

$DENFIS$type
[1] "Regression"

$DENFIS$parameters
  parameter   class           label
1      Dthr numeric       Threshold
2  max.iter numeric Max. Iterations

$DENFIS$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(Dthr = seq(0.1, 0.5, length = len), 
            max.iter = 100)
    }
    else {
        out <- data.frame(Dthr = runif(len, min = 0, max = 1), 
            max.iter = sample(1:20, replace = TRUE, size = len))
    }
    out
}

$DENFIS$loop
NULL

$DENFIS$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "DENFIS")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$Dthr <- param$Dthr
        theDots$control$max.iter <- param$max.iter
    }
    else theDots$control <- list(Dthr = param$Dthr, max.iter = param$max.iter, 
        step.size = 0.01, d = 2, method.type = "DENFIS", name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$DENFIS$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$DENFIS$prob
NULL

$DENFIS$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$DENFIS$tags
[1] "Rule-Based Model"

$DENFIS$levels
NULL

$DENFIS$sort
function (x) 
x[order(x$Dthr), ]


$dnn
$dnn$label
[1] "Stacked AutoEncoder Deep Neural Network"

$dnn$library
[1] "deepnet"

$dnn$loop
NULL

$dnn$type
[1] "Classification" "Regression"    

$dnn$parameters
        parameter   class           label
1          layer1 numeric  Hidden Layer 1
2          layer2 numeric  Hidden Layer 2
3          layer3 numeric  Hidden Layer 3
4  hidden_dropout numeric Hidden Dropouts
5 visible_dropout numeric Visible Dropout

$dnn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(layer1 = 1:len, layer2 = 0:(len - 
            1), layer3 = 0:(len - 1), hidden_dropout = 0, visible_dropout = 0)
    }
    else {
        out <- data.frame(layer1 = sample(2:20, replace = TRUE, 
            size = len), layer2 = sample(2:20, replace = TRUE, 
            size = len), layer3 = sample(2:20, replace = TRUE, 
            size = len), hidden_dropout = runif(len, min = 0, 
            max = 0.1), visible_dropout = runif(len, min = 0, 
            max = 0.1))
    }
    out
}

$dnn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    is_class <- is.factor(y)
    if (is_class) 
        y <- caret:::class2ind(y)
    layers <- c(param$layer1, param$layer2, param$layer3)
    layers <- layers[layers > 0]
    sae.dnn.train(x, y, hidden = layers, output = if (is_class) 
        "sigm"
    else "linear", hidden_dropout = param$hidden_dropout, visible_dropout = param$visible_dropout, 
        ...)
}

$dnn$predict
function (modelFit, newdata, submodels = NULL) 
{
    pred <- nn.predict(modelFit, as.matrix(newdata))
    if (ncol(pred) > 1) 
        pred <- modelFit$obsLevels[apply(pred, 1, which.max)]
    pred
}

$dnn$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- exp(nn.predict(modelFit, as.matrix(newdata)))
    out <- apply(out, 1, function(x) x/sum(x))
    t(out)
}

$dnn$predictors
function (x, ...) 
{
    NULL
}

$dnn$varImp
NULL

$dnn$levels
function (x) 
x$classes

$dnn$tags
[1] "Neural Network"

$dnn$sort
function (x) 
x[order(x[, 1]), ]


$dwdLinear
$dwdLinear$label
[1] "Linear Distance Weighted Discrimination"

$dwdLinear$library
[1] "kerndwd"

$dwdLinear$type
[1] "Classification"

$dwdLinear$parameters
  parameter   class                    label
1    lambda numeric Regularization Parameter
2      qval numeric                        q

$dwdLinear$grid
function (x, y, len = NULL, search = "grid") 
{
    if (length(levels(y)) != 2) 
        stop("Two class problems only")
    if (search == "grid") {
        out <- expand.grid(lambda = 10^seq(-5, 1, length = len), 
            qval = 1)
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            qval = runif(len, min = 0, 3))
    }
    out
}

$dwdLinear$loop
NULL

$dwdLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    out <- kerndwd(x = x, y = ifelse(y == lev[1], 1, -1), qval = param$qval, 
        lambda = param$lambda, kern = vanilladot(), ...)
    out$kern <- vanilladot()
    out$x <- x
    out
}

$dwdLinear$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x)[, 1]
    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
}

$dwdLinear$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x, type = "link")[, 1]
    out <- binomial()$linkinv(out)
    out <- cbind(out, 1 - out)
    colnames(out) <- modelFit$obsLevels
    out
}

$dwdLinear$levels
function (x) 
x$obsLevels

$dwdLinear$predictors
function (x, s = NULL, ...) 
x$xNames

$dwdLinear$tags
[1] "Discriminant Analysis"            "L2 Regularization"               
[3] "Kernel Method"                    "Linear Classifier"               
[5] "Distance Weighted Discrimination" "Two Class Only"                  

$dwdLinear$sort
function (x) 
x[order(x[, 1]), ]


$dwdPoly
$dwdPoly$label
[1] "Distance Weighted Discrimination with Polynomial Kernel"

$dwdPoly$library
[1] "kerndwd"

$dwdPoly$type
[1] "Classification"

$dwdPoly$parameters
  parameter   class                    label
1    lambda numeric Regularization Parameter
2      qval numeric                        q
3    degree numeric        Polynomial Degree
4     scale numeric                    Scale

$dwdPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (length(levels(y)) != 2) 
        stop("Two class problems only")
    if (search == "grid") {
        out <- expand.grid(lambda = 10^seq(-5, 1, length = len), 
            qval = 1, degree = seq(1, min(len, 3)), scale = 10^((1:len) - 
                4))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            qval = runif(len, min = 0, 3), degree = sample(1:3, 
                size = len, replace = TRUE), scale = 10^runif(len, 
                min = -5, 0))
    }
    out
}

$dwdPoly$loop
NULL

$dwdPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    kobj <- polydot(degree = param$degree, scale = param$scale, 
        offset = 1)
    out <- kerndwd(x = x, y = ifelse(y == lev[1], 1, -1), qval = param$qval, 
        lambda = param$lambda, kern = kobj, ...)
    out$kern <- kobj
    out$x <- x
    out
}

$dwdPoly$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x)[, 1]
    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
}

$dwdPoly$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x, type = "link")[, 1]
    out <- binomial()$linkinv(out)
    out <- cbind(out, 1 - out)
    colnames(out) <- modelFit$obsLevels
    out
}

$dwdPoly$levels
function (x) 
x$obsLevels

$dwdPoly$predictors
function (x, s = NULL, ...) 
x$xNames

$dwdPoly$tags
[1] "Discriminant Analysis"            "L2 Regularization"               
[3] "Kernel Method"                    "Polynomial Model"                
[5] "Distance Weighted Discrimination" "Two Class Only"                  

$dwdPoly$sort
function (x) 
x[order(x[, 1]), ]


$dwdRadial
$dwdRadial$label
[1] "Distance Weighted Discrimination with Radial Basis Function Kernel"

$dwdRadial$library
[1] "kernlab" "kerndwd"

$dwdRadial$type
[1] "Classification"

$dwdRadial$parameters
  parameter   class                    label
1    lambda numeric Regularization Parameter
2      qval numeric                        q
3     sigma numeric                    Sigma

$dwdRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (length(levels(y)) != 2) 
        stop("Two class problems only")
    if (search == "grid") {
        out <- expand.grid(lambda = 10^seq(-5, 1, length = len), 
            qval = 1, sigma = mean(as.vector(sigmas[-2])))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            qval = runif(len, min = 0, 3), sigma = exp(runif(len, 
                min = rng[1], max = rng[2])))
    }
    out
}

$dwdRadial$loop
NULL

$dwdRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    kobj <- rbfdot(sigma = param$sigma)
    out <- kerndwd(x = x, y = ifelse(y == lev[1], 1, -1), qval = param$qval, 
        lambda = param$lambda, kern = kobj, ...)
    out$kern <- kobj
    out$x <- x
    out
}

$dwdRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x)[, 1]
    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
}

$dwdRadial$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(object = modelFit, newx = newdata, kern = modelFit$kern, 
        x = modelFit$x, type = "link")[, 1]
    out <- binomial()$linkinv(out)
    out <- cbind(out, 1 - out)
    colnames(out) <- modelFit$obsLevels
    out
}

$dwdRadial$levels
function (x) 
x$obsLevels

$dwdRadial$predictors
function (x, s = NULL, ...) 
x$xNames

$dwdRadial$tags
[1] "Discriminant Analysis"            "L2 Regularization"               
[3] "Kernel Method"                    "Radial Basis Function"           
[5] "Distance Weighted Discrimination" "Two Class Only"                  

$dwdRadial$sort
function (x) 
x[order(x[, 1]), ]


$earth
$earth$label
[1] "Multivariate Adaptive Regression Spline"

$earth$library
[1] "earth"

$earth$type
[1] "Regression"     "Classification"

$earth$parameters
  parameter   class          label
1    nprune numeric         #Terms
2    degree numeric Product Degree

$earth$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    mod <- earth(.outcome ~ ., data = dat, pmethod = "none")
    maxTerms <- nrow(mod$dirs)
    maxTerms <- min(200, floor(maxTerms * 0.75) + 2)
    if (search == "grid") {
        out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, 
            length = len))), degree = 1)
    }
    else {
        out <- data.frame(nprune = sample(2:maxTerms, size = len, 
            replace = TRUE), degree = sample(1:2, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$earth$loop
function (grid) 
{
    deg <- unique(grid$degree)
    loop <- data.frame(degree = deg)
    loop$nprune <- NA
    submodels <- vector(mode = "list", length = length(deg))
    for (i in seq(along = deg)) {
        np <- grid[grid$degree == deg[i], "nprune"]
        loop$nprune[loop$degree == deg[i]] <- np[which.max(np)]
        submodels[[i]] <- data.frame(nprune = np[-which.max(np)])
    }
    list(loop = loop, submodels = submodels)
}

$earth$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots$keepxy <- TRUE
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(x = x, y = y, degree = param$degree, 
        nprune = param$nprune), theDots)
    if (is.factor(y)) 
        modelArgs$glm <- list(family = binomial)
    tmp <- do.call("earth", modelArgs)
    tmp$call["nprune"] <- param$nprune
    tmp$call["degree"] <- param$degree
    tmp
}

$earth$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata)
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- if (is.matrix(out)) 
            out[, 1]
        else out
        for (j in seq(along = submodels$nprune)) {
            prunedFit <- update(modelFit, nprune = submodels$nprune[j])
            if (modelFit$problemType == "Classification") {
                tmp[[j + 1]] <- predict(prunedFit, newdata, type = "class")
            }
            else {
                tmp[[j + 1]] <- predict(prunedFit, newdata)
            }
            if (is.matrix(tmp[[j + 1]])) 
                tmp[[j + 1]] <- tmp[[j + 1]][, 1]
        }
        out <- tmp
    }
    out
}

$earth$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nprune)) {
            prunedFit <- update(modelFit, nprune = submodels$nprune[j])
            tmp2 <- predict(prunedFit, newdata, type = "response")
            tmp2 <- cbind(1 - tmp2, tmp2)
            colnames(tmp2) <- modelFit$obsLevels
            tmp[[j + 1]] <- tmp2
        }
        out <- tmp
    }
    out
}

$earth$predictors
function (x, ...) 
{
    vi <- varImp(x)
    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 
        0)))))
    if (length(notZero) > 0) 
        rownames(vi)[notZero]
    else NULL
}

$earth$varImp
function (object, value = "gcv", ...) 
{
    earthImp <- evimp(object)
    if (!is.matrix(earthImp)) 
        earthImp <- t(as.matrix(earthImp))
    out <- earthImp
    perfCol <- which(colnames(out) == value)
    increaseInd <- out[, perfCol + 1]
    out <- as.data.frame(out[, perfCol, drop = FALSE])
    colnames(out) <- "Overall"
    if (any(earthImp[, "used"] == 0)) {
        dropList <- grep("-unused", rownames(earthImp), value = TRUE)
        out$Overall[rownames(out) %in% dropList] <- 0
    }
    rownames(out) <- gsub("-unused", "", rownames(out))
    out <- as.data.frame(out)
    xNames <- object$namesx.org
    if (any(!(xNames %in% rownames(out)))) {
        xNames <- xNames[!(xNames %in% rownames(out))]
        others <- data.frame(Overall = rep(0, length(xNames)), 
            row.names = xNames)
        out <- rbind(out, others)
    }
    out
}

$earth$levels
function (x) 
x$levels

$earth$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Implicit Feature Selection"              
[3] "Accepts Case Weights"                    

$earth$sort
function (x) 
x[order(x$degree, x$nprune), ]


$elm
$elm$label
[1] "Extreme Learning Machine"

$elm$library
[1] "elmNN"

$elm$loop
NULL

$elm$type
[1] "Classification" "Regression"    

$elm$parameters
  parameter     class               label
1      nhid   numeric       #Hidden Units
2    actfun character Activation Function

$elm$grid
function (x, y, len = NULL, search = "grid") 
{
    funs <- c("sin", "radbas", "purelin", "tansig")
    if (search == "grid") {
        out <- expand.grid(nhid = ((1:len) * 2) - 1, actfun = funs)
    }
    else {
        out <- data.frame(nhid = floor(runif(len, min = 1, max = 20)), 
            actfun = sample(funs, replace = TRUE, size = len))
    }
    out[!duplicated(out), ]
}

$elm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        factor2ind <- function(x) {
            x <- model.matrix(~x - 1, contrasts = list(x = "contr.treatment"))
            colnames(x) <- gsub("^x", "", colnames(x))
            att <- attributes(x)
            att$assign <- NULL
            att$contrasts <- NULL
            attributes(x) <- att
            x
        }
        out <- elmtrain(x = x, y = factor2ind(y), nhid = param$nhid, 
            actfun = param$actfun, ...)
        out$lev <- levels(y)
    }
    else {
        out <- elmtrain(x = x, y = y, nhid = param$nhid, actfun = param$actfun, 
            ...)
    }
    out$xNames <- colnames(x)
    out
}

$elm$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "class")
    if (modelFit$problemType == "Classification") {
        out <- modelFit$lev[apply(out, 1, which.max)]
        out <- factor(out, levels = modelFit$lev)
    }
    out
}

$elm$prob
NULL

$elm$varImp
NULL

$elm$predictors
function (x, ...) 
x$xNames

$elm$tags
[1] "Neural Network"

$elm$levels
function (x) 
x$lev

$elm$sort
function (x) 
x[order(x$nhid), ]


$enet
$enet$label
[1] "Elasticnet"

$enet$library
[1] "elasticnet"

$enet$type
[1] "Regression"

$enet$parameters
  parameter   class                     label
1  fraction numeric Fraction of Full Solution
2    lambda numeric              Weight Decay

$enet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 
            1)), fraction = seq(0.05, 1, length = len))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            fraction = runif(len, min = 0, max = 1))
    }
    out
}

$enet$loop
function (grid) 
{
    grid <- grid[order(grid$lambda, grid$fraction, decreasing = TRUE), 
        , drop = FALSE]
    uniqueLambda <- unique(grid$lambda)
    loop <- data.frame(lambda = uniqueLambda)
    loop$fraction <- NA
    submodels <- vector(mode = "list", length = length(uniqueLambda))
    for (i in seq(along = uniqueLambda)) {
        subFrac <- grid[grid$lambda == uniqueLambda[i], "fraction"]
        loop$fraction[loop$lambda == uniqueLambda[i]] <- subFrac[which.max(subFrac)]
        submodels[[i]] <- data.frame(fraction = subFrac[-which.max(subFrac)])
    }
    list(loop = loop, submodels = submodels)
}

$enet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    enet(as.matrix(x), y, lambda = param$lambda)
}

$enet$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, s = modelFit$tuneValue$fraction, 
        mode = "fraction")$fit
    if (!is.null(submodels)) {
        if (nrow(submodels) > 1) {
            out <- c(list(if (is.matrix(out)) out[, 1] else out), 
                as.list(as.data.frame(predict(modelFit, newx = newdata, 
                  s = submodels$fraction, mode = "fraction")$fit)))
        }
        else {
            tmp <- predict(modelFit, newx = newdata, s = submodels$fraction, 
                mode = "fraction")$fit
            out <- c(list(if (is.matrix(out)) out[, 1] else out), 
                list(tmp))
        }
    }
    out
}

$enet$predictors
function (x, s = NULL, ...) 
{
    if (is.null(s)) {
        if (!is.null(x$tuneValue)) {
            s <- x$tuneValue$fraction
        }
        else stop("must supply a vaue of s")
        out <- predict(x, s = s, type = "coefficients", mode = "fraction")$coefficients
    }
    else {
        out <- predict(x, s = s)$coefficients
    }
    names(out)[out != 0]
}

$enet$tags
[1] "Linear Regression"          "Implicit Feature Selection"
[3] "L1 Regularization"         

$enet$prob
NULL

$enet$sort
function (x) 
x[order(x$fraction, -x$lambda), ]


$enpls.fs
$enpls.fs$label
[1] "Ensemble Partial Least Squares Regression with Feature Selection"

$enpls.fs$library
[1] "enpls"

$enpls.fs$type
[1] "Regression"

$enpls.fs$parameters
  parameter   class             label
1   maxcomp numeric  Max. #Components
2 threshold numeric Importance Cutoff

$enpls.fs$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        comps <- caret::var_seq(p = ncol(x), classification = is.factor(y), 
            len = 1)
        out <- expand.grid(maxcomp = comps, threshold = seq(0, 
            2, length = len))
    }
    else {
        out <- data.frame(maxcomp = sample(1:comps, size = len, 
            replace = TRUE), threshold = runif(len, min = 0, 
            max = 5))
    }
    out
}

$enpls.fs$loop
NULL

$enpls.fs$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    x <- if (is.matrix(x)) 
        x
    else as.matrix(x)
    vi <- enpls.fs(x = x, y = y, maxcomp = param$maxcomp, ...)[[1]]
    if (any(vi > param$threshold)) {
        keepers <- names(vi)[vi > param$threshold]
    }
    else keepers <- names(vi)[which.max(vi)]
    enpls.en(x = x[, keepers, drop = FALSE], y = y, maxcomp = min(param$maxcomp, 
        length(keepers)), ...)
}

$enpls.fs$predict
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- if (is.matrix(newdata)) 
        newdata
    else as.matrix(newdata)
    keepers <- rownames(modelFit[[1]][[1]]$loadings)
    predict(modelFit, newdata[, keepers, drop = FALSE])
}

$enpls.fs$predictors
function (x, ...) 
rownames(x$projection)

$enpls.fs$tags
[1] "Partial Least Squares" "Ensemble Model"       

$enpls.fs$prob
NULL

$enpls.fs$sort
function (x) 
x[order(x[, 1]), ]


$enpls
$enpls$label
[1] "Ensemble Partial Least Squares Regression"

$enpls$library
[1] "enpls"

$enpls$type
[1] "Regression"

$enpls$parameters
  parameter   class            label
1   maxcomp numeric Max. #Components

$enpls$grid
function (x, y, len = NULL, search = "grid") 
{
    comps <- caret::var_seq(p = ncol(x), classification = is.factor(y), 
        len = 1)
    if (search == "grid") {
        out <- data.frame(maxcomp = comps)
    }
    else {
        out <- data.frame(maxcomp = sample(1:comps, size = len, 
            replace = TRUE))
    }
    out
}

$enpls$loop
NULL

$enpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    x <- if (is.matrix(x)) 
        x
    else as.matrix(x)
    enpls.en(x = x, y = y, maxcomp = param$maxcomp, ...)
}

$enpls$predict
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- if (is.matrix(newdata)) 
        newdata
    else as.matrix(newdata)
    predict(modelFit, newdata)
}

$enpls$predictors
function (x, ...) 
rownames(x$projection)

$enpls$tags
[1] "Partial Least Squares" "Ensemble Model"       

$enpls$prob
NULL

$enpls$sort
function (x) 
x[order(x[, 1]), ]


$evtree
$evtree$label
[1] "Tree Models from Genetic Algorithms"

$evtree$library
[1] "evtree"

$evtree$loop
NULL

$evtree$type
[1] "Regression"     "Classification"

$evtree$parameters
  parameter   class                label
1     alpha numeric Complexity Parameter

$evtree$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(alpha = seq(1, 3, length = len))
    }
    else {
        out <- data.frame(alpha = runif(len, min = 1, max = 5))
    }
    out
}

$evtree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$alpha <- param$alpha
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- evtree.control(alpha = param$alpha)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("evtree", modelArgs)
    out
}

$evtree$levels
function (x) 
x$obsLevels

$evtree$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$evtree$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
}

$evtree$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Accepts Case Weights"      

$evtree$sort
function (x) 
x[order(x[, 1]), ]


$extraTrees
$extraTrees$label
[1] "Random Forest by Randomization"

$extraTrees$library
[1] "extraTrees"

$extraTrees$loop
NULL

$extraTrees$type
[1] "Regression"     "Classification"

$extraTrees$parameters
      parameter   class                          label
1          mtry numeric # Randomly Selected Predictors
2 numRandomCuts numeric                  # Random Cuts

$extraTrees$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), numRandomCuts = 1:len)
    }
    else {
        out <- data.frame(mtry = sample(1:ncol(x), size = len, 
            replace = TRUE), numRandomCuts = sample(1:25, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$extraTrees$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
extraTrees(x, y, mtry = param$mtry, numRandomCuts = param$numRandomCuts, 
    ...)

$extraTrees$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$extraTrees$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, probability = TRUE)

$extraTrees$levels
function (x) 
x$obsLevels

$extraTrees$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$extraTrees$sort
function (x) 
x[order(x[, 1]), ]


$fda
$fda$label
[1] "Flexible Discriminant Analysis"

$fda$library
[1] "earth" "mda"  

$fda$loop
NULL

$fda$type
[1] "Classification"

$fda$parameters
  parameter   class          label
1    degree numeric Product Degree
2    nprune numeric         #Terms

$fda$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    mod <- earth(.outcome ~ ., data = dat, pmethod = "none")
    maxTerms <- nrow(mod$dirs)
    maxTerms <- min(200, floor(maxTerms * 0.75) + 2)
    if (search == "grid") {
        out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, 
            length = len))), degree = 1)
    }
    else {
        out <- data.frame(nprune = sample(2:maxTerms, size = len, 
            replace = TRUE), degree = sample(1:2, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$fda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    fda(.outcome ~ ., data = dat, method = earth, degree = param$degree, 
        nprune = param$nprune, weights = wts, ...)
}

$fda$levels
function (x) 
x$obsLevels

$fda$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Implicit Feature Selection"              
[3] "Accepts Case Weights"                    

$fda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$fda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "posterior")

$fda$predictors
function (x, ...) 
{
    code <- getModelInfo("earth", regex = FALSE)[[1]]$predictors
    tmp <- predictors(x$terms)
    out <- if (class(x$fit) == "earth") 
        code(x$fit)
    else tmp
    out
}

$fda$varImp
function (object, value = "gcv", ...) 
varImp(object$fit, value = value, ...)

$fda$sort
function (x) 
x[order(x$degree, x$nprune), ]


$FH.GBML
$FH.GBML$label
[1] "Fuzzy Rules Using Genetic Cooperative-Competitive Learning and Pittsburgh"

$FH.GBML$library
[1] "frbs"

$FH.GBML$type
[1] "Classification"

$FH.GBML$parameters
     parameter   class            label
1 max.num.rule numeric      Max. #Rules
2    popu.size numeric  Population Size
3      max.gen numeric Max. Generations

$FH.GBML$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(max.num.rule = 1 + (1:len) * 2, popu.size = 10, 
            max.gen = 10)
    }
    else {
        out <- data.frame(max.gen = sample(1:20, size = len, 
            replace = TRUE), popu.size = sample(seq(2, 20, by = 2), 
            size = len, replace = TRUE), max.num.rule = sample(1:20, 
            size = len, replace = TRUE))
    }
    out
}

$FH.GBML$loop
NULL

$FH.GBML$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))), 
        method.type = "FH.GBML")
    args$range.data <- apply(x, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$max.num.rule <- param$max.num.rule
        theDots$control$popu.size <- param$popu.size
        theDots$control$max.gen <- param$max.gen
    }
    else theDots$control <- list(max.num.rule = param$max.num.rule, 
        popu.size = param$popu.size, max.gen = param$max.gen, 
        persen_cross = 0.6, persen_mutant = 0.3, p.dcare = 0.5, 
        p.gccl = 0.5, num.class = length(unique(y)), name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$FH.GBML$predict
function (modelFit, newdata, submodels = NULL) 
{
    modelFit$obsLevels[predict(modelFit, newdata)[, 1]]
}

$FH.GBML$prob
NULL

$FH.GBML$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$FH.GBML$tags
[1] "Rule-Based Model"

$FH.GBML$levels
NULL

$FH.GBML$sort
function (x) 
x[order(x$max.num.rule), ]


$FIR.DM
$FIR.DM$label
[1] "Fuzzy Inference Rules by Descent Method"

$FIR.DM$library
[1] "frbs"

$FIR.DM$type
[1] "Regression"

$FIR.DM$parameters
   parameter   class           label
1 num.labels numeric    #Fuzzy Terms
2   max.iter numeric Max. Iterations

$FIR.DM$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, max.iter = 100)
    }
    else {
        out <- data.frame(max.iter = sample(1:20, replace = TRUE, 
            size = len), num.labels = sample(2:20, size = len, 
            replace = TRUE))
    }
    out
}

$FIR.DM$loop
NULL

$FIR.DM$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "FIR.DM")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.iter <- param$max.iter
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        max.iter = param$max.iter, step.size = 0.01, type.tnorm = "MIN", 
        type.snorm = "MAX", type.implication.func = "ZADEH", 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$FIR.DM$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$FIR.DM$prob
NULL

$FIR.DM$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$FIR.DM$tags
[1] "Rule-Based Model"

$FIR.DM$levels
NULL

$FIR.DM$sort
function (x) 
x[order(x$num.labels), ]


$foba
$foba$label
[1] "Ridge Regression with Variable Selection"

$foba$library
[1] "foba"

$foba$type
[1] "Regression"

$foba$parameters
  parameter   class               label
1         k numeric #Variables Retained
2    lambda numeric          L2 Penalty

$foba$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = 10^seq(-5, -1, length = len), 
            k = caret::var_seq(p = ncol(x), classification = is.factor(y), 
                len = len))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            k = sample(1:ncol(x), replace = TRUE, size = len))
    }
    out
}

$foba$loop
function (grid) 
{
    grid <- grid[order(grid$lambda, grid$k, decreasing = TRUE), 
        , drop = FALSE]
    uniqueLambda <- unique(grid$lambda)
    loop <- data.frame(lambda = uniqueLambda)
    loop$k <- NA
    submodels <- vector(mode = "list", length = length(uniqueLambda))
    for (i in seq(along = uniqueLambda)) {
        subK <- grid[grid$lambda == uniqueLambda[i], "k"]
        loop$k[loop$lambda == uniqueLambda[i]] <- subK[which.max(subK)]
        submodels[[i]] <- data.frame(k = subK[-which.max(subK)])
    }
    list(loop = loop, submodels = submodels)
}

$foba$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
foba(as.matrix(x), y, lambda = param$lambda, ...)

$foba$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, k = modelFit$tuneValue$k, 
        type = "fit")$fit
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$k)) {
            tmp[[j + 1]] <- predict(modelFit, newdata, k = submodels$k[j], 
                type = "fit")$fit
        }
        out <- tmp
    }
    out
}

$foba$predictors
function (x, k = NULL, ...) 
{
    if (is.null(k)) {
        if (!is.null(x$tuneValue)) 
            k <- x$tuneValue$k[1]
        else stop("Please specify k")
    }
    library(foba)
    names(predict(x, k = k, type = "coefficients")$selected.variables)
}

$foba$tags
[1] "Linear Regression"         "Ridge Regression"         
[3] "L2 Regularization"         "Feature Selection Wrapper"

$foba$prob
NULL

$foba$sort
function (x) 
x[order(x$k, -x$lambda), ]


$FRBCS.CHI
$FRBCS.CHI$label
[1] "Fuzzy Rules Using Chi's Method"

$FRBCS.CHI$library
[1] "frbs"

$FRBCS.CHI$type
[1] "Classification"

$FRBCS.CHI$parameters
   parameter     class               label
1 num.labels   numeric        #Fuzzy Terms
2    type.mf character Membership Function

$FRBCS.CHI$grid
function (x, y, len = NULL, search = "grid") 
{
    type <- c("GAUSSIAN", "TRAPEZOID", "TRIANGLE")
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, type.mf = type)
    }
    else {
        out <- data.frame(type.mf = sample(type, size = len, 
            replace = TRUE), num.labels = sample(2:20, size = len, 
            replace = TRUE))
    }
    out
}

$FRBCS.CHI$loop
NULL

$FRBCS.CHI$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))), 
        method.type = "FRBCS.CHI")
    args$range.data <- apply(x, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$type.mf <- param$type.mf
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        type.mf = param$type.mf, type.tnorm = "MIN", type.snorm = "MAX", 
        type.implication.func = "ZADEH", num.class = length(unique(y)), 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$FRBCS.CHI$predict
function (modelFit, newdata, submodels = NULL) 
{
    modelFit$obsLevels[predict(modelFit, newdata)[, 1]]
}

$FRBCS.CHI$prob
NULL

$FRBCS.CHI$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$FRBCS.CHI$tags
[1] "Rule-Based Model"

$FRBCS.CHI$levels
NULL

$FRBCS.CHI$sort
function (x) 
x[order(x$num.labels), ]


$FRBCS.W
$FRBCS.W$label
[1] "Fuzzy Rules with Weight Factor"

$FRBCS.W$library
[1] "frbs"

$FRBCS.W$type
[1] "Classification"

$FRBCS.W$parameters
   parameter     class               label
1 num.labels   numeric        #Fuzzy Terms
2    type.mf character Membership Function

$FRBCS.W$grid
function (x, y, len = NULL, search = "grid") 
{
    type <- c("GAUSSIAN", "TRAPEZOID", "TRIANGLE")
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, type.mf = type)
    }
    else {
        out <- data.frame(type.mf = sample(type, size = len, 
            replace = TRUE), num.labels = sample(2:20, size = len, 
            replace = TRUE))
    }
    out
}

$FRBCS.W$loop
NULL

$FRBCS.W$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))), 
        method.type = "FRBCS.W")
    args$range.data <- apply(x, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$type.mf <- param$type.mf
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        type.mf = param$type.mf, type.tnorm = "MIN", type.snorm = "MAX", 
        type.implication.func = "ZADEH", num.class = length(unique(y)), 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$FRBCS.W$predict
function (modelFit, newdata, submodels = NULL) 
{
    modelFit$obsLevels[predict(modelFit, newdata)[, 1]]
}

$FRBCS.W$prob
NULL

$FRBCS.W$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$FRBCS.W$tags
[1] "Rule-Based Model"

$FRBCS.W$levels
NULL

$FRBCS.W$sort
function (x) 
x[order(x$num.labels), ]


$FS.HGD
$FS.HGD$label
[1] "Simplified TSK Fuzzy Rules"

$FS.HGD$library
[1] "frbs"

$FS.HGD$type
[1] "Regression"

$FS.HGD$parameters
   parameter   class           label
1 num.labels numeric    #Fuzzy Terms
2   max.iter numeric Max. Iterations

$FS.HGD$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, max.iter = 100)
    }
    else {
        out <- data.frame(max.iter = sample(1:20, replace = TRUE, 
            size = len), num.labels = sample(2:20, size = len, 
            replace = TRUE))
    }
    out
}

$FS.HGD$loop
NULL

$FS.HGD$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "FS.HGD")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.iter <- param$max.iter
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        max.iter = param$max.iter, step.size = 0.01, alpha.heuristic = 1, 
        type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$FS.HGD$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$FS.HGD$prob
NULL

$FS.HGD$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$FS.HGD$tags
[1] "Rule-Based Model"

$FS.HGD$levels
NULL

$FS.HGD$sort
function (x) 
x[order(x$num.labels), ]


$gam
$gam$label
[1] "Generalized Additive Model using Splines"

$gam$library
[1] "mgcv"

$gam$loop
NULL

$gam$type
[1] "Regression"     "Classification"

$gam$parameters
  parameter     class             label
1    select   logical Feature Selection
2    method character            Method

$gam$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(select = c(TRUE, FALSE), method = "GCV.Cp")
    }
    else {
        out <- data.frame(select = sample(c(TRUE, FALSE), size = len, 
            replace = TRUE), method = sample(c("GCV.Cp", "ML"), 
            size = len, replace = TRUE))
    }
    out[!duplicated(out), ]
}

$gam$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    modForm <- caret:::smootherFormula(x)
    if (is.factor(y)) {
        dat$.outcome <- ifelse(y == lev[1], 0, 1)
        dist <- binomial()
    }
    else {
        dat$.outcome <- y
        dist <- gaussian()
    }
    modelArgs <- list(formula = modForm, data = dat, select = param$select, 
        method = as.character(param$method))
    theDots <- list(...)
    if (!any(names(theDots) == "family")) 
        modelArgs$family <- dist
    modelArgs <- c(modelArgs, theDots)
    out <- do.call(getFromNamespace("gam", "mgcv"), modelArgs)
    out
}

$gam$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- predict(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$gam$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$gam$predictors
function (x, ...) 
{
    predictors(x$terms)
}

$gam$levels
function (x) 
x$obsLevels

$gam$varImp
function (object, ...) 
{
    smoothed <- summary(object)$s.table[, "p-value", drop = FALSE]
    linear <- summary(object)$p.table
    linear <- linear[, grepl("^Pr", colnames(linear)), drop = FALSE]
    gams <- rbind(smoothed, linear)
    gams <- gams[rownames(gams) != "(Intercept)", , drop = FALSE]
    rownames(gams) <- gsub("^s\\(", "", rownames(gams))
    rownames(gams) <- gsub("\\)$", "", rownames(gams))
    colnames(gams)[1] <- "Overall"
    gams <- as.data.frame(gams)
    gams$Overall <- -log10(gams$Overall)
    allPreds <- colnames(attr(object$terms, "factors"))
    extras <- allPreds[!(allPreds %in% rownames(gams))]
    if (any(extras)) {
        tmp <- data.frame(Overall = rep(NA, length(extras)))
        rownames(tmp) <- extras
        gams <- rbind(gams, tmp)
    }
    gams
}

$gam$notes
[1] "Which terms enter the model in a nonlinear manner is determined by the number of unique values for the predictor. For example, if a predictor only has four unique values, most basis expansion method will fail because there are not enough granularity in the data. By default, a predictor must have at least 10 unique values to be used in a nonlinear basis expansion."

$gam$tags
[1] "Generalized Linear Model"   "Generalized Additive Model"

$gam$sort
function (x) 
x


$gamboost
$gamboost$label
[1] "Boosted Generalized Additive Model"

$gamboost$library
[1] "mboost" "plyr"  

$gamboost$type
[1] "Regression"     "Classification"

$gamboost$parameters
  parameter     class                 label
1     mstop   numeric # Boosting Iterations
2     prune character            AIC Prune?

$gamboost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mstop = floor((1:len) * 50), prune = "no")
    }
    else {
        out <- data.frame(mstop = sample(1:1000, size = len, 
            replace = TRUE), prune = sample(c("yes", "no"), size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$gamboost$loop
function (grid) 
{
    grid <- grid[order(-grid$mstop, grid$prune), ]
    loop <- ddply(grid, .(prune), function(x) data.frame(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        submodels[[i]] <- subset(grid, prune == loop$prune[i] & 
            mstop < loop$mstop[i])
    }
    list(loop = loop[, c("mstop", "prune")], submodels = submodels)
}

$gamboost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$mstop <- param$mstop
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- boost_control(mstop = param$mstop)
    if (!any(names(theDots) == "family")) 
        theDots$family <- if (is.factor(y)) 
            Binomial()
        else GaussReg()
    if (!is.null(wts)) 
        theDots$weights <- wts
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("gamboost", modelArgs)
    if (param$prune == "yes") {
        iters <- if (is.factor(y)) 
            mstop(AIC(out, "classical"))
        else mstop(AIC(out))
        if (iters < out$mstop()) 
            out <- out[iters]
    }
    out$.org.mstop <- out$mstop()
    out$call["x"] <- "xData"
    out$call["y"] <- "yData"
    out
}

$gamboost$predict
function (modelFit, newdata, submodels = NULL) 
{
    predType <- ifelse(modelFit$problemType == "Classification", 
        "class", "response")
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = predType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- as.vector(out)
        for (j in seq(along = submodels$mstop)) {
            this_mstop <- if (submodels$prune[j] == "yes" & submodels$mstop[j] > 
                modelFit$.org.mstop) 
                modelFit$.org.mstop
            else submodels$mstop[j]
            tmp[[j + 1]] <- as.vector(predict(modelFit[this_mstop], 
                newdata, type = predType))
        }
        out <- tmp
        mstop(modelFit) <- modelFit$.org.mstop
    }
    out
}

$gamboost$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    lp <- predict(modelFit, newdata)
    out <- cbind(binomial()$linkinv(-lp), 1 - binomial()$linkinv(-lp))
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            this_mstop <- if (submodels$prune[j] == "yes" & submodels$mstop[j] > 
                modelFit$.org.mstop) 
                modelFit$.org.mstop
            else submodels$mstop[j]
            tmpProb <- predict(modelFit[this_mstop], newdata)
            tmpProb <- cbind(binomial()$linkinv(-tmpProb), 1 - 
                binomial()$linkinv(-tmpProb))
            colnames(tmpProb) <- modelFit$obsLevels
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
        mstop(modelFit) <- modelFit$.org.mstop
    }
    out
}

$gamboost$predictors
function (x, ...) 
{
    strsplit(variable.names(x), ", ")[[1]]
}

$gamboost$notes
[1] "The `prune` option for this model enables the number of iterations to be determined by the optimal AIC value across all iterations. See the examples in `?mstop`. If pruning is not used, the ensemble makes predictions using the exact value of the `mstop` tuning parameter value."

$gamboost$tags
[1] "Generalized Additive Model" "Ensemble Model"            
[3] "Boosting"                   "Implicit Feature Selection"
[5] "Two Class Only"             "Accepts Case Weights"      

$gamboost$levels
function (x) 
levels(x$response)

$gamboost$sort
function (x) 
x[order(x$mstop, x$prune), ]


$gamLoess
$gamLoess$label
[1] "Generalized Additive Model using LOESS"

$gamLoess$library
[1] "gam"

$gamLoess$loop
NULL

$gamLoess$type
[1] "Regression"     "Classification"

$gamLoess$parameters
  parameter   class  label
1      span numeric   Span
2    degree numeric Degree

$gamLoess$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(span = 0.5, degree = 1)
    }
    else {
        out <- data.frame(span = runif(len, min = 0, max = 1), 
            degree = sample(1:2, size = len, replace = TRUE))
    }
    out
}

$gamLoess$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data = if (is.data.frame(x)) x else as.data.frame(x))
    args$data$.outcome <- y
    args$formula <- caret:::smootherFormula(x, smoother = "lo", 
        span = param$span, degree = param$degree)
    theDots <- list(...)
    if (!any(names(theDots) == "family")) 
        args$family <- if (is.factor(y)) 
            binomial
        else gaussian
    if (length(theDots) > 0) 
        args <- c(args, theDots)
    do.call(getFromNamespace("gam", "gam"), args)
}

$gamLoess$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- gam:::predict.gam(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- gam:::predict.gam(modelFit, newdata, type = "response")
    }
    out
}

$gamLoess$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$gamLoess$predictors
function (x, ...) 
{
    getNames <- function(x) {
        x <- strsplit(x, "(\\()|(,)|(\\))")
        x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
        unlist(lapply(x, function(x) x[1]))
    }
    getNames(predictors(x$terms))
}

$gamLoess$varImp
function (object, ...) 
{
    getNames <- function(x) {
        x <- strsplit(x, "(\\()|(,)|(\\))")
        x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
        unlist(lapply(x, function(x) x[1]))
    }
    gamSummary <- gam:::summary.gam(object)
    smoothed <- gamSummary$anova
    smoothed <- smoothed[complete.cases(smoothed), grepl("^P", 
        colnames(smoothed)), drop = FALSE]
    linear <- gamSummary$parametric.anova
    linear <- linear[complete.cases(linear), grepl("^P", colnames(linear)), 
        drop = FALSE]
    linear <- linear[!(rownames(linear) %in% rownames(smoothed)), 
        , drop = FALSE]
    colnames(smoothed) <- colnames(linear) <- "pval"
    gams <- rbind(smoothed, linear)
    gams <- gams[rownames(gams) != "(Intercept)", , drop = FALSE]
    rownames(gams) <- getNames(rownames(gams))
    colnames(gams)[1] <- "Overall"
    gams <- as.data.frame(gams)
    gams$Overall <- -log10(gams$Overall)
    allPreds <- getNames(colnames(attr(object$terms, "factors")))
    extras <- allPreds[!(allPreds %in% rownames(gams))]
    if (any(extras)) {
        tmp <- data.frame(Overall = rep(NA, length(extras)))
        rownames(tmp) <- extras
        gams <- rbind(gams, tmp)
    }
    gams
}

$gamLoess$levels
function (x) 
x$obsLevels

$gamLoess$notes
[1] "Which terms enter the model in a nonlinear manner is determined by the number of unique values for the predictor. For example, if a predictor only has four unique values, most basis expansion method will fail because there are not enough granularity in the data. By default, a predictor must have at least 10 unique values to be used in a nonlinear basis expansion."

$gamLoess$tags
[1] "Generalized Linear Model"   "Generalized Additive Model"

$gamLoess$sort
function (x) 
x


$gamSpline
$gamSpline$label
[1] "Generalized Additive Model using Splines"

$gamSpline$library
[1] "gam"

$gamSpline$loop
NULL

$gamSpline$type
[1] "Regression"     "Classification"

$gamSpline$parameters
  parameter   class              label
1        df numeric Degrees of Freedom

$gamSpline$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(df = seq(1, 3, length = len))
    }
    else {
        out <- data.frame(df = runif(len, min = 0, max = 5))
    }
    out
}

$gamSpline$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data = if (is.data.frame(x)) x else as.data.frame(x))
    args$data$.outcome <- y
    args$formula <- caret:::smootherFormula(x, smoother = "s", 
        df = param$df)
    theDots <- list(...)
    if (!any(names(theDots) == "family")) 
        args$family <- if (is.factor(y)) 
            binomial
        else gaussian
    if (length(theDots) > 0) 
        args <- c(args, theDots)
    do.call(getFromNamespace("gam", "gam"), args)
}

$gamSpline$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- gam:::predict.gam(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$gamSpline$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- gam:::predict.gam(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$gamSpline$levels
function (x) 
x$obsLevels

$gamSpline$predictors
function (x, ...) 
{
    getNames <- function(x) {
        x <- strsplit(x, "(\\()|(,)|(\\))")
        x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
        unlist(lapply(x, function(x) x[1]))
    }
    getNames(predictors(x$terms))
}

$gamSpline$varImp
function (object, ...) 
{
    getNames <- function(x) {
        x <- strsplit(x, "(\\()|(,)|(\\))")
        x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
        unlist(lapply(x, function(x) x[1]))
    }
    gamSummary <- gam:::summary.gam(object)
    smoothed <- gamSummary$anova
    smoothed <- smoothed[complete.cases(smoothed), grepl("^P", 
        colnames(smoothed)), drop = FALSE]
    linear <- gamSummary$parametric.anova
    linear <- linear[complete.cases(linear), grepl("^P", colnames(linear)), 
        drop = FALSE]
    linear <- linear[!(rownames(linear) %in% rownames(smoothed)), 
        , drop = FALSE]
    colnames(smoothed) <- colnames(linear) <- "pval"
    gams <- rbind(smoothed, linear)
    gams <- gams[rownames(gams) != "(Intercept)", , drop = FALSE]
    rownames(gams) <- getNames(rownames(gams))
    colnames(gams)[1] <- "Overall"
    gams <- as.data.frame(gams)
    gams$Overall <- -log10(gams$Overall)
    allPreds <- getNames(colnames(attr(object$terms, "factors")))
    extras <- allPreds[!(allPreds %in% rownames(gams))]
    if (any(extras)) {
        tmp <- data.frame(Overall = rep(NA, length(extras)))
        rownames(tmp) <- extras
        gams <- rbind(gams, tmp)
    }
    gams
}

$gamSpline$notes
[1] "Which terms enter the model in a nonlinear manner is determined by the number of unique values for the predictor. For example, if a predictor only has four unique values, most basis expansion method will fail because there are not enough granularity in the data. By default, a predictor must have at least 10 unique values to be used in a nonlinear basis expansion."

$gamSpline$tags
[1] "Generalized Linear Model"   "Generalized Additive Model"

$gamSpline$sort
function (x) 
x


$gaussprLinear
$gaussprLinear$label
[1] "Gaussian Process"

$gaussprLinear$library
[1] "kernlab"

$gaussprLinear$type
[1] "Regression"     "Classification"

$gaussprLinear$parameters
  parameter     class     label
1 parameter character Parameter

$gaussprLinear$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$gaussprLinear$loop
NULL

$gaussprLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    gausspr(x = as.matrix(x), y = y, kernel = vanilladot, kpar = list(), 
        ...)
}

$gaussprLinear$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$gaussprLinear$prob
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, as.matrix(newdata), type = "probabilities")
}

$gaussprLinear$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$gaussprLinear$tags
[1] "Kernel Method"     "Gaussian Process"  "Linear Classifier"

$gaussprLinear$levels
function (x) 
lev(x)

$gaussprLinear$sort
function (x) 
x


$gaussprPoly
$gaussprPoly$label
[1] "Gaussian Process with Polynomial Kernel"

$gaussprPoly$library
[1] "kernlab"

$gaussprPoly$type
[1] "Regression"     "Classification"

$gaussprPoly$parameters
  parameter   class             label
1    degree numeric Polynomial Degree
2     scale numeric             Scale

$gaussprPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(degree = seq(1, min(len, 3)), scale = 10^((1:len) - 
            4))
    }
    else {
        out <- data.frame(degree = sample(1:3, size = len, replace = TRUE), 
            scale = 10^runif(len, min = -5, 0))
    }
    out
}

$gaussprPoly$loop
NULL

$gaussprPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    gausspr(x = as.matrix(x), y = y, kernel = polydot(degree = param$degree, 
        scale = param$scale, offset = 1), ...)
}

$gaussprPoly$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$gaussprPoly$prob
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, as.matrix(newdata), type = "probabilities")
}

$gaussprPoly$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$xscale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$gaussprPoly$tags
[1] "Kernel Method"    "Gaussian Process" "Polynomial Model"

$gaussprPoly$levels
function (x) 
lev(x)

$gaussprPoly$sort
function (x) 
x


$gaussprRadial
$gaussprRadial$label
[1] "Gaussian Process with Radial Basis Function Kernel"

$gaussprRadial$library
[1] "kernlab"

$gaussprRadial$type
[1] "Regression"     "Classification"

$gaussprRadial$parameters
  parameter   class label
1     sigma numeric Sigma

$gaussprRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = mean(as.vector(sigmas[-2])))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])))
    }
    out
}

$gaussprRadial$loop
NULL

$gaussprRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    gausspr(x = as.matrix(x), y = y, kernel = rbfdot, kpar = list(sigma = param$sigma), 
        ...)
}

$gaussprRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$gaussprRadial$prob
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, as.matrix(newdata), type = "probabilities")
}

$gaussprRadial$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$gaussprRadial$tags
[1] "Kernel Method"         "Gaussian Process"      "Radial Basis Function"

$gaussprRadial$levels
function (x) 
lev(x)

$gaussprRadial$sort
function (x) 
x


$gbm
$gbm$label
[1] "Stochastic Gradient Boosting"

$gbm$library
[1] "gbm"  "plyr"

$gbm$type
[1] "Regression"     "Classification"

$gbm$parameters
          parameter   class                   label
1           n.trees numeric   # Boosting Iterations
2 interaction.depth numeric          Max Tree Depth
3         shrinkage numeric               Shrinkage
4    n.minobsinnode numeric Min. Terminal Node Size

$gbm$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(interaction.depth = seq(1, len), n.trees = floor((1:len) * 
            50), shrinkage = 0.1, n.minobsinnode = 10)
    }
    else {
        out <- data.frame(n.trees = floor(runif(len, min = 1, 
            max = 5000)), interaction.depth = sample(1:10, replace = TRUE, 
            size = len), shrinkage = runif(len, min = 0.001, 
            max = 0.6), n.minobsinnode = sample(5:25, replace = TRUE, 
            size = len))
        out <- out[!duplicated(out), ]
    }
    out
}

$gbm$loop
function (grid) 
{
    loop <- ddply(grid, c("shrinkage", "interaction.depth", "n.minobsinnode"), 
        function(x) c(n.trees = max(x$n.trees)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$n.trees)) {
        index <- which(grid$interaction.depth == loop$interaction.depth[i] & 
            grid$shrinkage == loop$shrinkage[i] & grid$n.minobsinnode == 
            loop$n.minobsinnode[i])
        trees <- grid[index, "n.trees"]
        submodels[[i]] <- data.frame(n.trees = trees[trees != 
            loop$n.trees[i]])
    }
    list(loop = loop, submodels = submodels)
}

$gbm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "distribution")) {
        modDist <- theDots$distribution
        theDots$distribution <- NULL
    }
    else {
        if (is.numeric(y)) {
            modDist <- "gaussian"
        }
        else modDist <- if (length(lev) == 2) 
            "bernoulli"
        else "multinomial"
    }
    if (!is.null(wts)) 
        theDots$w <- wts
    if (is.factor(y) && length(lev) == 2) 
        y <- ifelse(y == lev[1], 1, 0)
    modArgs <- list(x = x, y = y, interaction.depth = param$interaction.depth, 
        n.trees = param$n.trees, shrinkage = param$shrinkage, 
        n.minobsinnode = param$n.minobsinnode, distribution = modDist)
    if (any(names(theDots) == "family")) 
        modArgs$distribution <- NULL
    if (length(theDots) > 0) 
        modArgs <- c(modArgs, theDots)
    do.call("gbm.fit", modArgs)
}

$gbm$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response", n.trees = modelFit$tuneValue$n.trees)
    out[is.nan(out)] <- NA
    out <- switch(modelFit$distribution$name, multinomial = {
        colnames(out[, , 1, drop = FALSE])[apply(out[, , 1, drop = FALSE], 
            1, which.max)]
    }, bernoulli = , adaboost = , huberized = {
        ifelse(out >= 0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }, gaussian = , laplace = , tdist = , poisson = , quantile = {
        out
    })
    if (!is.null(submodels)) {
        tmp <- predict(modelFit, newdata, type = "response", 
            n.trees = submodels$n.trees)
        out <- switch(modelFit$distribution$name, multinomial = {
            lvl <- colnames(tmp[, , 1, drop = FALSE])
            tmp <- apply(tmp, 3, function(x) apply(x, 1, which.max))
            if (is.vector(tmp)) tmp <- matrix(tmp, nrow = 1)
            tmp <- t(apply(tmp, 1, function(x, lvl) lvl[x], lvl = lvl))
            if (nrow(tmp) == 1 & nrow(newdata) > 1) tmp <- t(tmp)
            tmp <- as.list(as.data.frame(tmp, stringsAsFactors = FALSE))
            c(list(out), tmp)
        }, bernoulli = , adaboost = , huberized = {
            tmp <- ifelse(tmp >= 0.5, modelFit$obsLevels[1], 
                modelFit$obsLevels[2])
            tmp <- as.list(as.data.frame(tmp, stringsAsFactors = FALSE))
            c(list(out), tmp)
        }, gaussian = , laplace = , tdist = , poisson = , quantile = {
            tmp <- as.list(as.data.frame(tmp))
            c(list(out), tmp)
        })
    }
    out
}

$gbm$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response", n.trees = modelFit$tuneValue$n.trees)
    out[is.nan(out)] <- NA
    out <- switch(modelFit$distribution$name, multinomial = {
        out <- if (dim(out)[3] == 1) as.data.frame(out) else out[, 
            , 1]
        colnames(out) <- modelFit$obsLevels
        out
    }, bernoulli = , adaboost = , huberized = {
        out <- cbind(out, 1 - out)
        colnames(out) <- modelFit$obsLevels
        out
    }, gaussian = , laplace = , tdist = , poisson = {
        out
    })
    if (!is.null(submodels)) {
        tmp <- predict(modelFit, newdata, type = "response", 
            n.trees = submodels$n.trees)
        tmp <- switch(modelFit$distribution$name, multinomial = {
            apply(tmp, 3, function(x) data.frame(x))
        }, bernoulli = , adaboost = , huberized = {
            tmp <- as.list(as.data.frame(tmp))
            lapply(tmp, function(x, lvl) {
                x <- cbind(x, 1 - x)
                colnames(x) <- lvl
                x
            }, lvl = modelFit$obsLevels)
        })
        out <- c(list(out), tmp)
    }
    out
}

$gbm$predictors
function (x, ...) 
{
    vi <- relative.influence(x, n.trees = x$tuneValue$n.trees)
    names(vi)[vi > 0]
}

$gbm$varImp
function (object, numTrees = NULL, ...) 
{
    if (is.null(numTrees)) 
        numTrees <- object$tuneValue$n.trees
    varImp <- relative.influence(object, n.trees = numTrees)
    out <- data.frame(varImp)
    colnames(out) <- "Overall"
    rownames(out) <- object$var.names
    out
}

$gbm$levels
function (x) 
{
    if (x$distribution$name %in% c("gaussian", "laplace", "tdist")) 
        return(NULL)
    if (is.null(x$classes)) {
        out <- if (any(names(x) == "obsLevels")) 
            x$obsLevels
        else NULL
    }
    else {
        out <- x$classes
    }
    out
}

$gbm$tags
[1] "Tree-Based Model"           "Boosting"                  
[3] "Ensemble Model"             "Implicit Feature Selection"
[5] "Accepts Case Weights"      

$gbm$sort
function (x) 
{
    x[order(x$n.trees, x$interaction.depth, x$shrinkage), ]
}


$gcvEarth
$gcvEarth$label
[1] "Multivariate Adaptive Regression Splines"

$gcvEarth$library
[1] "earth"

$gcvEarth$type
[1] "Regression"     "Classification"

$gcvEarth$parameters
  parameter   class          label
1    degree numeric Product Degree

$gcvEarth$grid
function (x, y, len = NULL, search = "grid") 
{
    data.frame(degree = 1)
}

$gcvEarth$loop
NULL

$gcvEarth$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots$keepxy <- TRUE
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(x = x, y = y, degree = param$degree), 
        theDots)
    if (is.factor(y)) 
        modelArgs$glm <- list(family = binomial)
    tmp <- do.call("earth", modelArgs)
    tmp$call["degree"] <- param$degree
    tmp
}

$gcvEarth$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata)
    }
    as.vector(out)
}

$gcvEarth$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    colnames(out) <- modelFit$obsLevels
    out
}

$gcvEarth$predictors
function (x, ...) 
{
    vi <- varImp(x)
    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 
        0)))))
    if (length(notZero) > 0) 
        rownames(vi)[notZero]
    else NULL
}

$gcvEarth$varImp
function (object, value = "gcv", ...) 
{
    earthImp <- evimp(object)
    if (!is.matrix(earthImp)) 
        earthImp <- t(as.matrix(earthImp))
    out <- earthImp
    perfCol <- which(colnames(out) == value)
    increaseInd <- out[, perfCol + 1]
    out <- as.data.frame(out[, perfCol, drop = FALSE])
    colnames(out) <- "Overall"
    if (any(earthImp[, "used"] == 0)) {
        dropList <- grep("-unused", rownames(earthImp), value = TRUE)
        out$Overall[rownames(out) %in% dropList] <- 0
    }
    rownames(out) <- gsub("-unused", "", rownames(out))
    out <- as.data.frame(out)
    xNames <- object$namesx.org
    if (any(!(xNames %in% rownames(out)))) {
        xNames <- xNames[!(xNames %in% rownames(out))]
        others <- data.frame(Overall = rep(0, length(xNames)), 
            row.names = xNames)
        out <- rbind(out, others)
    }
    out
}

$gcvEarth$levels
function (x) 
x$levels

$gcvEarth$tags
[1] "Multivariate Adaptive Regression Splines"
[2] "Implicit Feature Selection"              
[3] "Accepts Case Weights"                    

$gcvEarth$sort
function (x) 
x[order(x$degree), ]


$GFS.FR.MOGUL
$GFS.FR.MOGUL$label
[1] "Fuzzy Rules via MOGUL"

$GFS.FR.MOGUL$library
[1] "frbs"

$GFS.FR.MOGUL$type
[1] "Regression"

$GFS.FR.MOGUL$parameters
  parameter   class                  label
1   max.gen numeric       Max. Generations
2  max.iter numeric        Max. Iterations
3  max.tune numeric Max. Tuning Iterations

$GFS.FR.MOGUL$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(max.gen = 10 * (1:len), max.iter = 10, 
            max.tune = 10 * (1:len))
    }
    else {
        out <- data.frame(max.gen = sample(1:20, size = len, 
            replace = TRUE), max.iter = sample(1:20, replace = TRUE, 
            size = len), max.tune = sample(1:20, size = len, 
            replace = TRUE))
    }
    out
}

$GFS.FR.MOGUL$loop
NULL

$GFS.FR.MOGUL$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "GFS.FR.MOGUL")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$max.gen <- param$max.gen
        theDots$control$max.iter <- param$max.iter
        theDots$control$max.tune <- param$max.tune
    }
    else theDots$control <- list(max.gen = param$max.gen, max.iter = param$max.iter, 
        max.tune = param$max.tune, persen_cross = 0.6, persen_mutant = 0.3, 
        epsilon = 0.4, name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$GFS.FR.MOGUL$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$GFS.FR.MOGUL$prob
NULL

$GFS.FR.MOGUL$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$GFS.FR.MOGUL$tags
[1] "Rule-Based Model"

$GFS.FR.MOGUL$levels
NULL

$GFS.FR.MOGUL$sort
function (x) 
x[order(x$max.iter), ]


$GFS.GCCL
$GFS.GCCL$label
[1] "Fuzzy Rules Using Genetic Cooperative-Competitive Learning"

$GFS.GCCL$library
[1] "frbs"

$GFS.GCCL$type
[1] "Classification"

$GFS.GCCL$parameters
   parameter   class            label
1 num.labels numeric     #Fuzzy Terms
2  popu.size numeric  Population Size
3    max.gen numeric Max. Generations

$GFS.GCCL$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, popu.size = 10, 
            max.gen = 10)
    }
    else {
        out <- data.frame(num.labels = sample(2:20, size = len, 
            replace = TRUE), popu.size = sample(seq(2, 20, by = 2), 
            size = len, replace = TRUE), max.gen = sample(1:20, 
            size = len, replace = TRUE))
    }
    out
}

$GFS.GCCL$loop
NULL

$GFS.GCCL$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))), 
        method.type = "GFS.GCCL")
    args$range.data <- apply(x, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$popu.size <- param$popu.size
        theDots$control$max.gen <- param$max.gen
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        popu.size = param$popu.size, max.gen = param$max.gen, 
        persen_cross = 0.6, persen_mutant = 0.3, num.class = length(unique(y)), 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$GFS.GCCL$predict
function (modelFit, newdata, submodels = NULL) 
{
    modelFit$obsLevels[predict(modelFit, newdata)[, 1]]
}

$GFS.GCCL$prob
NULL

$GFS.GCCL$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$GFS.GCCL$tags
[1] "Rule-Based Model"

$GFS.GCCL$levels
NULL

$GFS.GCCL$sort
function (x) 
x[order(x$num.labels), ]


$GFS.LT.RS
$GFS.LT.RS$label
[1] "Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems"

$GFS.LT.RS$library
[1] "frbs"

$GFS.LT.RS$type
[1] "Regression"

$GFS.LT.RS$parameters
   parameter   class            label
1  popu.size numeric  Population Size
2 num.labels numeric   # Fuzzy Labels
3    max.gen numeric Max. Generations

$GFS.LT.RS$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(popu.size = 10 * (1:len), num.labels = 1 + 
            (1:len) * 2, max.gen = 10)
    }
    else {
        out <- data.frame(max.gen = sample(1:20, size = len, 
            replace = TRUE), popu.size = sample(seq(2, 20, by = 2), 
            size = len, replace = TRUE), num.labels = sample(2:20, 
            size = len, replace = TRUE))
    }
    out
}

$GFS.LT.RS$loop
NULL

$GFS.LT.RS$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "GFS.LT.RS")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$popu.size <- param$popu.size
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.gen <- param$max.gen
    }
    else theDots$control <- list(popu.size = param$popu.size, 
        num.labels = param$num.labels, max.gen = param$max.gen, 
        persen_cross = 0.6, persen_mutant = 0.3, mode.tuning = "GLOBAL", 
        type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", 
        type.defuz = "WAM", rule.selection = FALSE, name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$GFS.LT.RS$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$GFS.LT.RS$prob
NULL

$GFS.LT.RS$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$GFS.LT.RS$tags
[1] "Rule-Based Model"

$GFS.LT.RS$levels
NULL

$GFS.LT.RS$sort
function (x) 
x[order(x$num.labels), ]


$GFS.THRIFT
$GFS.THRIFT$label
[1] "Fuzzy Rules via Thrift"

$GFS.THRIFT$library
[1] "frbs"

$GFS.THRIFT$type
[1] "Regression"

$GFS.THRIFT$parameters
   parameter   class            label
1  popu.size numeric  Population Size
2 num.labels numeric   # Fuzzy Labels
3    max.gen numeric Max. Generations

$GFS.THRIFT$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(popu.size = 10 * (1:len), num.labels = 1 + 
            (1:len) * 2, max.gen = 10)
    }
    else {
        out <- data.frame(max.gen = sample(1:20, size = len, 
            replace = TRUE), popu.size = sample(seq(2, 20, by = 2), 
            size = len, replace = TRUE), num.labels = sample(2:20, 
            size = len, replace = TRUE))
    }
    out
}

$GFS.THRIFT$loop
NULL

$GFS.THRIFT$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "GFS.THRIFT")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$popu.size <- param$popu.size
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.gen <- param$max.gen
    }
    else theDots$control <- list(popu.size = param$popu.size, 
        num.labels = param$num.labels, max.gen = param$max.gen, 
        persen_cross = 0.6, persen_mutant = 0.3, type.defuz = "WAM", 
        type.tnorm = "MIN", type.snorm = "MAX", type.mf = "TRIANGLE", 
        type.implication.func = "ZADEH", name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$GFS.THRIFT$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$GFS.THRIFT$prob
NULL

$GFS.THRIFT$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$GFS.THRIFT$tags
[1] "Rule-Based Model"

$GFS.THRIFT$levels
NULL

$GFS.THRIFT$sort
function (x) 
x[order(x$num.labels), ]


$glm
$glm$label
[1] "Generalized Linear Model"

$glm$library
NULL

$glm$loop
NULL

$glm$type
[1] "Regression"     "Classification"

$glm$parameters
  parameter     class     label
1 parameter character parameter

$glm$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$glm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (length(levels(y)) > 2) 
        stop("glm models can only use 2-class outcomes")
    theDots <- list(...)
    if (!any(names(theDots) == "family")) {
        theDots$family <- if (is.factor(y)) 
            binomial()
        else gaussian()
    }
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat), theDots)
    out <- do.call("glm", modelArgs)
    out$call <- NULL
    out
}

$glm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- predict(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$glm$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    dimnames(out)[[2]] <- modelFit$obsLevels
    out
}

$glm$varImp
function (object, ...) 
{
    values <- summary(object)$coef
    varImps <- abs(values[-1, grep("value$", colnames(values))])
    out <- data.frame(varImps)
    colnames(out) <- "Overall"
    if (!is.null(names(varImps))) 
        rownames(out) <- names(varImps)
    out
}

$glm$predictors
function (x, ...) 
predictors(x$terms)

$glm$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$glm$trim
function (x) 
{
    x$y = c()
    x$model = c()
    x$residuals = c()
    x$fitted.values = c()
    x$effects = c()
    x$qr$qr = c()
    x$linear.predictors = c()
    x$weights = c()
    x$prior.weights = c()
    x$data = c()
    x$family$variance = c()
    x$family$dev.resids = c()
    x$family$aic = c()
    x$family$validmu = c()
    x$family$simulate = c()
    attr(x$terms, ".Environment") = c()
    attr(x$formula, ".Environment") = c()
    x
}

$glm$tags
[1] "Generalized Linear Model" "Linear Classifier"       
[3] "Two Class Only"           "Accepts Case Weights"    

$glm$sort
function (x) 
x


$glmboost
$glmboost$label
[1] "Boosted Generalized Linear Model"

$glmboost$library
[1] "plyr"   "mboost"

$glmboost$type
[1] "Regression"     "Classification"

$glmboost$parameters
  parameter     class                 label
1     mstop   numeric # Boosting Iterations
2     prune character            AIC Prune?

$glmboost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mstop = floor((1:len) * 50), prune = "no")
    }
    else {
        out <- data.frame(mstop = sample(1:1000, size = len, 
            replace = TRUE), prune = sample(c("yes", "no"), size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$glmboost$loop
function (grid) 
{
    grid <- grid[order(-grid$mstop, grid$prune), ]
    loop <- ddply(grid, .(prune), function(x) data.frame(mstop = max(x$mstop)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$mstop)) {
        submodels[[i]] <- subset(grid, prune == loop$prune[i] & 
            mstop < loop$mstop[i])
    }
    list(loop = loop[, c("mstop", "prune")], submodels = submodels)
}

$glmboost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$mstop <- param$mstop
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- boost_control(mstop = param$mstop)
    if (!any(names(theDots) == "family")) 
        theDots$family <- if (is.factor(y)) 
            Binomial()
        else GaussReg()
    if (!is.null(wts)) 
        theDots$weights <- wts
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call(mboost:::glmboost.formula, modelArgs)
    if (param$prune == "yes") {
        iters <- if (is.factor(y)) 
            mstop(AIC(out, "classical"))
        else mstop(AIC(out))
        if (iters < out$mstop()) 
            out <- out[iters]
    }
    out$.org.mstop <- out$mstop()
    out$call["x"] <- "xData"
    out$call["y"] <- "yData"
    out
}

$glmboost$predict
function (modelFit, newdata, submodels = NULL) 
{
    predType <- ifelse(modelFit$problemType == "Classification", 
        "class", "response")
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = predType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- as.vector(out)
        for (j in seq(along = submodels$mstop)) {
            this_mstop <- if (submodels$prune[j] == "yes" & submodels$mstop[j] > 
                modelFit$.org.mstop) 
                modelFit$.org.mstop
            else submodels$mstop[j]
            tmp[[j + 1]] <- as.vector(predict(modelFit[this_mstop], 
                newdata, type = predType))
        }
        out <- tmp
        mstop(modelFit) <- modelFit$.org.mstop
    }
    out
}

$glmboost$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    lp <- predict(modelFit, newdata)
    out <- cbind(binomial()$linkinv(-lp), 1 - binomial()$linkinv(-lp))
    colnames(out) <- modelFit$obsLevels
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$mstop)) {
            this_mstop <- if (submodels$prune[j] == "yes" & submodels$mstop[j] > 
                modelFit$.org.mstop) 
                modelFit$.org.mstop
            else submodels$mstop[j]
            tmpProb <- predict(modelFit[this_mstop], newdata)
            tmpProb <- cbind(binomial()$linkinv(-tmpProb), 1 - 
                binomial()$linkinv(-tmpProb))
            colnames(tmpProb) <- modelFit$obsLevels
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
        mstop(modelFit) <- modelFit$.org.mstop
    }
    out
}

$glmboost$predictors
function (x, ...) 
{
    strsplit(variable.names(x), ", ")[[1]]
}

$glmboost$levels
function (x) 
levels(x$response)

$glmboost$notes
[1] "The `prune` option for this model enables the number of iterations to be determined by the optimal AIC value across all iterations. See the examples in `?mstop`. If pruning is not used, the ensemble makes predictions using the exact value of the `mstop` tuning parameter value."

$glmboost$tags
[1] "Generalized Linear Model" "Ensemble Model"          
[3] "Boosting"                 "Linear Classifier"       
[5] "Two Class Only"           "Accepts Case Weights"    

$glmboost$sort
function (x) 
x[order(x$mstop, x$prune), ]


$glmnet
$glmnet$label
[1] "glmnet"

$glmnet$library
[1] "glmnet"

$glmnet$type
[1] "Regression"     "Classification"

$glmnet$parameters
  parameter   class                    label
1     alpha numeric        Mixing Percentage
2    lambda numeric Regularization Parameter

$glmnet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        numLev <- if (is.character(y) | is.factor(y)) 
            length(levels(y))
        else NA
        if (!is.na(numLev)) {
            fam <- ifelse(numLev > 2, "multinomial", "binomial")
        }
        else fam <- "gaussian"
        init <- glmnet(as.matrix(x), y, family = fam, nlambda = len + 
            2, alpha = 0.5)
        lambda <- unique(init$lambda)
        lambda <- lambda[-c(1, length(lambda))]
        lambda <- lambda[1:min(length(lambda), len)]
        out <- expand.grid(alpha = seq(0.1, 1, length = len), 
            lambda = lambda)
    }
    else {
        out <- data.frame(alpha = runif(len, min = 0, 1), lambda = 2^runif(len, 
            min = -10, 3))
    }
    out
}

$glmnet$loop
function (grid) 
{
    alph <- unique(grid$alpha)
    loop <- data.frame(alpha = alph)
    loop$lambda <- NA
    submodels <- vector(mode = "list", length = length(alph))
    for (i in seq(along = alph)) {
        np <- grid[grid$alpha == alph[i], "lambda"]
        loop$lambda[loop$alpha == alph[i]] <- np[which.max(np)]
        submodels[[i]] <- data.frame(lambda = np[-which.max(np)])
    }
    list(loop = loop, submodels = submodels)
}

$glmnet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    numLev <- if (is.character(y) | is.factor(y)) 
        length(levels(y))
    else NA
    theDots <- list(...)
    if (all(names(theDots) != "family")) {
        if (!is.na(numLev)) {
            fam <- ifelse(numLev > 2, "multinomial", "binomial")
        }
        else fam <- "gaussian"
        theDots$family <- fam
    }
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(x = as.matrix(x), y = y, alpha = param$alpha), 
        theDots)
    out <- do.call("glmnet", modelArgs)
    if (!is.na(param$lambda[1])) 
        out$lambdaOpt <- param$lambda[1]
    out
}

$glmnet$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    if (length(modelFit$obsLevels) < 2) {
        out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)
    }
    else {
        out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, 
            type = "class")
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    if (!is.null(submodels)) {
        if (length(modelFit$obsLevels) < 2) {
            tmp <- as.list(as.data.frame(predict(modelFit, newdata, 
                s = submodels$lambda)))
        }
        else {
            tmp <- predict(modelFit, newdata, s = submodels$lambda, 
                type = "class")
            tmp <- if (is.matrix(tmp)) 
                as.data.frame(tmp, stringsAsFactors = FALSE)
            else as.character(tmp)
            tmp <- as.list(tmp)
        }
        out <- c(list(out), tmp)
    }
    out
}

$glmnet$prob
function (modelFit, newdata, submodels = NULL) 
{
    obsLevels <- if ("classnames" %in% names(modelFit)) 
        modelFit$classnames
    else NULL
    probs <- predict(modelFit, as.matrix(newdata), s = modelFit$lambdaOpt, 
        type = "response")
    if (length(obsLevels) == 2) {
        probs <- as.vector(probs)
        probs <- as.data.frame(cbind(1 - probs, probs))
        colnames(probs) <- modelFit$obsLevels
    }
    else {
        probs <- as.data.frame(probs[, , 1, drop = FALSE])
        names(probs) <- modelFit$obsLevels
    }
    if (!is.null(submodels)) {
        tmp <- predict(modelFit, as.matrix(newdata), s = submodels$lambda, 
            type = "response")
        if (length(obsLevels) == 2) {
            tmp <- as.list(as.data.frame(tmp))
            tmp <- lapply(tmp, function(x, lev) {
                x <- as.vector(x)
                tmp <- data.frame(1 - x, x)
                names(tmp) <- lev
                tmp
            }, lev = modelFit$obsLevels)
        }
        else tmp <- apply(tmp, 3, function(x) data.frame(x))
        probs <- if (is.list(tmp)) 
            c(list(probs), tmp)
        else list(probs, tmp)
    }
    probs
}

$glmnet$predictors
function (x, lambda = NULL, ...) 
{
    if (is.null(lambda)) {
        if (length(lambda) > 1) 
            stop("Only one value of lambda is allowed right now")
        if (!is.null(x$lambdaOpt)) {
            lambda <- x$lambdaOpt
        }
        else stop("must supply a value of lambda")
    }
    allVar <- if (is.list(x$beta)) 
        rownames(x$beta[[1]])
    else rownames(x$beta)
    out <- unlist(predict(x, s = lambda, type = "nonzero"))
    out <- unique(out)
    if (length(out) > 0) {
        out <- out[!is.na(out)]
        out <- allVar[out]
    }
    out
}

$glmnet$varImp
function (object, lambda = NULL, ...) 
{
    if (is.null(lambda)) {
        if (length(lambda) > 1) 
            stop("Only one value of lambda is allowed right now")
        if (!is.null(object$lambdaOpt)) {
            lambda <- object$lambdaOpt
        }
        else stop("must supply a value of lambda")
    }
    beta <- predict(object, s = lambda, type = "coef")
    if (is.list(beta)) {
        out <- do.call("cbind", lapply(beta, function(x) x[, 
            1]))
        out <- as.data.frame(out)
    }
    else out <- data.frame(Overall = beta[, 1])
    out <- abs(out[rownames(out) != "(Intercept)", , drop = FALSE])
    out
}

$glmnet$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$glmnet$tags
[1] "Generalized Linear Model"   "Implicit Feature Selection"
[3] "L1 Regularization"          "L2 Regularization"         
[5] "Linear Classifier"          "Linear Regression"         

$glmnet$sort
function (x) 
x[order(-x$lambda, x$alpha), ]

$glmnet$trim
function (x) 
{
    x$call <- NULL
    x$df <- NULL
    x$dev.ratio <- NULL
    x
}


$glmStepAIC
$glmStepAIC$label
[1] "Generalized Linear Model with Stepwise Feature Selection"

$glmStepAIC$library
[1] "MASS"

$glmStepAIC$loop
NULL

$glmStepAIC$type
[1] "Regression"     "Classification"

$glmStepAIC$parameters
  parameter     class     label
1 parameter character parameter

$glmStepAIC$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$glmStepAIC$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (length(levels(y)) > 2) 
        stop("glm models can only use 2-class outcomes")
    stepArgs <- names(formals(stepAIC))
    stepArgs <- stepArgs[!(stepArgs %in% c("object", "..."))]
    theDots <- list(...)
    glmArgs <- list()
    if (!any(names(theDots) == "family")) {
        glmArgs$family <- if (is.factor(y)) 
            binomial()
        else gaussian()
    }
    else glmArgs$family <- theDots$family
    if (any(!(names(theDots) %in% stepArgs))) 
        theDots <- theDots[names(theDots) %in% stepArgs]
    if (any(names(theDots) == "direction")) {
        if (theDots$direction == "forward") {
            start_form <- as.formula(".outcome ~ 1")
            if (!any(names(theDots) == "scope")) {
                theDots$scope <- list(lower = as.formula(".outcome ~ 1"), 
                  upper = as.formula(paste0(".outcome~", paste0(colnames(x), 
                    collapse = "+"))))
            }
        }
        else {
            start_form <- as.formula(".outcome ~ .")
        }
    }
    else start_form <- as.formula(".outcome ~ .")
    if (!is.null(wts)) 
        glmArgs$weights <- wts
    modelArgs <- c(list(formula = start_form, data = dat), glmArgs)
    mod <- do.call("glm", modelArgs)
    theDots$object <- mod
    out <- do.call("stepAIC", theDots)
    out$call <- NULL
    out
}

$glmStepAIC$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    if (modelFit$problemType == "Classification") {
        probs <- predict(modelFit, newdata, type = "response")
        out <- ifelse(probs < 0.5, modelFit$obsLevel[1], modelFit$obsLevel[2])
    }
    else {
        out <- predict(modelFit, newdata, type = "response")
    }
    out
}

$glmStepAIC$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    dimnames(out)[[2]] <- modelFit$obsLevels
    out
}

$glmStepAIC$levels
function (x) 
x$obsLevels

$glmStepAIC$tags
[1] "Generalized Linear Model"   "Feature Selection Wrapper" 
[3] "Linear Classifier"          "Implicit Feature Selection"
[5] "Two Class Only"             "Accepts Case Weights"      

$glmStepAIC$sort
function (x) 
x


$gpls
$gpls$label
[1] "Generalized Partial Least Squares"

$gpls$library
[1] "gpls"

$gpls$loop
NULL

$gpls$type
[1] "Classification"

$gpls$parameters
  parameter   class       label
1    K.prov numeric #Components

$gpls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(K.prov = seq(1, len))
    }
    else {
        out <- data.frame(K.prov = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$gpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
gpls(x, y, K.prov = param$K.prov, ...)

$gpls$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$gpls$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$predicted
    out <- cbind(out, 1 - out)
    colnames(out) <- modelFit$obsLevels
    out
}

$gpls$predictors
function (x, ...) 
{
    out <- if (hasTerms(x)) 
        predictors(x$terms)
    else colnames(x$data$x.order)
    out[!(out %in% "Intercept")]
}

$gpls$tags
[1] "Logistic Regression"   "Partial Least Squares" "Linear Classifier"    

$gpls$sort
function (x) 
x[order(x[, 1]), ]

$gpls$levels
function (x) 
x$obsLevels


$hda
$hda$label
[1] "Heteroscedastic Discriminant Analysis"

$hda$library
[1] "hda"

$hda$loop
NULL

$hda$type
[1] "Classification"

$hda$parameters
  parameter   class                                    label
1     gamma numeric                                    Gamma
2    lambda numeric                                   Lambda
3    newdim numeric Dimension of the Discriminative Subspace

$hda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(gamma = seq(0.1, 1, length = len), 
            lambda = seq(0, 1, length = len), newdim = 2:(min(len, 
                ncol(x))))
    }
    else {
        out <- data.frame(gamma = runif(len, min = 0, max = 1), 
            lambda = runif(len, min = 0, max = 1), newdim = sample(2:ncol(x), 
                size = len, replace = TRUE))
    }
    out
}

$hda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
hda(x, y, newdim = param$newdim, reg.lamb = param$lambda, reg.gamm = param$gamma, 
    crule = TRUE, ...)

$hda$predict
function (modelFit, newdata, submodels = NULL) 
{
    tmp <- predict(modelFit, as.matrix(newdata))
    if (is.vector(tmp)) 
        tmp <- matrix(tmp, ncol = 1)
    as.character(predict(modelFit$naivebayes, tmp))
}

$hda$prob
function (modelFit, newdata, submodels = NULL) 
{
    tmp <- predict(modelFit, as.matrix(newdata))
    if (is.vector(tmp)) 
        tmp <- matrix(tmp, ncol = 1)
    as.data.frame(predict(modelFit$naivebayes, tmp, type = "raw"))
}

$hda$levels
function (x) 
x$obsLevels

$hda$tags
[1] "Discriminant Analysis" "Linear Classifier"     "Regularization"       

$hda$sort
function (x) 
x[order(x$newdim, -x$lambda, x$gamma), ]


$hdda
$hdda$label
[1] "High Dimensional Discriminant Analysis"

$hdda$library
[1] "HDclassif"

$hdda$loop
NULL

$hdda$type
[1] "Classification"

$hdda$parameters
  parameter     class      label
1 threshold character  Threshold
2     model   numeric Model Type

$hdda$grid
function (x, y, len = NULL, search = "grid") 
{
    mods <- c("AkjBkQkDk", "AkBkQkDk", "ABkQkDk", "AkjBQkDk", 
        "AkBQkDk", "ABQkDk", "AkjBkQkD", "AkBkQkD", "ABkQkD", 
        "AkjBQkD", "AkBQkD", "ABQkD", "AjBQD", "ABQD")
    if (search == "grid") {
        out <- expand.grid(model = c("all"), threshold = seq(0.05, 
            0.3, length = len))
    }
    else {
        out <- data.frame(model = sample(mods, size = len, replace = TRUE), 
            threshold = runif(len, min = 0, max = 1))
    }
    out
}

$hdda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    hdda(x, y, model = as.character(param$model), threshold = param$threshold, 
        ...)
}

$hdda$predict
function (modelFit, newdata, submodels = NULL) 
{
    as.character(predict(modelFit, newdata)$class)
}

$hdda$prob
function (modelFit, newdata, submodels = NULL) 
{
    data.frame(unclass(predict(modelFit, newdata)$posterior))
}

$hdda$levels
function (x) 
x$obsLevels

$hdda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$hdda$sort
function (x) 
x[order(-x$threshold), ]


$hdrda
$hdrda$label
[1] "High-Dimensional Regularized Discriminant Analysis"

$hdrda$library
[1] "sparsediscrim"

$hdrda$loop
NULL

$hdrda$type
[1] "Classification"

$hdrda$parameters
       parameter     class          label
1          gamma   numeric          Gamma
2         lambda   numeric         Lambda
3 shrinkage_type character Shrinkage Type

$hdrda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(gamma = seq(0, 1, length = len), lambda = seq(0, 
            1, length = len), shrinkage_type = c("ridge", "convex"))
    }
    else {
        out <- data.frame(gamma = runif(len, min = 0, max = 1), 
            lambda = runif(len, min = 0, max = 1), shrinkage_type = sample(c("ridge", 
                "convex"), size = len, replace = TRUE))
    }
    out
}

$hdrda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    hdrda(x, y, gamma = param$gamma, lambda = param$lambda, shrinkage_type = as.character(param$shrinkage_type), 
        ...)
}

$hdrda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$hdrda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$posterior

$hdrda$predictors
function (x, ...) 
x$varnames

$hdrda$tags
[1] "Discriminant Analysis" "Polynomial Model"      "Regularization"       
[4] "Linear Classifier"    

$hdrda$levels
function (x) 
names(x$prior)

$hdrda$sort
function (x) 
{
    x[order(-x$lambda, x$gamma), ]
}


$HYFIS
$HYFIS$label
[1] "Hybrid Neural Fuzzy Inference System"

$HYFIS$library
[1] "frbs"

$HYFIS$type
[1] "Regression"

$HYFIS$parameters
   parameter   class           label
1 num.labels numeric    #Fuzzy Terms
2   max.iter numeric Max. Iterations

$HYFIS$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, max.iter = 10)
    }
    else {
        out <- data.frame(num.labels = sample(2:20, size = len, 
            replace = TRUE), max.iter = sample(1:20, replace = TRUE, 
            size = len))
    }
    out
}

$HYFIS$loop
NULL

$HYFIS$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)), method.type = "HYFIS")
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.iter <- param$max.iter
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        max.iter = param$max.iter, step.size = 0.01, type.tnorm = "MIN", 
        type.snorm = "MAX", type.defuz = "COG", type.implication.func = "ZADEH", 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$HYFIS$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$HYFIS$prob
NULL

$HYFIS$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$HYFIS$tags
[1] "Rule-Based Model"

$HYFIS$levels
NULL

$HYFIS$sort
function (x) 
x[order(x$num.labels), ]


$icr
$icr$label
[1] "Independent Component Regression"

$icr$library
[1] "fastICA"

$icr$loop
NULL

$icr$type
[1] "Regression"

$icr$parameters
  parameter   class       label
1    n.comp numeric #Components

$icr$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(n.comp = 1:len)
    }
    else {
        out <- data.frame(n.comp = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$icr$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    icr(x, y, n.comp = param$n.comp, ...)
}

$icr$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$icr$prob
NULL

$icr$tags
[1] "Linear Regression"  "Feature Extraction"

$icr$sort
function (x) 
x[order(x[, 1]), ]


$J48
$J48$label
[1] "C4.5-like Trees"

$J48$library
[1] "RWeka"

$J48$loop
NULL

$J48$type
[1] "Classification"

$J48$parameters
  parameter   class                label
1         C numeric Confidence Threshold

$J48$grid
function (x, y, len = NULL, search = "grid") 
data.frame(C = 0.25)

$J48$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$C <- param$C
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(C = param$C)
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("J48", modelArgs)
    out
}

$J48$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$J48$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "probability")
}

$J48$levels
function (x) 
x$obsLevels

$J48$predictors
function (x, ...) 
predictors(x$terms)

$J48$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"

$J48$sort
function (x) 
x[order(x[, 1]), ]


$JRip
$JRip$label
[1] "Rule-Based Classifier"

$JRip$library
[1] "RWeka"

$JRip$loop
NULL

$JRip$type
[1] "Classification"

$JRip$parameters
  parameter   class           label
1    NumOpt numeric # Optimizations

$JRip$grid
function (x, y, len = NULL, search = "grid") 
data.frame(NumOpt = 1:len)

$JRip$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$N <- param$NumOpt
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(N = param$NumOpt)
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("JRip", modelArgs)
    out
}

$JRip$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$JRip$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "probability")
}

$JRip$levels
function (x) 
x$obsLevels

$JRip$predictors
function (x, ...) 
predictors(x$terms)

$JRip$tags
[1] "Rule-Based Model"           "Implicit Feature Selection"

$JRip$varImp
function (object, ...) 
{
    dat <- caret:::ripperRuleSummary(object)
    out <- dat$varUsage[, "Overall", drop = FALSE]
    rownames(out) <- dat$varUsage$Var
    out
}

$JRip$sort
function (x) 
x[order(x[, 1], decreasing = TRUE), ]


$kernelpls
$kernelpls$label
[1] "Partial Least Squares"

$kernelpls$library
[1] "pls"

$kernelpls$type
[1] "Regression"     "Classification"

$kernelpls$parameters
  parameter   class       label
1     ncomp numeric #Components

$kernelpls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), 
            by = 1))
    }
    else {
        out <- data.frame(ncomp = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$kernelpls$loop
function (grid) 
{
    grid <- grid[order(grid$ncomp, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$kernelpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- if (is.factor(y)) {
        plsda(x, y, method = "oscorespls", ncomp = param$ncomp, 
            ...)
    }
    else {
        dat <- if (is.data.frame(x)) 
            x
        else as.data.frame(x)
        dat$.outcome <- y
        plsr(.outcome ~ ., data = dat, method = "kernelpls", 
            ncomp = param$ncomp, ...)
    }
    out
}

$kernelpls$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- if (modelFit$problemType == "Classification") {
        if (!is.matrix(newdata)) 
            newdata <- as.matrix(newdata)
        out <- predict(modelFit, newdata, type = "class")
    }
    else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels))
        if (modelFit$problemType == "Classification") {
            if (length(submodels$ncomp) > 1) {
                tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
            }
            else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))
        }
        else {
            tmp <- as.list(as.data.frame(apply(predict(modelFit, 
                newdata, ncomp = submodels$ncomp), 3, function(x) list(x))))
        }
        out <- c(list(out), tmp)
    }
    out
}

$kernelpls$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, type = "prob", ncomp = modelFit$tuneValue$ncomp)
    if (length(dim(out)) == 3) {
        if (dim(out)[1] > 1) {
            out <- out[, , 1]
        }
        else {
            out <- as.data.frame(t(out[, , 1]))
        }
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$ncomp)) {
            tmpProb <- predict(modelFit, newdata, type = "prob", 
                ncomp = submodels$ncomp[j])
            if (length(dim(tmpProb)) == 3) {
                if (dim(tmpProb)[1] > 1) {
                  tmpProb <- tmpProb[, , 1]
                }
                else {
                  tmpProb <- as.data.frame(t(tmpProb[, , 1]))
                }
            }
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$kernelpls$varImp
function (object, estimate = NULL, ...) 
{
    modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
    perf <- MSEP(object)$val
    nms <- dimnames(perf)
    if (length(nms$estimate) > 1) {
        pIndex <- if (is.null(estimate)) 
            1
        else which(nms$estimate == estimate)
        perf <- perf[pIndex, , , drop = FALSE]
    }
    numResp <- dim(modelCoef)[2]
    if (numResp <= 2) {
        modelCoef <- modelCoef[, 1, , drop = FALSE]
        perf <- perf[, 1, ]
        delta <- -diff(perf)
        delta <- delta/sum(delta)
        out <- data.frame(Overall = apply(abs(modelCoef), 1, 
            weighted.mean, w = delta))
    }
    else {
        perf <- -t(apply(perf[1, , ], 1, diff))
        perf <- t(apply(perf, 1, function(u) u/sum(u)))
        out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])
        for (i in 1:numResp) {
            tmp <- abs(modelCoef[, i, , drop = FALSE])
            out[, i] <- apply(tmp, 1, weighted.mean, w = perf[i, 
                ])
        }
        colnames(out) <- dimnames(modelCoef)[[2]]
        rownames(out) <- dimnames(modelCoef)[[1]]
    }
    as.data.frame(out)
}

$kernelpls$predictors
function (x, ...) 
rownames(x$projection)

$kernelpls$levels
function (x) 
x$obsLevels

$kernelpls$tags
[1] "Partial Least Squares" "Feature Extraction"    "Kernel Method"        
[4] "Linear Classifier"     "Linear Regression"    

$kernelpls$sort
function (x) 
x[order(x[, 1]), ]


$kknn
$kknn$label
[1] "k-Nearest Neighbors"

$kknn$library
[1] "kknn"

$kknn$loop
NULL

$kknn$type
[1] "Regression"     "Classification"

$kknn$parameters
  parameter     class           label
1      kmax   numeric Max. #Neighbors
2  distance   numeric        Distance
3    kernel character          Kernel

$kknn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(kmax = (5:((2 * len) + 4))[(5:((2 * 
            len) + 4))%%2 > 0], distance = 2, kernel = "optimal")
    }
    else {
        by_val <- if (is.factor(y)) 
            length(levels(y))
        else 1
        kerns <- c("rectangular", "triangular", "epanechnikov", 
            "biweight", "triweight", "cos", "inv", "gaussian")
        out <- data.frame(kmax = sample(seq(1, floor(nrow(x)/3), 
            by = by_val), size = len, replace = TRUE), distance = runif(len, 
            min = 0, max = 3), kernel = sample(kerns, size = len, 
            replace = TRUE))
    }
    out
}

$kknn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    train.kknn(.outcome ~ ., data = dat, kmax = param$kmax, distance = param$distance, 
        kernel = as.character(param$kernel), ...)
}

$kknn$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$kknn$levels
function (x) 
x$obsLevels

$kknn$tags
[1] "Prototype Models"

$kknn$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
}

$kknn$sort
function (x) 
x[order(-x[, 1]), ]


$knn
$knn$label
[1] "k-Nearest Neighbors"

$knn$library
NULL

$knn$loop
NULL

$knn$type
[1] "Classification" "Regression"    

$knn$parameters
  parameter   class      label
1         k numeric #Neighbors

$knn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(k = (5:((2 * len) + 4))[(5:((2 * len) + 
            4))%%2 > 0])
    }
    else {
        by_val <- if (is.factor(y)) 
            length(levels(y))
        else 1
        out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), 
            by = by_val), size = len, replace = TRUE))
    }
    out
}

$knn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        knn3(as.matrix(x), y, k = param$k, ...)
    }
    else {
        knnreg(as.matrix(x), y, k = param$k, ...)
    }
}

$knn$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata)
    }
    out
}

$knn$predictors
function (x, ...) 
colnames(x$learn$X)

$knn$tags
[1] "Prototype Models"

$knn$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$knn$levels
function (x) 
levels(x$learn$y)

$knn$sort
function (x) 
x[order(-x[, 1]), ]


$krlsPoly
$krlsPoly$label
[1] "Polynomial Kernel Regularized Least Squares"

$krlsPoly$library
[1] "KRLS"

$krlsPoly$loop
NULL

$krlsPoly$type
[1] "Regression"

$krlsPoly$parameters
  parameter   class                    label
1    lambda numeric Regularization Parameter
2    degree numeric        Polynomial Degree

$krlsPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = NA, degree = 1:3)
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 0), 
            degree = sample(1:3, size = len, replace = TRUE))
    }
    out
}

$krlsPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!(param$degree %in% 1:4)) 
        stop("Degree should be either 1, 2, 3 or 4")
    krn <- switch(param$degree, `1` = "linear", `2` = "poly2", 
        `3` = "poly3", `4` = "poly4")
    krls(x, y, lambda = if (is.na(param$lambda)) 
        NULL
    else param$lambda, derivative = FALSE, whichkernel = krn, 
        ...)
}

$krlsPoly$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)$fit[, 1]
}

$krlsPoly$tags
[1] "Kernel Method"     "L2 Regularization" "Polynomial Model" 

$krlsPoly$prob
NULL

$krlsPoly$sort
function (x) 
x[order(x$degree, x$lambda), ]


$krlsRadial
$krlsRadial$label
[1] "Radial Basis Function Kernel Regularized Least Squares"

$krlsRadial$library
[1] "KRLS"    "kernlab"

$krlsRadial$loop
NULL

$krlsRadial$type
[1] "Regression"

$krlsRadial$parameters
  parameter   class                    label
1    lambda numeric Regularization Parameter
2     sigma numeric                    Sigma

$krlsRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    sigmaEstimate <- try(sigest(x, na.action = na.omit, scaled = TRUE), 
        silent = TRUE)
    if (!(class(sigmaEstimate) == "try-error")) {
        if (search == "grid") {
            out <- expand.grid(lambda = NA, sigma = 1/seq(sigmaEstimate[1], 
                sigmaEstimate[3], length = len))
        }
        else {
            rng <- extendrange(log(sigmaEstimate), f = 0.75)
            out <- data.frame(lambda = 10^runif(len, min = -5, 
                0), sigma = 1/exp(runif(len, min = rng[1], max = rng[2])))
        }
    }
    else {
        if (search == "grid") {
            out <- expand.grid(lambda = NA, sigma = 1/(10^((1:len) - 
                3)))
        }
        else {
            out <- data.frame(lambda = 10^runif(len, min = -5, 
                0), sigma = 1/(10^runif(len, min = -4, max = 0)))
        }
    }
    out
}

$krlsRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    krls(x, y, lambda = if (is.na(param$lambda)) 
        NULL
    else param$lambda, sigma = param$sigma, ...)
}

$krlsRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)$fit[, 1]
}

$krlsRadial$tags
[1] "Kernel Method"         "L2 Regularization"     "Radial Basis Function"

$krlsRadial$prob
NULL

$krlsRadial$sort
function (x) 
x[order(x$lambda), ]


$lars
$lars$label
[1] "Least Angle Regression"

$lars$library
[1] "lars"

$lars$type
[1] "Regression"

$lars$parameters
  parameter   class    label
1  fraction numeric Fraction

$lars$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(fraction = seq(0.05, 1, length = len))
    }
    else {
        out <- data.frame(fraction = runif(len, min = 0, max = 1))
    }
    out
}

$lars$loop
function (grid) 
{
    grid <- grid[order(grid$fraction, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$lars$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
lars(as.matrix(x), y, ...)

$lars$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata), type = "fit", 
        mode = "fraction", s = modelFit$tuneValue$fraction)$fit
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$fraction)) {
            tmp[[j + 1]] <- predict(modelFit, as.matrix(newdata), 
                type = "fit", mode = "fraction", s = submodels$fraction[j])$fit
        }
        out <- tmp
    }
    out
}

$lars$predictors
function (x, s = NULL, ...) 
{
    if (is.null(s)) {
        if (!is.null(x$tuneValue)) {
            s <- x$tuneValue$fraction
        }
        else stop("must supply a vaue of s")
        out <- predict(x, s = s, type = "coefficients", mode = "fraction")$coefficients
    }
    else {
        out <- predict(x, s = s, ...)$coefficients
    }
    names(out)[out != 0]
}

$lars$tags
[1] "Linear Regression"          "Implicit Feature Selection"
[3] "L1 Regularization"         

$lars$prob
NULL

$lars$sort
function (x) 
x[order(x[, 1]), ]


$lars2
$lars2$label
[1] "Least Angle Regression"

$lars2$library
[1] "lars"

$lars2$type
[1] "Regression"

$lars2$parameters
  parameter   class  label
1      step numeric #Steps

$lars2$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(step = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(step = sample(1:ncol(x), size = len, 
            replace = TRUE))
    }
    out
}

$lars2$loop
function (grid) 
{
    grid <- grid[order(grid$step, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$lars2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
lars(as.matrix(x), y, ...)

$lars2$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata), type = "fit", 
        mode = "step", s = modelFit$tuneValue$step)$fit
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$step)) {
            tmp[[j + 1]] <- predict(modelFit, as.matrix(newdata), 
                type = "fit", mode = "step", s = submodels$step[j])$fit
        }
        out <- tmp
    }
    out
}

$lars2$predictors
function (x, s = NULL, ...) 
{
    if (is.null(s)) {
        if (!is.null(x$tuneValue)) {
            s <- x$tuneValue$.fraction
        }
        else stop("must supply a vaue of s")
        out <- predict(x, s = s, type = "coefficients", mode = "fraction")$coefficients
    }
    else {
        out <- predict(x, s = s, ...)$coefficients
    }
    names(out)[out != 0]
}

$lars2$tags
[1] "Linear Regression"          "Implicit Feature Selection"
[3] "L1 Regularization"         

$lars2$prob
NULL

$lars2$sort
function (x) 
x[order(x[, 1]), ]


$lasso
$lasso$label
[1] "The lasso"

$lasso$library
[1] "elasticnet"

$lasso$type
[1] "Regression"

$lasso$parameters
  parameter   class                     label
1  fraction numeric Fraction of Full Solution

$lasso$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(fraction = seq(0.1, 0.9, length = len))
    }
    else {
        out <- data.frame(fraction = runif(len, min = 0, max = 1))
    }
    out
}

$lasso$loop
function (grid) 
{
    grid <- grid[order(grid$fraction, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$lasso$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    enet(as.matrix(x), y, lambda = 0, ...)
}

$lasso$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, s = modelFit$tuneValue$fraction, 
        mode = "fraction")$fit
    if (!is.null(submodels)) {
        if (nrow(submodels) > 1) {
            out <- c(list(if (is.matrix(out)) out[, 1] else out), 
                as.list(as.data.frame(predict(modelFit, newx = newdata, 
                  s = submodels$fraction, mode = "fraction")$fit)))
        }
        else {
            tmp <- predict(modelFit, newx = newdata, s = submodels$fraction, 
                mode = "fraction")$fit
            out <- c(list(if (is.matrix(out)) out[, 1] else out), 
                list(tmp))
        }
    }
    out
}

$lasso$predictors
function (x, s = NULL, ...) 
{
    if (is.null(s)) {
        if (!is.null(x$tuneValue)) {
            s <- x$tuneValue$fraction
        }
        else stop("must supply a vaue of s")
        out <- predict(x, s = s, type = "coefficients", mode = "fraction")$coefficients
    }
    else {
        out <- predict(x, s = s)$coefficients
    }
    names(out)[out != 0]
}

$lasso$tags
[1] "Linear Regression"          "Implicit Feature Selection"
[3] "L1 Regularization"         

$lasso$prob
NULL

$lasso$sort
function (x) 
x[order(x$fraction), ]


$lda
$lda$label
[1] "Linear Discriminant Analysis"

$lda$library
[1] "MASS"

$lda$loop
NULL

$lda$type
[1] "Classification"

$lda$parameters
  parameter     class     label
1 parameter character parameter

$lda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$lda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
lda(x, y, ...)

$lda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$lda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$posterior

$lda$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else colnames(x$means)

$lda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$lda$levels
function (x) 
names(x$prior)

$lda$sort
function (x) 
x


$lda2
$lda2$label
[1] "Linear Discriminant Analysis"

$lda2$library
[1] "MASS"

$lda2$loop
function (grid) 
{
    grid <- grid[order(grid$dimen, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$lda2$type
[1] "Classification"

$lda2$parameters
  parameter   class                   label
1     dimen numeric #Discriminant Functions

$lda2$grid
function (x, y, len = NULL, search = "grid") 
data.frame(dimen = 1:min(ncol(x), length(levels(y)) - 1))

$lda2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
lda(x, y, ...)

$lda2$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- as.character(predict(modelFit, newdata, dimen = modelFit$tuneValue$dimen)$class)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$dimen)) {
            tmp[[j + 1]] <- as.character(predict(modelFit, newdata, 
                dimen = submodels$dimen[j])$class)
        }
        out <- tmp
    }
    out
}

$lda2$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, dimen = modelFit$tuneValue$dimen)$posterior
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$dimen)) {
            tmpProb <- predict(modelFit, newdata, dimen = submodels$dimen[j])$posterior
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$lda2$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else colnames(x$means)

$lda2$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$lda2$levels
function (x) 
names(x$prior)

$lda2$sort
function (x) 
x[order(x[, 1]), ]


$leapBackward
$leapBackward$label
[1] "Linear Regression with Backwards Selection"

$leapBackward$library
[1] "leaps"

$leapBackward$type
[1] "Regression"

$leapBackward$parameters
  parameter   class                        label
1     nvmax numeric Maximum Number of Predictors

$leapBackward$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(nvmax = 2:(len + 1))
    }
    else {
        out <- data.frame(nvmax = sort(unique(sample(2:(ncol(x) - 
            1), size = len, replace = TRUE))))
    }
    out
}

$leapBackward$loop
function (grid) 
{
    grid <- grid[order(grid$nvmax, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$leapBackward$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "nbest")) 
        stop("'nbest' should not be specified")
    if (any(names(theDots) == "method")) 
        stop("'method' should not be specified")
    if (any(names(theDots) == "nvmax")) 
        stop("'nvmax' should not be specified")
    regsubsets(x, y, weights = if (!is.null(wts)) 
        wts
    else rep(1, length(y)), nbest = 1, nvmax = param$nvmax, method = "backward", 
        ...)
}

$leapBackward$predict
function (modelFit, newdata, submodels = NULL) 
{
    foo <- function(b, x) x[, names(b), drop = FALSE] %*% b
    path <- 1:(modelFit$nvmax - 1)
    betas <- coef(modelFit, id = 1:(modelFit$nvmax - 1))
    newdata <- cbind(rep(1, nrow(newdata)), as.matrix(newdata))
    colnames(newdata)[1] <- "(Intercept)"
    out <- foo(betas[[length(betas)]], newdata)[, 1]
    if (!is.null(submodels)) {
        numTerms <- unlist(lapply(betas, length))
        if (any(names(betas[[length(betas)]]) == "(Intercept)")) 
            numTerms <- numTerms - 1
        keepers <- which(numTerms %in% submodels$nvmax)
        if (length(keepers) != length(submodels$nvmax)) 
            stop("Some values of 'nvmax' are not in the model sequence.")
        keepers <- rev(keepers)
        preds <- lapply(betas[keepers], foo, x = newdata)
        preds <- do.call("cbind", preds)
        out <- as.data.frame(cbind(out, preds))
    }
    out
}

$leapBackward$tags
[1] "Linear Regression"         "Feature Selection Wrapper"

$leapBackward$prob
NULL

$leapBackward$sort
function (x) 
x[order(x[, 1]), ]


$leapForward
$leapForward$label
[1] "Linear Regression with Forward Selection"

$leapForward$library
[1] "leaps"

$leapForward$type
[1] "Regression"

$leapForward$parameters
  parameter   class                        label
1     nvmax numeric Maximum Number of Predictors

$leapForward$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(nvmax = 2:(len + 1))
    }
    else {
        out <- data.frame(nvmax = sort(unique(sample(2:(ncol(x) - 
            1), size = len, replace = TRUE))))
    }
    out
}

$leapForward$loop
function (grid) 
{
    grid <- grid[order(grid$nvmax, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$leapForward$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "nbest")) 
        stop("'nbest' should not be specified")
    if (any(names(theDots) == "method")) 
        stop("'method' should not be specified")
    if (any(names(theDots) == "nvmax")) 
        stop("'nvmax' should not be specified")
    regsubsets(x, y, weights = if (!is.null(wts)) 
        wts
    else rep(1, length(y)), nbest = 1, nvmax = param$nvmax, method = "forward", 
        ...)
}

$leapForward$predict
function (modelFit, newdata, submodels = NULL) 
{
    foo <- function(b, x) x[, names(b), drop = FALSE] %*% b
    path <- 1:(modelFit$nvmax - 1)
    betas <- coef(modelFit, id = 1:(modelFit$nvmax - 1))
    newdata <- cbind(rep(1, nrow(newdata)), as.matrix(newdata))
    colnames(newdata)[1] <- "(Intercept)"
    out <- foo(betas[[length(betas)]], newdata)[, 1]
    if (!is.null(submodels)) {
        numTerms <- unlist(lapply(betas, length))
        if (any(names(betas[[length(betas)]]) == "(Intercept)")) 
            numTerms <- numTerms - 1
        keepers <- which(numTerms %in% submodels$nvmax)
        if (length(keepers) != length(submodels$nvmax)) 
            stop("Some values of 'nvmax' are not in the model sequence.")
        keepers <- rev(keepers)
        preds <- lapply(betas[keepers], foo, x = newdata)
        preds <- do.call("cbind", preds)
        out <- as.data.frame(cbind(out, preds))
    }
    out
}

$leapForward$tags
[1] "Linear Regression"         "Feature Selection Wrapper"

$leapForward$prob
NULL

$leapForward$sort
function (x) 
x[order(x[, 1]), ]


$leapSeq
$leapSeq$label
[1] "Linear Regression with Stepwise Selection"

$leapSeq$library
[1] "leaps"

$leapSeq$type
[1] "Regression"

$leapSeq$parameters
  parameter   class                        label
1     nvmax numeric Maximum Number of Predictors

$leapSeq$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(nvmax = 2:(len + 1))
    }
    else {
        out <- data.frame(nvmax = sort(unique(sample(2:(ncol(x) - 
            1), size = len, replace = TRUE))))
    }
    out
}

$leapSeq$loop
function (grid) 
{
    grid <- grid[order(grid$nvmax, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$leapSeq$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "nbest")) 
        stop("'nbest' should not be specified")
    if (any(names(theDots) == "method")) 
        stop("'method' should not be specified")
    if (any(names(theDots) == "nvmax")) 
        stop("'nvmax' should not be specified")
    regsubsets(x, y, weights = if (!is.null(wts)) 
        wts
    else rep(1, length(y)), nbest = 1, nvmax = param$nvmax, method = "seqrep", 
        ...)
}

$leapSeq$predict
function (modelFit, newdata, submodels = NULL) 
{
    foo <- function(b, x) x[, names(b), drop = FALSE] %*% b
    path <- 1:(modelFit$nvmax - 1)
    betas <- coef(modelFit, id = 1:(modelFit$nvmax - 1))
    newdata <- cbind(rep(1, nrow(newdata)), as.matrix(newdata))
    colnames(newdata)[1] <- "(Intercept)"
    out <- foo(betas[[length(betas)]], newdata)[, 1]
    if (!is.null(submodels)) {
        numTerms <- unlist(lapply(betas, length))
        if (any(names(betas[[length(betas)]]) == "(Intercept)")) 
            numTerms <- numTerms - 1
        keepers <- which(numTerms %in% submodels$nvmax)
        if (length(keepers) != length(submodels$nvmax)) 
            stop("Some values of 'nvmax' are not in the model sequence.")
        keepers <- rev(keepers)
        preds <- lapply(betas[keepers], foo, x = newdata)
        preds <- do.call("cbind", preds)
        out <- as.data.frame(cbind(out, preds))
    }
    out
}

$leapSeq$tags
[1] "Linear Regression"         "Feature Selection Wrapper"

$leapSeq$prob
NULL

$leapSeq$sort
function (x) 
x[order(x[, 1]), ]


$Linda
$Linda$label
[1] "Robust Linear Discriminant Analysis"

$Linda$library
[1] "rrcov"

$Linda$loop
NULL

$Linda$type
[1] "Classification"

$Linda$parameters
  parameter     class label
1 parameter character  none

$Linda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$Linda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
rrcov:::Linda(x, y, ...)

$Linda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)@classification

$Linda$prob
function (modelFit, newdata, submodels = NULL) 
{
    probs <- predict(modelFit, newdata)@posterior
    colnames(probs) <- names(modelFit@prior)
    probs
}

$Linda$tags
[1] "Discriminant Analysis" "Linear Classifier"     "Robust Model"         

$Linda$levels
function (x) 
names(x@prior)

$Linda$sort
function (x) 
x


$lm
$lm$label
[1] "Linear Regression"

$lm$library
NULL

$lm$loop
NULL

$lm$type
[1] "Regression"

$lm$parameters
  parameter   class     label
1 intercept logical intercept

$lm$grid
function (x, y, len = NULL, search = "grid") 
data.frame(intercept = TRUE)

$lm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        if (param$intercept) 
            out <- lm(.outcome ~ ., data = dat, weights = wts, 
                ...)
        else out <- lm(.outcome ~ 0 + ., data = dat, weights = wts, 
            ...)
    }
    else {
        if (param$intercept) 
            out <- lm(.outcome ~ ., data = dat, ...)
        else out <- lm(.outcome ~ 0 + ., data = dat, ...)
    }
    out
}

$lm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$lm$prob
NULL

$lm$predictors
function (x, ...) 
predictors(x$terms)

$lm$tags
[1] "Linear Regression"    "Accepts Case Weights"

$lm$varImp
function (object, ...) 
{
    values <- summary(object)$coef
    varImps <- abs(values[-1, grep("value$", colnames(values))])
    out <- data.frame(varImps)
    colnames(out) <- "Overall"
    if (!is.null(names(varImps))) 
        rownames(out) <- names(varImps)
    out
}

$lm$sort
function (x) 
x


$lmStepAIC
$lmStepAIC$label
[1] "Linear Regression with Stepwise Selection"

$lmStepAIC$library
[1] "MASS"

$lmStepAIC$loop
NULL

$lmStepAIC$type
[1] "Regression"

$lmStepAIC$parameters
  parameter     class     label
1 parameter character parameter

$lmStepAIC$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$lmStepAIC$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- stepAIC(lm(.outcome ~ ., data = dat, weights = wts), 
            ...)
    }
    else out <- stepAIC(lm(.outcome ~ ., data = dat), ...)
    out
}

$lmStepAIC$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$lmStepAIC$prob
NULL

$lmStepAIC$tags
[1] "Linear Regression"         "Feature Selection Wrapper"
[3] "Accepts Case Weights"     

$lmStepAIC$sort
function (x) 
x


$LMT
$LMT$label
[1] "Logistic Model Trees"

$LMT$library
[1] "RWeka"

$LMT$loop
NULL

$LMT$type
[1] "Classification"

$LMT$parameters
  parameter   class       label
1      iter numeric # Iteratons

$LMT$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(iter = 1 + (0:(len - 1)) * 20)
    }
    else {
        out <- data.frame(iter = unique(sample(1:100, size = len, 
            replace = TRUE)))
    }
    out
}

$LMT$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$I <- param$iter
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(I = param$iter)
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("LMT", modelArgs)
    out
}

$LMT$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$LMT$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "probability")
}

$LMT$levels
function (x) 
x$obsLevels

$LMT$predictors
function (x, ...) 
predictors(x$terms)

$LMT$tags
[1] "Model Tree"                 "Implicit Feature Selection"
[3] "Logistic Regression"        "Linear Classifier"         

$LMT$sort
function (x) 
x[order(x[, 1]), ]


$loclda
$loclda$label
[1] "Localized Linear Discriminant Analysis"

$loclda$library
[1] "klaR"

$loclda$loop
NULL

$loclda$type
[1] "Classification"

$loclda$parameters
  parameter   class              label
1         k numeric #Nearest Neighbors

$loclda$grid
function (x, y, len = NULL, search = "grid") 
{
    min_p <- ncol(x)/nrow(x) + 0.05
    p_seq <- seq(min_p, min(0.9, min_p + 1/3), length = len)
    if (search == "grid") {
        out <- data.frame(k = floor(p_seq * nrow(x)))
    }
    else {
        by_val <- if (is.factor(y)) 
            length(levels(y))
        else 1
        out <- data.frame(k = floor(runif(len, min = nrow(x) * 
            min_p, max = nrow(x) * min(0.9, min_p + 1/3))))
    }
    out
}

$loclda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
loclda(x, y, k = floor(param$k), ...)

$loclda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$loclda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$posterior

$loclda$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else colnames(x$means)

$loclda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$loclda$levels
function (x) 
names(x$prior)

$loclda$sort
function (x) 
x


$logicBag
$logicBag$label
[1] "Bagged Logic Regression"

$logicBag$library
[1] "logicFS"

$logicBag$loop
NULL

$logicBag$type
[1] "Regression"     "Classification"

$logicBag$parameters
  parameter   class                    label
1   nleaves numeric Maximum Number of Leaves
2    ntrees numeric          Number of Trees

$logicBag$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(ntrees = (1:len) + 1, nleaves = 2^((1:len) + 
            6))
    }
    else {
        out <- data.frame(ntrees = sample(1:10, size = len, replace = TRUE), 
            nleaves = sample(1:10, size = len, replace = TRUE))
    }
    out
}

$logicBag$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    logic.bagging(as.matrix(x), y, ntrees = param$ntrees, nleaves = param$nleaves, 
        ...)
}

$logicBag$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        if (length(modelFit$obsLevels) == 2) {
            as.character(modelFit$obsLevels[predict(modelFit, 
                newData = newdata) + 1])
        }
        else {
            as.character(predict(modelFit, newData = newdata))
        }
    }
    else predict(modelFit, newData = newdata)
}

$logicBag$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (length(modelFit$obsLevels) == 2) {
        out <- predict(modelFit, newData = newdata, type = "prob")
        out <- as.data.frame(cbind(out, 1 - out))
        colnames(out) <- modelFit$obsLevels
    }
    else {
        out <- predict(modelFit, newData = newdata, type = "prob")
    }
    out
}

$logicBag$predictors
function (x, ...) 
{
    varNums <- lapply(x$logreg.model, function(y) lapply(y$trees, 
        function(z) z$trees$knot))
    varNums <- sort(unique(unlist(varNums)))
    varNums <- varNums[varNums > 0]
    if (length(varNums) > 0) 
        colnames(x$data)[varNums]
    else NA
}

$logicBag$levels
function (x) 
x$obsLevels

$logicBag$tags
[1] "Logic Regression"       "Linear Classifier"      "Linear Regression"     
[4] "Logistic Regression"    "Bagging"                "Ensemble Model"        
[7] "Two Class Only"         "Binary Predictors Only"

$logicBag$sort
function (x) 
x[order(x$ntrees, x$nleaves), ]


$LogitBoost
$LogitBoost$label
[1] "Boosted Logistic Regression"

$LogitBoost$library
[1] "caTools"

$LogitBoost$loop
function (grid) 
{
    loop <- grid[which.max(grid$nIter), , drop = FALSE]
    submodels <- grid[-which.max(grid$nIter), , drop = FALSE]
    submodels <- list(submodels)
    list(loop = loop, submodels = submodels)
}

$LogitBoost$type
[1] "Classification"

$LogitBoost$parameters
  parameter   class                 label
1     nIter numeric # Boosting Iterations

$LogitBoost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(nIter = 1 + ((1:len) * 10))
    }
    else {
        out <- data.frame(nIter = unique(sample(1:100, size = len, 
            replace = TRUE)))
    }
    out
}

$LogitBoost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    caTools::LogitBoost(as.matrix(x), y, nIter = param$nIter)
}

$LogitBoost$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- caTools::predict.LogitBoost(modelFit, newdata, type = "class")
    if (!is.null(submodels)) {
        tmp <- out
        out <- vector(mode = "list", length = nrow(submodels) + 
            1)
        out[[1]] <- tmp
        for (j in seq(along = submodels$nIter)) {
            out[[j + 1]] <- caTools::predict.LogitBoost(modelFit, 
                newdata, nIter = submodels$nIter[j])
        }
    }
    out
}

$LogitBoost$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- caTools::predict.LogitBoost(modelFit, newdata, type = "raw")
    out <- t(apply(out, 1, function(x) x/sum(x)))
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nIter)) {
            tmpProb <- caTools::predict.LogitBoost(modelFit, 
                newdata, type = "raw", nIter = submodels$nIter[j])
            tmpProb <- out <- t(apply(tmpProb, 1, function(x) x/sum(x)))
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$LogitBoost$predictors
function (x, ...) 
{
    if (!is.null(x$xNames)) {
        out <- unique(x$xNames[x$Stump[, "feature"]])
    }
    else out <- NA
    out
}

$LogitBoost$levels
function (x) 
x$obsLevels

$LogitBoost$tags
[1] "Ensemble Model"             "Boosting"                  
[3] "Implicit Feature Selection" "Tree-Based Model"          
[5] "Logistic Regression"       

$LogitBoost$sort
function (x) 
x[order(x[, 1]), ]


$logreg
$logreg$label
[1] "Logic Regression"

$logreg$library
[1] "LogicReg"

$logreg$loop
NULL

$logreg$type
[1] "Regression"     "Classification"

$logreg$parameters
  parameter   class                    label
1  treesize numeric Maximum Number of Leaves
2    ntrees numeric          Number of Trees

$logreg$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(ntrees = (1:3) + 1, treesize = 2^(1 + (1:len)))

$logreg$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    isReg <- is.numeric(y)
    if (is.factor(y)) 
        y <- ifelse(y == levels(y)[1], 1, 0)
    logreg(resp = y, bin = x, ntrees = param$ntrees, tree.control = logreg.tree.control(treesize = param$treesize), 
        select = 1, type = ifelse(isReg, 2, 3), ...)
}

$logreg$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$type == "logistic") {
        out <- ifelse(predict(modelFit, newbin = newdata) >= 
            0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }
    else out <- predict(modelFit, newbin = newdata)
    out
}

$logreg$prob
function (modelFit, newdata, submodels = NULL) 
{
    tmp <- predict(modelFit, newbin = newdata)
    out <- cbind(tmp, 1 - tmp)
    colnames(out) <- modelFit$obsLevels
    out
}

$logreg$predictors
function (x, ...) 
{
    getVarIndex <- function(y) unique(y$trees$knot)
    varNums <- unique(unlist(lapply(x$model$trees, getVarIndex)))
    varNums <- varNums[varNums > 0]
    if (length(varNums) > 0) 
        colnames(x$binary)[varNums]
    else NA
}

$logreg$levels
function (x) 
x$obsLevels

$logreg$tags
[1] "Logic Regression"       "Linear Classifier"      "Linear Regression"     
[4] "Logistic Regression"    "Two Class Only"         "Binary Predictors Only"

$logreg$sort
function (x) 
x[order(x$ntrees, x$treesize), ]


$lssvmLinear
$lssvmLinear$label
[1] "Least Squares Support Vector Machine"

$lssvmLinear$library
[1] "kernlab"

$lssvmLinear$type
[1] "Classification"

$lssvmLinear$parameters
  parameter   class                    label
1       tau numeric Regularization Parameter

$lssvmLinear$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(tau = 2^((1:len) - 5))
    }
    else {
        out <- data.frame(tau = 2^runif(len, min = -5, max = 10))
    }
    out
}

$lssvmLinear$loop
NULL

$lssvmLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    lssvm(x = as.matrix(x), y = y, tau = param$tau, kernel = polydot(degree = 1, 
        scale = 1, offset = 1), ...)
}

$lssvmLinear$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$lssvmLinear$prob
NULL

$lssvmLinear$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$lssvmLinear$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Classifier"      

$lssvmLinear$levels
function (x) 
lev(x)

$lssvmLinear$sort
function (x) 
x


$lssvmPoly
$lssvmPoly$label
[1] "Least Squares Support Vector Machine with Polynomial Kernel"

$lssvmPoly$library
[1] "kernlab"

$lssvmPoly$type
[1] "Classification"

$lssvmPoly$parameters
  parameter   class                    label
1    degree numeric        Polynomial Degree
2     scale numeric                    Scale
3       tau numeric Regularization Parameter

$lssvmPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(degree = seq(1, min(len, 3)), scale = 10^((1:len) - 
            4), tau = 2^((1:len) - 5))
    }
    else {
        out <- data.frame(degree = sample(1:3, size = len, replace = TRUE), 
            scale = 10^runif(len, min = -5, log10(2)), tau = 2^runif(len, 
                min = -5, max = 10))
    }
    out
}

$lssvmPoly$loop
NULL

$lssvmPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    lssvm(x = as.matrix(x), y = y, tau = param$tau, kernel = polydot(degree = param$degree, 
        scale = param$scale, offset = 1), ...)
}

$lssvmPoly$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$lssvmPoly$prob
NULL

$lssvmPoly$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$xscale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$lssvmPoly$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Polynomial Model"       

$lssvmPoly$levels
function (x) 
lev(x)

$lssvmPoly$sort
function (x) 
x


$lssvmRadial
$lssvmRadial$label
[1] "Least Squares Support Vector Machine with Radial Basis Function Kernel"

$lssvmRadial$library
[1] "kernlab"

$lssvmRadial$type
[1] "Classification"

$lssvmRadial$parameters
  parameter   class                    label
1     sigma numeric                    Sigma
2       tau numeric Regularization Parameter

$lssvmRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = seq(min(sigmas), max(sigmas), 
            length = min(6, len)), tau = 2^((1:len) - 5))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])), tau = 2^runif(len, min = -5, max = 10))
    }
    out
}

$lssvmRadial$loop
NULL

$lssvmRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    lssvm(x = as.matrix(x), y = y, tau = param$tau, kernel = rbfdot, 
        kpar = list(sigma = param$sigma), ...)
}

$lssvmRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata))
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$lssvmRadial$prob
NULL

$lssvmRadial$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$lssvmRadial$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Radial Basis Function"  

$lssvmRadial$levels
function (x) 
lev(x)

$lssvmRadial$sort
function (x) 
x


$lvq
$lvq$label
[1] "Learning Vector Quantization"

$lvq$library
[1] "class"

$lvq$loop
NULL

$lvq$type
[1] "Classification"

$lvq$parameters
  parameter   class         label
1      size numeric Codebook Size
2         k numeric   #Prototypes

$lvq$grid
function (x, y, len = NULL, search = "grid") 
{
    p <- ncol(x)
    ng <- length(levels(y))
    n <- nrow(x)
    tmp <- min(round(0.4 * ng * (ng - 1 + p/2), 0), n)
    if (search == "grid") {
        out <- expand.grid(size = floor(seq(tmp, 2 * tmp, length = len)), 
            k = -4 + (1:len) * 5)
        out$size <- floor(out$size)
        out <- out[!duplicated(out), ]
    }
    else {
        out <- data.frame(size = sample(tmp:(2 * tmp), size = len, 
            replace = TRUE), k = sample(1:(nrow(x) - 2), size = len, 
            replace = TRUE))
    }
    out <- subset(out, size < n & k < n)
    out
}

$lvq$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    lvq3(x, y, lvqinit(x, y, size = param$size, k = min(param$k, 
        nrow(x) - length(levels(y)))), ...)
}

$lvq$predict
function (modelFit, newdata, submodels = NULL) 
lvqtest(modelFit, newdata)

$lvq$levels
function (x) 
x$obsLevels

$lvq$prob
NULL

$lvq$tags
[1] "Prototype Models"

$lvq$sort
function (x) 
x[order(-x$k, -x$size), ]


$M5
$M5$label
[1] "Model Tree"

$M5$library
[1] "RWeka"

$M5$loop
NULL

$M5$type
[1] "Regression"

$M5$parameters
  parameter     class    label
1    pruned character   Pruned
2  smoothed character Smoothed
3     rules character    Rules

$M5$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(pruned = c("Yes", "No"), smoothed = c("Yes", "No"), 
    rules = c("Yes", "No"))

$M5$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$N <- ifelse(param$pruned == "No", TRUE, 
            FALSE)
        theDots$control$U <- ifelse(param$smoothed == "No", TRUE, 
            FALSE)
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(N = ifelse(param$pruned == "No", 
        TRUE, FALSE), U = ifelse(param$smoothed == "No", TRUE, 
        FALSE))
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        control = ctl), theDots)
    modelArgs$data <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    modelArgs$data$.outcome <- y
    out <- do.call(if (param$rules == "Yes") 
        "M5Rules"
    else "M5P", modelArgs)
    out
}

$M5$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$M5$prob
NULL

$M5$predictors
function (x, ...) 
predictors(x$terms)

$M5$tags
[1] "Rule-Based Model"           "Tree-Based Model"          
[3] "Linear Regression"          "Implicit Feature Selection"
[5] "Model Tree"                

$M5$sort
function (x) 
{
    x$pruned <- factor(as.character(x$pruned), levels = c("Yes", 
        "No"))
    x$smoothed <- factor(as.character(x$smoothed), levels = c("Yes", 
        "No"))
    x$rules <- factor(as.character(x$rules), levels = c("Yes", 
        "No"))
    x[order(x$pruned, x$smoothed, x$rules), ]
}


$M5Rules
$M5Rules$label
[1] "Model Rules"

$M5Rules$library
[1] "RWeka"

$M5Rules$loop
NULL

$M5Rules$type
[1] "Regression"

$M5Rules$parameters
  parameter     class    label
1    pruned character   Pruned
2  smoothed character Smoothed

$M5Rules$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(pruned = c("Yes", "No"), smoothed = c("Yes", "No"))

$M5Rules$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$N <- ifelse(param$pruned == "No", TRUE, 
            FALSE)
        theDots$control$U <- ifelse(param$smoothed == "No", TRUE, 
            FALSE)
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(N = ifelse(param$pruned == "No", 
        TRUE, FALSE), U = ifelse(param$smoothed == "No", TRUE, 
        FALSE))
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        control = ctl), theDots)
    modelArgs$data <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    modelArgs$data$.outcome <- y
    out <- do.call("M5Rules", modelArgs)
    out
}

$M5Rules$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$M5Rules$predictors
function (x, ...) 
predictors(x$terms)

$M5Rules$prob
NULL

$M5Rules$tags
[1] "Rule-Based Model"           "Linear Regression"         
[3] "Implicit Feature Selection" "Model Tree"                

$M5Rules$sort
function (x) 
{
    x$pruned <- factor(as.character(x$pruned), levels = c("Yes", 
        "No"))
    x$smoothed <- factor(as.character(x$smoothed), levels = c("Yes", 
        "No"))
    x[order(x$pruned, x$smoothed), ]
}


$manb
$manb$label
[1] "Model Averaged Naive Bayes Classifier"

$manb$library
[1] "bnclassify"

$manb$type
[1] "Classification"

$manb$parameters
  parameter   class               label
1    smooth numeric Smoothing Parameter
2     prior numeric   Prior Probability

$manb$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(smooth = 0:(len - 1), prior = seq(0.1, 
            0.9, length = len))
    }
    else {
        out <- data.frame(smooth = runif(len, min = 0, max = 10), 
            prior = runif(len))
    }
    out$smooth[out$smooth <= 0] <- 0.05
    out
}

$manb$loop
NULL

$manb$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    struct <- nb(class = ".outcome", dataset = dat)
    lp(struct, dat, smooth = param$smooth, manb_prior = param$prior, 
        ...)
}

$manb$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$manb$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$manb$levels
function (x) 
x$obsLevels

$manb$predictors
function (x, s = NULL, ...) 
x$xNames

$manb$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$manb$sort
function (x) 
x[order(x[, 1]), ]


$mda
$mda$label
[1] "Mixture Discriminant Analysis"

$mda$library
[1] "mda"

$mda$loop
NULL

$mda$type
[1] "Classification"

$mda$parameters
   parameter   class                 label
1 subclasses numeric #Subclasses Per Class

$mda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(subclasses = (1:len) + 1)

$mda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    mda(as.formula(".outcome ~ ."), data = dat, subclasses = param$subclasses, 
        ...)
}

$mda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$mda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "posterior")

$mda$predictors
function (x, ...) 
predictors(x$terms)

$mda$levels
function (x) 
x$obsLevels

$mda$tags
[1] "Discriminant Analysis" "Mixture Model"        

$mda$sort
function (x) 
x[order(x[, 1]), ]


$Mlda
$Mlda$label
[1] "Maximum Uncertainty Linear Discriminant Analysis"

$Mlda$library
[1] "HiDimDA"

$Mlda$loop
NULL

$Mlda$type
[1] "Classification"

$Mlda$parameters
  parameter     class     label
1 parameter character parameter

$Mlda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$Mlda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
Mlda(x, y, q = param$.q, maxq = param$.q, ...)

$Mlda$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$class
    out <- modelFit$obsLevels[as.numeric(out)]
    out
}

$Mlda$levels
function (x) 
x$obsLevels

$Mlda$prob
NULL

$Mlda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$Mlda$sort
function (x) 
x[order(x[, 1]), ]


$mlp
$mlp$label
[1] "Multi-Layer Perceptron"

$mlp$library
[1] "RSNNS"

$mlp$loop
NULL

$mlp$type
[1] "Regression"     "Classification"

$mlp$parameters
  parameter   class         label
1      size numeric #Hidden Units

$mlp$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(size = ((1:len) * 2) - 1)
    }
    else {
        out <- data.frame(size = unique(sample(1:20, size = len, 
            replace = TRUE)))
    }
    out
}

$mlp$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
    if (is.factor(y)) {
        y <- RSNNS:::decodeClassLabels(y)
        lin <- FALSE
    }
    else lin <- TRUE
    args <- list(x = x, y = y, size = param$size, linOut = lin)
    args <- c(args, theDots)
    do.call("mlp", args)
}

$mlp$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$mlp$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$mlp$levels
function (x) 
x$obsLevels

$mlp$tags
[1] "Neural Network"

$mlp$sort
function (x) 
x[order(x$size), ]


$mlpML
$mlpML$label
[1] "Multi-Layer Perceptron, with multiple layers"

$mlpML$library
[1] "RSNNS"

$mlpML$loop
NULL

$mlpML$type
[1] "Regression"     "Classification"

$mlpML$parameters
  parameter   class                label
1    layer1 numeric #Hidden Units layer1
2    layer2 numeric #Hidden Units layer2
3    layer3 numeric #Hidden Units layer3

$mlpML$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(layer1 = ((1:len) * 2) - 1, layer2 = 0, 
            layer3 = 0)
    }
    else {
        out <- data.frame(layer1 = sample(2:20, replace = TRUE, 
            size = len), layer2 = sample(c(0, 2:20), replace = TRUE, 
            size = len), layer3 = sample(c(0, 2:20), replace = TRUE, 
            size = len))
    }
    out
}

$mlpML$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
    if (is.factor(y)) {
        y <- RSNNS:::decodeClassLabels(y)
        lin <- FALSE
    }
    else lin <- TRUE
    if (param$layer1 == 0) 
        stop("the first layer must have at least one hidden unit")
    if (param$layer2 == 0 & param$layer2 > 0) 
        stop("the second layer must have at least one hidden unit if a third layer is specified")
    nodes <- c(param$layer1)
    if (param$layer2 > 0) {
        nodes <- c(nodes, param$layer2)
        if (param$layer3 > 0) 
            nodes <- c(nodes, param$layer3)
    }
    args <- list(x = x, y = y, size = nodes, linOut = lin)
    args <- c(args, theDots)
    do.call("mlp", args)
}

$mlpML$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$mlpML$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$mlpML$levels
function (x) 
x$obsLevels

$mlpML$tags
[1] "Neural Network"

$mlpML$sort
function (x) 
x[order(x$layer1, x$layer2, x$layer3), ]


$mlpSGD
$mlpSGD$label
[1] "Multilayer Perceptron Network by Stochastic Gradient Descent"

$mlpSGD$library
[1] "FCNN4R"

$mlpSGD$loop
NULL

$mlpSGD$type
[1] "Regression"

$mlpSGD$parameters
    parameter   class                 label
1        size numeric         #Hidden Units
2       l2reg numeric     L2 Regularization
3      lambda numeric RMSE Gradient Scaling
4  learn_rate numeric         Learning Rate
5    momentum numeric              Momentum
6       gamma numeric                 Decay
7 minibatchsz numeric            Batch Size
8     repeats numeric               #Models

$mlpSGD$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(size = ((1:len) * 2) - 1, l2reg = c(0, 
            10^seq(-1, -4, length = len - 1)), lambda = 0, learn_rate = 2e-06, 
            momentum = 0.9, gamma = 10^seq(-3, -1, length = len - 
                1), minibatchsz = floor(nrow(x)/3), repeats = 1)
    }
    else {
        out <- data.frame(size = sample(2:20, replace = TRUE, 
            size = len), l2reg = 10^runif(len, min = -5, 1), 
            lambda = runif(len, max = 0.4), learn_rate = runif(len), 
            momentum = runif(len, min = 0.5), gamma = 10^runif(len, 
                min = -3, 1), minibatchsz = sample(1:(floor(100 * 
                2/3) + 1), replace = TRUE, size = len), repeats = sample(1:10, 
                replace = TRUE, size = len))
    }
    out
}

$mlpSGD$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    y <- matrix(y, ncol = 1)
    obj <- function(net) return(mlp_mse(net, inp, outp))
    net <- mlp_net(c(ncol(x), param$size, 1))
    net <- mlp_set_activation(net, layer = "h", activation = "sigmoid")
    net <- mlp_set_activation(net, layer = "o", activation = "linear")
    args <- list(net = net, input = x, output = y, learn_rate = param$learn_rate, 
        minibatchsz = param$minibatchsz, l2reg = param$l2reg, 
        lambda = param$lambda, gamma = param$gamma, momentum = param$momentum)
    the_dots <- list(...)
    if (!any(names(the_dots) == "tol_level")) 
        args$tol_level <- sd(y[, 1])/sqrt(nrow(y))
    if (!any(names(the_dots) == "max_epochs")) 
        args$max_epochs <- 1000
    args <- c(args, the_dots)
    out <- list(models = vector(mode = "list", length = param$repeats))
    for (i in 1:param$repeats) {
        args$net <- mlp_rnd_weights(args$net)
        out$models[[i]] <- do.call("mlp_teach_sgd", args)
    }
    out
}

$mlpSGD$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- lapply(modelFit$models, function(obj, newdata) mlp_eval(obj$net, 
        input = newdata), newdata = newdata)
    out <- if (length(out) == 1) 
        out[[1]][, 1]
    else {
        out <- do.call("rbind", out)
        out <- apply(out, 1, mean)
    }
    out
}

$mlpSGD$prob
NULL

$mlpSGD$tags
[1] "Neural Network"    "L2 Regularization"

$mlpSGD$sort
function (x) 
x[order(x$size, -x$l2reg, -x$gamma), ]


$mlpWeightDecay
$mlpWeightDecay$label
[1] "Multi-Layer Perceptron"

$mlpWeightDecay$library
[1] "RSNNS"

$mlpWeightDecay$loop
NULL

$mlpWeightDecay$type
[1] "Regression"     "Classification"

$mlpWeightDecay$parameters
  parameter   class         label
1      size numeric #Hidden Units
2     decay numeric  Weight Decay

$mlpWeightDecay$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(size = ((1:len) * 2) - 1, decay = c(0, 
            10^seq(-1, -4, length = len - 1)))
    }
    else {
        out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
            decay = 10^runif(len, min = -5, max = 1))
    }
    out
}

$mlpWeightDecay$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
    if (any(names(theDots) == "learnFunc")) {
        theDots$learnFunc <- NULL
        warning("Cannot over-ride 'learnFunc' argument for this model. BackpropWeightDecay is used.")
    }
    if (any(names(theDots) == "learnFuncParams")) {
        prms <- theDots$learnFuncParams
        prms[2] <- param$decay
        warning("Over-riding weight decay value in the 'learnFuncParams' argument you passed in. Other values are retained")
    }
    else prms <- c(0.2, param$decay, 0, 0)
    if (is.factor(y)) {
        y <- RSNNS:::decodeClassLabels(y)
        lin <- FALSE
    }
    else lin <- TRUE
    args <- list(x = x, y = y, learnFunc = "BackpropWeightDecay", 
        learnFuncParams = prms, size = param$size, linOut = lin)
    args <- c(args, theDots)
    do.call("mlp", args)
}

$mlpWeightDecay$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$mlpWeightDecay$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$mlpWeightDecay$levels
function (x) 
x$obsLevels

$mlpWeightDecay$tags
[1] "Neural Network"    "L2 Regularization"

$mlpWeightDecay$sort
function (x) 
x[order(x$size, -x$decay), ]


$mlpWeightDecayML
$mlpWeightDecayML$label
[1] "Multi-Layer Perceptron, multiple layers"

$mlpWeightDecayML$library
[1] "RSNNS"

$mlpWeightDecayML$loop
NULL

$mlpWeightDecayML$type
[1] "Regression"     "Classification"

$mlpWeightDecayML$parameters
  parameter   class                label
1    layer1 numeric #Hidden Units layer1
2    layer2 numeric #Hidden Units layer2
3    layer3 numeric #Hidden Units layer3
4     decay numeric         Weight Decay

$mlpWeightDecayML$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, 
            layer3 = 0, decay = c(0, 10^seq(-1, -4, length = len - 
                1)))
    }
    else {
        out <- data.frame(layer1 = sample(2:20, replace = TRUE, 
            size = len), layer2 = sample(c(0, 2:20), replace = TRUE, 
            size = len), layer3 = sample(c(0, 2:20), replace = TRUE, 
            size = len), decay = 10^runif(len, min = -5, max = 1))
    }
    out
}

$mlpWeightDecayML$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
    if (any(names(theDots) == "learnFunc")) {
        theDots$learnFunc <- NULL
        warning("Cannot over-ride 'learnFunc' argument for this model. BackpropWeightDecay is used.")
    }
    if (any(names(theDots) == "learnFuncParams")) {
        prms <- theDots$learnFuncParams
        prms[2] <- param$decay
        warning("Over-riding weight decay value in the 'learnFuncParams' argument you passed in. Other values are retained")
    }
    else prms <- c(0.2, param$decay, 0, 0)
    if (is.factor(y)) {
        y <- RSNNS:::decodeClassLabels(y)
        lin <- FALSE
    }
    else lin <- TRUE
    if (param$layer1 == 0) 
        stop("the first layer must have at least one hidden unit")
    if (param$layer2 == 0 & param$layer2 > 0) 
        stop("the second layer must have at least one hidden unit if a third layer is specified")
    nodes <- c(param$layer1)
    if (param$layer2 > 0) {
        nodes <- c(nodes, param$layer2)
        if (param$layer3 > 0) 
            nodes <- c(nodes, param$layer3)
    }
    args <- list(x = x, y = y, learnFunc = "BackpropWeightDecay", 
        learnFuncParams = prms, size = nodes, linOut = lin)
    args <- c(args, theDots)
    do.call("mlp", args)
}

$mlpWeightDecayML$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$mlpWeightDecayML$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$mlpWeightDecayML$levels
function (x) 
x$obsLevels

$mlpWeightDecayML$tags
[1] "Neural Network"    "L2 Regularization"

$mlpWeightDecayML$sort
function (x) 
x[order(x$layer1, x$layer2, x$layer3, -x$decay), ]


$multinom
$multinom$label
[1] "Penalized Multinomial Regression"

$multinom$library
[1] "nnet"

$multinom$loop
NULL

$multinom$type
[1] "Classification"

$multinom$parameters
  parameter   class        label
1     decay numeric Weight Decay

$multinom$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(decay = c(0, 10^seq(-1, -4, length = len - 
            1)))
    }
    else {
        out <- data.frame(decay = 10^runif(len, min = -5, 1))
    }
    out
}

$multinom$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- multinom(.outcome ~ ., data = dat, weights = wts, 
            decay = param$decay, ...)
    }
    else out <- multinom(.outcome ~ ., data = dat, decay = param$decay, 
        ...)
    out
}

$multinom$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "class")

$multinom$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "probs")
    if (ncol(as.data.frame(out)) == 1) {
        out <- cbind(out, 1 - out)
        colnames(out) <- rev(modelFit$obsLevels)
    }
    out
}

$multinom$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else NA

$multinom$varImp
function (object, ...) 
{
    out <- abs(coef(object))
    if (is.vector(out)) {
        out <- data.frame(Overall = out)
        rownames(out) <- names(coef(object))
    }
    else {
        out <- as.data.frame(apply(out, 2, sum))
        names(out)[1] <- "Overall"
    }
    subset(out, rownames(out) != "(Intercept)")
}

$multinom$levels
function (x) 
x$obsLevels

$multinom$tags
[1] "Neural Network"       "L2 Regularization"    "Logistic Regression" 
[4] "Linear Classifier"    "Accepts Case Weights"

$multinom$sort
function (x) 
x[order(-x[, 1]), ]


$nb
$nb$label
[1] "Naive Bayes"

$nb$library
[1] "klaR"

$nb$loop
NULL

$nb$type
[1] "Classification"

$nb$parameters
  parameter   class                label
1        fL numeric   Laplace Correction
2 usekernel logical    Distribution Type
3    adjust numeric Bandwidth Adjustment

$nb$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(usekernel = c(TRUE, FALSE), fL = 0, adjust = 1)

$nb$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (param$usekernel) {
        out <- NaiveBayes(x, y, usekernel = TRUE, fL = param$fL, 
            adjust = param$adjust, ...)
    }
    else out <- NaiveBayes(x, y, usekernel = FALSE, fL = param$fL, 
        ...)
    out
}

$nb$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)$class
}

$nb$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "raw")$posterior
}

$nb$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else x$varnames

$nb$tags
[1] "Bayesian Model"

$nb$levels
function (x) 
x$levels

$nb$sort
function (x) 
x[order(x[, 1]), ]


$nbDiscrete
$nbDiscrete$label
[1] "Naive Bayes Classifier"

$nbDiscrete$library
[1] "bnclassify"

$nbDiscrete$type
[1] "Classification"

$nbDiscrete$parameters
  parameter   class               label
1    smooth numeric Smoothing Parameter

$nbDiscrete$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(smooth = 0:(len - 1))
    }
    else {
        out <- data.frame(smooth = runif(len, min = 0, max = 10))
    }
    out
}

$nbDiscrete$loop
NULL

$nbDiscrete$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    bnc("nb", class = ".outcome", dataset = dat, smooth = param$smooth, 
        ...)
}

$nbDiscrete$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$nbDiscrete$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$nbDiscrete$levels
function (x) 
x$obsLevels

$nbDiscrete$predictors
function (x, s = NULL, ...) 
x$xNames

$nbDiscrete$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$nbDiscrete$sort
function (x) 
x[order(x[, 1]), ]


$nbSearch
$nbSearch$label
[1] "Semi-Naive Structure Learner Wrapper"

$nbSearch$library
[1] "bnclassify"

$nbSearch$type
[1] "Classification"

$nbSearch$parameters
     parameter     class                        label
1            k   numeric                       #Folds
2      epsilon   numeric Minimum Absolute Improvement
3       smooth   numeric          Smoothing Parameter
4 final_smooth   numeric    Final Smoothing Parameter
5    direction character             Search Direction

$nbSearch$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(k = 10, epsilon = 0.01, smooth = 0.01, 
            final_smooth = 1, direction = c("forward", "backwards"))
    }
    else {
        out <- data.frame(k = sample(3:10, size = len, replace = TRUE), 
            epsilon = runif(len, min = 0, max = 0.05), smooth = runif(len, 
                min = 0, max = 10), final_smooth = runif(len, 
                min = 0, max = 10), direction = sample(c("forward", 
                "backwards"), size = len, replace = TRUE))
    }
    out
}

$nbSearch$loop
NULL

$nbSearch$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (param$direction == "forward") {
        struct <- fssj(class = ".outcome", dataset = dat, k = param$k, 
            epsilon = param$epsilon, smooth = param$smooth)
    }
    else {
        struct <- bsej(class = ".outcome", dataset = dat, k = param$k, 
            epsilon = param$epsilon, smooth = param$smooth)
    }
    lp(struct, dat, smooth = param$final_smooth, ...)
}

$nbSearch$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$nbSearch$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$nbSearch$levels
function (x) 
x$obsLevels

$nbSearch$predictors
function (x, s = NULL, ...) 
x$xNames

$nbSearch$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$nbSearch$sort
function (x) 
x[order(x[, 1]), ]


$neuralnet
$neuralnet$label
[1] "Neural Network"

$neuralnet$library
[1] "neuralnet"

$neuralnet$loop
NULL

$neuralnet$type
[1] "Regression"

$neuralnet$parameters
  parameter   class                    label
1    layer1 numeric #Hidden Units in Layer 1
2    layer2 numeric #Hidden Units in Layer 2
3    layer3 numeric #Hidden Units in Layer 3

$neuralnet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, 
            layer3 = 0)
    }
    else {
        out <- data.frame(layer1 = sample(2:20, replace = TRUE, 
            size = len), layer2 = sample(c(0, 2:20), replace = TRUE, 
            size = len), layer3 = sample(c(0, 2:20), replace = TRUE, 
            size = len))
    }
    out
}

$neuralnet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    colNames <- colnames(x)
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    form <- as.formula(paste(".outcome ~", paste(colNames, collapse = "+")))
    if (param$layer1 == 0) 
        stop("the first layer must have at least one hidden unit")
    if (param$layer2 == 0 & param$layer2 > 0) 
        stop("the second layer must have at least one hidden unit if a third layer is specified")
    nodes <- c(param$layer1)
    if (param$layer2 > 0) {
        nodes <- c(nodes, param$layer2)
        if (param$layer3 > 0) 
            nodes <- c(nodes, param$layer3)
    }
    neuralnet(form, data = dat, hidden = nodes, ...)
}

$neuralnet$predict
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- newdata[, modelFit$model.list$variables, drop = FALSE]
    compute(modelFit, covariate = newdata)$net.result[, 1]
}

$neuralnet$prob
NULL

$neuralnet$tags
[1] "Neural Network"

$neuralnet$sort
function (x) 
x[order(x$layer1, x$layer2, x$layer3), ]


$nnet
$nnet$label
[1] "Neural Network"

$nnet$library
[1] "nnet"

$nnet$loop
NULL

$nnet$type
[1] "Classification" "Regression"    

$nnet$parameters
  parameter   class         label
1      size numeric #Hidden Units
2     decay numeric  Weight Decay

$nnet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(size = ((1:len) * 2) - 1, decay = c(0, 
            10^seq(-1, -4, length = len - 1)))
    }
    else {
        out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
            decay = 10^runif(len, min = -5, 1))
    }
    out
}

$nnet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- nnet(.outcome ~ ., data = dat, weights = wts, 
            size = param$size, decay = param$decay, ...)
    }
    else out <- nnet(.outcome ~ ., data = dat, size = param$size, 
        decay = param$decay, ...)
    out
}

$nnet$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata, type = "raw")
    }
    out
}

$nnet$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (ncol(as.data.frame(out)) == 1) {
        out <- cbind(out, 1 - out)
        dimnames(out)[[2]] <- rev(modelFit$obsLevels)
    }
    out
}

$nnet$varImp
function (object, ...) 
{
    imp <- caret:::GarsonWeights(object, ...)
    if (ncol(imp) > 1) {
        imp <- cbind(apply(imp, 1, mean), imp)
        colnames(imp)[1] <- "Overall"
    }
    else {
        imp <- as.data.frame(imp)
        names(imp) <- "Overall"
    }
    if (!is.null(object$xNames)) 
        rownames(imp) <- object$xNames
    imp
}

$nnet$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else NA

$nnet$tags
[1] "Neural Network"       "L2 Regularization"    "Accepts Case Weights"

$nnet$levels
function (x) 
x$lev

$nnet$sort
function (x) 
x[order(x$size, -x$decay), ]


$nnls
$nnls$label
[1] "Non-Negative Least Squares"

$nnls$library
[1] "nnls"

$nnls$loop
NULL

$nnls$type
[1] "Regression"

$nnls$parameters
  parameter     class     label
1 parameter character parameter

$nnls$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$nnls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    out <- nnls(x, y)
    names(out$x) <- colnames(x)
    out
}

$nnls$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        x <- as.matrix(newdata)
    out <- newdata %*% modelFit$x
    out[, 1]
}

$nnls$prob
NULL

$nnls$predictors
function (x, ...) 
names(x$x)[x$x != 0]

$nnls$tags
[1] "Linear Regression"

$nnls$varImp
function (object, ...) 
{
    out <- data.frame(Overall = object$x)
    rownames(out) <- names(object$x)
    out
    out
}

$nnls$sort
function (x) 
x


$nodeHarvest
$nodeHarvest$label
[1] "Tree-Based Ensembles"

$nodeHarvest$library
[1] "nodeHarvest"

$nodeHarvest$loop
NULL

$nodeHarvest$type
[1] "Regression"     "Classification"

$nodeHarvest$parameters
  parameter     class                     label
1  maxinter   numeric Maximum Interaction Depth
2      mode character           Prediction Mode

$nodeHarvest$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(maxinter = 1:len, mode = c("mean", 
            "outbag"))
    }
    else {
        out <- data.frame(maxinter = sample(1:20, size = len, 
            replace = TRUE), mode = sample(c("mean", "outbag"), 
            size = len, replace = TRUE))
    }
    out
}

$nodeHarvest$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.numeric(y)) {
        out <- nodeHarvest(x, y, maxinter = param$maxinter, mode = param$mode, 
            ...)
    }
    else {
        if (length(levels(y)) > 2) 
            stop("Two Class problems only")
        out <- nodeHarvest(x, ifelse(y == levels(y)[1], 1, 0), 
            maxinter = param$maxinter, mode = param$mode, ...)
    }
    out
}

$nodeHarvest$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Regression") {
        predict(modelFit, as.matrix(newdata), maxshow = 0)
    }
    else {
        prbs <- predict(modelFit, as.matrix(newdata), maxshow = 0)
        ifelse(prbs > 0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
    }
}

$nodeHarvest$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata), maxshow = 0)
    if (is.vector(out)) {
        out <- cbind(out, 1 - out)
        colnames(out) <- modelFit$obsLevels
    }
    out
}

$nodeHarvest$levels
function (x) 
x$obsLevels

$nodeHarvest$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Ensemble Model"             "Two Class Only"            

$nodeHarvest$sort
function (x) 
x[order(x$maxinter, x$mode), ]


$oblique.tree
$oblique.tree$label
[1] "Oblique Trees"

$oblique.tree$library
[1] "oblique.tree"

$oblique.tree$loop
NULL

$oblique.tree$type
[1] "Classification"

$oblique.tree$parameters
           parameter     class                     label
1     oblique.splits character            Oblique Splits
2 variable.selection character Variable Selection Method

$oblique.tree$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(oblique.splits = c("only", "on", "off"), variable.selection = c("none", 
    "model.selection.aic"))

$oblique.tree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    oblique.tree(.outcome ~ ., data = dat, oblique.splits = as.character(param$oblique.splits), 
        variable.selection = as.character(param$variable.selection), 
        ...)
}

$oblique.tree$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    predict(modelFit, newdata, type = "class")
}

$oblique.tree$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
        levels = modelFit$obsLevels)
    predict(modelFit, newdata, type = "vector")
}

$oblique.tree$levels
function (x) 
x$obsLevels

$oblique.tree$tags
[1] "Tree-Based Model"           "Implicit Feature Selection"
[3] "Oblique Tree"              

$oblique.tree$sort
function (x) 
x[order(x$variable.selection), ]


$OneR
$OneR$label
[1] "Single Rule Classification"

$OneR$library
[1] "RWeka"

$OneR$loop
NULL

$OneR$type
[1] "Classification"

$OneR$parameters
  parameter     class label
1 parameter character  none

$OneR$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$OneR$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat), theDots)
    out <- do.call("OneR", modelArgs)
    out
}

$OneR$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$OneR$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "probability")
}

$OneR$levels
function (x) 
x$obsLevels

$OneR$predictors
function (x, ...) 
predictors(x$terms)

$OneR$tags
[1] "Rule-Based Model"           "Implicit Feature Selection"

$OneR$sort
function (x) 
x


$ordinalNet
$ordinalNet$label
[1] "Penalized Ordinal Regression"

$ordinalNet$library
[1] "ordinalNet" "plyr"      

$ordinalNet$type
[1] "Regression"     "Classification"

$ordinalNet$parameters
  parameter     class               label
1     alpha   numeric   Mixing Percentage
2  criteria character Selection Criterion
3      link character       Link Function

$ordinalNet$grid
function (x, y, len = NULL, search = "grid") 
{
    links <- c("logit", "probit", "cloglog", "cauchit")
    if (search == "grid") {
        out <- expand.grid(alpha = seq(0.1, 1, length = len), 
            criteria = "aic", link = links)
    }
    else {
        out <- data.frame(alpha = runif(len, min = 0, 1), criteria = sample(c("aic", 
            "bic"), size = len, replace = TRUE), links = sample(links, 
            size = len, replace = TRUE))
    }
    out
}

$ordinalNet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    out <- ordinalNet(x = x, y = y, alpha = param$alpha, link = as.character(param$link), 
        ...)
    out$.criteria <- as.character(param$criteria)
    out
}

$ordinalNet$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- modelFit$obsLevels[predict(modelFit, newdata, criteria = modelFit$.criteria)]
    out
}

$ordinalNet$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, criteria = modelFit$.criteria, 
        type = "prob")
    colnames(out) <- modelFit$obsLevels
    out
}

$ordinalNet$predictors
function (x, lambda = NULL, ...) 
{
    betas <- coef(x, criteria = x$.criteria)
    out <- names(betas)[betas != 0]
    out[!grepl("^Intercept", out)]
}

$ordinalNet$varImp
function (object, lambda = NULL, ...) 
{
    betas <- coef(object, criteria = object$.criteria)
    betas <- betas[!grepl("^Intercept", names(betas))]
    out <- data.frame(Overall = abs(betas))
    rownames(out) <- names(betas)
    out
}

$ordinalNet$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$ordinalNet$tags
[1] "Generalized Linear Model"   "Implicit Feature Selection"
[3] "L1 Regularization"          "L2 Regularization"         
[5] "Linear Classifier"          "Linear Regression"         

$ordinalNet$sort
function (x) 
x[order(x$alpha), ]


$ORFlog
$ORFlog$label
[1] "Oblique Random Forest"

$ORFlog$library
[1] "obliqueRF"

$ORFlog$loop
NULL

$ORFlog$type
[1] "Classification"

$ORFlog$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$ORFlog$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$ORFlog$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
obliqueRF(as.matrix(x), y, training_method = "log", ...)

$ORFlog$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$ORFlog$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$ORFlog$levels
function (x) 
x$obsLevels

$ORFlog$tags
[1] "Random Forest"              "Oblique Tree"              
[3] "Logistic Regression"        "Implicit Feature Selection"
[5] "Ensemble Model"             "Two Class Only"            

$ORFlog$sort
function (x) 
x[order(x[, 1]), ]


$ORFpls
$ORFpls$label
[1] "Oblique Random Forest"

$ORFpls$library
[1] "obliqueRF"

$ORFpls$loop
NULL

$ORFpls$type
[1] "Classification"

$ORFpls$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$ORFpls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$ORFpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
obliqueRF(as.matrix(x), y, training_method = "pls", ...)

$ORFpls$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$ORFpls$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$ORFpls$levels
function (x) 
x$obsLevels

$ORFpls$tags
[1] "Random Forest"              "Oblique Tree"              
[3] "Partial Least Squares"      "Implicit Feature Selection"
[5] "Ensemble Model"             "Two Class Only"            

$ORFpls$sort
function (x) 
x[order(x[, 1]), ]


$ORFridge
$ORFridge$label
[1] "Oblique Random Forest"

$ORFridge$library
[1] "obliqueRF"

$ORFridge$loop
NULL

$ORFridge$type
[1] "Classification"

$ORFridge$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$ORFridge$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$ORFridge$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
obliqueRF(as.matrix(x), y, training_method = "ridge", ...)

$ORFridge$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$ORFridge$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$ORFridge$levels
function (x) 
x$obsLevels

$ORFridge$tags
[1] "Random Forest"              "Oblique Tree"              
[3] "Ridge Regression"           "Implicit Feature Selection"
[5] "Ensemble Model"             "Two Class Only"            
[7] "L2 Regularization"         

$ORFridge$sort
function (x) 
x[order(x[, 1]), ]


$ORFsvm
$ORFsvm$label
[1] "Oblique Random Forest"

$ORFsvm$library
[1] "obliqueRF"

$ORFsvm$loop
NULL

$ORFsvm$type
[1] "Classification"

$ORFsvm$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$ORFsvm$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$ORFsvm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
obliqueRF(as.matrix(x), y, training_method = "svm", ...)

$ORFsvm$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$ORFsvm$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$ORFsvm$levels
function (x) 
x$obsLevels

$ORFsvm$tags
[1] "Random Forest"              "Oblique Tree"              
[3] "Kernel Method"              "Implicit Feature Selection"
[5] "Ensemble Model"             "Two Class Only"            

$ORFsvm$sort
function (x) 
x[order(x[, 1]), ]


$ownn
$ownn$label
[1] "Optimal Weighted Nearest Neighbor Classifier"

$ownn$library
[1] "snn"

$ownn$loop
NULL

$ownn$type
[1] "Classification"

$ownn$parameters
  parameter   class      label
1         K numeric #Neighbors

$ownn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(K = (5:((2 * len) + 4))[(5:((2 * len) + 
            4))%%2 > 0])
    }
    else {
        by_val <- if (is.factor(y)) 
            length(levels(y))
        else 1
        out <- data.frame(K = sample(seq(1, floor(nrow(x)/3), 
            by = by_val), size = len, replace = TRUE))
    }
    out
}

$ownn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    if (!(class(x[1, 1]) %in% c("integer", "numeric"))) 
        stop("predictors should be all numeric")
    x <- cbind(x, as.numeric(y))
    colnames(x)[ncol(x)] <- ".outcome"
    list(dat = x, K = param$K)
}

$ownn$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- myownn(train = modelFit$dat, test = newdata, K = modelFit$K)
    modelFit$obsLevels[out]
}

$ownn$predictors
function (x, ...) 
x$xNames

$ownn$tags
[1] "Prototype Models"

$ownn$prob
NULL

$ownn$levels
function (x) 
x$obsLevels

$ownn$sort
function (x) 
x[order(-x[, 1]), ]


$pam
$pam$label
[1] "Nearest Shrunken Centroids"

$pam$library
[1] "pamr"

$pam$type
[1] "Classification"

$pam$parameters
  parameter   class               label
1 threshold numeric Shrinkage Threshold

$pam$grid
function (x, y, len = NULL, search = "grid") 
{
    cc <- complete.cases(x) & complete.cases(y)
    x <- x[cc, , drop = FALSE]
    y <- y[cc]
    initialThresh <- pamr.train(list(x = t(x), y = y))$threshold
    initialThresh <- initialThresh[-c(1, length(initialThresh))]
    if (search == "grid") {
        out <- data.frame(threshold = seq(from = min(initialThresh), 
            to = max(initialThresh), length = len))
    }
    else {
        out <- data.frame(threshold = runif(len, min = min(initialThresh), 
            max = max(initialThresh)))
    }
    out
}

$pam$loop
function (grid) 
{
    grid <- grid[order(grid$threshold, decreasing = TRUE), , 
        drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$pam$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
pamr.train(list(x = t(x), y = y), threshold = param$threshold, 
    ...)

$pam$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- pamr.predict(modelFit, t(newdata), threshold = modelFit$tuneValue$threshold)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$threshold)) {
            tmp[[j + 1]] <- pamr.predict(modelFit, t(newdata), 
                threshold = submodels$threshold[j])
        }
        out <- tmp
    }
    out
}

$pam$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- pamr.predict(modelFit, t(newdata), threshold = modelFit$tuneValue$threshold, 
        type = "posterior")
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$threshold)) {
            tmpProb <- pamr.predict(modelFit, t(newdata), threshold = submodels$threshold[j], 
                type = "posterior")
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$pam$predictors
function (x, newdata = NULL, threshold = NULL, ...) 
{
    if (is.null(newdata)) {
        if (!is.null(x$xData)) 
            newdata <- x$xData
        else stop("must supply newdata")
    }
    if (is.null(threshold)) {
        if (!is.null(x$threshold)) 
            threshold <- x$threshold
        else stop("must supply threshold")
    }
    varIndex <- pamr.predict(x, newx = newdata, threshold = threshold, 
        type = "nonzero")
    colnames(newdata)[varIndex]
}

$pam$varImp
function (object, threshold = NULL, data = NULL, ...) 
{
    if (is.null(data)) 
        data <- object$xData
    if (is.null(threshold)) 
        threshold <- object$tuneValue$threshold
    if (dim(object$centroids)[1] != dim(data)[2]) 
        stop("the number of columns (=variables) is not consistent with the pamr object")
    if (is.null(dimnames(data))) {
        featureNames <- paste("Feature", seq(along = data[1, 
            ]), sep = "")
        colnames(data) <- featureNames
    }
    else featureNames <- dimnames(data)[[2]]
    x <- t(data)
    retainedX <- x[object$gene.subset, object$sample.subset, 
        drop = F]
    centroids <- pamr.predict(object, x, threshold = threshold, 
        type = "cent")
    standCentroids <- (centroids - object$centroid.overall)/object$sd
    rownames(standCentroids) <- featureNames
    colnames(standCentroids) <- names(object$prior)
    as.data.frame(standCentroids)
}

$pam$levels
function (x) 
names(x$prior)

$pam$tags
[1] "Prototype Models"           "Implicit Feature Selection"
[3] "Linear Classifier"         

$pam$sort
function (x) 
x[order(x[, 1]), ]


$parRF
$parRF$label
[1] "Parallel Random Forest"

$parRF$library
[1] "e1071"        "randomForest" "foreach"     

$parRF$loop
NULL

$parRF$type
[1] "Classification" "Regression"    

$parRF$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$parRF$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$parRF$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    workers <- getDoParWorkers()
    theDots <- list(...)
    theDots$ntree <- if (is.null(theDots$ntree)) 
        250
    else theDots$ntree
    theDots$x <- x
    theDots$y <- y
    theDots$mtry <- param$mtry
    theDots$ntree <- ceiling(theDots$ntree/workers)
    out <- foreach(ntree = 1:workers, .combine = combine) %dopar% 
        {
            library(randomForest)
            do.call("randomForest", theDots)
        }
    out$call["x"] <- "x"
    out$call["y"] <- "y"
    out
}

$parRF$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$parRF$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$parRF$predictors
function (x, ...) 
{
    varIndex <- as.numeric(names(table(x$forest$bestvar)))
    varIndex <- varIndex[varIndex > 0]
    varsUsed <- names(x$forest$ncat)[varIndex]
    varsUsed
}

$parRF$varImp
function (object, ...) 
{
    varImp <- randomForest::importance(object, ...)
    if (object$type == "regression") 
        varImp <- data.frame(Overall = varImp[, "%IncMSE"])
    else {
        retainNames <- levels(object$y)
        if (all(retainNames %in% colnames(varImp))) {
            varImp <- varImp[, retainNames]
        }
        else {
            varImp <- data.frame(Overall = varImp[, 1])
        }
    }
    out <- as.data.frame(varImp)
    if (dim(out)[2] == 2) {
        tmp <- apply(out, 1, mean)
        out[, 1] <- out[, 2] <- tmp
    }
    out
}

$parRF$levels
function (x) 
x$classes

$parRF$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$parRF$sort
function (x) 
x[order(x[, 1]), ]

$parRF$oob
function (x) 
{
    out <- switch(x$type, regression = c(sqrt(max(x$mse[length(x$mse)], 
        0)), x$rsq[length(x$rsq)]), classification = c(1 - x$err.rate[x$ntree, 
        "OOB"], e1071::classAgreement(x$confusion[, -dim(x$confusion)[2]])[["kappa"]]))
    names(out) <- if (x$type == "regression") 
        c("RMSE", "Rsquared")
    else c("Accuracy", "Kappa")
    out
}


$PART
$PART$label
[1] "Rule-Based Classifier"

$PART$library
[1] "RWeka"

$PART$loop
NULL

$PART$type
[1] "Classification"

$PART$parameters
  parameter     class                label X.Pruning.
1 threshold   numeric Confidence Threshold    Pruning
2    pruned character Confidence Threshold    Pruning

$PART$grid
function (x, y, len = NULL, search = "grid") 
data.frame(threshold = 0.25, pruned = "yes")

$PART$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$U <- ifelse(param$pruned == "No", TRUE, 
            FALSE)
        theDots$control$C <- param$threshold
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- Weka_control(N = ifelse(param$pruned == "No", 
        TRUE, FALSE), C = param$threshold)
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = dat, control = ctl), theDots)
    out <- do.call("PART", modelArgs)
    out
}

$PART$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$PART$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "probability")
}

$PART$levels
function (x) 
x$obsLevels

$PART$predictors
function (x, ...) 
predictors(x$terms)

$PART$tags
[1] "Rule-Based Model"           "Implicit Feature Selection"

$PART$varImp
function (object, ...) 
{
    dat <- caret:::partRuleSummary(object)
    out <- dat$varUsage[, "Overall", drop = FALSE]
    rownames(out) <- dat$varUsage$Var
    out
}

$PART$sort
function (x) 
x[order(x$pruned, -x$threshold), ]


$partDSA
$partDSA$label
[1] "partDSA"

$partDSA$library
[1] "partDSA"

$partDSA$type
[1] "Regression"     "Classification"

$partDSA$parameters
       parameter   class                         label
1 cut.off.growth numeric Number of Terminal Partitions
2            MPD numeric    Minimum Percent Difference

$partDSA$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cut.off.growth = 1:len, MPD = 0.1)
    }
    else {
        out <- data.frame(cut.off.growth = sample(1:20, size = len, 
            replace = TRUE), MPD = runif(len, min = 0, max = 0.5))
    }
    out
}

$partDSA$loop
function (grid) 
{
    grid <- grid[order(grid$MPD, grid$cut.off.growth, decreasing = TRUE), 
        , drop = FALSE]
    uniqueMPD <- unique(grid$MPD)
    loop <- data.frame(MPD = uniqueMPD)
    loop$cut.off.growth <- NA
    submodels <- vector(mode = "list", length = length(uniqueMPD))
    for (i in seq(along = uniqueMPD)) {
        subCuts <- grid[grid$MPD == uniqueMPD[i], "cut.off.growth"]
        loop$cut.off.growth[loop$MPD == uniqueMPD[i]] <- subCuts[which.max(subCuts)]
        submodels[[i]] <- data.frame(cut.off.growth = subCuts[-which.max(subCuts)])
    }
    list(loop = loop, submodels = submodels)
}

$partDSA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    partDSA(x, y, control = DSA.control(cut.off.growth = param$cut.off.growth, 
        MPD = param$MPD, vfold = 1), ...)
}

$partDSA$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.null(submodels)) {
        tmp <- c(modelFit$tuneValue$cut.off.growth, submodels$cut.off.growth)
        if (modelFit$problemType == "Classification") {
            out <- predict(modelFit, newdata)
            if (max(tmp) > length(out)) 
                tmp[tmp > length(out)] <- length(out)
            out <- out[tmp]
        }
        else {
            out <- predict(modelFit, newdata)
            if (max(tmp) > ncol(out)) 
                tmp[tmp > ncol(out)] <- ncol(out)
            out <- out[, tmp, drop = FALSE]
            out <- as.list(as.data.frame(out))
        }
    }
    else {
        index <- min(modelFit$cut.off.growth, length(modelFit$test.set.risk.DSA))
        if (modelFit$problemType == "Classification") {
            out <- as.character(predict(modelFit, newdata)[[index]])
        }
        else {
            out <- predict(modelFit, newdata)[, index]
        }
    }
    out
}

$partDSA$predictors
function (x, cuts = NULL, ...) 
{
    if (is.null(cuts) & !is.null(x$tuneValue)) {
        cuts <- x$tuneValue$cut.off.growth[1]
    }
    else {
        if (is.null(cuts)) 
            stop("please supply a value for 'cuts'")
    }
    tmp <- x$var.importance[, cuts]
    names(tmp)[which(tmp != 0)]
}

$partDSA$levels
function (x) 
x$obsLevels

$partDSA$tags
[1] ""

$partDSA$prob
NULL

$partDSA$varImp
function (object, cuts = NULL, ...) 
{
    if (is.null(cuts) & !is.null(object$tuneValue)) {
        cuts <- object$tuneValue$cut.off.growth[1]
    }
    else {
        if (is.null(cuts)) 
            stop("please supply a value for 'cuts'")
    }
    tmp <- object$var.importance[, cuts]
    out <- data.frame(Overall = tmp)
    rownames(out) <- names(tmp)
    out
}

$partDSA$sort
function (x) 
x[order(x$cut.off.growth, x$MPD), ]


$pcaNNet
$pcaNNet$label
[1] "Neural Networks with Feature Extraction"

$pcaNNet$library
[1] "nnet"

$pcaNNet$loop
NULL

$pcaNNet$type
[1] "Classification" "Regression"    

$pcaNNet$parameters
  parameter   class         label
1      size numeric #Hidden Units
2     decay numeric  Weight Decay

$pcaNNet$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(size = ((1:len) * 2) - 1, decay = c(0, 
            10^seq(-1, -4, length = len - 1)))
    }
    else {
        out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
            decay = 10^runif(len, min = -5, 1))
    }
    out
}

$pcaNNet$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    library(nnet)
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- pcaNNet(.outcome ~ ., data = dat, weights = wts, 
            size = param$size, decay = param$decay, ...)
    }
    else out <- pcaNNet(.outcome ~ ., data = dat, size = param$size, 
        decay = param$decay, ...)
    out
}

$pcaNNet$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata, type = "class")
    }
    else {
        out <- predict(modelFit, newdata, type = "raw")
    }
    out
}

$pcaNNet$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "prob")
    if (ncol(as.data.frame(out)) == 1) {
        out <- cbind(out, 1 - out)
        dimnames(out)[[2]] <- rev(modelFit$obsLevels)
    }
    out
}

$pcaNNet$predictors
function (x, ...) 
rownames(x$pc$rotation)

$pcaNNet$levels
function (x) 
x$model$lev

$pcaNNet$tags
[1] "Neural Network"       "Feature Extraction"   "L2 Regularization"   
[4] "Accepts Case Weights"

$pcaNNet$sort
function (x) 
x[order(x$size, -x$decay), ]


$pcr
$pcr$label
[1] "Principal Component Analysis"

$pcr$library
[1] "pls"

$pcr$type
[1] "Regression"

$pcr$parameters
  parameter   class       label
1     ncomp numeric #Components

$pcr$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), 
            by = 1))
    }
    else {
        out <- data.frame(ncomp = unique(sample(1:(ncol(x) - 
            1), size = len, replace = TRUE)))
    }
    out
}

$pcr$loop
function (grid) 
{
    grid <- grid[order(grid$ncomp, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$pcr$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    pcr(.outcome ~ ., data = dat, ncomp = param$ncomp, ...)
}

$pcr$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
    if (!is.null(submodels)) {
        tmp <- apply(predict(modelFit, newdata, ncomp = submodels$ncomp), 
            3, function(x) list(x))
        tmp <- as.data.frame(tmp)
        out <- c(list(out), as.list(tmp))
    }
    out
}

$pcr$predictors
function (x, ...) 
rownames(x$projection)

$pcr$tags
[1] "Linear Regression"  "Feature Extraction"

$pcr$prob
NULL

$pcr$sort
function (x) 
x[order(x[, 1]), ]


$pda
$pda$label
[1] "Penalized Discriminant Analysis"

$pda$library
[1] "mda"

$pda$loop
NULL

$pda$type
[1] "Classification"

$pda$parameters
  parameter   class                         label
1    lambda numeric Shrinkage Penalty Coefficient

$pda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(lambda = c(0, 10^seq(-1, -4, length = len - 
            1)))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1))
    }
    out
}

$pda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- fda(as.formula(".outcome ~ ."), data = dat, method = gen.ridge, 
            weights = wts, lambda = param$lambda, ...)
    }
    else {
        out <- fda(as.formula(".outcome ~ ."), data = dat, method = gen.ridge, 
            lambda = param$lambda, ...)
    }
    out
}

$pda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$pda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "posterior")

$pda$levels
function (x) 
x$obsLevels

$pda$tags
[1] "Discriminant Analysis" "Polynomial Model"      "Accepts Case Weights" 

$pda$sort
function (x) 
x[order(x[, 1]), ]


$pda2
$pda2$label
[1] "Penalized Discriminant Analysis"

$pda2$library
[1] "mda"

$pda2$loop
NULL

$pda2$type
[1] "Classification"

$pda2$parameters
  parameter   class              label
1        df numeric Degrees of Freedom

$pda2$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(df = 2 * (0:(len - 1) + 1))
    }
    else {
        out <- data.frame(df = runif(len, min = 1, max = 5))
    }
    out
}

$pda2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- fda(as.formula(".outcome ~ ."), data = dat, method = gen.ridge, 
            weights = wts, df = param$df, ...)
    }
    else {
        out <- fda(as.formula(".outcome ~ ."), data = dat, method = gen.ridge, 
            df = param$df, ...)
    }
    out
}

$pda2$levels
function (x) 
x$obsLevels

$pda2$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$pda2$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "posterior")

$pda2$tags
[1] "Discriminant Analysis" "Polynomial Model"      "Accepts Case Weights" 

$pda2$sort
function (x) 
x[order(x[, 1]), ]


$penalized
$penalized$label
[1] "Penalized Linear Regression"

$penalized$library
[1] "penalized"

$penalized$type
[1] "Regression"

$penalized$parameters
  parameter   class      label
1   lambda1 numeric L1 Penalty
2   lambda2 numeric L2 Penalty

$penalized$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda1 = 2^((1:len) - 1), lambda2 = 2^((1:len) - 
            1))
    }
    else {
        out <- data.frame(lambda1 = 10^runif(len, min = -5, 1), 
            lambda2 = 10^runif(len, min = -5, 1))
    }
    out
}

$penalized$loop
NULL

$penalized$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    penalized(y, x, model = "linear", lambda1 = param$lambda1, 
        lambda2 = param$lambda2, ...)
}

$penalized$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    out <- if (is.vector(out)) 
        out["mu"]
    else out[, "mu"]
    out
}

$penalized$prob
NULL

$penalized$predictors
function (x, ...) 
{
    out <- coef(x, "all")
    out <- names(out)[out != 0]
    out[out != "(Intercept)"]
}

$penalized$tags
[1] "Implicit Feature Selection" "L1 Regularization"         
[3] "L2 Regularization"          "Linear Regression"         

$penalized$sort
function (x) 
x[order(x$lambda1, x$lambda2), ]


$PenalizedLDA
$PenalizedLDA$label
[1] "Penalized Linear Discriminant Analysis"

$PenalizedLDA$library
[1] "penalizedLDA" "plyr"        

$PenalizedLDA$loop
function (grid) 
{
    loop <- ddply(grid, .(lambda), function(x) c(K = max(x$K)))
    if (length(unique(loop$K)) == 1) 
        return(list(loop = loop, submodels = NULL))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$K)) {
        index <- which(grid$lambda == loop$lambda[i])
        subK <- grid[index, "K"]
        otherK <- data.frame(K = subK[subK != loop$K[i]])
        if (nrow(otherK) > 0) 
            submodels[[i]] <- otherK
    }
    list(loop = loop, submodels = submodels)
}

$PenalizedLDA$type
[1] "Classification"

$PenalizedLDA$parameters
  parameter   class                   label
1    lambda numeric              L1 Penalty
2         K numeric #Discriminant Functions

$PenalizedLDA$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(lambda = 10^seq(-1, -4, length = len), 
            K = length(levels(y)) - 1)
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            K = sample(1:(length(levels(y)) - 1), size = len, 
                replace = TRUE))
    }
    out
}

$PenalizedLDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
penalizedLDA:::PenalizedLDA(as.matrix(x), as.numeric(y), lambda = param$lambda, 
    K = param$K, ...)

$PenalizedLDA$predict
function (modelFit, newdata, submodels = NULL) 
{
    out0 <- predict(modelFit, newdata)$ypred
    out <- out0[, ncol(out0)]
    out <- modelFit$obsLevels[out]
    if (!is.null(submodels)) {
        tmp <- out0[, submodels$K, drop = FALSE]
        tmp <- apply(tmp, 2, function(x, l) l[x], l = modelFit$obsLevels)
        out <- as.data.frame(cbind(out, tmp), stringsAsFactors = FALSE)
    }
    out
}

$PenalizedLDA$levels
function (x) 
x$obsLevels

$PenalizedLDA$prob
NULL

$PenalizedLDA$tags
[1] "Discriminant Analysis"      "L1 Regularization"         
[3] "Implicit Feature Selection" "Linear Classifier"         

$PenalizedLDA$sort
function (x) 
x[order(x$lambda, x$K), ]


$plr
$plr$label
[1] "Penalized Logistic Regression"

$plr$library
[1] "stepPlr"

$plr$loop
NULL

$plr$type
[1] "Classification"

$plr$parameters
  parameter     class                label
1    lambda   numeric           L2 Penalty
2        cp character Complexity Parameter

$plr$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cp = "bic", lambda = c(0, 10^seq(-1, 
            -4, length = len - 1)))
    }
    else {
        out <- data.frame(cp = sample(c("aic", "bic"), size = len, 
            replace = TRUE), lambda = 10^runif(len, min = -5, 
            1))
    }
    out
}

$plr$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    y <- ifelse(y == levels(y)[1], 1, 0)
    plr(x, y, lambda = param$lambda, cp = as.character(param$cp), 
        weights = if (!is.null(wts)) 
            wts
        else rep(1, length(y)), ...)
}

$plr$predict
function (modelFit, newdata, submodels = NULL) 
{
    ifelse(predict(modelFit, as.matrix(newdata), type = "class") == 
        1, modelFit$obsLevels[1], modelFit$obsLevels[2])
}

$plr$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata), type = "response")
    out <- cbind(out, 1 - out)
    dimnames(out)[[2]] <- modelFit$obsLevels
    out
}

$plr$levels
function (x) 
x$obsLevels

$plr$tags
[1] "L2 Regularization"   "Logistic Regression" "Linear Classifier"  

$plr$sort
function (x) 
x[order(-x$lambda), ]


$pls
$pls$label
[1] "Partial Least Squares"

$pls$library
[1] "pls"

$pls$type
[1] "Regression"     "Classification"

$pls$parameters
  parameter   class       label
1     ncomp numeric #Components

$pls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), 
            by = 1))
    }
    else {
        out <- data.frame(ncomp = unique(sample(1:ncol(x), replace = TRUE)))
    }
    out
}

$pls$loop
function (grid) 
{
    grid <- grid[order(grid$ncomp, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$pls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- if (is.factor(y)) {
        plsda(x, y, method = "oscorespls", ncomp = param$ncomp, 
            ...)
    }
    else {
        dat <- if (is.data.frame(x)) 
            x
        else as.data.frame(x)
        dat$.outcome <- y
        plsr(.outcome ~ ., data = dat, method = "oscorespls", 
            ncomp = param$ncomp, ...)
    }
    out
}

$pls$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- if (modelFit$problemType == "Classification") {
        if (!is.matrix(newdata)) 
            newdata <- as.matrix(newdata)
        out <- predict(modelFit, newdata, type = "class")
    }
    else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels))
        if (modelFit$problemType == "Classification") {
            if (length(submodels$ncomp) > 1) {
                tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
            }
            else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))
        }
        else {
            tmp <- as.list(as.data.frame(apply(predict(modelFit, 
                newdata, ncomp = submodels$ncomp), 3, function(x) list(x))))
        }
        out <- c(list(out), tmp)
    }
    out
}

$pls$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, type = "prob", ncomp = modelFit$tuneValue$ncomp)
    if (length(dim(out)) == 3) {
        if (dim(out)[1] > 1) {
            out <- out[, , 1]
        }
        else {
            out <- as.data.frame(t(out[, , 1]))
        }
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$ncomp)) {
            tmpProb <- predict(modelFit, newdata, type = "prob", 
                ncomp = submodels$ncomp[j])
            if (length(dim(tmpProb)) == 3) {
                if (dim(tmpProb)[1] > 1) {
                  tmpProb <- tmpProb[, , 1]
                }
                else {
                  tmpProb <- as.data.frame(t(tmpProb[, , 1]))
                }
            }
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])
        }
        out <- tmp
    }
    out
}

$pls$varImp
function (object, estimate = NULL, ...) 
{
    modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
    perf <- MSEP(object)$val
    nms <- dimnames(perf)
    if (length(nms$estimate) > 1) {
        pIndex <- if (is.null(estimate)) 
            1
        else which(nms$estimate == estimate)
        perf <- perf[pIndex, , , drop = FALSE]
    }
    numResp <- dim(modelCoef)[2]
    if (numResp <= 2) {
        modelCoef <- modelCoef[, 1, , drop = FALSE]
        perf <- perf[, 1, ]
        delta <- -diff(perf)
        delta <- delta/sum(delta)
        out <- data.frame(Overall = apply(abs(modelCoef), 1, 
            weighted.mean, w = delta))
    }
    else {
        perf <- -t(apply(perf[1, , ], 1, diff))
        perf <- t(apply(perf, 1, function(u) u/sum(u)))
        out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])
        for (i in 1:numResp) {
            tmp <- abs(modelCoef[, i, , drop = FALSE])
            out[, i] <- apply(tmp, 1, weighted.mean, w = perf[i, 
                ])
        }
        colnames(out) <- dimnames(modelCoef)[[2]]
        rownames(out) <- dimnames(modelCoef)[[1]]
    }
    as.data.frame(out)
}

$pls$predictors
function (x, ...) 
rownames(x$projection)

$pls$levels
function (x) 
x$obsLevels

$pls$tags
[1] "Partial Least Squares" "Feature Extraction"    "Linear Classifier"    
[4] "Linear Regression"    

$pls$sort
function (x) 
x[order(x[, 1]), ]


$plsRglm
$plsRglm$label
[1] "Partial Least Squares Generalized Linear Models "

$plsRglm$library
[1] "plsRglm"

$plsRglm$loop
NULL

$plsRglm$type
[1] "Classification" "Regression"    

$plsRglm$parameters
          parameter   class             label
1                nt numeric   #PLS Components
2 alpha.pvals.expli numeric p-Value threshold

$plsRglm$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(nt = 1:len, alpha.pvals.expli = 10^(c(-2:(len - 
            3), 0)))
    }
    else {
        out <- data.frame(nt = sample(1:ncol(x), size = len, 
            replace = TRUE), alpha.pvals.expli = runif(len, min = 0, 
            0.2))
    }
    out
}

$plsRglm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        lv <- levels(y)
        y <- as.numeric(y) - 1
        dst <- "pls-glm-logistic"
    }
    else {
        lv <- NULL
        dst <- "pls-glm-gaussian"
    }
    theDots <- list(...)
    if (any(names(theDots) == "modele")) {
        mod <- plsRglm(y, x, nt = param$nt, pvals.expli = param$alpha.pvals.expli < 
            1, sparse = param$alpha.pvals.expli < 1, alpha.pvals.expli = param$alpha.pvals.expli, 
            ...)
    }
    else {
        mod <- plsRglm(y, x, nt = param$nt, modele = dst, pvals.expli = param$alpha.pvals.expli < 
            1, sparse = param$alpha.pvals.expli < 1, alpha.pvals.expli = param$alpha.pvals.expli, 
            ...)
    }
    mod
}

$plsRglm$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    if (modelFit$problemType == "Classification") {
        out <- factor(ifelse(out >= 0.5, modelFit$obsLevels[2], 
            modelFit$obsLevels[1]))
    }
    out
}

$plsRglm$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, type = "response")
    out <- cbind(1 - out, out)
    dimnames(out)[[2]] <- rev(modelFit$obsLevels)
    out
}

$plsRglm$varImp
NULL

$plsRglm$predictors
function (x, ...) 
{
    vars <- names(which(coef(x)[[2]][, 1] != 0))
    vars[vars != "Intercept"]
}

$plsRglm$tags
[1] "Generalized Linear Models" "Partial Least Squares"    
[3] "Two Class Only"           

$plsRglm$levels
function (x) 
x$lev

$plsRglm$sort
function (x) 
x[order(-x$alpha.pvals.expli, x$nt), ]


$polr
$polr$label
[1] "Ordered Logistic or Probit Regression"

$polr$library
[1] "MASS"

$polr$loop
NULL

$polr$type
[1] "Classification"

$polr$parameters
  parameter     class     label
1    method character parameter

$polr$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(method = c("logistic", "probit", "loglog", 
            "cloglog", "cauchit"))
    }
    else {
        out <- data.frame(sample(c("logistic", "probit", "loglog", 
            "cloglog", "cauchit"), size = len, replace = TRUE))
    }
    out[!duplicated(out), , drop = FALSE]
}

$polr$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    modelArgs <- list(...)
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    modelArgs <- c(list(formula = .outcome ~ ., data = dat, method = as.character(param$method)), 
        modelArgs)
    modelArgs$Hess <- TRUE
    if (!is.null(wts)) 
        modelArgs$weights <- wts
    ans <- do.call("polr", modelArgs)
    ans$call <- NULL
    ans
}

$polr$predict
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
predict(modelFit, newdata = newdata, type = "class")

$polr$prob
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
predict(modelFit, newdata = newdata, type = "probs")

$polr$varImp
function (object, ...) 
{
    cf <- coef(object)
    ncf <- length(cf)
    se <- sqrt(diag(vcov(object)))
    se <- se[seq_len(ncf)]
    z <- cf/se
    out <- data.frame(Overall = abs(z))
    if (!is.null(names(cf))) 
        rownames(out) <- names(cf)
    out
}

$polr$predictors
function (x, ...) 
predictors(terms(x))

$polr$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$polr$tags
[1] "Logistic Regression"  "Linear Classifier"    "Accepts Case Weights"
[4] "Ordinal Outcomes"    

$polr$sort
function (x) 
x


$ppr
$ppr$label
[1] "Projection Pursuit Regression"

$ppr$library
NULL

$ppr$loop
NULL

$ppr$type
[1] "Regression"

$ppr$parameters
  parameter   class   label
1    nterms numeric # Terms

$ppr$grid
function (x, y, len = NULL, search = "grid") 
data.frame(nterms = 1:len)

$ppr$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.null(wts)) {
        out <- ppr(as.matrix(x), y, weights = wts, nterms = param$nterms, 
            ...)
    }
    else {
        out <- ppr(as.matrix(x), y, nterms = param$nterms, ...)
    }
    out
}

$ppr$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$ppr$prob
NULL

$ppr$predictors
function (x, ...) 
x$xnames

$ppr$tags
[1] "Feature Extraction"   "Accepts Case Weights"

$ppr$sort
function (x) 
x[order(x[, 1]), ]


$protoclass
$protoclass$label
[1] "Greedy Prototype Selection"

$protoclass$library
[1] "proxy"      "protoclass"

$protoclass$loop
NULL

$protoclass$type
[1] "Classification"

$protoclass$parameters
  parameter   class          label
1       eps numeric      Ball Size
2 Minkowski numeric Distance Order

$protoclass$grid
function (x, y, len = NULL, search = "grid") 
data.frame(eps = 1:len, Minkowski = 2)

$protoclass$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- protoclass(x = x, y = y, dxz = as.matrix(proxy:::dist(x, 
        x, method = "Minkowski", p = as.double(param$Minkowski))), 
        eps = param$eps, ...)
    out$Minkowski <- 2
    out$training <- x
    out
}

$protoclass$levels
function (x) 
x$obsLevels

$protoclass$predict
function (modelFit, newdata, submodels = NULL) 
as.character(predictwithd.protoclass(modelFit, as.matrix(proxy:::dist(newdata, 
    modelFit$training, "Minkowski", p = modelFit$Minkowski))))

$protoclass$prob
NULL

$protoclass$tags
[1] "Prototype Models"

$protoclass$sort
function (x) 
x[order(-x$eps), ]


$pythonKnnReg
$pythonKnnReg$label
[1] "Knn regression via sklearn.neighbors.KNeighborsRegressor"

$pythonKnnReg$library
[1] "rPython"

$pythonKnnReg$check
function (pkg) 
{
    testpd <- try(rPython::python.exec("import pandas as pd"), 
        silent = TRUE)
    if (class(testpd)[1] == "try-error") 
        stop("Please install the `pandas` python library")
    testsk <- try(rPython::python.exec("from sklearn.neighbors import KNeighborsRegressor"), 
        silent = TRUE)
    if (class(testsk)[1] == "try-error") 
        stop("Please install the `sklearn` python library")
    TRUE
}

$pythonKnnReg$loop
NULL

$pythonKnnReg$type
[1] "Regression"

$pythonKnnReg$parameters
    parameter     class           label
1 n_neighbors   numeric      #Neighbors
2     weights character Weight Function
3   algorithm character       Algorithm
4   leaf_size   numeric       Leaf Size
5      metric character Distance Metric
6           p   numeric               p

$pythonKnnReg$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(n_neighbors = (5:((2 * len) + 4))[(5:((2 * 
            len) + 4))%%2 > 0], weights = c("uniform", "distance"), 
            algorithm = c("auto"), leaf_size = c(30), metric = c("minkowski"), 
            p = 2)
    }
    else {
        out <- data.frame(n_neighbors = sample(1:floor(nrow(x)/3), 
            size = len, replace = TRUE), weights = sample(c("uniform", 
            "distance"), size = len, replace = TRUE), algorithm = c("auto"), 
            leaf_size = c(30), metric = c("minkowski"), p = sample(1:2, 
                size = len, replace = TRUE))
    }
    out
}

$pythonKnnReg$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    mySeed = sample.int(1e+05, 1)
    python.exec("import numpy as np")
    python.assign("mySeed", mySeed)
    python.exec("np.random.seed(mySeed)")
    python.assign("X", x)
    python.exec("X = pd.DataFrame(X)")
    python.assign("Y", y)
    python.exec(paste0("neigh = KNeighborsRegressor(", "n_neighbors=", 
        param$n_neighbors, ",", "weights='", as.character(param$weights), 
        "',", "algorithm='", as.character(param$algorithm), "',", 
        "leaf_size=", param$leaf_size, ",", "metric='", as.character(param$metric), 
        "',", "p=", param$p, ")"))
    python.exec("neigh.fit(X, Y)")
    return(list())
}

$pythonKnnReg$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    python.assign("newdata", newdata)
    python.exec("newdata = pd.DataFrame(newdata)")
    python.exec("pred=neigh.predict(newdata)")
    python.exec("pred = pred.tolist()")
    pred = python.get("pred")
}

$pythonKnnReg$levels
function (x) 
x$obsLevels

$pythonKnnReg$tags
[1] "Prototype Models"

$pythonKnnReg$prob
NULL

$pythonKnnReg$sort
function (x) 
x[order(-x[, 1]), ]


$qda
$qda$label
[1] "Quadratic Discriminant Analysis"

$qda$library
[1] "MASS"

$qda$loop
NULL

$qda$type
[1] "Classification"

$qda$parameters
  parameter     class     label
1 parameter character parameter

$qda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$qda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
qda(x, y, ...)

$qda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$qda$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$posterior

$qda$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else colnames(x$means)

$qda$tags
[1] "Discriminant Analysis" "Polynomial Model"     

$qda$levels
function (x) 
names(x$prior)

$qda$sort
function (x) 
x


$QdaCov
$QdaCov$label
[1] "Robust Quadratic Discriminant Analysis"

$QdaCov$library
[1] "rrcov"

$QdaCov$loop
NULL

$QdaCov$type
[1] "Classification"

$QdaCov$parameters
  parameter     class     label
1 parameter character parameter

$QdaCov$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$QdaCov$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
rrcov:::QdaCov(x, y, ...)

$QdaCov$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)@classification

$QdaCov$prob
function (modelFit, newdata, submodels = NULL) 
{
    probs <- predict(modelFit, newdata)@posterior
    colnames(probs) <- names(modelFit@prior)
    probs
}

$QdaCov$tags
[1] "Discriminant Analysis" "Polynomial Model"     

$QdaCov$levels
function (x) 
names(x@prior)

$QdaCov$sort
function (x) 
x


$qrf
$qrf$label
[1] "Quantile Random Forest"

$qrf$library
[1] "quantregForest"

$qrf$loop
NULL

$qrf$type
[1] "Regression"

$qrf$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$qrf$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$qrf$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
quantregForest(x, y, mtry = param$mtry, ...)

$qrf$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, quantiles = 0.5)
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$qrf$prob
NULL

$qrf$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Quantile Regression"        "Robust Model"              

$qrf$sort
function (x) 
x[order(x[, 1]), ]


$qrnn
$qrnn$label
[1] "Quantile Regression Neural Network"

$qrnn$library
[1] "qrnn"

$qrnn$loop
NULL

$qrnn$type
[1] "Regression"

$qrnn$parameters
  parameter   class          label
1  n.hidden numeric  #Hidden Units
2   penalty numeric   Weight Decay
3       bag logical Bagged Models?

$qrnn$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(n.hidden = ((1:len) * 2) - 1, penalty = c(0, 10^seq(-1, 
    -4, length = len - 1)), bag = FALSE)

$qrnn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    qrnn.fit(as.matrix(x), matrix(y), n.hidden = param$n.hidden, 
        print.level = 0, penalty = param$penalty, bag = param$bag, 
        ...)
}

$qrnn$predict
function (modelFit, newdata, submodels = NULL) 
qrnn.predict(as.matrix(newdata), modelFit)[, 1]

$qrnn$prob
NULL

$qrnn$tags
[1] "Neural Network"      "L2 Regularization"   "Quantile Regression"
[4] "Bagging"             "Ensemble Model"      "Robust Model"       

$qrnn$sort
function (x) 
x[order(x$n.hidden, -x$penalty), ]


$randomGLM
$randomGLM$label
[1] "Ensembles of Generalized Lienar Models"

$randomGLM$library
[1] "randomGLM"

$randomGLM$type
[1] "Regression"     "Classification"

$randomGLM$parameters
            parameter   class             label
1 maxInteractionOrder numeric Interaction Order

$randomGLM$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(maxInteractionOrder = 1:min(len, 3))
    }
    else {
        out <- data.frame(maxInteractionOrder = sample(1:3, length = len))
    }
    out
}

$randomGLM$loop
NULL

$randomGLM$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    mod <- randomGLM(x = x, y, maxInteractionOrder = param$maxInteractionOrder, 
        ...)
    mod
}

$randomGLM$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") 
        out <- modelFit$obsLevel[apply(out, 1, which.max)]
    out
}

$randomGLM$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    predict(modelFit, newdata)
}

$randomGLM$predictors
function (x, s = NULL, ...) 
{
    all_pred <- lapply(x$models, function(x) names(coef(x)))
    all_pred <- unique(unlist(all_pred))
    all_pred <- strsplit(all_pred, ".times.", fixed = TRUE)
    all_pred <- unique(unlist(all_pred))
    all_pred[all_pred != "(Intercept)"]
}

$randomGLM$tags
[1] "Generalized Linear Model" "Linear Classifier"       
[3] "Ensemble Model"           "Bagging"                 

$randomGLM$prob
NULL

$randomGLM$sort
function (x) 
x


$ranger
$ranger$label
[1] "Random Forest"

$ranger$library
[1] "e1071"  "ranger"

$ranger$loop
NULL

$ranger$type
[1] "Classification" "Regression"    

$ranger$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$ranger$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$ranger$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    x$.outcome <- y
    if (!is.null(wts)) {
        out <- ranger(.outcome ~ ., data = x, mtry = param$mtry, 
            write.forest = TRUE, probability = classProbs, case.weights = wts, 
            ...)
    }
    else {
        out <- ranger(.outcome ~ ., data = x, mtry = param$mtry, 
            write.forest = TRUE, probability = classProbs, ...)
    }
    if (!last) 
        out$y <- y
    out
}

$ranger$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)$predictions
    if (!is.null(modelFit$obsLevels) & modelFit$treetype == "Probability estimation") {
        out <- colnames(out)[apply(out, 1, which.max)]
    }
    out
}

$ranger$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)$predictions
}

$ranger$predictors
function (x, ...) 
{
    var_index <- sort(unique(unlist(lapply(x$forest$split.varIDs, 
        function(x) x))))
    var_index <- var_index[var_index > 0]
    x$forest$independent.variable.names[var_index]
}

$ranger$varImp
function (object, ...) 
{
    if (length(object$variable.importance) == 0) 
        stop("No importance values available")
    imps <- importance(object)
    out <- data.frame(Overall = as.vector(imps))
    rownames(out) <- names(imps)
    out
}

$ranger$levels
function (x) 
{
    if (x$treetype == "Probability estimation") {
        out <- colnames(x$predictions)
    }
    else {
        if (x$treetype == "Classification") {
            out <- levels(x$predictions)
        }
        else out <- NULL
    }
    out
}

$ranger$oob
function (x) 
{
    postResample(x$predictions, x$y)
}

$ranger$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Accepts Case Weights"      

$ranger$sort
function (x) 
x[order(x[, 1]), ]


$rbf
$rbf$label
[1] "Radial Basis Function Network"

$rbf$library
[1] "RSNNS"

$rbf$loop
NULL

$rbf$type
[1] "Classification" "Regression"    

$rbf$parameters
  parameter   class         label
1      size numeric #Hidden Units

$rbf$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(size = ((1:len) * 2) - 1)
    }
    else {
        out <- data.frame(size = unique(sample(1:20, size = len, 
            replace = TRUE)))
    }
    out
}

$rbf$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
    if (any(names(theDots) == "learnFunc")) {
        theDots$learnFunc <- NULL
        warning("Cannot over-ride 'learnFunc' argument for this model. RadialBasisLearning is used.")
    }
    if (!any(names(theDots) == "initFuncParams")) {
        theDots$initFuncParams <- c(0, 1, 0, 0.02, 0.04)
        if (is.factor(y)) 
            theDots$initFuncParams[1:2] <- c(-4, 4)
    }
    if (!any(names(theDots) == "learnFuncParams")) {
        theDots$learnFuncParams <- c(1e-08, 0, 1e-08, 0.1, 0.8)
    }
    if (is.factor(y)) {
        y <- RSNNS:::decodeClassLabels(y)
        lin <- FALSE
    }
    else lin <- TRUE
    args <- list(x = x, y = y, size = param$size, linOut = lin)
    args <- c(args, theDots)
    do.call("rbf", args)
}

$rbf$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$rbf$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$rbf$levels
function (x) 
x$obsLevels

$rbf$tags
[1] "Neural Network"        "L2 Regularization"     "Radial Basis Function"

$rbf$sort
function (x) 
x[order(x$size), ]


$rbfDDA
$rbfDDA$label
[1] "Radial Basis Function Network"

$rbfDDA$library
[1] "RSNNS"

$rbfDDA$loop
NULL

$rbfDDA$type
[1] "Regression"     "Classification"

$rbfDDA$parameters
          parameter   class                                    label
1 negativeThreshold numeric Activation Limit for Conflicting Classes

$rbfDDA$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(negativeThreshold = 10^(-(1:len)))
    }
    else {
        out <- data.frame(negativeThreshold = runif(len, min = 0, 
            3))
    }
    out
}

$rbfDDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "learnFunc")) {
        theDots$learnFunc <- NULL
        warning("Cannot over-ride 'learnFunc' argument for this model. RBF-DDA is used.")
    }
    if (any(names(theDots) == "learnFuncParams")) {
        theDots$learnFuncParams[2] <- param$negativeThreshold
    }
    else theDots$learnFuncParams <- c(0.4, param$negativeThreshold, 
        5)
    y <- RSNNS:::decodeClassLabels(y)
    args <- list(x = x, y = y)
    args <- c(args, theDots)
    do.call("rbfDDA", args)
}

$rbfDDA$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    else out <- out[, 1]
    out
}

$rbfDDA$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    colnames(out) <- modelFit$obsLevels
    out
}

$rbfDDA$levels
function (x) 
x$obsLevels

$rbfDDA$tags
[1] "Neural Network"        "L2 Regularization"     "Radial Basis Function"

$rbfDDA$sort
function (x) 
x[order(-x$negativeThreshold), ]


$Rborist
$Rborist$label
[1] "Random Forest"

$Rborist$library
[1] "Rborist"

$Rborist$loop
NULL

$Rborist$type
[1] "Classification" "Regression"    

$Rborist$parameters
  parameter   class                         label
1 predFixed numeric #Randomly Selected Predictors

$Rborist$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(predFixed = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(predFixed = unique(sample(1:ncol(x), 
            size = len, replace = TRUE)))
    }
    out
}

$Rborist$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
Rborist(x, y, predFixed = param$predFixed, ...)

$Rborist$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$yPred
    if (modelFit$problemType == "Classification") 
        out <- modelFit$obsLevels[out]
    out
}

$Rborist$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$census
    out <- t(apply(out, 1, function(x) x/sum(x)))
    out
}

$Rborist$predictors
function (x, ...) 
x$xNames[x$training$info != 0]

$Rborist$varImp
function (object, ...) 
{
    out <- data.frame(Overall = object$training$info)
    rownames(out) <- object$xNames
    out
}

$Rborist$levels
function (x) 
colnames(x$validation$confusion)

$Rborist$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$Rborist$sort
function (x) 
x[order(x[, 1]), ]

$Rborist$oob
function (x) 
{
    out <- switch(x$problemType, Regression = c(sqrt(x$validation$mse), 
        x$validation$rsq), Classification = c(sum(diag(x$validation$confusion))/sum(x$validation$confusion), 
        e1071::classAgreement(x$validation$confusion)[["kappa"]]))
    names(out) <- if (x$problemType == "Regression") 
        c("RMSE", "Rsquared")
    else c("Accuracy", "Kappa")
    out
}


$rda
$rda$label
[1] "Regularized Discriminant Analysis"

$rda$library
[1] "klaR"

$rda$loop
NULL

$rda$type
[1] "Classification"

$rda$parameters
  parameter   class  label
1     gamma numeric  Gamma
2    lambda numeric Lambda

$rda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(gamma = seq(0, 1, length = len), lambda = seq(0, 
            1, length = len))
    }
    else {
        out <- data.frame(gamma = runif(len, min = 0, max = 1), 
            lambda = runif(len, min = 0, max = 1))
    }
    out
}

$rda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    klaR:::rda(x, y, gamma = param$gamma, lambda = param$lambda, 
        ...)
}

$rda$predict
function (modelFit, newdata, submodels = NULL) 
klaR:::predict.rda(modelFit, newdata)$class

$rda$prob
function (modelFit, newdata, submodels = NULL) 
klaR:::predict.rda(modelFit, newdata)$posterior

$rda$predictors
function (x, ...) 
x$varnames

$rda$tags
[1] "Discriminant Analysis" "Polynomial Model"      "Regularization"       
[4] "Linear Classifier"    

$rda$levels
function (x) 
names(x$prior)

$rda$sort
function (x) 
{
    x[order(-x$lambda, x$gamma), ]
}


$relaxo
$relaxo$label
[1] "Relaxed Lasso"

$relaxo$library
[1] "relaxo" "plyr"  

$relaxo$type
[1] "Regression"

$relaxo$parameters
  parameter   class                label
1    lambda numeric    Penalty Parameter
2       phi numeric Relaxation Parameter

$relaxo$grid
function (x, y, len = NULL, search = "grid") 
{
    library(relaxo)
    tmp <- relaxo(as.matrix(x), y)
    lambdas <- log10(tmp$lambda)[-c(1, length(tmp$lambda))]
    if (search == "grid") {
        out <- expand.grid(phi = seq(0.1, 0.9, length = len), 
            lambda = 10^seq(min(lambdas), quantile(lambdas, probs = 0.9), 
                length = len))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = min(lambdas), 
            max = max(lambdas)), phi = runif(len, min = 0, max = 1))
    }
    out
}

$relaxo$loop
function (grid) 
{
    loop <- ddply(grid, .(phi), function(x) c(lambda = max(x$lambda)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = submodels)) {
        submodels[[i]] <- data.frame(lambda = subset(grid, subset = phi == 
            loop$phi[i] & lambda < loop$lambda[i])$lambda)
    }
    list(loop = loop, submodels = submodels)
}

$relaxo$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    relaxo(as.matrix(x), y, phi = param$phi, ...)
}

$relaxo$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, as.matrix(newdata), lambda = min(max(modelFit$lambda), 
        modelFit$tuneValue$lambda), phi = modelFit$tuneValue$phi)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$lambda)) {
            tmp[[j + 1]] <- predict(modelFit, as.matrix(newdata), 
                lambda = min(max(modelFit$lambda), submodels$lambda[j]), 
                phi = modelFit$tuneValue$phi)
        }
        out <- tmp
    }
    out
}

$relaxo$prob
NULL

$relaxo$tags
[1] "Implicit Feature Selection" "L1 Regularization"         
[3] "L2 Regularization"          "Linear Regression"         

$relaxo$sort
function (x) 
x[order(x$phi, -x$lambda), ]


$rf
$rf$label
[1] "Random Forest"

$rf$library
[1] "randomForest"

$rf$loop
NULL

$rf$type
[1] "Classification" "Regression"    

$rf$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$rf$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$rf$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
randomForest(x, y, mtry = param$mtry, ...)

$rf$predict
function (modelFit, newdata, submodels = NULL) 
if (!is.null(newdata)) predict(modelFit, newdata) else predict(modelFit)

$rf$prob
function (modelFit, newdata, submodels = NULL) 
if (!is.null(newdata)) predict(modelFit, newdata, type = "prob") else predict(modelFit, 
    type = "prob")

$rf$predictors
function (x, ...) 
{
    varIndex <- as.numeric(names(table(x$forest$bestvar)))
    varIndex <- varIndex[varIndex > 0]
    varsUsed <- names(x$forest$ncat)[varIndex]
    varsUsed
}

$rf$varImp
function (object, ...) 
{
    varImp <- randomForest::importance(object, ...)
    if (object$type == "regression") 
        varImp <- data.frame(Overall = varImp[, "%IncMSE"])
    else {
        retainNames <- levels(object$y)
        if (all(retainNames %in% colnames(varImp))) {
            varImp <- varImp[, retainNames]
        }
        else {
            varImp <- data.frame(Overall = varImp[, 1])
        }
    }
    out <- as.data.frame(varImp)
    if (dim(out)[2] == 2) {
        tmp <- apply(out, 1, mean)
        out[, 1] <- out[, 2] <- tmp
    }
    out
}

$rf$levels
function (x) 
x$classes

$rf$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$rf$sort
function (x) 
x[order(x[, 1]), ]

$rf$oob
function (x) 
{
    out <- switch(x$type, regression = c(sqrt(max(x$mse[length(x$mse)], 
        0)), x$rsq[length(x$rsq)]), classification = c(1 - x$err.rate[x$ntree, 
        "OOB"], e1071::classAgreement(x$confusion[, -dim(x$confusion)[2]])[["kappa"]]))
    names(out) <- if (x$type == "regression") 
        c("RMSE", "Rsquared")
    else c("Accuracy", "Kappa")
    out
}


$rFerns
$rFerns$label
[1] "Random Ferns"

$rFerns$library
[1] "rFerns"

$rFerns$loop
NULL

$rFerns$type
[1] "Classification"

$rFerns$parameters
  parameter   class      label
1     depth numeric Fern Depth

$rFerns$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(depth = unique(floor(seq(1, 16, length = len))))
    }
    else {
        out <- data.frame(depth = unique(sample(1:16, size = len, 
            replace = TRUE)))
    }
    out
}

$rFerns$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.data.frame(x)) 
        newdata <- as.data.frame(x)
    rFerns(x, y, depth = param$depth, ...)
}

$rFerns$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$rFerns$levels
function (x) 
x$obsLevels

$rFerns$prob
NULL

$rFerns$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$rFerns$sort
function (x) 
x[order(x[, 1]), ]


$RFlda
$RFlda$label
[1] "Factor-Based Linear Discriminant Analysis"

$RFlda$library
[1] "HiDimDA"

$RFlda$loop
NULL

$RFlda$type
[1] "Classification"

$RFlda$parameters
  parameter   class     label
1         q numeric # Factors

$RFlda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(q = 1:len)
    }
    else {
        out <- data.frame(q = unique(sample(1:10, size = len, 
            replace = TRUE)))
    }
    out
}

$RFlda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
RFlda(x, y, q = param$q, maxq = param$q, ...)

$RFlda$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$class
    out <- modelFit$obsLevels[as.numeric(out)]
    out
}

$RFlda$levels
function (x) 
x$obsLevels

$RFlda$prob
NULL

$RFlda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$RFlda$sort
function (x) 
x[order(x[, 1]), ]


$rfRules
$rfRules$label
[1] "Random Forest Rule-Based Model"

$rfRules$library
[1] "randomForest" "inTrees"      "plyr"        

$rfRules$type
[1] "Classification" "Regression"    

$rfRules$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors
2  maxdepth numeric            Maximum Rule Depth

$rfRules$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), maxdepth = (1:len) + 
            1)
    }
    else {
        out <- data.frame(mtry = sample(1:ncol(x), size = len, 
            replace = TRUE), maxdepth = sample(1:15, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), ]
}

$rfRules$loop
function (grid) 
{
    loop <- ddply(grid, c("mtry"), function(x) c(maxdepth = max(x$maxdepth)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$maxdepth)) {
        index <- which(grid$mtry == loop$mtry[i])
        trees <- grid[index, "maxdepth"]
        submodels[[i]] <- data.frame(maxdepth = trees[trees != 
            loop$maxdepth[i]])
    }
    list(loop = loop, submodels = submodels)
}

$rfRules$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    RFor <- randomForest(x, y, mtry = param$mtry, ...)
    treeList <- RF2List(RFor)
    exec <- extractRules(treeList, x, maxdepth = param$maxdepth, 
        ntree = RFor$ntree)
    ruleMetric <- getRuleMetric(exec, x, y)
    ruleMetric <- pruneRule(ruleMetric, x, y)
    ruleMetric <- selectRuleRRF(ruleMetric, x, y)
    out <- list(model = buildLearner(ruleMetric, x, y))
    if (!last) {
        out$rf <- treeList
        out$x <- x
        out$y <- y
        out$trees <- RFor$ntree
    }
    out
}

$rfRules$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- applyLearner(modelFit$model, newdata)
    if (modelFit$problemType == "Regression") 
        out <- as.numeric(out)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- if (is.matrix(out)) 
            out[, 1]
        else out
        for (i in seq(along = submodels$maxdepth)) {
            exec <- extractRules(modelFit$rf, modelFit$x, maxdepth = submodels$maxdepth[i], 
                ntree = modelFit$trees)
            ruleMetric <- getRuleMetric(exec, modelFit$x, modelFit$y)
            ruleMetric <- pruneRule(ruleMetric, modelFit$x, modelFit$y)
            ruleMetric <- selectRuleRRF(ruleMetric, modelFit$x, 
                modelFit$y)
            mod <- buildLearner(ruleMetric, modelFit$x, modelFit$y)
            tmp[[i + 1]] <- applyLearner(mod, newdata)
            if (modelFit$problemType == "Regression") 
                tmp[[i + 1]] <- as.numeric(tmp[[i + 1]])
        }
        out <- tmp
    }
    out
}

$rfRules$prob
NULL

$rfRules$predictors
function (x, ...) 
{
    split_up <- strsplit(x$model[, "condition"], "&")
    isolate <- function(x) {
        index <- gregexpr("]", x, fixed = TRUE)
        out <- NULL
        for (i in seq_along(index)) {
            if (all(index[[i]] > 0)) {
                tmp <- substring(x[i], 1, index[[i]][1])
                tmp <- gsub("(X)|(\\[)|(\\])|(,)|( )", "", tmp)
                tmp <- tmp[tmp != ""]
                out <- c(out, as.numeric(tmp))
            }
        }
        as.numeric(unique(out))
    }
    var_index <- unique(unlist(lapply(split_up, isolate)))
    if (length(var_index) > 0) 
        x$xNames[var_index]
    else NULL
}

$rfRules$varImp
function (object, ...) 
{
    split_up <- strsplit(object$model[, "condition"], "&")
    isolate <- function(x) {
        index <- gregexpr("]", x, fixed = TRUE)
        out <- NULL
        for (i in seq_along(index)) {
            if (all(index[[i]] > 0)) {
                tmp <- substring(x[i], 1, index[[i]][1])
                tmp <- gsub("(X)|(\\[)|(\\])|(,)|( )", "", tmp)
                tmp <- tmp[tmp != ""]
                out <- c(out, as.numeric(tmp))
            }
        }
        as.numeric(unique(out))
    }
    var_index <- lapply(split_up, isolate)
    vars_dat <- lapply(var_index, function(x, p) {
        out <- rep(0, p)
        if (length(x) > 0) 
            out[x] <- 1
        out
    }, p = length(object$xNames))
    vars_dat <- do.call("rbind", vars_dat)
    colnames(vars_dat) <- object$xNames
    freqs <- as.numeric(object$model[, "freq"])
    vars_dat <- vars_dat * freqs
    var_imp <- apply(vars_dat, 2, sum)
    out <- data.frame(Overall = as.vector(var_imp))
    rownames(out) <- names(var_imp)
    out
}

$rfRules$levels
function (x) 
x$obsLevels

$rfRules$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Rule-Based Model"          

$rfRules$sort
function (x) 
x[order(x[, "maxdepth"]), ]


$ridge
$ridge$label
[1] "Ridge Regression"

$ridge$library
[1] "elasticnet"

$ridge$type
[1] "Regression"

$ridge$parameters
  parameter   class        label
1    lambda numeric Weight Decay

$ridge$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 
            1)))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1))
    }
    out
}

$ridge$loop
NULL

$ridge$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    enet(as.matrix(x), y, lambda = param$lambda)
}

$ridge$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata, s = 1, mode = "fraction")$fit
}

$ridge$predictors
function (x, s = NULL, ...) 
{
    if (is.null(s)) {
        if (!is.null(x$tuneValue)) {
            s <- x$tuneValue$.fraction
        }
        else stop("must supply a vaue of s")
        out <- predict(x, s = s, type = "coefficients", mode = "fraction")$coefficients
    }
    else {
        out <- predict(x, s = s)$coefficients
    }
    names(out)[out != 0]
}

$ridge$tags
[1] "Linear Regression" "L2 Regularization"

$ridge$prob
NULL

$ridge$sort
function (x) 
x[order(-x$lambda), ]


$rlda
$rlda$label
[1] "Regularized Linear Discriminant Analysis"

$rlda$library
[1] "sparsediscrim"

$rlda$loop
NULL

$rlda$type
[1] "Classification"

$rlda$parameters
  parameter     class                 label
1 estimator character Regularization Method

$rlda$grid
function (x, y, len = NULL, search = "grid") 
{
    data.frame(estimator = c("Moore-Penrose Pseudo-Inverse", 
        "Schafer-Strimmer", "Thomaz-Kitani-Gillies"))
}

$rlda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (as.character(param$estimator == "Moore-Penrose Pseudo-Inverse")) {
        out <- lda_pseudo(x, y, ...)
    }
    else {
        if (as.character(param$estimator == "Schafer-Strimmer")) {
            out <- lda_schafer(x, y, ...)
        }
        else out <- lda_thomaz(x, y, ...)
    }
    out
}

$rlda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$rlda$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$scores
    as.data.frame(t(apply(out, 2, function(x) exp(x)/sum(exp(x)))))
}

$rlda$predictors
function (x, ...) 
x$varnames

$rlda$tags
[1] "Discriminant Analysis" "Polynomial Model"      "Regularization"       
[4] "Linear Classifier"    

$rlda$levels
function (x) 
names(x$prior)

$rlda$sort
function (x) 
x


$rlm
$rlm$label
[1] "Robust Linear Model"

$rlm$library
[1] "MASS"

$rlm$loop
NULL

$rlm$type
[1] "Regression"

$rlm$parameters
  parameter     class     label
1 intercept   logical intercept
2       psi character       psi

$rlm$grid
function (x, y, len = NULL, search = "grid") 
expand.grid(intercept = c(TRUE, FALSE), psi = c("psi.huber", 
    "psi.hampel", "psi.bisquare"))

$rlm$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    psi <- psi.huber
    if (param$psi == "psi.bisquare") 
        psi <- psi.bisquare
    else if (param$psi == "psi.hampel") 
        psi <- psi.hampel
    if (!is.null(wts)) {
        if (param$intercept) 
            out <- rlm(.outcome ~ ., data = dat, weights = wts, 
                psi = psi, ...)
        else out <- rlm(.outcome ~ 0 + ., data = dat, weights = wts, 
            psi = psi, ...)
    }
    else {
        if (param$intercept) 
            out <- rlm(.outcome ~ ., data = dat, psi = psi, ...)
        else out <- rlm(.outcome ~ 0 + ., data = dat, psi = psi, 
            ...)
    }
    out
}

$rlm$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$rlm$prob
NULL

$rlm$tags
[1] "Linear Regression"    "Robust Model"         "Accepts Case Weights"

$rlm$sort
function (x) 
x


$rmda
$rmda$label
[1] "Robust Mixture Discriminant Analysis"

$rmda$library
[1] "robustDA"

$rmda$loop
NULL

$rmda$type
[1] "Classification"

$rmda$parameters
  parameter     class                 label
1         K   numeric #Subclasses Per Class
2     model character                 Model

$rmda$grid
function (x, y, len = NULL, search = "grid") 
{
    mods <- c("EII", "VII", "EEI", "EVI", "VEI", "VVI")
    if (search == "grid") {
        out <- expand.grid(K = (1:len) + 1, model = c("VEV"))
    }
    else {
        out <- data.frame(K = sample(2:10, size = len, replace = TRUE), 
            model = sample(mods, size = len, replace = TRUE))
    }
    out
}

$rmda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    mod <- rmda(x, as.numeric(y), K = param$K, model = as.character(param$model), 
        ...)
    mod$levels <- levels(y)
    mod
}

$rmda$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$cls
    factor(modelFit$levels[out], levels = modelFit$levels)
}

$rmda$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$P
    colnames(out) <- modelFit$obsLevels
    out
}

$rmda$varImp
NULL

$rmda$predictors
function (x, ...) 
colnames(x$prms$data)

$rmda$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$rmda$tags
[1] "Discriminant Analysis" "Mixture Model"         "Robust Methods"       

$rmda$sort
function (x) 
x


$rocc
$rocc$label
[1] "ROC-Based Classifier"

$rocc$library
[1] "rocc"

$rocc$loop
NULL

$rocc$type
[1] "Classification"

$rocc$parameters
  parameter   class               label
1    xgenes numeric #Variables Retained

$rocc$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(xgenes = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(xgenes = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$rocc$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    newY <- factor(ifelse(y == levels(y)[1], 1, 0), levels = c("0", 
        "1"))
    tr.rocc(g = t(as.matrix(x)), out = newY, xgenes = param$xgenes)
}

$rocc$predict
function (modelFit, newdata, submodels = NULL) 
{
    tmp <- p.rocc(modelFit, t(as.matrix(newdata)))
    factor(ifelse(tmp == "1", modelFit$obsLevels[1], modelFit$obsLevels[2]), 
        levels = modelFit$obsLevels)
}

$rocc$levels
function (x) 
x$obsLevels

$rocc$prob
NULL

$rocc$predictors
function (x, ...) 
x$genes

$rocc$tags
[1] "ROC Curves"

$rocc$sort
function (x) 
x[order(x$xgenes), ]


$rotationForest
$rotationForest$label
[1] "Rotation Forest"

$rotationForest$library
[1] "rotationForest"

$rotationForest$type
[1] "Classification"

$rotationForest$parameters
  parameter   class             label
1         K numeric #Variable Subsets
2         L numeric     Ensemble Size

$rotationForest$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(K = 1:min(len, ncol(x) - 1), L = (1:len) * 
            3)
    }
    else {
        out <- data.frame(K = sample(1:min(len, ncol(x) - 1), 
            size = len, replace = TRUE), L = sample(1:100, size = len, 
            replace = TRUE))
    }
    out
}

$rotationForest$loop
function (grid) 
{
    grid <- grid[order(grid$K, -grid$L, decreasing = TRUE), , 
        drop = FALSE]
    unique_k <- unique(grid$K)
    loop <- data.frame(K = unique_k, L = NA)
    submodels <- vector(mode = "list", length = length(unique_k))
    for (i in seq(along = unique_k)) {
        sub_L <- grid[grid$K == unique_k[i], "L"]
        loop$L[loop$K == unique_k[i]] <- sub_L[which.max(sub_L)]
        submodels[[i]] <- data.frame(L = sub_L[-which.max(sub_L)])
    }
    list(loop = loop, submodels = submodels)
}

$rotationForest$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (length(lev) != 2) 
        stop("rotationForest is only implemented for binary classification")
    y <- ifelse(y == lev[1], 1, 0)
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    rotationForest(x, y, K = param$K, L = param$L, ...)
}

$rotationForest$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)
    out <- ifelse(out >= 0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        all_L <- predict(modelFit, newdata, all = TRUE)
        for (j in seq(along = submodels$L)) {
            tmp_pred <- apply(all_L[, 1:submodels$L[j], drop = FALSE], 
                1, mean)
            tmp[[j + 1]] <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
                modelFit$obsLevels[2])
        }
        out <- tmp
    }
    out
}

$rotationForest$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    all_L <- predict(modelFit, newdata, all = TRUE)
    out <- apply(all_L, 1, mean)
    out <- data.frame(x = out, y = 1 - out)
    colnames(out) <- modelFit$obsLevels
    if (!is.null(rownames(newdata))) 
        rownames(out) <- rownames(newdata)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$L)) {
            tmp_pred <- apply(all_L[, 1:submodels$L[j], drop = FALSE], 
                1, mean)
            tmp_pred <- data.frame(x = tmp_pred, y = 1 - tmp_pred)
            colnames(tmp_pred) <- modelFit$obsLevels
            if (!is.null(rownames(newdata))) 
                rownames(tmp_pred) <- rownames(newdata)
            tmp[[j + 1]] <- tmp_pred
        }
        out <- tmp
    }
    out
}

$rotationForest$predictors
function (x, ...) 
{
    non_zero <- function(x) {
        out <- apply(x, 1, function(x) any(x != 0))
        names(out)[out]
    }
    sort(unique(unlist(lapply(x$loadings, non_zero))))
}

$rotationForest$varImp
function (object, ...) 
{
    vis <- lapply(object$models, varImp, scale = FALSE)
    wgt <- vector(mode = "list", length = length(vis))
    for (i in seq(along = vis)) {
        tmp <- vis[[i]]
        vi1 <- tmp[, 1]
        names(vi1) <- rownames(tmp)
        l1 <- object$loadings[[i]]
        tmp2 <- vi1 %*% abs(as.matrix(l1[names(vi1), ]))
        tmp2 <- tmp2[, sort(colnames(tmp2))]
        wgt[[i]] <- tmp2
    }
    wgt <- do.call("rbind", wgt)
    vi <- apply(wgt, 2, mean)
    out <- data.frame(Overall = vi)
    rownames(out) <- colnames(wgt)
    out
}

$rotationForest$levels
function (x) 
x$obsLevels

$rotationForest$tags
[1] "Ensemble Model"             "Implicit Feature Selection"
[3] "Feature Extraction Models"  "Tree-Based Model"          
[5] "Two Class Only"            

$rotationForest$sort
function (x) 
x[order(x[, 1]), ]


$rotationForestCp
$rotationForestCp$label
[1] "Rotation Forest"

$rotationForestCp$library
[1] "rpart"          "plyr"           "rotationForest"

$rotationForestCp$type
[1] "Classification"

$rotationForestCp$parameters
  parameter   class                label
1         K numeric    #Variable Subsets
2         L numeric        Ensemble Size
3        cp numeric Complexity Parameter

$rotationForestCp$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(K = 1:min(len, ncol(x) - 1), L = (1:len) * 
            3, cp = unique(seq(0, 0.1, length = len)))
    }
    else {
        out <- data.frame(K = sample(1:min(len, ncol(x) - 1), 
            size = len, replace = TRUE), L = sample(10:100, size = len, 
            replace = TRUE), cp = runif(len, 0, 0.1))
    }
    out
}

$rotationForestCp$loop
function (grid) 
{
    loop <- ddply(grid, .(cp, K), function(x) c(L = max(x$L)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$L)) {
        index <- which(grid$cp == loop$cp[i] & grid$K == loop$K[i])
        bases <- grid[index, "L"]
        submodels[[i]] <- data.frame(L = bases[bases != loop$L[i]])
    }
    list(loop = loop, submodels = submodels)
}

$rotationForestCp$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (length(lev) != 2) 
        stop("rotationForest is only implemented for binary classification")
    y <- ifelse(y == lev[1], 1, 0)
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$cp <- param$cp
        theDots$control$xval <- 0
        rpctl <- theDots$control
    }
    else rpctl <- rpart.control(cp = param$cp, xval = 0)
    rotationForest(x, y, K = param$K, L = param$L, control = rpctl)
}

$rotationForestCp$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata)
    out <- ifelse(out >= 0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        all_L <- predict(modelFit, newdata, all = TRUE)
        for (j in seq(along = submodels$L)) {
            tmp_pred <- apply(all_L[, 1:submodels$L[j], drop = FALSE], 
                1, mean)
            tmp[[j + 1]] <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
                modelFit$obsLevels[2])
        }
        out <- tmp
    }
    out
}

$rotationForestCp$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    all_L <- predict(modelFit, newdata, all = TRUE)
    out <- apply(all_L, 1, mean)
    out <- data.frame(x = out, y = 1 - out)
    colnames(out) <- modelFit$obsLevels
    if (!is.null(rownames(newdata))) 
        rownames(out) <- rownames(newdata)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$L)) {
            tmp_pred <- apply(all_L[, 1:submodels$L[j], drop = FALSE], 
                1, mean)
            tmp_pred <- data.frame(x = tmp_pred, y = 1 - tmp_pred)
            colnames(tmp_pred) <- modelFit$obsLevels
            if (!is.null(rownames(newdata))) 
                rownames(tmp_pred) <- rownames(newdata)
            tmp[[j + 1]] <- tmp_pred
        }
        out <- tmp
    }
    out
}

$rotationForestCp$predictors
function (x, ...) 
{
    non_zero <- function(x) {
        out <- apply(x, 1, function(x) any(x != 0))
        names(out)[out]
    }
    sort(unique(unlist(lapply(x$loadings, non_zero))))
}

$rotationForestCp$varImp
function (object, ...) 
{
    vis <- lapply(object$models, varImp, scale = FALSE)
    wgt <- vector(mode = "list", length = length(vis))
    for (i in seq(along = vis)) {
        tmp <- vis[[i]]
        vi1 <- tmp[, 1]
        names(vi1) <- rownames(tmp)
        l1 <- object$loadings[[i]]
        tmp2 <- vi1 %*% abs(as.matrix(l1[names(vi1), ]))
        tmp2 <- tmp2[, sort(colnames(tmp2))]
        wgt[[i]] <- tmp2
    }
    wgt <- do.call("rbind", wgt)
    vi <- apply(wgt, 2, mean)
    out <- data.frame(Overall = vi)
    rownames(out) <- colnames(wgt)
    out
}

$rotationForestCp$levels
function (x) 
x$obsLevels

$rotationForestCp$tags
[1] "Ensemble Model"             "Implicit Feature Selection"
[3] "Feature Extraction Models"  "Tree-Based Model"          
[5] "Two Class Only"            

$rotationForestCp$sort
function (x) 
x[order(x[, 1]), ]


$rpart
$rpart$label
[1] "CART"

$rpart$library
[1] "rpart"

$rpart$type
[1] "Regression"     "Classification"

$rpart$parameters
  parameter   class                label
1        cp numeric Complexity Parameter

$rpart$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    initialFit <- rpart(.outcome ~ ., data = dat, control = rpart.control(cp = 0))$cptable
    initialFit <- initialFit[order(-initialFit[, "CP"]), , drop = FALSE]
    if (search == "grid") {
        if (nrow(initialFit) < len) {
            tuneSeq <- data.frame(cp = seq(min(initialFit[, "CP"]), 
                max(initialFit[, "CP"]), length = len))
        }
        else tuneSeq <- data.frame(cp = initialFit[1:len, "CP"])
        colnames(tuneSeq) <- "cp"
    }
    else {
        tuneSeq <- data.frame(cp = unique(sample(initialFit[, 
            "CP"], size = len, replace = TRUE)))
    }
    tuneSeq
}

$rpart$loop
function (grid) 
{
    grid <- grid[order(grid$cp, decreasing = FALSE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$rpart$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    cpValue <- if (!last) 
        param$cp
    else 0
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$cp <- cpValue
        theDots$control$xval <- 0
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(cp = cpValue, xval = 0)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = if (is.data.frame(x)) x else as.data.frame(x), 
        control = ctl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("rpart", modelArgs)
    if (last) 
        out <- prune.rpart(out, cp = param$cp)
    out
}

$rpart$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    pType <- if (modelFit$problemType == "Classification") 
        "class"
    else "vector"
    out <- predict(modelFit, newdata, type = pType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$cp)) {
            prunedFit <- prune.rpart(modelFit, cp = submodels$cp[j])
            tmp[[j + 1]] <- predict(prunedFit, newdata, type = pType)
        }
        out <- tmp
    }
    out
}

$rpart$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "prob")
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$cp)) {
            prunedFit <- prune.rpart(modelFit, cp = submodels$cp[j])
            tmpProb <- predict(prunedFit, newdata, type = "prob")
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$rpart$predictors
function (x, surrogate = TRUE, ...) 
{
    out <- as.character(x$frame$var)
    out <- out[!(out %in% c("<leaf>"))]
    if (surrogate) {
        splits <- x$splits
        splits <- splits[splits[, "adj"] > 0, ]
        out <- c(out, rownames(splits))
    }
    unique(out)
}

$rpart$varImp
function (object, surrogates = FALSE, competes = TRUE, ...) 
{
    tmp <- rownames(object$splits)
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""
    frame <- as.data.frame(object$frame)
    index <- 0
    for (i in 1:nrow(frame)) {
        if (frame$var[i] != "<leaf>") {
            index <- index + 1
            splits$type[index] <- "primary"
            if (frame$ncompete[i] > 0) {
                for (j in 1:frame$ncompete[i]) {
                  index <- index + 1
                  splits$type[index] <- "competing"
                }
            }
            if (frame$nsurrogate[i] > 0) {
                for (j in 1:frame$nsurrogate[i]) {
                  index <- index + 1
                  splits$type[index] <- "surrogate"
                }
            }
        }
    }
    splits$var <- factor(as.character(splits$var))
    if (!surrogates) 
        splits <- subset(splits, type != "surrogate")
    if (!competes) 
        splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve, list(Variable = splits$var), 
        sum, na.rm = TRUE)
    allVars <- colnames(attributes(object$terms)$factors)
    if (!all(allVars %in% out$Variable)) {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missingVars)), 
            Variable = missingVars)
        out <- rbind(out, zeros)
    }
    out2 <- data.frame(Overall = out$x)
    rownames(out2) <- out$Variable
    out2
}

$rpart$levels
function (x) 
x$obsLevels

$rpart$trim
function (x) 
{
    x$call <- list(na.action = (x$call)$na.action)
    x$x <- NULL
    x$y <- NULL
    x$where <- NULL
    x
}

$rpart$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$rpart$sort
function (x) 
x[order(x[, 1], decreasing = TRUE), ]


$rpart1SE
$rpart1SE$label
[1] "CART"

$rpart1SE$library
[1] "rpart"

$rpart1SE$type
[1] "Regression"     "Classification"

$rpart1SE$parameters
  parameter     class     label
1 parameter character parameter

$rpart1SE$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$rpart1SE$loop
NULL

$rpart1SE$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        out <- rpart(.outcome ~ ., data = dat, ...)
    }
    else {
        out <- rpart(.outcome ~ ., data = dat, weights = wts, 
            ...)
    }
    out
}

$rpart1SE$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- if (modelFit$problemType == "Classification") 
        predict(modelFit, newdata, type = "class")
    else predict(modelFit, newdata)
    out
}

$rpart1SE$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
}

$rpart1SE$predictors
function (x, surrogate = TRUE, ...) 
{
    out <- as.character(x$frame$var)
    out <- out[!(out %in% c("<leaf>"))]
    if (surrogate) {
        splits <- x$splits
        splits <- splits[splits[, "adj"] > 0, ]
        out <- c(out, rownames(splits))
    }
    unique(out)
}

$rpart1SE$varImp
function (object, surrogates = FALSE, competes = TRUE, ...) 
{
    tmp <- rownames(object$splits)
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""
    frame <- as.data.frame(object$frame)
    index <- 0
    for (i in 1:nrow(frame)) {
        if (frame$var[i] != "<leaf>") {
            index <- index + 1
            splits$type[index] <- "primary"
            if (frame$ncompete[i] > 0) {
                for (j in 1:frame$ncompete[i]) {
                  index <- index + 1
                  splits$type[index] <- "competing"
                }
            }
            if (frame$nsurrogate[i] > 0) {
                for (j in 1:frame$nsurrogate[i]) {
                  index <- index + 1
                  splits$type[index] <- "surrogate"
                }
            }
        }
    }
    splits$var <- factor(as.character(splits$var))
    if (!surrogates) 
        splits <- subset(splits, type != "surrogate")
    if (!competes) 
        splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve, list(Variable = splits$var), 
        sum, na.rm = TRUE)
    allVars <- colnames(attributes(object$terms)$factors)
    if (!all(allVars %in% out$Variable)) {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missingVars)), 
            Variable = missingVars)
        out <- rbind(out, zeros)
    }
    out2 <- data.frame(Overall = out$x)
    rownames(out2) <- out$Variable
    out2
}

$rpart1SE$levels
function (x) 
x$obsLevels

$rpart1SE$trim
function (x) 
{
    x$call <- list(na.action = (x$call)$na.action)
    x$x <- NULL
    x$y <- NULL
    x$where <- NULL
    x
}

$rpart1SE$notes
[1] "This CART model replicates the same process used by the `rpart` function where the model complexity is determined using the one-standard error method. This procedure is replicated inside of the resampling done by `train` so that an external resampling estimate can be obtained."

$rpart1SE$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$rpart1SE$sort
function (x) 
x[order(x[, 1], decreasing = TRUE), ]


$rpart2
$rpart2$label
[1] "CART"

$rpart2$library
[1] "rpart"

$rpart2$type
[1] "Regression"     "Classification"

$rpart2$parameters
  parameter   class          label
1  maxdepth numeric Max Tree Depth

$rpart2$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    initialFit <- rpart(.outcome ~ ., data = dat, control = rpart.control(cp = 0))$cptable
    initialFit <- initialFit[order(-initialFit[, "CP"]), "nsplit", 
        drop = FALSE]
    initialFit <- initialFit[initialFit[, "nsplit"] > 0 & initialFit[, 
        "nsplit"] <= 30, , drop = FALSE]
    if (search == "grid") {
        if (dim(initialFit)[1] < len) {
            cat("note: only", nrow(initialFit), "possible values of the max tree depth from the initial fit.\n", 
                "Truncating the grid to", nrow(initialFit), ".\n\n")
            tuneSeq <- as.data.frame(initialFit)
        }
        else tuneSeq <- as.data.frame(initialFit[1:len, ])
        colnames(tuneSeq) <- "maxdepth"
    }
    else {
        tuneSeq <- data.frame(maxdepth = unique(sample(as.vector(initialFit[, 
            1]), size = len, replace = TRUE)))
    }
    tuneSeq
}

$rpart2$loop
function (grid) 
{
    grid <- grid[order(grid$maxdepth, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$rpart2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$maxdepth <- param$maxdepth
        theDots$control$xval <- 0
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(maxdepth = param$maxdepth, xval = 0)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = if (is.data.frame(x)) x else as.data.frame(x), 
        control = ctl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("rpart", modelArgs)
    out
}

$rpart2$predict
function (modelFit, newdata, submodels = NULL) 
{
    depth2cp <- function(x, depth) {
        out <- approx(x[, "nsplit"], x[, "CP"], depth)$y
        out[depth > max(x[, "nsplit"])] <- min(x[, "CP"]) * 0.99
        out
    }
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    pType <- if (modelFit$problemType == "Classification") 
        "class"
    else "vector"
    out <- predict(modelFit, newdata, type = pType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        cpValues <- depth2cp(modelFit$cptable, submodels$maxdepth)
        for (j in seq(along = cpValues)) {
            prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
            tmp[[j + 1]] <- predict(prunedFit, newdata, type = pType)
        }
        out <- tmp
    }
    out
}

$rpart2$prob
function (modelFit, newdata, submodels = NULL) 
{
    depth2cp <- function(x, depth) {
        out <- approx(x[, "nsplit"], x[, "CP"], depth)$y
        out[depth > max(x[, "nsplit"])] <- min(x[, "CP"]) * 0.99
        out
    }
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata, type = "prob")
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        cpValues <- depth2cp(modelFit$cptable, submodels$maxdepth)
        for (j in seq(along = cpValues)) {
            prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
            tmpProb <- predict(prunedFit, newdata, type = "prob")
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$rpart2$predictors
function (x, surrogate = TRUE, ...) 
{
    out <- as.character(x$frame$var)
    out <- out[!(out %in% c("<leaf>"))]
    if (surrogate) {
        splits <- x$splits
        splits <- splits[splits[, "adj"] > 0, ]
        out <- c(out, rownames(splits))
    }
    unique(out)
}

$rpart2$varImp
function (object, surrogates = FALSE, competes = TRUE, ...) 
{
    tmp <- rownames(object$splits)
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""
    frame <- as.data.frame(object$frame)
    index <- 0
    for (i in 1:nrow(frame)) {
        if (frame$var[i] != "<leaf>") {
            index <- index + 1
            splits$type[index] <- "primary"
            if (frame$ncompete[i] > 0) {
                for (j in 1:frame$ncompete[i]) {
                  index <- index + 1
                  splits$type[index] <- "competing"
                }
            }
            if (frame$nsurrogate[i] > 0) {
                for (j in 1:frame$nsurrogate[i]) {
                  index <- index + 1
                  splits$type[index] <- "surrogate"
                }
            }
        }
    }
    splits$var <- factor(as.character(splits$var))
    if (!surrogates) 
        splits <- subset(splits, type != "surrogate")
    if (!competes) 
        splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve, list(Variable = splits$var), 
        sum, na.rm = TRUE)
    allVars <- colnames(attributes(object$terms)$factors)
    if (!all(allVars %in% out$Variable)) {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missingVars)), 
            Variable = missingVars)
        out <- rbind(out, zeros)
    }
    out2 <- data.frame(Overall = out$x)
    rownames(out2) <- out$Variable
    out2
}

$rpart2$levels
function (x) 
x$obsLevels

$rpart2$trim
function (x) 
{
    x$call <- list(na.action = (x$call)$na.action)
    x$x <- NULL
    x$y <- NULL
    x$where <- NULL
    x
}

$rpart2$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$rpart2$sort
function (x) 
x[order(x[, 1]), ]


$rpartCost
$rpartCost$label
[1] "Cost-Sensitive CART"

$rpartCost$library
[1] "rpart"

$rpartCost$type
[1] "Classification"

$rpartCost$parameters
  parameter   class                label
1        cp numeric Complexity Parameter
2      Cost numeric                 Cost

$rpartCost$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    initialFit <- rpart(.outcome ~ ., data = dat, control = rpart.control(cp = 0))$cptable
    initialFit <- initialFit[order(-initialFit[, "CP"]), , drop = FALSE]
    if (search == "grid") {
        if (nrow(initialFit) < len) {
            tuneSeq <- expand.grid(cp = seq(min(initialFit[, 
                "CP"]), max(initialFit[, "CP"]), length = len), 
                Cost = 1:len)
        }
        else tuneSeq <- data.frame(cp = initialFit[1:len, "CP"], 
            Cost = 1:len)
        colnames(tuneSeq) <- c("cp", "Cost")
    }
    else {
        tuneSeq <- data.frame(cp = unique(sample(initialFit[, 
            "CP"], size = len, replace = TRUE)), Cost = runif(len, 
            min = 1, max = 30))
    }
    tuneSeq
}

$rpartCost$loop
function (grid) 
{
    grid <- grid[order(grid$Cost, grid$cp, decreasing = TRUE), 
        , drop = FALSE]
    uniqueCost <- unique(grid$Cost)
    loop <- data.frame(Cost = uniqueCost)
    loop$cp <- NA
    submodels <- vector(mode = "list", length = length(uniqueCost))
    for (i in seq(along = uniqueCost)) {
        subCP <- grid[grid$Cost == uniqueCost[i], "cp"]
        loop$cp[loop$Cost == uniqueCost[i]] <- subCP[which.min(subCP)]
        submodels[[i]] <- data.frame(cp = subCP[-which.max(subCP)])
    }
    list(loop = loop, submodels = submodels)
}

$rpartCost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$cp <- param$cp
        theDots$control$xval <- 0
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(cp = param$cp, xval = 0)
    lmat <- matrix(c(0, 1, param$Cost, 0), ncol = 2)
    rownames(lmat) <- colnames(lmat) <- levels(y)
    if (any(names(theDots) == "parms")) {
        theDots$parms$loss <- lmat
    }
    else parms <- list(loss = lmat)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = if (is.data.frame(x)) x else as.data.frame(x), 
        parms = parms, control = ctl), theDots)
    modelArgs$data$.outcome <- y
    out <- do.call("rpart", modelArgs)
    out
}

$rpartCost$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    pType <- if (modelFit$problemType == "Classification") 
        "class"
    else "vector"
    out <- predict(modelFit, newdata, type = pType)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$cp)) {
            prunedFit <- prune.rpart(modelFit, cp = submodels$cp[j])
            tmp[[j + 1]] <- predict(prunedFit, newdata, type = pType)
        }
        out <- tmp
    }
    out
}

$rpartCost$levels
function (x) 
x$obsLevels

$rpartCost$prob
NULL

$rpartCost$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Cost Sensitive Learning"       "Two Class Only"               
[5] "Handle Missing Predictor Data" "Accepts Case Weights"         

$rpartCost$sort
function (x) 
x[order(-x$cp, -x$Cost), ]


$rpartScore
$rpartScore$label
[1] "CART or Ordinal Responses"

$rpartScore$library
[1] "rpartScore" "plyr"      

$rpartScore$type
[1] "Classification"

$rpartScore$parameters
  parameter     class                label
1        cp   numeric Complexity Parameter
2     split character       Split Function
3     prune character      Pruning Measure

$rpartScore$grid
function (x, y, len = NULL, search = "grid") 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    initialFit <- rpart(.outcome ~ ., data = dat, control = rpart.control(cp = 0))$cptable
    initialFit <- initialFit[order(-initialFit[, "CP"]), , drop = FALSE]
    if (search == "grid") {
        if (nrow(initialFit) < len) {
            tuneSeq <- expand.grid(cp = seq(min(initialFit[, 
                "CP"]), max(initialFit[, "CP"]), length = len), 
                split = c("abs", "quad"), prune = c("mr", "mc"))
        }
        else tuneSeq <- expand.grid(cp = initialFit[1:len, "CP"], 
            split = c("abs", "quad"), prune = c("mr", "mc"))
        colnames(tuneSeq)[1] <- "cp"
    }
    else {
        tuneSeq <- expand.grid(cp = unique(sample(initialFit[, 
            "CP"], size = len, replace = TRUE)), split = c("abs", 
            "quad"), prune = c("mr", "mc"))
    }
    tuneSeq
}

$rpartScore$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    cpValue <- if (!last) 
        param$cp
    else 0
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$cp <- cpValue
        theDots$control$xval <- 0
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- rpart.control(cp = cpValue, xval = 0)
    if (!is.null(wts)) 
        theDots$weights <- wts
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
        data = if (is.data.frame(x)) x else as.data.frame(x), 
        split = as.character(param$split), prune = as.character(param$prune), 
        control = ctl), theDots)
    modelArgs$data$.outcome <- as.numeric(y)
    out <- do.call("rpartScore", modelArgs)
    if (last) 
        out <- prune.rpart(out, cp = param$cp)
    out
}

$rpartScore$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- modelFit$obsLevels[predict(modelFit, newdata)]
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$cp)) {
            prunedFit <- prune.rpart(modelFit, cp = submodels$cp[j])
            tmp[[j + 1]] <- modelFit$obsLevels[predict(prunedFit, 
                newdata)]
        }
        out <- tmp
    }
    out
}

$rpartScore$prob
NULL

$rpartScore$predictors
function (x, surrogate = TRUE, ...) 
{
    out <- as.character(x$frame$var)
    out <- out[!(out %in% c("<leaf>"))]
    if (surrogate) {
        splits <- x$splits
        splits <- splits[splits[, "adj"] > 0, ]
        out <- c(out, rownames(splits))
    }
    unique(out)
}

$rpartScore$varImp
function (object, surrogates = FALSE, competes = TRUE, ...) 
{
    allVars <- all.vars(object$terms)
    allVars <- allVars[allVars != ".outcome"]
    out <- data.frame(Overall = object$variable.importance, Variable = names(object$variable.importance))
    rownames(out) <- names(object$variable.importance)
    if (!all(allVars %in% out$Variable)) {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(Overall = rep(0, length(missingVars)), 
            Variable = missingVars)
        out <- rbind(out, zeros)
    }
    rownames(out) <- out$Variable
    out$Variable <- NULL
    out
}

$rpartScore$levels
function (x) 
x$obsLevels

$rpartScore$trim
function (x) 
{
    x$call <- list(na.action = (x$call)$na.action)
    x$x <- NULL
    x$y <- NULL
    x$where <- NULL
    x
}

$rpartScore$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         
[5] "Ordinal Outcomes"             

$rpartScore$sort
function (x) 
x[order(x[, 1], decreasing = TRUE), ]


$rqlasso
$rqlasso$label
[1] "Quantile Regression with LASSO penalty"

$rqlasso$library
[1] "rqPen"

$rqlasso$type
[1] "Regression"

$rqlasso$parameters
  parameter   class      label
1    lambda numeric L1 Penalty

$rqlasso$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(10^seq(-1, -4, length = len)))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1))
    }
    out
}

$rqlasso$loop
NULL

$rqlasso$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    rq.lasso.fit(as.matrix(x), y, lambda = param$lambda, ...)
}

$rqlasso$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newx = as.matrix(newdata))[, 1]
}

$rqlasso$predictors
function (x, ...) 
{
    out <- coef(x)
    out <- out[names(out) != "intercept"]
    names(out)[out != 0]
}

$rqlasso$tags
[1] "Linear Regression"          "Quantile Regression"       
[3] "Implicit Feature Selection" "L1 Regularization"         

$rqlasso$prob
NULL

$rqlasso$sort
function (x) 
x[order(-x$lambda), ]


$rqnc
$rqnc$label
[1] "Non-Convex Penalized Quantile Regression"

$rqnc$library
[1] "rqPen"

$rqnc$type
[1] "Regression"

$rqnc$parameters
  parameter     class        label
1    lambda   numeric   L1 Penalty
2   penalty character Penalty Type

$rqnc$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(10^seq(-1, -4, length = len)), 
            penalty = c("MCP", "SCAD"))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            penalty = sample(c("MCP", "SCAD"), size = len, replace = TRUE))
    }
    out
}

$rqnc$loop
NULL

$rqnc$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    rq.nc.fit(as.matrix(x), y, lambda = param$lambda, penalty = as.character(param$penalty), 
        ...)
}

$rqnc$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newx = as.matrix(newdata))[, 1]
}

$rqnc$predictors
function (x, ...) 
{
    out <- coef(x)
    out <- out[names(out) != "intercept"]
    names(out)[out != 0]
}

$rqnc$tags
[1] "Linear Regression"          "Quantile Regression"       
[3] "Implicit Feature Selection" "L1 Regularization"         

$rqnc$prob
NULL

$rqnc$sort
function (x) 
x[order(-x$lambda), ]


$RRF
$RRF$label
[1] "Regularized Random Forest"

$RRF$library
[1] "randomForest" "RRF"         

$RRF$loop
NULL

$RRF$type
[1] "Regression"     "Classification"

$RRF$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors
2   coefReg numeric          Regularization Value
3   coefImp numeric        Importance Coefficient

$RRF$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), coefReg = seq(0.01, 
            1, length = len), coefImp = seq(0, 1, length = len))
    }
    else {
        out <- data.frame(mtry = sample(1:ncol(x), size = len, 
            replace = TRUE), coefReg = runif(len, min = 0, max = 1), 
            coefImp = runif(len, min = 0, max = 1))
    }
    out
}

$RRF$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    theDots$importance <- TRUE
    args <- list(x = x, y = y, mtry = param$mtry)
    args <- c(args, theDots)
    firstFit <- do.call("randomForest", args)
    firstImp <- randomForest:::importance(firstFit)
    if (is.factor(y)) {
        firstImp <- firstImp[, "MeanDecreaseGini"]/max(firstImp[, 
            "MeanDecreaseGini"])
    }
    else firstImp <- firstImp[, "%IncMSE"]/max(firstImp[, "%IncMSE"])
    firstImp <- ((1 - param$coefImp) * param$coefReg) + (param$coefImp * 
        firstImp)
    RRF(x, y, mtry = param$mtry, coefReg = firstImp, ...)
}

$RRF$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$RRF$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$RRF$varImp
function (object, ...) 
{
    varImp <- RRF::importance(object, ...)
    if (object$type == "regression") 
        varImp <- data.frame(Overall = varImp[, "%IncMSE"])
    else {
        retainNames <- levels(object$y)
        if (all(retainNames %in% colnames(varImp))) {
            varImp <- varImp[, retainNames]
        }
        else {
            varImp <- data.frame(Overall = varImp[, 1])
        }
    }
    out <- as.data.frame(varImp)
    if (dim(out)[2] == 2) {
        tmp <- apply(out, 1, mean)
        out[, 1] <- out[, 2] <- tmp
    }
    out
}

$RRF$levels
function (x) 
x$obsLevels

$RRF$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Regularization"            

$RRF$sort
function (x) 
x[order(x$coefReg), ]


$RRFglobal
$RRFglobal$label
[1] "Regularized Random Forest"

$RRFglobal$library
[1] "RRF"

$RRFglobal$loop
NULL

$RRFglobal$type
[1] "Regression"     "Classification"

$RRFglobal$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors
2   coefReg numeric          Regularization Value

$RRFglobal$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), coefReg = seq(0.01, 
            1, length = len))
    }
    else {
        out <- data.frame(mtry = sample(1:ncol(x), size = len, 
            replace = TRUE), coefReg = runif(len, min = 0, max = 1))
    }
    out
}

$RRFglobal$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    RRF(x, y, mtry = param$mtry, coefReg = param$coefReg, ...)
}

$RRFglobal$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$RRFglobal$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$RRFglobal$varImp
function (object, ...) 
{
    varImp <- RRF::importance(object, ...)
    if (object$type == "regression") 
        varImp <- data.frame(Overall = varImp[, "%IncMSE"])
    else {
        retainNames <- levels(object$y)
        if (all(retainNames %in% colnames(varImp))) {
            varImp <- varImp[, retainNames]
        }
        else {
            varImp <- data.frame(Overall = varImp[, 1])
        }
    }
    out <- as.data.frame(varImp)
    if (dim(out)[2] == 2) {
        tmp <- apply(out, 1, mean)
        out[, 1] <- out[, 2] <- tmp
    }
    out
}

$RRFglobal$levels
function (x) 
x$obsLevels

$RRFglobal$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"
[5] "Regularization"            

$RRFglobal$sort
function (x) 
x[order(x$coefReg), ]


$rrlda
$rrlda$label
[1] "Robust Regularized Linear Discriminant Analysis"

$rrlda$library
[1] "rrlda"

$rrlda$loop
NULL

$rrlda$type
[1] "Classification"

$rrlda$parameters
  parameter     class                label
1    lambda   numeric    Penalty Parameter
2        hp   numeric Robustness Parameter
3   penalty character         Penalty Type

$rrlda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = (1:len) * 0.25, hp = seq(0.5, 
            1, length = len), penalty = "L2")
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            hp = runif(len, min = 0, 1), penalty = sample(c("L1", 
                "L2"), size = len, replace = TRUE))
    }
    out
}

$rrlda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    rrlda:::rrlda(x, as.numeric(y), lambda = param$lambda, hp = param$hp, 
        penalty = as.character(param$penalty), ...)
}

$rrlda$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$class
    modelFit$obsLevels[as.numeric(out)]
}

$rrlda$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)$posterior
    colnames(out) <- modelFit$obsLevels
    out
}

$rrlda$levels
function (x) 
x$obsLevels

$rrlda$tags
[1] "Discriminant Analysis" "Robust Model"          "Regularization"       
[4] "Linear Classifier"    

$rrlda$sort
function (x) 
x[order(-x$lambda), ]


$RSimca
$RSimca$label
[1] "Robust SIMCA"

$RSimca$library
[1] "rrcovHD"

$RSimca$loop
NULL

$RSimca$type
[1] "Classification"

$RSimca$parameters
  parameter     class     label
1 parameter character parameter

$RSimca$grid
function (x, y, len = NULL, search = "grid") 
{
    data.frame(parameter = "none")
}

$RSimca$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
RSimca(x, y, ...)

$RSimca$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)@classification

$RSimca$prob
NULL

$RSimca$tags
[1] "Robust Model"      "Linear Classifier"

$RSimca$levels
function (x) 
names(x@prior)

$RSimca$sort
function (x) 
x


$rvmLinear
$rvmLinear$label
[1] "Relevance Vector Machines with Linear Kernel"

$rvmLinear$library
[1] "kernlab"

$rvmLinear$loop
NULL

$rvmLinear$type
[1] "Regression"

$rvmLinear$parameters
  parameter     class     label
1 parameter character parameter

$rvmLinear$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$rvmLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    kernlab:::rvm(x = as.matrix(x), y = y, kernel = vanilladot(), 
        ...)
}

$rvmLinear$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$rvmLinear$prob
NULL

$rvmLinear$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$rvmLinear$tags
[1] "Kernel Method"             "Relevance Vector Machines"
[3] "Linear Regression"         "Robust Methods"           

$rvmLinear$sort
function (x) 
x


$rvmPoly
$rvmPoly$label
[1] "Relevance Vector Machines with Polynomial Kernel"

$rvmPoly$library
[1] "kernlab"

$rvmPoly$loop
NULL

$rvmPoly$type
[1] "Regression"

$rvmPoly$parameters
  parameter   class             label
1     scale numeric             Scale
2    degree numeric Polynomial Degree

$rvmPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(degree = seq(1, min(len, 3)), scale = 10^((1:len) - 
            4))
    }
    else {
        out <- data.frame(degree = sample(1:3, size = len, replace = TRUE), 
            scale = 10^runif(len, min = -5, 0))
    }
    out
}

$rvmPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    kernlab:::rvm(x = as.matrix(x), y = y, kernel = polydot, 
        kpar = list(degree = param$degree, scale = param$scale, 
            offset = 1), ...)
}

$rvmPoly$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$rvmPoly$prob
NULL

$rvmPoly$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$xscale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$rvmPoly$tags
[1] "Kernel Method"             "Relevance Vector Machines"
[3] "Polynomial Model"          "Robust Methods"           

$rvmPoly$sort
function (x) 
x[order(x$degree, x$scale), ]


$rvmRadial
$rvmRadial$label
[1] "Relevance Vector Machines with Radial Basis Function Kernel"

$rvmRadial$library
[1] "kernlab"

$rvmRadial$loop
NULL

$rvmRadial$type
[1] "Regression"

$rvmRadial$parameters
  parameter   class label
1     sigma numeric Sigma

$rvmRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = mean(as.vector(sigmas[-2])))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])))
    }
    out
}

$rvmRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    kernlab:::rvm(x = as.matrix(x), y = y, kernel = rbfdot, kpar = list(sigma = param$sigma), 
        ...)
}

$rvmRadial$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$rvmRadial$prob
NULL

$rvmRadial$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$rvmRadial$tags
[1] "Kernel Method"             "Relevance Vector Machines"
[3] "Radial Basis Function"     "Robust Methods"           

$rvmRadial$sort
function (x) 
x[order(-x$sigma), ]


$SBC
$SBC$label
[1] "Subtractive Clustering and Fuzzy c-Means Rules"

$SBC$library
[1] "frbs"

$SBC$type
[1] "Regression"

$SBC$parameters
  parameter   class           label
1       r.a numeric          Radius
2  eps.high numeric Upper Threshold
3   eps.low numeric Lower Threshold

$SBC$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(r.a = seq(0, 1, length = len), eps.high = seq(0, 
            1, length = len), eps.low = seq(0, 1, length = len))
    }
    else {
        out <- data.frame(r.a = sample(1:20, size = len * 10, 
            replace = TRUE), eps.high = runif(len * 10, min = 0, 
            max = 1), eps.low = runif(len * 10, min = 0, max = 1))
    }
    out <- subset(out, eps.high > eps.low)
    out[1:min(nrow(out), len), ]
}

$SBC$loop
NULL

$SBC$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- as.matrix(cbind(x, y))
    frbs.learn(data.train = dat, range.data = apply(dat, 2, extendrange), 
        method = "SBC", control = list(r.a = param$r.a, eps.high = param$eps.high, 
            eps.low = param$eps.low))
}

$SBC$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$SBC$prob
NULL

$SBC$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$SBC$tags
[1] "Rule-Based Model"

$SBC$levels
NULL

$SBC$sort
function (x) 
x[order(x$r.a), ]


$sda
$sda$label
[1] "Shrinkage Discriminant Analysis"

$sda$library
[1] "sda"

$sda$loop
NULL

$sda$type
[1] "Classification"

$sda$parameters
  parameter   class       label
1  diagonal logical Diagonalize
2    lambda numeric   shrinkage

$sda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(diagonal = FALSE, lambda = seq(0, 1, 
            length = len))
    }
    else {
        out <- data.frame(lambda = runif(len, min = 0, 1), diagonal = sample(c(TRUE, 
            FALSE), size = len, replace = TRUE))
    }
    out
}

$sda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
sda::sda(as.matrix(x), y, diagonal = param$diagonal, lambda = param$lambda, 
    ...)

$sda$predict
function (modelFit, newdata, submodels = NULL) 
sda::predict.sda(modelFit, as.matrix(newdata))$class

$sda$prob
function (modelFit, newdata, submodels = NULL) 
sda::predict.sda(modelFit, as.matrix(newdata))$posterior

$sda$predictors
function (x, ...) 
{
    colnames(x$beta)
}

$sda$levels
function (x) 
x$obsLevels

$sda$tags
[1] "Discriminant Analysis" "Regularization"        "Linear Classifier"    

$sda$sort
function (x) 
x[order(x$diagonal, x$lambda), ]


$sddaLDA
$sddaLDA$label
[1] "Stepwise Diagonal Linear Discriminant Analysis"

$sddaLDA$library
[1] "SDDA"

$sddaLDA$loop
NULL

$sddaLDA$type
[1] "Classification"

$sddaLDA$parameters
  parameter     class     label
1 parameter character parameter

$sddaLDA$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$sddaLDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
sdda(as.matrix(x), y, method = "lda", ...)

$sddaLDA$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata), type = "class")

$sddaLDA$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata), type = "prob")

$sddaLDA$levels
function (x) 
x$obsLevels

$sddaLDA$tags
[1] "Discriminant Analysis"     "Feature Selection Wrapper"
[3] "Linear Classifier"        

$sddaLDA$sort
function (x) 
x


$sddaQDA
$sddaQDA$label
[1] "Stepwise Diagonal Quadratic Discriminant Analysis"

$sddaQDA$library
[1] "SDDA"

$sddaQDA$loop
NULL

$sddaQDA$type
[1] "Classification"

$sddaQDA$parameters
  parameter     class     label
1 parameter character parameter

$sddaQDA$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$sddaQDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
sdda(as.matrix(x), y, method = "qda", ...)

$sddaQDA$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata), type = "class")

$sddaQDA$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata), type = "prob")

$sddaQDA$levels
function (x) 
x$obsLevels

$sddaQDA$tags
[1] "Discriminant Analysis"     "Feature Selection Wrapper"
[3] "Polynomial Model"         

$sddaQDA$sort
function (x) 
x


$sdwd
$sdwd$label
[1] "Sparse Distance Weighted Discrimination"

$sdwd$library
[1] "sdwd"

$sdwd$type
[1] "Classification"

$sdwd$parameters
  parameter   class      label
1    lambda numeric L1 Penalty
2   lambda2 numeric L2 Penalty

$sdwd$grid
function (x, y, len = NULL, search = "grid") 
{
    lev <- levels(y)
    y <- ifelse(y == lev[1], 1, -1)
    init <- sdwd(as.matrix(x), y, nlambda = len + 2, lambda2 = 0)
    lambda <- unique(init$lambda)
    lambda <- lambda[-c(1, length(lambda))]
    if (search == "grid") {
        lambda <- lambda[1:min(length(lambda), len)]
        out <- expand.grid(lambda = lambda, lambda2 = seq(0.1, 
            1, length = len))
    }
    else {
        out <- data.frame(lambda = runif(len, min = min(lambda), 
            max(lambda)), lambda2 = 10^runif(len, min = -5, 0))
    }
    out
}

$sdwd$loop
NULL

$sdwd$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    y <- ifelse(y == lev[1], 1, -1)
    sdwd(as.matrix(x), y = y, lambda = param$lambda, lambda2 = param$lambda2, 
        ...)
}

$sdwd$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newx = newdata, type = "class")
    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
}

$sdwd$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newx = newdata, type = "link")
    out <- binomial()$linkinv(out)
    out <- data.frame(c1 = out, c2 = 1 - out)
    colnames(out) <- modelFit$obsLevels
    out
}

$sdwd$predictors
function (x, ...) 
{
    out <- apply(x$beta, 1, function(x) any(x != 0))
    names(out)[out]
}

$sdwd$varImp
function (object, lambda = NULL, ...) 
{
    out <- as.data.frame(as.matrix(abs(object$beta)))
    colnames(out) <- "Overall"
    out
}

$sdwd$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$sdwd$tags
[1] "Discriminant Analysis Models"     "Implicit Feature Selection"      
[3] "L1 Regularization"                "L2 Regularization"               
[5] "Linear Classifier"                "Distance Weighted Discrimination"

$sdwd$sort
function (x) 
x[order(-x$lambda, -x$lambda2), ]

$sdwd$trim
function (x) 
{
    x$call <- NULL
    x
}


$simpls
$simpls$label
[1] "Partial Least Squares"

$simpls$library
[1] "pls"

$simpls$type
[1] "Regression"     "Classification"

$simpls$parameters
  parameter   class       label
1     ncomp numeric #Components

$simpls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), 
            by = 1))
    }
    else {
        out <- data.frame(ncomp = unique(sample(1:(ncol(x) - 
            1), size = len, replace = TRUE)))
    }
    out
}

$simpls$loop
function (grid) 
{
    grid <- grid[order(grid$ncomp, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$simpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- if (is.factor(y)) {
        plsda(x, y, method = "oscorespls", ncomp = param$ncomp, 
            ...)
    }
    else {
        dat <- if (is.data.frame(x)) 
            x
        else as.data.frame(x)
        dat$.outcome <- y
        plsr(.outcome ~ ., data = dat, method = "simpls", ncomp = param$ncomp, 
            ...)
    }
    out
}

$simpls$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- if (modelFit$problemType == "Classification") {
        if (!is.matrix(newdata)) 
            newdata <- as.matrix(newdata)
        out <- predict(modelFit, newdata, type = "class")
    }
    else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels))
        if (modelFit$problemType == "Classification") {
            if (length(submodels$ncomp) > 1) {
                tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
            }
            else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))
        }
        else {
            tmp <- as.list(as.data.frame(apply(predict(modelFit, 
                newdata, ncomp = submodels$ncomp), 3, function(x) list(x))))
        }
        out <- c(list(out), tmp)
    }
    out
}

$simpls$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, type = "prob", ncomp = modelFit$tuneValue$ncomp)
    if (length(dim(out)) == 3) {
        if (dim(out)[1] > 1) {
            out <- out[, , 1]
        }
        else {
            out <- as.data.frame(t(out[, , 1]))
        }
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$ncomp)) {
            tmpProb <- predict(modelFit, newdata, type = "prob", 
                ncomp = submodels$ncomp[j])
            if (length(dim(tmpProb)) == 3) {
                if (dim(tmpProb)[1] > 1) {
                  tmpProb <- tmpProb[, , 1]
                }
                else {
                  tmpProb <- as.data.frame(t(tmpProb[, , 1]))
                }
            }
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$simpls$varImp
function (object, estimate = NULL, ...) 
{
    modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
    perf <- MSEP(object)$val
    nms <- dimnames(perf)
    if (length(nms$estimate) > 1) {
        pIndex <- if (is.null(estimate)) 
            1
        else which(nms$estimate == estimate)
        perf <- perf[pIndex, , , drop = FALSE]
    }
    numResp <- dim(modelCoef)[2]
    if (numResp <= 2) {
        modelCoef <- modelCoef[, 1, , drop = FALSE]
        perf <- perf[, 1, ]
        delta <- -diff(perf)
        delta <- delta/sum(delta)
        out <- data.frame(Overall = apply(abs(modelCoef), 1, 
            weighted.mean, w = delta))
    }
    else {
        perf <- -t(apply(perf[1, , ], 1, diff))
        perf <- t(apply(perf, 1, function(u) u/sum(u)))
        out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])
        for (i in 1:numResp) {
            tmp <- abs(modelCoef[, i, , drop = FALSE])
            out[, i] <- apply(tmp, 1, weighted.mean, w = perf[i, 
                ])
        }
        colnames(out) <- dimnames(modelCoef)[[2]]
        rownames(out) <- dimnames(modelCoef)[[1]]
    }
    as.data.frame(out)
}

$simpls$levels
function (x) 
x$obsLevels

$simpls$predictors
function (x, ...) 
rownames(x$projection)

$simpls$tags
[1] "Partial Least Squares" "Feature Extraction"    "Linear Classifier"    
[4] "Linear Regression"    

$simpls$sort
function (x) 
x[order(x[, 1]), ]


$SLAVE
$SLAVE$label
[1] "Fuzzy Rules Using the Structural Learning Algorithm on Vague Environment"

$SLAVE$library
[1] "frbs"

$SLAVE$type
[1] "Classification"

$SLAVE$parameters
   parameter   class            label
1 num.labels numeric     #Fuzzy Terms
2   max.iter numeric  Max. Iterations
3    max.gen numeric Max. Generations

$SLAVE$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, max.iter = 10, 
            max.gen = 10)
    }
    else {
        out <- data.frame(num.labels = sample(2:20, size = len, 
            replace = TRUE), max.iter = sample(1:20, replace = TRUE, 
            size = len), max.gen = sample(1:20, size = len, replace = TRUE))
    }
    out
}

$SLAVE$loop
NULL

$SLAVE$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))), 
        method.type = "SLAVE")
    args$range.data <- apply(x, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$max.iter <- param$max.iter
        theDots$control$max.gen <- param$max.gen
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        max.iter = param$max.iter, max.gen = param$max.gen, persen_cross = 0.6, 
        persen_mutant = 0.3, k.lower = 0.25, k.upper = 0.75, 
        epsilon = 0.1, num.class = length(unique(y)), name = "sim-0")
    mod <- try(do.call("frbs.learn", c(args, theDots)), silent = TRUE)
    mod
}

$SLAVE$predict
function (modelFit, newdata, submodels = NULL) 
{
    modelFit$obsLevels[predict(modelFit, newdata)[, 1]]
}

$SLAVE$prob
NULL

$SLAVE$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$SLAVE$tags
[1] "Rule-Based Model"

$SLAVE$levels
NULL

$SLAVE$sort
function (x) 
x[order(x$num.labels), ]


$slda
$slda$label
[1] "Stabilized Linear Discriminant Analysis"

$slda$library
[1] "ipred"

$slda$loop
NULL

$slda$type
[1] "Classification"

$slda$parameters
  parameter     class label
1 parameter character  none

$slda$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$slda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    slda(.outcome ~ ., data = dat, ...)
}

$slda$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)$class
}

$slda$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)$posterior
}

$slda$levels
function (x) 
x$obsLevels

$slda$predictors
function (x, ...) 
if (hasTerms(x)) predictors(x$terms) else predictors(x$mylda)

$slda$tags
[1] "Discriminant Analysis" "Linear Classifier"    

$slda$sort
function (x) 
x


$smda
$smda$label
[1] "Sparse Mixture Discriminant Analysis"

$smda$library
[1] "sparseLDA"

$smda$loop
NULL

$smda$type
[1] "Classification"

$smda$parameters
  parameter   class        label
1   NumVars numeric # Predictors
2    lambda numeric       Lambda
3         R numeric # Subclasses

$smda$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(NumVars = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), R = (1:len) + 
            1, lambda = c(0, 10^seq(-1, -4, length = len - 1)))
    }
    else {
        out <- data.frame(NumVars = sample(1:ncol(x), size = len, 
            replace = TRUE), lambda = 10^runif(len, min = -5, 
            1), R = sample(2:5, size = len, replace = TRUE))
    }
    out
}

$smda[[7]]
NULL

$smda$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
smda(x, y, Rj = param$R, lambda = param$lambda, stop = -param$NumVars, 
    ...)

$smda$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)$class

$smda$prob
NULL

$smda$levels
function (x) 
x$obsLevels

$smda$predictors
function (x, ...) 
x$varNames

$smda$tags
[1] "Discriminant Analysis"      "L1 Regularization"         
[3] "Implicit Feature Selection" "Mixture Model"             

$smda$sort
function (x) 
x[order(x$NumVars, x$R, -x$lambda), ]


$snn
$snn$label
[1] "Stabilized Nearest Neighbor Classifier"

$snn$library
[1] "snn"

$snn$loop
NULL

$snn$type
[1] "Classification"

$snn$parameters
  parameter   class                   label
1    lambda numeric Stabilization Parameter

$snn$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(0, 2^seq(-5, 5, length = len - 
            1)))
    }
    else {
        out <- data.frame(lambda = 2^runif(len, min = -5, 5))
    }
    out
}

$snn$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    if (!(class(x[1, 1]) %in% c("integer", "numeric"))) 
        stop("predictors should be all numeric")
    x <- cbind(x, as.numeric(y))
    colnames(x)[ncol(x)] <- ".outcome"
    list(dat = x, lambda = param$lambda)
}

$snn$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- mysnn(train = modelFit$dat, test = newdata, lambda = modelFit$lambda)
    modelFit$obsLevels[out]
}

$snn$predictors
function (x, ...) 
x$xNames

$snn$tags
[1] "Prototype Models"

$snn$prob
NULL

$snn$levels
function (x) 
x$obsLevels

$snn$sort
function (x) 
x[order(-x[, 1]), ]


$sparseLDA
$sparseLDA$label
[1] "Sparse Linear Discriminant Analysis"

$sparseLDA$library
[1] "sparseLDA"

$sparseLDA$loop
NULL

$sparseLDA$type
[1] "Classification"

$sparseLDA$parameters
  parameter   class        label
1   NumVars numeric # Predictors
2    lambda numeric       Lambda

$sparseLDA$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(NumVars = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len), lambda = c(0, 
            10^seq(-1, -4, length = len - 1)))
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
            NumVars = sample(1:ncol(x), size = len, replace = TRUE))
    }
    out
}

$sparseLDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
sparseLDA:::sda(x, y, lambda = param$lambda, stop = -param$NumVars, 
    ...)

$sparseLDA$predictors
function (x) 
x$xNames[x$varIndex]

$sparseLDA$predict
function (modelFit, newdata, submodels = NULL) 
sparseLDA:::predict.sda(modelFit, newdata)$class

$sparseLDA$prob
function (modelFit, newdata, submodels = NULL) 
sparseLDA:::predict.sda(modelFit, newdata)$posterior

$sparseLDA$levels
function (x) 
x$obsLevels

$sparseLDA$tags
[1] "Discriminant Analysis"      "L1 Regularization"         
[3] "Implicit Feature Selection" "Linear Classifier"         

$sparseLDA$sort
function (x) 
x[order(x$NumVars, -x$lambda), ]


$spikeslab
$spikeslab$label
[1] "Spike and Slab Regression"

$spikeslab$library
[1] "spikeslab" "plyr"     

$spikeslab$type
[1] "Regression"

$spikeslab$parameters
  parameter   class              label
1      vars numeric Variables Retained

$spikeslab$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(vars = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(vars = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$spikeslab$loop
function (grid) 
{
    grid <- grid[order(grid$vars, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$spikeslab$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    mod <- spikeslab(x = as.matrix(x), y = y, max.var = param$vars, 
        ...)
    path <- data.frame(k = apply(mod$gnet.path$path, 1, function(x) sum(x != 
        0)))
    path$index <- 1:nrow(path)
    path <- ddply(path, .(k), function(x) x[which.min(x$index), 
        ])
    if (all(path$k != ncol(x))) 
        path <- rbind(path, data.frame(k = ncol(x), index = max(path$index)))
    mod$.path <- path
    mod$.size <- param$vars
    mod
}

$spikeslab$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata)$yhat.gnet.path
    if (is.vector(out)) 
        out <- matrix(out, nrow = 1)
    if (!is.null(submodels)) {
        vars <- data.frame(k = c(modelFit$.size, submodels$vars))
        vars$order <- 1:nrow(vars)
        vars <- merge(vars, modelFit$.path, all.x = TRUE)
        vars <- vars[order(vars$order), ]
        out <- out[, vars$index]
        out <- as.list(as.data.frame(out))
    }
    else {
        index <- modelFit$.path$index[modelFit$.path$k == modelFit$.size]
        out <- out[, index]
    }
    out
}

$spikeslab$predictors
function (x, s = NULL, ...) 
{
    coefs <- x$gnet
    names(coefs)[coefs != 0]
}

$spikeslab$tags
[1] "Linear Regression"          "Bayesian Model"            
[3] "Implicit Feature Selection"

$spikeslab$prob
NULL

$spikeslab$sort
function (x) 
x


$spls
$spls$label
[1] "Sparse Partial Least Squares"

$spls$library
[1] "spls"

$spls$type
[1] "Regression"     "Classification"

$spls$parameters
  parameter   class       label
1         K numeric #Components
2       eta numeric   Threshold
3     kappa numeric       Kappa

$spls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(K = 1:min(nrow(x), ncol(x)), eta = seq(0.1, 
            0.9, length = len), kappa = 0.5)
    }
    else {
        out <- data.frame(kappa = runif(len, min = 0, max = 0.5), 
            eta = runif(len, min = 0, max = 1), K = sample(1:min(nrow(x), 
                ncol(x)), size = len, replace = TRUE))
    }
    out
}

$spls$loop
NULL

$spls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        caret:::splsda(x, y, K = param$K, eta = param$eta, kappa = param$kappa, 
            ...)
    }
    else {
        spls(x, y, K = param$K, eta = param$eta, kappa = param$kappa, 
            ...)
    }
}

$spls$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (length(modelFit$obsLevels) < 2) {
        predict(modelFit, newdata)
    }
    else {
        as.character(caret:::predict.splsda(modelFit, newdata, 
            type = "class"))
    }
}

$spls$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    caret:::predict.splsda(modelFit, newdata, type = "prob")
}

$spls$predictors
function (x, ...) 
colnames(x$x)[x$A]

$spls$tags
[1] "Partial Least Squares" "Feature Extraction"    "Linear Classifier"    
[4] "Linear Regression"     "L1 Regularization"    

$spls$levels
function (x) 
x$obsLevels

$spls$sort
function (x) 
x[order(-x$eta, x$K), ]


$stepLDA
$stepLDA$label
[1] "Linear Discriminant Analysis with Stepwise Feature Selection"

$stepLDA$library
[1] "klaR" "MASS"

$stepLDA$loop
NULL

$stepLDA$type
[1] "Classification"

$stepLDA$parameters
  parameter     class              label
1    maxvar   numeric Maximum #Variables
2 direction character   Search Direction

$stepLDA$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(maxvar = Inf, direction = "both")
    }
    else {
        out <- data.frame(direction = sample(c("both", "forward", 
            "backward"), size = len, replace = TRUE), maxvar = sample(1:ncol(x), 
            size = len, replace = TRUE))
    }
    out
}

$stepLDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- stepclass(x, y, method = "lda", maxvar = param$maxvar, 
        direction = as.character(param$direction), ...)
    out$fit <- lda(x[, out$model$name, drop = FALSE], y, ...)
    out
}

$stepLDA$predict
function (modelFit, newdata, submodels = NULL) 
{
    code <- getModelInfo("lda", regex = FALSE)[[1]]$predictors
    predict(modelFit$fit, newdata[, code(modelFit$fit), drop = FALSE])$class
}

$stepLDA$prob
function (modelFit, newdata, submodels = NULL) 
{
    code <- getModelInfo("lda", regex = FALSE)[[1]]$predictors
    predict(modelFit$fit, newdata[, code(modelFit$fit), drop = FALSE])$posterior
}

$stepLDA$predictors
function (x, ...) 
{
    form <- x$formula
    form[[2]] <- NULL
    all.vars(form)
}

$stepLDA$levels
function (x) 
x$obsLevels

$stepLDA$tags
[1] "Discriminant Analysis"     "Feature Selection Wrapper"
[3] "Linear Classifier"        

$stepLDA$sort
function (x) 
x


$stepQDA
$stepQDA$label
[1] "Quadratic Discriminant Analysis with Stepwise Feature Selection"

$stepQDA$library
[1] "klaR" "MASS"

$stepQDA$loop
NULL

$stepQDA$type
[1] "Classification"

$stepQDA$parameters
  parameter     class              label
1    maxvar   numeric Maximum #Variables
2 direction character   Search Direction

$stepQDA$grid
function (x, y, len = NULL, search = "grid") 
data.frame(maxvar = Inf, direction = "both")

$stepQDA$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- stepclass(x, y, method = "qda", maxvar = param$maxvar, 
        direction = as.character(param$direction), ...)
    out$fit <- qda(x[, out$model$name, drop = FALSE], y, ...)
    out
}

$stepQDA$predict
function (modelFit, newdata, submodels = NULL) 
{
    code <- getModelInfo("qda", regex = FALSE)[[1]]$predictors
    predict(modelFit$fit, newdata[, code(modelFit$fit), drop = FALSE])$class
}

$stepQDA$prob
function (modelFit, newdata, submodels = NULL) 
{
    code <- getModelInfo("qda", regex = FALSE)[[1]]$predictors
    predict(modelFit$fit, newdata[, code(modelFit$fit), drop = FALSE])$posterior
}

$stepQDA$predictors
function (x, ...) 
{
    form <- x$formula
    form[[2]] <- NULL
    all.vars(form)
}

$stepQDA$levels
function (x) 
x$obsLevels

$stepQDA$tags
[1] "Discriminant Analysis"     "Feature Selection Wrapper"
[3] "Polynomial Model"         

$stepQDA$sort
function (x) 
x


$superpc
$superpc$label
[1] "Supervised Principal Component Analysis"

$superpc$library
[1] "superpc"

$superpc$type
[1] "Regression"

$superpc$parameters
     parameter   class       label
1    threshold numeric   Threshold
2 n.components numeric #Components

$superpc$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(n.components = 1:3, threshold = seq(0.1, 
            0.9, length = len))
    }
    else {
        out <- data.frame(threshold = runif(len, min = 0, max = 1), 
            n.components = sample(1:3, size = len, replace = TRUE))
    }
    out
}

$superpc$loop
function (grid) 
{
    ordering <- order(-grid$n.components, -grid$threshold)
    loop <- grid[ordering[1], , drop = FALSE]
    submodels <- list(grid[ordering[-1], , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$superpc$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- superpc.train(list(x = t(x), y = y), type = "regression", 
        ...)
    out$data <- list(x = t(x), y = y)
    out$tuneValue <- list(n.components = param$n.components, 
        threshold = param$threshold)
    out
}

$superpc$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- superpc.predict(modelFit, modelFit$data, newdata = list(x = t(newdata)), 
        n.components = modelFit$tuneValue$n.components, threshold = modelFit$tuneValue$threshold)$v.pred.1df
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$threshold)) {
            tmp[[j + 1]] <- superpc.predict(modelFit, modelFit$data, 
                newdata = list(x = t(newdata)), threshold = submodels$threshold[j], 
                n.components = submodels$n.components[j])$v.pred.1df
        }
        out <- tmp
    }
    out
}

$superpc$prob
NULL

$superpc$tags
[1] "Feature Extraction" "Linear Regression" 

$superpc$sort
function (x) 
x[order(x$threshold, x$n.components), ]


$svmBoundrangeString
$svmBoundrangeString$label
[1] "Support Vector Machines with Boundrange String Kernel"

$svmBoundrangeString$library
[1] "kernlab"

$svmBoundrangeString$type
[1] "Regression"     "Classification"

$svmBoundrangeString$parameters
  parameter   class  label
1    length numeric length
2         C numeric   Cost

$svmBoundrangeString$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(length = 2:(len + 1), C = 2^((1:len) - 
            3))
    }
    else {
        out <- data.frame(length = sample(1:20, size = len, replace = TRUE), 
            C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmBoundrangeString$loop
NULL

$svmBoundrangeString$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "boundrange", 
            length = param$length), C = param$C, ...)
    }
    else {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "boundrange", 
            length = param$length), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmBoundrangeString$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata[, 1]), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    out
}

$svmBoundrangeString$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata[, 1], type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmBoundrangeString$predictors
function (x, ...) 
{
    iNA
}

$svmBoundrangeString$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "String Kernel"           "Robust Methods"         
[5] "Text Mining"            

$svmBoundrangeString$levels
function (x) 
lev(x)

$svmBoundrangeString$sort
function (x) 
{
    x[order(x$C, -x$length), ]
}


$svmExpoString
$svmExpoString$label
[1] "Support Vector Machines with Exponential String Kernel"

$svmExpoString$library
[1] "kernlab"

$svmExpoString$type
[1] "Regression"     "Classification"

$svmExpoString$parameters
  parameter   class  label
1    lambda numeric lambda
2         C numeric   Cost

$svmExpoString$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = 0.25 + 2^((1:len) - 1), C = 2^((1:len) - 
            3))
    }
    else {
        out <- data.frame(lambda = 2^runif(len, min = -5, max = 6), 
            C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmExpoString$loop
NULL

$svmExpoString$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "exponential", 
            lambda = param$lambda), C = param$C, ...)
    }
    else {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "exponential", 
            lambda = param$lambda), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmExpoString$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata[, 1]), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    out
}

$svmExpoString$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata[, 1], type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmExpoString$predictors
function (x, ...) 
{
    iNA
}

$svmExpoString$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "String Kernel"           "Robust Methods"         
[5] "Text Mining"            

$svmExpoString$levels
function (x) 
lev(x)

$svmExpoString$sort
function (x) 
{
    x[order(x$C, -x$lambda), ]
}


$svmLinear
$svmLinear$label
[1] "Support Vector Machines with Linear Kernel"

$svmLinear$library
[1] "kernlab"

$svmLinear$type
[1] "Regression"     "Classification"

$svmLinear$parameters
  parameter   class label
1         C numeric  Cost

$svmLinear$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(C = 1)
    }
    else {
        out <- data.frame(C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmLinear$loop
NULL

$svmLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = vanilladot(), 
            C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = vanilladot(), 
            C = param$C, prob.model = classProbs, ...)
    }
    out
}

$svmLinear$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmLinear$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmLinear$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmLinear$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Regression"       "Linear Classifier"      
[5] "Robust Methods"         

$svmLinear$levels
function (x) 
lev(x)

$svmLinear$sort
function (x) 
{
    x[order(x$C), ]
}


$svmLinear2
$svmLinear2$label
[1] "Support Vector Machines with Linear Kernel"

$svmLinear2$library
[1] "e1071"

$svmLinear2$type
[1] "Regression"     "Classification"

$svmLinear2$parameters
  parameter   class label
1      cost numeric  Cost

$svmLinear2$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3))
    }
    else {
        out <- data.frame(cost = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmLinear2$loop
NULL

$svmLinear2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "probability") | is.numeric(y)) {
        out <- svm(x = as.matrix(x), y = y, kernel = "linear", 
            cost = param$cost, ...)
    }
    else {
        out <- svm(x = as.matrix(x), y = y, kernel = "linear", 
            cost = param$cost, probability = classProbs, ...)
    }
    out
}

$svmLinear2$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$svmLinear2$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, probability = TRUE)
    attr(out, "probabilities")
}

$svmLinear2$predictors
function (x, ...) 
{
    out <- if (!is.null(x$terms)) 
        predictors.terms(x$terms)
    else x$xNames
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmLinear2$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Regression"       "Linear Classifier"      
[5] "Robust Methods"         

$svmLinear2$levels
function (x) 
x$levels

$svmLinear2$sort
function (x) 
{
    x[order(x$cost), ]
}


$svmLinear3
$svmLinear3$label
[1] "L2 Regularized Support Vector Machine (dual) with Linear Kernel"

$svmLinear3$library
[1] "LiblineaR"

$svmLinear3$type
[1] "Regression"     "Classification"

$svmLinear3$parameters
  parameter     class         label
1      cost   numeric          Cost
2      Loss character Loss Function

$svmLinear3$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3), Loss = c("L1", 
            "L2"))
    }
    else {
        out <- data.frame(cost = 2^runif(len, min = -10, max = 10), 
            Loss = sample(c("L1", "L2"), size = len, replace = TRUE))
    }
    out
}

$svmLinear3$loop
NULL

$svmLinear3$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (param$Loss == "L2") {
        model_type <- if (is.factor(y)) 
            3
        else 13
    }
    else model_type <- if (is.factor(y)) 
        2
    else 11
    out <- LiblineaR(data = as.matrix(x), target = y, cost = param$cost, 
        type = model_type, ...)
    out
}

$svmLinear3$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)$predictions
}

$svmLinear3$prob
NULL

$svmLinear3$predictors
function (x, ...) 
{
    out <- colnames(x$W)
    out[out != "Bias"]
}

$svmLinear3$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Regression"       "Linear Classifier"      
[5] "Robust Methods"         

$svmLinear3$levels
function (x) 
x$levels

$svmLinear3$sort
function (x) 
{
    x[order(x$cost), ]
}


$svmLinearWeights
$svmLinearWeights$label
[1] "Linear Support Vector Machines with Class Weights"

$svmLinearWeights$library
[1] "e1071"

$svmLinearWeights$type
[1] "Classification"

$svmLinearWeights$parameters
  parameter   class        label
1      cost numeric         Cost
2    weight numeric Class Weight

$svmLinearWeights$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3), weight = 1:len)
    }
    else {
        out <- data.frame(cost = 2^runif(len, min = -5, max = 10), 
            weight = runif(len, min = 1, max = 25))
    }
    out
}

$svmLinearWeights$loop
NULL

$svmLinearWeights$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (length(levels(y)) != 2) 
        stop("Currently implemented for 2-class problems")
    cwts <- c(1, param$weight)
    names(cwts) <- levels(y)
    out <- svm(x = as.matrix(x), y = y, kernel = "linear", cost = param$cost, 
        probability = classProbs, class.weights = cwts, ...)
    out
}

$svmLinearWeights$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$svmLinearWeights$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata, probability = TRUE)
    attr(out, "probabilities")
}

$svmLinearWeights$predictors
function (x, ...) 
{
    out <- if (!is.null(x$terms)) 
        predictors.terms(x$terms)
    else x$xNames
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmLinearWeights$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Classifier"       "Robust Methods"         
[5] "Cost Sensitive Learning" "Two Class Only"         

$svmLinearWeights$levels
function (x) 
x$levels

$svmLinearWeights$sort
function (x) 
{
    x[order(x$cost, x$weight), ]
}


$svmLinearWeights2
$svmLinearWeights2$label
[1] "L2 Regularized Linear Support Vector Machines with Class Weights"

$svmLinearWeights2$library
[1] "LiblineaR"

$svmLinearWeights2$type
[1] "Classification"

$svmLinearWeights2$parameters
  parameter     class         label
1      cost   numeric          Cost
2      Loss character Loss Function
3    weight   numeric  Class Weight

$svmLinearWeights2$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3), Loss = c("L1", 
            "L2"), weight = 1:len)
    }
    else {
        out <- data.frame(cost = 2^runif(len, min = -10, max = 10), 
            Loss = sample(c("L1", "L2"), size = len, replace = TRUE), 
            weight = runif(len, min = 1, max = 25))
    }
    out
}

$svmLinearWeights2$loop
NULL

$svmLinearWeights2$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    model_type <- if (param$Loss == "L2") 
        3
    else 2
    if (length(levels(y)) != 2) 
        stop("Currently implemented for 2-class problems")
    cwts <- c(1, param$weight)
    names(cwts) <- levels(y)
    out <- LiblineaR(data = as.matrix(x), target = y, cost = param$cost, 
        type = model_type, wi = cwts, ...)
    out
}

$svmLinearWeights2$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)$predictions
}

$svmLinearWeights2$prob
NULL

$svmLinearWeights2$predictors
function (x, ...) 
{
    out <- colnames(x$W)
    out[out != "Bias"]
}

$svmLinearWeights2$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Linear Classifier"       "Robust Methods"         
[5] "Cost Sensitive Learning" "Two Class Only"         

$svmLinearWeights2$levels
function (x) 
x$levels

$svmLinearWeights2$sort
function (x) 
{
    x[order(x$cost), ]
}


$svmPoly
$svmPoly$label
[1] "Support Vector Machines with Polynomial Kernel"

$svmPoly$library
[1] "kernlab"

$svmPoly$type
[1] "Regression"     "Classification"

$svmPoly$parameters
  parameter   class             label
1    degree numeric Polynomial Degree
2     scale numeric             Scale
3         C numeric              Cost

$svmPoly$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(degree = seq(1, min(len, 3)), scale = 10^((1:len) - 
            4), C = 2^((1:len) - 3))
    }
    else {
        out <- data.frame(degree = sample(1:3, size = len, replace = TRUE), 
            scale = 10^runif(len, min = -5, log10(2)), C = 2^runif(len, 
                min = -5, max = 10))
    }
    out
}

$svmPoly$loop
NULL

$svmPoly$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = polydot(degree = param$degree, 
            scale = param$scale, offset = 1), C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = polydot(degree = param$degree, 
            scale = param$scale, offset = 1), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmPoly$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmPoly$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmPoly$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$xscale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmPoly$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Polynomial Model"        "Robust Methods"         

$svmPoly$levels
function (x) 
lev(x)

$svmPoly$sort
function (x) 
x[order(x$degree, x$C, x$scale), ]


$svmRadial
$svmRadial$label
[1] "Support Vector Machines with Radial Basis Function Kernel"

$svmRadial$library
[1] "kernlab"

$svmRadial$type
[1] "Regression"     "Classification"

$svmRadial$parameters
  parameter   class label
1     sigma numeric Sigma
2         C numeric  Cost

$svmRadial$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = mean(as.vector(sigmas[-2])), 
            C = 2^((1:len) - 3))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])), C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmRadial$loop
NULL

$svmRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmRadial$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmRadial$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmRadial$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Radial Basis Function"   "Robust Methods"         

$svmRadial$levels
function (x) 
lev(x)

$svmRadial$sort
function (x) 
{
    x[order(x$C, -x$sigma), ]
}


$svmRadialCost
$svmRadialCost$label
[1] "Support Vector Machines with Radial Basis Function Kernel"

$svmRadialCost$library
[1] "kernlab"

$svmRadialCost$type
[1] "Regression"     "Classification"

$svmRadialCost$parameters
  parameter   class label
1         C numeric  Cost

$svmRadialCost$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(C = 2^((1:len) - 3))
    }
    else {
        out <- data.frame(C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmRadialCost$loop
NULL

$svmRadialCost$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = "rbfdot", 
            C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = "rbfdot", 
            C = param$C, prob.model = classProbs, ...)
    }
    out
}

$svmRadialCost$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmRadialCost$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmRadialCost$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmRadialCost$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Radial Basis Function"  

$svmRadialCost$levels
function (x) 
lev(x)

$svmRadialCost$sort
function (x) 
{
    x[order(x$C), ]
}


$svmRadialSigma
$svmRadialSigma$label
[1] "Support Vector Machines with Radial Basis Function Kernel"

$svmRadialSigma$library
[1] "kernlab"

$svmRadialSigma$type
[1] "Regression"     "Classification"

$svmRadialSigma$parameters
  parameter   class label
1     sigma numeric Sigma
2         C numeric  Cost

$svmRadialSigma$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = seq(min(sigmas), max(sigmas), 
            length = min(6, len)), C = 2^((1:len) - 3))
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])), C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmRadialSigma$loop
NULL

$svmRadialSigma$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmRadialSigma$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmRadialSigma$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmRadialSigma$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmRadialSigma$notes
[1] "This SVM model tunes over the cost parameter and the RBF kernel parameter sigma. In the latter case, using `tuneLength` will, at most, evaluate six values of the kernel parameter. This enables a broad search over the cost parameter and a relatively narrow search over `sigma`"

$svmRadialSigma$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Radial Basis Function"   "Robust Methods"         

$svmRadialSigma$levels
function (x) 
lev(x)

$svmRadialSigma$sort
function (x) 
{
    x[order(x$C, -x$sigma), ]
}


$svmRadialWeights
$svmRadialWeights$label
[1] "Support Vector Machines with Class Weights"

$svmRadialWeights$library
[1] "kernlab"

$svmRadialWeights$type
[1] "Classification"

$svmRadialWeights$parameters
  parameter   class  label
1     sigma numeric  Sigma
2         C numeric   Cost
3    Weight numeric Weight

$svmRadialWeights$grid
function (x, y, len = NULL, search = "grid") 
{
    library(kernlab)
    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    if (search == "grid") {
        out <- expand.grid(sigma = mean(as.vector(sigmas[-2])), 
            C = 2^((1:len) - 3), Weight = 1:len)
    }
    else {
        rng <- extendrange(log(sigmas), f = 0.75)
        out <- data.frame(sigma = exp(runif(len, min = rng[1], 
            max = rng[2])), C = 2^runif(len, min = -5, max = 10), 
            Weight = runif(len, min = 1, max = 25))
    }
    out
}

$svmRadialWeights$loop
NULL

$svmRadialWeights$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (param$Weight != 1) {
        wts <- c(param$Weight, 1)
        names(wts) <- levels(y)
    }
    else wts <- NULL
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), class.weights = wts, 
            C = param$C, ...)
    }
    else {
        out <- ksvm(x = as.matrix(x), y = y, kernel = rbfdot, 
            kpar = list(sigma = param$sigma), class.weights = wts, 
            C = param$C, prob.model = classProbs, ...)
    }
    out
}

$svmRadialWeights$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- predict(modelFit, newdata)
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmRadialWeights$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata, type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmRadialWeights$predictors
function (x, ...) 
{
    if (hasTerms(x) & !is.null(x@terms)) {
        out <- predictors.terms(x@terms)
    }
    else {
        out <- colnames(attr(x, "xmatrix"))
    }
    if (is.null(out)) 
        out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if (is.null(out)) 
        out <- NA
    out
}

$svmRadialWeights$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "Radial Basis Function"   "Cost Sensitive Learning"
[5] "Two Class Only"         

$svmRadialWeights$levels
function (x) 
lev(x)

$svmRadialWeights$sort
function (x) 
x[order(x$C, -x$sigma, x$Weight), ]


$svmSpectrumString
$svmSpectrumString$label
[1] "Support Vector Machines with Spectrum String Kernel"

$svmSpectrumString$library
[1] "kernlab"

$svmSpectrumString$type
[1] "Regression"     "Classification"

$svmSpectrumString$parameters
  parameter   class  label
1    length numeric length
2         C numeric   Cost

$svmSpectrumString$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(length = 2:(len + 1), C = 2^((1:len) - 
            3))
    }
    else {
        out <- data.frame(length = sample(1:20, size = len, replace = TRUE), 
            C = 2^runif(len, min = -5, max = 10))
    }
    out
}

$svmSpectrumString$loop
NULL

$svmSpectrumString$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (any(names(list(...)) == "prob.model") | is.numeric(y)) {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "spectrum", 
            length = param$length), C = param$C, ...)
    }
    else {
        out <- ksvm(x = x[, 1], y = y, kernel = stringdot, kpar = list(type = "spectrum", 
            length = param$length), C = param$C, prob.model = classProbs, 
            ...)
    }
    out
}

$svmSpectrumString$predict
function (modelFit, newdata, submodels = NULL) 
{
    svmPred <- function(obj, x) {
        hasPM <- !is.null(unlist(obj@prob.model))
        if (hasPM) {
            pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                1, which.max)]
        }
        else pred <- predict(obj, x)
        pred
    }
    out <- try(svmPred(modelFit, newdata[, 1]), silent = TRUE)
    if (is.character(lev(modelFit))) {
        if (class(out)[1] == "try-error") {
            warning("kernlab class prediction calculations failed; returning NAs")
            out <- rep("", nrow(newdata))
            out[seq(along = out)] <- NA
        }
    }
    else {
        if (class(out)[1] == "try-error") {
            warning("kernlab prediction calculations failed; returning NAs")
            out <- rep(NA, nrow(newdata))
        }
    }
    if (is.matrix(out)) 
        out <- out[, 1]
    out
}

$svmSpectrumString$prob
function (modelFit, newdata, submodels = NULL) 
{
    out <- try(predict(modelFit, newdata[, 1], type = "probabilities"), 
        silent = TRUE)
    if (class(out)[1] != "try-error") {
        if (any(out < 0)) {
            out[out < 0] <- 0
            out <- t(apply(out, 1, function(x) x/sum(x)))
        }
        out <- out[, lev(modelFit), drop = FALSE]
    }
    else {
        warning("kernlab class probability calculations failed; returning NAs")
        out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), 
            ncol = length(lev(modelFit)))
        colnames(out) <- lev(modelFit)
    }
    out
}

$svmSpectrumString$predictors
function (x, ...) 
{
    NA
}

$svmSpectrumString$tags
[1] "Kernel Method"           "Support Vector Machines"
[3] "String Kernel"           "Robust Methods"         
[5] "Text Mining"            

$svmSpectrumString$levels
function (x) 
lev(x)

$svmSpectrumString$sort
function (x) 
{
    x[order(x$C, -x$length), ]
}


$tan
$tan$label
[1] "Tree Augmented Naive Bayes Classifier"

$tan$library
[1] "bnclassify"

$tan$type
[1] "Classification"

$tan$parameters
  parameter     class               label
1     score character      Score Function
2    smooth   numeric Smoothing Parameter

$tan$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(score = c("loglik", "bic", "aic"), 
            smooth = 0:(len - 1))
    }
    else {
        out <- data.frame(smooth = runif(len, min = 0, max = 10), 
            score = sample(c("loglik", "bic", "aic"), size = len, 
                replace = TRUE))
    }
    out
}

$tan$loop
NULL

$tan$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    bnc("tan_cl", class = ".outcome", dataset = dat, smooth = param$smooth, 
        dag_args = list(score = as.character(param$score)), ...)
}

$tan$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$tan$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$tan$levels
function (x) 
x$obsLevels

$tan$predictors
function (x, s = NULL, ...) 
x$xNames

$tan$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$tan$sort
function (x) 
x[order(x[, 1]), ]


$tanSearch
$tanSearch$label
[1] "Tree Augmented Naive Bayes Classifier Structure Learner Wrapper"

$tanSearch$library
[1] "bnclassify"

$tanSearch$type
[1] "Classification"

$tanSearch$parameters
     parameter   class                        label
1            k numeric                       #Folds
2      epsilon numeric Minimum Absolute Improvement
3       smooth numeric          Smoothing Parameter
4 final_smooth numeric    Final Smoothing Parameter
5           sp logical                 Super-Parent

$tanSearch$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(k = 10, epsilon = 0.01, smooth = 0.01, 
            final_smooth = 1, sp = c(TRUE, FALSE))
    }
    else {
        out <- data.frame(k = sample(3:10, size = len, replace = TRUE), 
            epsilon = runif(len, min = 0, max = 0.05), smooth = runif(len, 
                min = 0, max = 10), final_smooth = runif(len, 
                min = 0, max = 10), sp = sample(c(TRUE, FALSE), 
                size = len, replace = TRUE))
    }
    out
}

$tanSearch$loop
NULL

$tanSearch$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (param$sp) {
        struct <- tan_hcsp(class = ".outcome", dataset = dat, 
            k = param$k, epsilon = param$epsilon, smooth = param$smooth, 
            ...)
    }
    else {
        struct <- tan_hc(class = ".outcome", dataset = dat, k = param$k, 
            epsilon = param$epsilon, smooth = param$smooth, ...)
    }
    lp(struct, dat, smooth = param$final_smooth, ...)
}

$tanSearch$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$tanSearch$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, prob = TRUE)
}

$tanSearch$levels
function (x) 
x$obsLevels

$tanSearch$predictors
function (x, s = NULL, ...) 
x$xNames

$tanSearch$tags
[1] "Bayesian Model"              "Categorical Predictors Only"

$tanSearch$sort
function (x) 
x[order(x[, 1]), ]


$treebag
$treebag$label
[1] "Bagged CART"

$treebag$library
[1] "ipred" "plyr"  "e1071"

$treebag$loop
NULL

$treebag$type
[1] "Regression"     "Classification"

$treebag$parameters
  parameter     class     label
1 parameter character parameter

$treebag$grid
function (x, y, len = NULL, search = "grid") 
data.frame(parameter = "none")

$treebag$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (!any(names(theDots) == "keepX")) 
        theDots$keepX <- FALSE
    modelArgs <- c(list(X = x, y = y), theDots)
    if (!is.null(wts)) 
        modelArgs$weights <- wts
    do.call("ipredbagg", modelArgs)
}

$treebag$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata)

$treebag$prob
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, newdata, type = "prob")

$treebag$predictors
function (x, surrogate = TRUE, ...) 
{
    code <- getModelInfo("rpart", regex = FALSE)[[1]]$predictors
    eachTree <- lapply(x$mtree, function(u, surr) code(u$btree, 
        surrogate = surr), surr = surrogate)
    unique(unlist(eachTree))
}

$treebag$varImp
function (object, ...) 
{
    allImp <- lapply(object$mtrees, function(x) varImp(x$btree), 
        ...)
    allImp <- lapply(allImp, function(x) {
        x$variable <- rownames(x)
        x
    })
    allImp <- do.call("rbind", allImp)
    meanImp <- ddply(allImp, .(variable), function(x) c(Overall = mean(x$Overall)))
    out <- data.frame(Overall = meanImp$Overall)
    rownames(out) <- meanImp$variable
    out
}

$treebag$trim
function (x) 
{
    trim_rpart <- function(x) {
        x$call <- list(na.action = (x$call)$na.action)
        x$x <- NULL
        x$y <- NULL
        x$where <- NULL
        x
    }
    x$mtrees <- lapply(x$mtrees, function(x) {
        x$bindx <- NULL
        x$btree <- trim_rpart(x$btree)
        x
    })
    x
}

$treebag$tags
[1] "Tree-Based Model"     "Ensemble Model"       "Bagging"             
[4] "Accepts Case Weights"

$treebag$levels
function (x) 
levels(x$y)

$treebag$sort
function (x) 
x

$treebag$oob
function (x) 
{
    if (is.null(x$X)) 
        stop("to get OOB stats, keepX must be TRUE when calling the bagging function")
    foo <- function(object, y, x) {
        holdY <- y[-object$bindx]
        tmp_x <- x[-object$bindx, , drop = FALSE]
        if (!is.data.frame(tmp_x)) 
            tmp_x <- as.data.frame(tmp_x)
        if (is.factor(y)) {
            tmp <- predict(object$btree, tmp_x, type = "class")
            tmp <- factor(as.character(tmp), levels = levels(y))
            out <- c(mean(holdY == tmp), e1071::classAgreement(table(holdY, 
                tmp))$kappa)
        }
        else {
            tmp <- predict(object$btree, tmp_x)
            out <- c(sqrt(mean((tmp - holdY)^2, na.rm = TRUE)), 
                cor(holdY, tmp, use = "pairwise.complete.obs")^2)
        }
        out
    }
    eachStat <- lapply(x$mtrees, foo, y = x$y, x = x$X)
    eachStat <- matrix(unlist(eachStat), nrow = length(eachStat[[1]]))
    out <- c(apply(eachStat, 1, mean, na.rm = TRUE), apply(eachStat, 
        1, sd, na.rm = TRUE))
    names(out) <- if (is.factor(x$y)) 
        c("Accuracy", "Kappa", "AccuracySD", "KappaSD")
    else c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
    out
}


$vbmpRadial
$vbmpRadial$label
[1] "Variational Bayesian Multinomial Probit Regression"

$vbmpRadial$library
[1] "vbmp"

$vbmpRadial$loop
NULL

$vbmpRadial$type
[1] "Classification"

$vbmpRadial$parameters
      parameter     class           label
1 estimateTheta character Theta Estimated

$vbmpRadial$grid
function (x, y, len = NULL, search = "grid") 
data.frame(estimateTheta = "yes")

$vbmpRadial$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$bThetaEstimate <- ifelse(param$estimateTheta == 
            "Yes", TRUE, FALSE)
        ctl <- theDots$control
        theDots$control <- NULL
    }
    else ctl <- list(bThetaEstimate = ifelse(param$estimateTheta == 
        "Yes", TRUE, FALSE))
    if (any(names(theDots) == "theta")) {
        theta <- theDots$theta
        theDots$theta <- NULL
    }
    else theta <- runif(ncol(x))
    vbmp(x, as.numeric(y), theta = theta, control = ctl, X.TEST = x[1, 
        ], t.class.TEST = as.numeric(y)[1])
}

$vbmpRadial$predict
function (modelFit, newdata, submodels = NULL) 
{
    probs <- predictCPP(modelFit, newdata)
    modelFit$obsLevels[apply(probs, 1, which.max)]
}

$vbmpRadial$prob
function (modelFit, newdata, submodels = NULL) 
{
    probs <- predictCPP(modelFit, newdata)
    colnames(probs) <- modelFit$obsLevels
    probs
}

$vbmpRadial$levels
function (x) 
x$obsLevels

$vbmpRadial$tags
[1] "Gaussian Process"      "Bayesian Model"        "Radial Basis Function"

$vbmpRadial$sort
function (x) 
x


$vglmAdjCat
$vglmAdjCat$label
[1] "Adjacent Categories Probability Model for Ordinal Data"

$vglmAdjCat$library
[1] "VGAM"

$vglmAdjCat$loop
NULL

$vglmAdjCat$type
[1] "Classification"

$vglmAdjCat$parameters
  parameter     class           label
1  parallel   logical Parallel Curves
2      link character   Link Function

$vglmAdjCat$grid
function (x, y, len = NULL, search = "grid") 
{
    links <- c("loge")
    if (search == "grid") {
        out <- expand.grid(parallel = c(TRUE, FALSE), link = links)
    }
    else {
        out <- data.frame(parallel = sample(c(TRUE, FALSE), size = len, 
            replace = TRUE), links = sample(links, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), , drop = FALSE]
}

$vglmAdjCat$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "family")) {
        stop(paste("The `family` argument cannot be pass from `train` to `vglm`.", 
            "If you need to change the values of `reverse`, multiple.responses`", 
            "or `whitespace` you will have to use a custom model (see", 
            "http://topepo.github.io/caret/custom_models.html for details)."))
    }
    fam <- do.call("cumulative", list(link = as.character(param$link), 
        parallel = param$parallel))
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "acat(link = '", .lnk, 
                "', ", "parallel = ", .par, "), ", "data = dat)", 
                sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    else {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "acat(link = '", .lnk, 
                "', ", "parallel = ", .par, "), weights = wts,", 
                "data = dat)", sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    out
}

$vglmAdjCat$predict
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata = newdata, type = "response")
    ordered(modelFit@misc$ynames[apply(out, 1, which.max)], levels = modelFit@misc$ynames)
}

$vglmAdjCat$prob
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata = newdata, type = "response")
}

$vglmAdjCat$varImp
NULL

$vglmAdjCat$predictors
function (x, ...) 
caret:::predictors.terms(x@terms$terms)

$vglmAdjCat$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$vglmAdjCat$tags
[1] "Logistic Regression"  "Linear Classifier"    "Accepts Case Weights"
[4] "Ordinal Outcomes"    

$vglmAdjCat$sort
function (x) 
x


$vglmContRatio
$vglmContRatio$label
[1] "Continuation Ratio Model for Ordinal Data"

$vglmContRatio$library
[1] "VGAM"

$vglmContRatio$loop
NULL

$vglmContRatio$type
[1] "Classification"

$vglmContRatio$parameters
  parameter     class           label
1  parallel   logical Parallel Curves
2      link character   Link Function

$vglmContRatio$grid
function (x, y, len = NULL, search = "grid") 
{
    links <- c("logit", "probit", "cloglog", "cauchit", "logc")
    if (search == "grid") {
        out <- expand.grid(parallel = c(TRUE, FALSE), link = links)
    }
    else {
        out <- data.frame(parallel = sample(c(TRUE, FALSE), size = len, 
            replace = TRUE), links = sample(links, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), , drop = FALSE]
}

$vglmContRatio$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "family")) {
        stop(paste("The `family` argument cannot be pass from `train` to `vglm`.", 
            "If you need to change the values of `reverse`", 
            "or `whitespace` you will have to use a custom model (see", 
            "http://topepo.github.io/caret/custom_models.html for details)."))
    }
    fam <- do.call("cumulative", list(link = as.character(param$link), 
        parallel = param$parallel))
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "cratio(link = '", .lnk, 
                "', ", "parallel = ", .par, "), ", "data = dat)", 
                sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    else {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "cratio(link = '", .lnk, 
                "', ", "parallel = ", .par, "), weights = wts,", 
                "data = dat)", sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    out
}

$vglmContRatio$predict
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata = newdata, type = "response")
    colnames(out) <- modelFit@misc$ynames
    ordered(modelFit@misc$ynames[apply(out, 1, which.max)], levels = modelFit@misc$ynames)
}

$vglmContRatio$prob
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata = newdata, type = "response")
    if (is.matrix(out)) 
        out <- as.data.frame(out)
    colnames(out) <- modelFit@misc$ynames
    out
}

$vglmContRatio$varImp
NULL

$vglmContRatio$predictors
function (x, ...) 
caret:::predictors.terms(x@terms$terms)

$vglmContRatio$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$vglmContRatio$tags
[1] "Logistic Regression"  "Linear Classifier"    "Accepts Case Weights"
[4] "Ordinal Outcomes"    

$vglmContRatio$sort
function (x) 
x


$vglmCumulative
$vglmCumulative$label
[1] "Cumulative Probability Model for Ordinal Data"

$vglmCumulative$library
[1] "VGAM"

$vglmCumulative$loop
NULL

$vglmCumulative$type
[1] "Classification"

$vglmCumulative$parameters
  parameter     class           label
1  parallel   logical Parallel Curves
2      link character   Link Function

$vglmCumulative$grid
function (x, y, len = NULL, search = "grid") 
{
    links <- c("logit", "probit", "cloglog", "cauchit", "logc")
    if (search == "grid") {
        out <- expand.grid(parallel = c(TRUE, FALSE), link = links)
    }
    else {
        out <- data.frame(parallel = sample(c(TRUE, FALSE), size = len, 
            replace = TRUE), links = sample(links, size = len, 
            replace = TRUE))
    }
    out[!duplicated(out), , drop = FALSE]
}

$vglmCumulative$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    theDots <- list(...)
    if (any(names(theDots) == "family")) {
        stop(paste("The `family` argument cannot be pass from `train` to `vglm`.", 
            "If you need to change the values of `reverse`, multiple.responses`", 
            "or `whitespace` you will have to use a custom model (see", 
            "http://topepo.github.io/caret/custom_models.html for details)."))
    }
    fam <- do.call("cumulative", list(link = as.character(param$link), 
        parallel = param$parallel))
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    if (!is.null(wts)) {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "cumulative(link = '", 
                .lnk, "', ", "parallel = ", .par, "), ", "data = dat)", 
                sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    else {
        run_this <- eval(substitute(expression({
            paste("vglm(.outcome ~ ., ", "cumulative(link = '", 
                .lnk, "', ", "parallel = ", .par, "), weights = wts,", 
                "data = dat)", sep = "")
        }), list(.par = param$parallel, .lnk = as.character(param$link))))
        run_this <- eval(run_this)
        out <- eval(parse(text = run_this))
    }
    out
}

$vglmCumulative$predict
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    out <- predict(modelFit, newdata = newdata, type = "response")
    ordered(modelFit@misc$ynames[apply(out, 1, which.max)], levels = modelFit@misc$ynames)
}

$vglmCumulative$prob
function (modelFit, newdata, preProc = NULL, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata = newdata, type = "response")
}

$vglmCumulative$varImp
NULL

$vglmCumulative$predictors
function (x, ...) 
caret:::predictors.terms(x@terms$terms)

$vglmCumulative$levels
function (x) 
if (any(names(x) == "obsLevels")) x$obsLevels else NULL

$vglmCumulative$tags
[1] "Logistic Regression"  "Linear Classifier"    "Accepts Case Weights"
[4] "Ordinal Outcomes"    

$vglmCumulative$sort
function (x) 
x


$widekernelpls
$widekernelpls$label
[1] "Partial Least Squares"

$widekernelpls$library
[1] "pls"

$widekernelpls$type
[1] "Regression"     "Classification"

$widekernelpls$parameters
  parameter   class       label
1     ncomp numeric #Components

$widekernelpls$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), 
            by = 1))
    }
    else {
        out <- data.frame(ncomp = unique(sample(1:(ncol(x) - 
            1), size = len, replace = TRUE)))
    }
    out
}

$widekernelpls$loop
function (grid) 
{
    grid <- grid[order(grid$ncomp, decreasing = TRUE), , drop = FALSE]
    loop <- grid[1, , drop = FALSE]
    submodels <- list(grid[-1, , drop = FALSE])
    list(loop = loop, submodels = submodels)
}

$widekernelpls$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    out <- if (is.factor(y)) {
        plsda(x, y, method = "oscorespls", ncomp = param$ncomp, 
            ...)
    }
    else {
        dat <- if (is.data.frame(x)) 
            x
        else as.data.frame(x)
        dat$.outcome <- y
        plsr(.outcome ~ ., data = dat, method = "widekernelpls", 
            ncomp = param$ncomp, ...)
    }
    out
}

$widekernelpls$predict
function (modelFit, newdata, submodels = NULL) 
{
    out <- if (modelFit$problemType == "Classification") {
        if (!is.matrix(newdata)) 
            newdata <- as.matrix(newdata)
        out <- predict(modelFit, newdata, type = "class")
    }
    else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels))
        if (modelFit$problemType == "Classification") {
            if (length(submodels$ncomp) > 1) {
                tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
            }
            else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))
        }
        else {
            tmp <- as.list(as.data.frame(apply(predict(modelFit, 
                newdata, ncomp = submodels$ncomp), 3, function(x) list(x))))
        }
        out <- c(list(out), tmp)
    }
    out
}

$widekernelpls$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.matrix(newdata)) 
        newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, type = "prob", ncomp = modelFit$tuneValue$ncomp)
    if (length(dim(out)) == 3) {
        if (dim(out)[1] > 1) {
            out <- out[, , 1]
        }
        else {
            out <- as.data.frame(t(out[, , 1]))
        }
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$ncomp)) {
            tmpProb <- predict(modelFit, newdata, type = "prob", 
                ncomp = submodels$ncomp[j])
            if (length(dim(tmpProb)) == 3) {
                if (dim(tmpProb)[1] > 1) {
                  tmpProb <- tmpProb[, , 1]
                }
                else {
                  tmpProb <- as.data.frame(t(tmpProb[, , 1]))
                }
            }
            tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, 
                drop = FALSE])
        }
        out <- tmp
    }
    out
}

$widekernelpls$predictors
function (x, ...) 
rownames(x$projection)

$widekernelpls$varImp
function (object, estimate = NULL, ...) 
{
    modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
    perf <- MSEP(object)$val
    nms <- dimnames(perf)
    if (length(nms$estimate) > 1) {
        pIndex <- if (is.null(estimate)) 
            1
        else which(nms$estimate == estimate)
        perf <- perf[pIndex, , , drop = FALSE]
    }
    numResp <- dim(modelCoef)[2]
    if (numResp <= 2) {
        modelCoef <- modelCoef[, 1, , drop = FALSE]
        perf <- perf[, 1, ]
        delta <- -diff(perf)
        delta <- delta/sum(delta)
        out <- data.frame(Overall = apply(abs(modelCoef), 1, 
            weighted.mean, w = delta))
    }
    else {
        perf <- -t(apply(perf[1, , ], 1, diff))
        perf <- t(apply(perf, 1, function(u) u/sum(u)))
        out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])
        for (i in 1:numResp) {
            tmp <- abs(modelCoef[, i, , drop = FALSE])
            out[, i] <- apply(tmp, 1, weighted.mean, w = perf[i, 
                ])
        }
        colnames(out) <- dimnames(modelCoef)[[2]]
        rownames(out) <- dimnames(modelCoef)[[1]]
    }
    as.data.frame(out)
}

$widekernelpls$levels
function (x) 
x$obsLevels

$widekernelpls$tags
[1] "Partial Least Squares" "Feature Extraction"    "Linear Classifier"    
[4] "Linear Regression"    

$widekernelpls$sort
function (x) 
x[order(x[, 1]), ]


$WM
$WM$label
[1] "Wang and Mendel Fuzzy Rules"

$WM$library
[1] "frbs"

$WM$type
[1] "Regression"

$WM$parameters
   parameter     class               label
1 num.labels   numeric        #Fuzzy Terms
2    type.mf character Membership Function

$WM$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(num.labels = 1 + (1:len) * 2, type.mf = c("GAUSSIAN", 
            "TRAPEZOID", "TRIANGLE"))
    }
    else {
        out <- data.frame(num.labels = sample(2:20, size = len, 
            replace = TRUE), type.mf = sample(c("GAUSSIAN", "TRAPEZOID", 
            "TRIANGLE"), size = len, replace = TRUE))
    }
    out
}

$WM$loop
NULL

$WM$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    args <- list(data.train = as.matrix(cbind(x, y)))
    args$range.data <- apply(args$data.train, 2, extendrange)
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
        theDots$control$num.labels <- param$num.labels
        theDots$control$type.mf <- param$type.mf
    }
    else theDots$control <- list(num.labels = param$num.labels, 
        type.mf = param$type.mf, type.defuz = "WAM", type.tnorm = "MIN", 
        type.snorm = "MAX", type.implication.func = "ZADEH", 
        name = "sim-0")
    do.call("frbs.learn", c(args, theDots))
}

$WM$predict
function (modelFit, newdata, submodels = NULL) 
{
    predict(modelFit, newdata)
}

$WM$prob
NULL

$WM$predictors
function (x, ...) 
{
    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
}

$WM$tags
[1] "Rule-Based Model"

$WM$levels
NULL

$WM$sort
function (x) 
x[order(x$num.labels), ]


$wsrf
$wsrf$label
[1] "Weighted Subspace Random Forest"

$wsrf$library
[1] "wsrf"

$wsrf$loop
NULL

$wsrf$type
[1] "Classification"

$wsrf$parameters
  parameter   class                         label
1      mtry numeric #Randomly Selected Predictors

$wsrf$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
            classification = is.factor(y), len = len))
    }
    else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, 
            replace = TRUE)))
    }
    out
}

$wsrf$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    wsrf(.outcome ~ ., data = dat, mtry = param$mtry, ...)
}

$wsrf$predict
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata)
}

$wsrf$prob
function (modelFit, newdata, submodels = NULL) 
{
    if (!is.data.frame(newdata)) 
        newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
}

$wsrf$predictors
function (x, ...) 
x$xNames

$wsrf$varImp
NULL

$wsrf$levels
function (x) 
x$obsLevels

$wsrf$tags
[1] "Random Forest"              "Ensemble Model"            
[3] "Bagging"                    "Implicit Feature Selection"

$wsrf$sort
function (x) 
x[order(x[, 1]), ]


$xgbLinear
$xgbLinear$label
[1] "eXtreme Gradient Boosting"

$xgbLinear$library
[1] "xgboost"

$xgbLinear$type
[1] "Regression"     "Classification"

$xgbLinear$parameters
  parameter   class                 label
1   nrounds numeric # Boosting Iterations
2    lambda numeric     L2 Regularization
3     alpha numeric     L1 Regularization
4       eta numeric         Learning Rate

$xgbLinear$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 
            1)), alpha = c(0, 10^seq(-1, -4, length = len - 1)), 
            nrounds = floor((1:len) * 50), eta = 0.3)
    }
    else {
        out <- data.frame(lambda = 10^runif(len, min = -5, 0), 
            alpha = 10^runif(len, min = -5, 0), nrounds = sample(1:100, 
                size = len, replace = TRUE), eta = runif(len, 
                max = 3))
    }
    out
}

$xgbLinear$loop
NULL

$xgbLinear$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        if (length(lev) == 2) {
            y <- ifelse(y == lev[1], 1, 0)
            dat <- xgb.DMatrix(as.matrix(x), label = y)
            out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
                data = dat, nrounds = param$nrounds, objective = "binary:logistic", 
                ...)
        }
        else {
            y <- as.numeric(y) - 1
            dat <- xgb.DMatrix(as.matrix(x), label = y)
            out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
                data = dat, num_class = length(lev), nrounds = param$nrounds, 
                objective = "multi:softprob", ...)
        }
    }
    else {
        dat <- xgb.DMatrix(as.matrix(x), label = y)
        out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
            data = dat, nrounds = param$nrounds, objective = "reg:linear", 
            ...)
    }
    out
}

$xgbLinear$predict
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- xgb.DMatrix(as.matrix(newdata))
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        if (length(modelFit$obsLevels) == 2) {
            out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
                modelFit$obsLevels[2])
        }
        else {
            out <- matrix(out, ncol = length(modelFit$obsLevels), 
                byrow = TRUE)
            out <- modelFit$obsLevels[apply(out, 1, which.max)]
        }
    }
    out
}

$xgbLinear$prob
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- xgb.DMatrix(as.matrix(newdata))
    out <- predict(modelFit, newdata)
    if (length(modelFit$obsLevels) == 2) {
        out <- cbind(out, 1 - out)
        colnames(out) <- modelFit$obsLevels
    }
    else {
        out <- matrix(out, ncol = length(modelFit$obsLevels), 
            byrow = TRUE)
        colnames(out) <- modelFit$obsLevels
    }
    as.data.frame(out)
}

$xgbLinear$predictors
function (x, ...) 
{
    imp <- xgb.importance(x$xNames, model = x)
    x$xNames[x$xNames %in% imp$Feature]
}

$xgbLinear$varImp
function (object, numTrees = NULL, ...) 
{
    imp <- xgb.importance(object$xNames, model = object)
    imp <- as.data.frame(imp)[, 1:2]
    rownames(imp) <- as.character(imp[, 1])
    imp <- imp[, 2, drop = FALSE]
    colnames(imp) <- "Overall"
    imp
}

$xgbLinear$levels
function (x) 
x$obsLevels

$xgbLinear$tags
[1] "Linear Classifier Models"   "Linear Regression Models"  
[3] "L1 Regularization Models"   "L2 Regularization Models"  
[5] "Boosting"                   "Ensemble Model"            
[7] "Implicit Feature Selection"

$xgbLinear$sort
function (x) 
{
    x[order(x$nrounds, x$alpha, x$lambda), ]
}


$xgbTree
$xgbTree$label
[1] "eXtreme Gradient Boosting"

$xgbTree$library
[1] "xgboost" "plyr"   

$xgbTree$type
[1] "Regression"     "Classification"

$xgbTree$parameters
         parameter   class                          label
1          nrounds numeric          # Boosting Iterations
2        max_depth numeric                 Max Tree Depth
3              eta numeric                      Shrinkage
4            gamma numeric         Minimum Loss Reduction
5 colsample_bytree numeric     Subsample Ratio of Columns
6 min_child_weight numeric Minimum Sum of Instance Weight

$xgbTree$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 
            50), eta = c(0.3, 0.4), gamma = 0, colsample_bytree = c(0.6, 
            0.8), min_child_weight = c(1))
    }
    else {
        out <- data.frame(nrounds = sample(1:1000, size = len, 
            replace = TRUE), max_depth = sample(1:10, replace = TRUE, 
            size = len), eta = runif(len, min = 0.001, max = 0.6), 
            gamma = runif(len, min = 0, max = 10), colsample_bytree = runif(len, 
                min = 0.3, max = 0.7), min_child_weight = sample(0:20, 
                size = len, replace = TRUE))
        out$nrounds <- floor(out$nrounds)
        out <- out[!duplicated(out), ]
    }
    out
}

$xgbTree$loop
function (grid) 
{
    loop <- ddply(grid, c("eta", "max_depth", "gamma", "colsample_bytree", 
        "min_child_weight"), function(x) c(nrounds = max(x$nrounds)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$nrounds)) {
        index <- which(grid$max_depth == loop$max_depth[i] & 
            grid$eta == loop$eta[i] & grid$gamma == loop$gamma[i] & 
            grid$colsample_bytree == loop$colsample_bytree[i] & 
            grid$min_child_weight == loop$min_child_weight[i])
        trees <- grid[index, "nrounds"]
        submodels[[i]] <- data.frame(nrounds = trees[trees != 
            loop$nrounds[i]])
    }
    list(loop = loop, submodels = submodels)
}

$xgbTree$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
{
    if (is.factor(y)) {
        if (length(lev) == 2) {
            y <- ifelse(y == lev[1], 1, 0)
            dat <- xgb.DMatrix(as.matrix(x), label = y)
            out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
                gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
                min_child_weight = param$min_child_weight), data = dat, 
                nrounds = param$nrounds, objective = "binary:logistic", 
                ...)
        }
        else {
            y <- as.numeric(y) - 1
            dat <- xgb.DMatrix(as.matrix(x), label = y)
            out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
                gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
                min_child_weight = param$min_child_weight), data = dat, 
                num_class = length(lev), nrounds = param$nrounds, 
                objective = "multi:softprob", ...)
        }
    }
    else {
        dat <- xgb.DMatrix(as.matrix(x), label = y)
        out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
            gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
            min_child_weight = param$min_child_weight), data = dat, 
            nrounds = param$nrounds, objective = "reg:linear", 
            ...)
    }
    out
}

$xgbTree$predict
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- xgb.DMatrix(as.matrix(newdata))
    out <- predict(modelFit, newdata)
    if (modelFit$problemType == "Classification") {
        if (length(modelFit$obsLevels) == 2) {
            out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
                modelFit$obsLevels[2])
        }
        else {
            out <- matrix(out, ncol = length(modelFit$obsLevels), 
                byrow = TRUE)
            out <- modelFit$obsLevels[apply(out, 1, which.max)]
        }
    }
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nrounds)) {
            tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
            if (modelFit$problemType == "Classification") {
                if (length(modelFit$obsLevels) == 2) {
                  tmp_pred <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
                    modelFit$obsLevels[2])
                }
                else {
                  tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
                    byrow = TRUE)
                  tmp_pred <- modelFit$obsLevels[apply(tmp_pred, 
                    1, which.max)]
                }
            }
            tmp[[j + 1]] <- tmp_pred
        }
        out <- tmp
    }
    out
}

$xgbTree$prob
function (modelFit, newdata, submodels = NULL) 
{
    newdata <- xgb.DMatrix(as.matrix(newdata))
    out <- predict(modelFit, newdata)
    if (length(modelFit$obsLevels) == 2) {
        out <- cbind(out, 1 - out)
        colnames(out) <- modelFit$obsLevels
    }
    else {
        out <- matrix(out, ncol = length(modelFit$obsLevels), 
            byrow = TRUE)
        colnames(out) <- modelFit$obsLevels
    }
    out <- as.data.frame(out)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nrounds)) {
            tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
            if (length(modelFit$obsLevels) == 2) {
                tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
                colnames(tmp_pred) <- modelFit$obsLevels
            }
            else {
                tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
                  byrow = TRUE)
                colnames(tmp_pred) <- modelFit$obsLevels
            }
            tmp_pred <- as.data.frame(tmp_pred)
            tmp[[j + 1]] <- tmp_pred
        }
        out <- tmp
    }
    out
}

$xgbTree$predictors
function (x, ...) 
{
    imp <- xgb.importance(x$xNames, model = x)
    x$xNames[x$xNames %in% imp$Feature]
}

$xgbTree$varImp
function (object, numTrees = NULL, ...) 
{
    imp <- xgb.importance(object$xNames, model = object)
    imp <- as.data.frame(imp)[, 1:2]
    rownames(imp) <- as.character(imp[, 1])
    imp <- imp[, 2, drop = FALSE]
    colnames(imp) <- "Overall"
    imp
}

$xgbTree$levels
function (x) 
x$obsLevels

$xgbTree$tags
[1] "Tree-Based Model"           "Boosting"                  
[3] "Ensemble Model"             "Implicit Feature Selection"

$xgbTree$sort
function (x) 
{
    x[order(x$nrounds, x$max_depth, x$eta, x$gamma, x$colsample_bytree, 
        x$min_child_weight), ]
}


$xyf
$xyf$label
[1] "Self-Organizing Maps"

$xyf$library
[1] "kohonen"

$xyf$loop
NULL

$xyf$type
[1] "Classification" "Regression"    

$xyf$parameters
  parameter     class    label
1      xdim   numeric      Row
2      ydim   numeric  Columns
3   xweight   numeric X Weight
4      topo character Topology

$xyf$grid
function (x, y, len = NULL, search = "grid") 
{
    if (search == "grid") {
        out <- expand.grid(xdim = 1:len, ydim = 2:(len + 1), 
            xweight = seq(0.5, 0.9, length = len), topo = "hexagonal")
        out <- subset(out, xdim <= ydim)
    }
    else {
        out <- data.frame(xdim = sample(1:len, size = len * 10, 
            replace = TRUE), ydim = sample(1:len, size = len * 
            10, replace = TRUE), topo = sample(c("rectangular", 
            "hexagonal"), size = len * 10, replace = TRUE), xweight = runif(len * 
            10, min = 0.5, max = 1))
        out <- subset(out, xdim <= ydim & xdim * ydim < nrow(x))
        out <- out[1:max(nrow(out), len), ]
    }
    out
}

$xyf$fit
function (x, y, wts, param, lev, last, classProbs, ...) 
xyf(as.matrix(x), Y = if (is.factor(y)) classvec2classmat(y) else y, 
    xweight = param$xweight, contin = !is.factor(y), grid = somgrid(param$xdim, 
        param$ydim, as.character(param$topo)), ...)

$xyf$predict
function (modelFit, newdata, submodels = NULL) 
predict(modelFit, as.matrix(newdata))$prediction

$xyf$prob
function (modelFit, newdata, submodels = NULL) 
{
    preds <- predict(modelFit, as.matrix(newdata))
    preds$unit.predictions[preds$unit.classif, , drop = FALSE]
}

$xyf$levels
function (x) 
x$obsLevels

$xyf$tags
[1] "Self-Organising Maps"

$xyf$sort
function (x) 
x[order(x$xdim, x$ydim), ]


> 
> 
