

##R中有意思的东西

####如何将文本型的命令执行？

结合eval和parse 可以将文本型的命令执行。

```r
eval(parse(text = 'date()'))
```

#### 如何将数值分配到一个文本型的变量？

```R
assign('a',value = iris)

assign('a',value = lm(y~x))
 
```

#### 如何得到一个文本型变量的值？

```r
get('a')
```
####如何生成一个命令？

使用call
```r

eval(call('lm',Petal.Width~.,data=iris))
# lm(Petal.Width~.,data=iris)

call('round',call('sum',55,25)，2) 
#round(sum(55, 25), 2)

eval(call('round',call('sum',55,25),2))#80
```

```r
outlierKD <- function(dt, var) {
     var_name <- eval(substitute(var),eval(dt))
     tot <- sum(!is.na(var_name))
     na1 <- sum(is.na(var_name))
     m1 <- mean(var_name, na.rm = T)
     par(mfrow=c(2, 2), oma=c(0,0,3,0))
     boxplot(var_name, main="With outliers")
     hist(var_name, main="With outliers", xlab=NA, ylab=NA)
     outlier <- boxplot.stats(var_name)$out
     mo <- mean(outlier)
     var_name <- ifelse(var_name %in% outlier, NA, var_name)
     boxplot(var_name, main="Without outliers")
     hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
     title("Outlier Check", outer=TRUE)
     na2 <- sum(is.na(var_name))
     cat("Outliers identified:", na2 - na1, "\n")
     cat("Propotion (%) of outliers:", round((na2 - na1) / tot*100, 1), "\n")
     cat("Mean of the outliers:", round(mo, 2), "\n")
     m2 <- mean(var_name, na.rm = T)
     cat("Mean without removing outliers:", round(m1, 2), "\n")
     cat("Mean if we remove outliers:", round(m2, 2), "\n")
     dt[as.character(substitute(var))] <- invisible(var_name)
     assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
     cat("Outliers successfully removed", "\n")
     return(invisible(dt))
}
```
