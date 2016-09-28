p-value of 3.6 using an F(4,43) distribution:
```r
1-pf(3.6,4,3)
# [1] 0.1604624
```
p-value of 2.8 using an t-distribution with the degree of freedom 21
for both one-tailed and two-tailed t-test, respectively:
```r
 1-pt(2.8,21)
# 0.005364828
2*(1-pt(2.8,21))
# [1] 0.01072966
```
p-value of 5.8 using a chi-square distribution with the degree of
freedom 3:
```r
1-pchisq(5.8,3)
# [1] 0.1217566
```

```r

> t.test(rnorm(100),rnorm(100))

	Welch Two Sample t-test

data:  rnorm(100) and rnorm(100)
t = 1.4924, df = 197.66, p-value = 0.1372
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.07048279  0.50911911
sample estimates:
  mean of x   mean of y 
 0.14572071 -0.07359744 

> 2*(1-pt( 1.4924,df = 197.66 ))
[1] 0.1371887
> 
```



```r
 t.test(rnorm(100),rnorm(100))

	Welch Two Sample t-test

data:  rnorm(100) and rnorm(100)
t = -0.78268, df = 190.78, p-value = 0.4348
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.3737408  0.1613970
sample estimates:
  mean of x   mean of y 
-0.04917313  0.05699874 

> 2*(1-pt( -0.78268,df = 190.78 ))
[1] 1.565215
> 2*(pt( -0.78268,df = 190.78 ))
[1] 0.4347855
```
