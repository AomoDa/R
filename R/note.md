

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

