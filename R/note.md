

##R中有意思的东西

####如何将文本型的命令执行？

结合eval和parse 可以将文本型的命令执行。

```r
eval(parse(text = 'date()'))
```

#### 如何将数值分配到一个文本型的变量？

```
assign('a',value = iris)

assign('a',value = lm(y~x))
 
```
