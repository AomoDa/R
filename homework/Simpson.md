---
title: ''
author: '14307110064'
date: "2016年11月16日"
output:
  word_document: default
---




#Q:从 Wilkinson & TFSI (1999)

**1. 不要用假设检验的不显著或显著来支持方差齐性**

It is hard to imagine a situation in which a dichotomous accept-reject decision is better than reporting an actual p value or, better still, a confidence interval. Never use the unfortunate expression "accept the null hypothesis. " Always provide some effectsize estimate when reporting a p value. Cohen (1994) has written on this subject in this journal. All psychologists would benefit from reading his insightful article.

**2. Tukey HSD 之前不需要做 方差分析**

One of the most prevalent strategies psychologists use to handle multiplicity is to follow an ANOVA with pairwise multiple-comparison tests. This approach is usually wrong for several reasons. 

- First, pairwise methods such as Tukey's honestly significant difference procedure were designed to control a familywise error rate based on the sample size and number of comparisons. Preceding them with an omnibus F test in a stagewise testing procedure defeats this design, making it unnecessarily conservative. 
- Second, researchers rarely need to compare all possible means to understand their results or assess their theory; by setting their sights large, they sacrifice their power to see small. 
- Third, the lattice of all possible pairs is a straightjacket; forcing themselves to wear it often restricts researchers to uninteresting hypotheses and induces them to ignore more fruitful ones.





#Q:模拟一个Simpson 悖论 两个系，两个性别


从x系来看，A大学男生比例高，同样的从y系来看，A大学男生比例高，但是总体来看，B大学的男生比例高于A大学。

-----

大学|系|男|女|比例
----|--|--|--|---
**A**|x| 140|   17| **8.2352941**
B|x| 1111|  151| 7.3576159
**A**|y| 153 |1201| **0.1273938**
B|y|   19 | 192|0.09895833
A|all|293| 1218 |0.2405583
**B**|all|1130 | 343| **3.2944606**

-----

```{r,echo = FALSE}
# A大学和B大学分别都有x和y两个系。
A <- data.frame(boy=c(140,153),girl=c(17,1201),row.names = c('x','y'))
B <- data.frame(boy=c(1111,19),girl=c(151,192),row.names = c('x','y'))
AB <- as.data.frame(rbind(colSums(A),colSums(B)))
A$rate <- A$boy/A$girl
B$rate <- B$boy/B$girl
AB$rate <- AB$boy/AB$girl
A
B
AB
``````


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
