
# R关于机器学习

机器学习是计算机科学和统计学的边缘交叉领域，R关于机器学习的包主要包括以下几个方面： 

网址：http://cran.r-project.org/web/views/MachineLearning.html维护人员：Torsten Hothorn


##1 神经网络（Neural Networks）：

nnet包执行单隐层前馈神经网络，nnet是VR包的一部分（http://cran.r-project.org/web/packages/VR/index.html）。 

##2 递归拆分（Recursive Partitioning）：


+ rpart包（http://cran.r-project.org/web/packages/rpart/index.html）执行 递归拆分利用树形结构模型，来做回归、分类和生存分析。尤其推荐rpart包。

+ rpart.plot 提供rpart的可视化功能。

+ tree包（http://cran.r-project.org/web/packages/tree/index.html）执行 递归拆分利用树形结构模型，来做回归、分类和生存分析。

+ Weka包 （http://cran.r-project.org/web/packages/RWeka/index.html）里也有这样的递归拆分法，如：J4.8, C4.5, M5，提供了R与Weka的函数的接口。

+ party包提供两类递归拆分算法，能做到无偏的变量选择和停止标准：函数ctree()用非参条件推断法检测自变量和因变量的关系；而函数mob()能用来建立参数模型（http://cran.r-project.org/web/packages/party/index.html）。另外，party包里也提供二分支树和节点分布的可视化展示。 

+ mvpart包是rpart的改进包，处理多元因变量的问题（http://cran.r-project.org/web/packages/mvpart/index.html）。

+ rpart.permutation包用置换法（permutation）评估树的有效性（http://cran.r-project.org/web/packages/rpart.permutation/index.html）。

+ knnTree包建立一个分类树，每个叶子节点是一个knn分类器（http://cran.r-project.org/web/packages/knnTree/index.html）。

+ LogicReg包做逻辑回归分析，针对大多数自变量是二元变量的情况（http://cran.r-project.org/web/packages/LogicReg/index.html）。

+ maptree包（http://cran.r-project.org/web/packages/maptree/index.html）

+ pinktoe包（http://cran.r-project.org/web/packages/pinktoe/index.html）提供树结构的可视化函数。 
