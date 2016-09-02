


--lateral view用于和split, explode等UDTF一起使用，
--它能够将一行数据拆成多行数据，在此基础上可以对拆分后的数据进行聚合。
--lateral view首先为原始表的每行调用UDTF，UTDF会把一行拆分成一或者多行，
--lateral view再把结果组合，产生一个支持别名表的虚拟表。

SELECT * FROM exampleTable LATERAL VIEW explode(col1) myTable1 AS myCol1 LATERAL VIEW explode(myCol1) myTable2 AS myCol2;




select uuid,my_new
from user_access_tag  LATERAL VIEW explode(trace) user_access_tag AS my_new
where uuid ='1159658';

