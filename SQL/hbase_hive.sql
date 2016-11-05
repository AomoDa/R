
CREATE TABLE hbase_table_test(key int, s1 string,s2 string,s3 string) 
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:s1,cf1:s2,cf2:s3")   
TBLPROPERTIES ("hbase.table.name" = "hive_table_test");



---
CREATE TABLE hbase_table_1(key int, value string)    
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:val")   
TBLPROPERTIES ("hbase.table.name" = "xyz");




CREATE TABLE hbase_table_test(key int, s1 string,s2 string,s3 string) 
PARTITIONED BY ( 
  `dt` string)
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:s1,cf1:s2,cf2:s3")   
TBLPROPERTIES ("hbase.table.name" = "hive_table_test");



---
CREATE TABLE hbase_table_1(key int, value string)    
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:val")   
TBLPROPERTIES ("hbase.table.name" = "xyz");



--
insert into table hbase_table_test  partition (dt='2')  values(10,20,30,40) ;


---hbase shell


put 'hive_table_test','20','cf2:s3','111'


select goods_id, sum(goods_id) over(partition by goods_id rows between 1 PRECEDING and 1 PRECEDING) from mds_v6_goods_releation





select 
xcontext['deviceid'],
xcontext['currayamount']
from trackdefault.events
where appid='c68024f42b8c11d34f21725bb1c7cf03' 
     and ds between '2016-10-07' and '2016-10-18' and xwhat='payment'





select deviceid
from trackconceptions.payment_bucket
where appid='24b8a1b11c75b4cc0ddbde9316665ac6'
      and channelid='0beb0c86920a01de641fa6cf4f6dcd24' 
      and ds >= '2016-10-07' and ds <= '2016-10-18'
