
CREATE TABLE hbase_table_test(key int, s1 string,s2 string,s3 string) 
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:s1,cf1:s2,cf2:s3")   
TBLPROPERTIES ("hbase.table.name" = "hive_table_test");



---
CREATE TABLE hbase_table_1(key int, value string)    
STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler'  
WITH SERDEPROPERTIES ("hbase.columns.mapping" = ":key,cf1:val")   
TBLPROPERTIES ("hbase.table.name" = "xyz");
