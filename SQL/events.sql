CREATE EXTERNAL TABLE `tkiodefault.events2`(
xxxx int 
)
PARTITIONED BY ( 
  `ds` string, 
  `appid` string)
ROW FORMAT SERDE 
  'org.apache.hadoop.hive.serde2.lazy.LazySimpleSerDe' 
WITH SERDEPROPERTIES ( 
  'colelction.delim'='\u0002', 
  'field.delim'='\t', 
  'mapkey.delim'='\u0001', 
  'serialization.format'='\t') 
STORED AS INPUTFORMAT 
  'org.apache.hadoop.mapred.TextInputFormat' 
OUTPUTFORMAT 
  'org.apache.hadoop.hive.ql.io.HiveIgnoreKeyTextOutputFormat'
LOCATION
  'xxxx'
TBLPROPERTIES (
  'transient_lastDdlTime'='1490840216')
