



--load data
--load data local inpath '/tmp/part-00000'  into table temp_growingio_page partition(dt='2016-07-24')


-- action
create table ods_gio_action (
userid string ,
sessionid string ,
sendtime string ,
eventtime string ,
domain string ,
path string ,
eventtype string ,
eventvalue string ,
href string ,
page_id string ,
action_id string
) 
PARTITIONED BY (dt string) 
ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
WITH SERDEPROPERTIES (
  "separatorChar" = ",",
   "quoteChar"     = '"',
   "escapeChar"    = "\\"
)  
STORED AS TEXTFILE;



--page
create table ods_gio_page (
userid string ,
sessionid string ,
sendtime string ,
eventtime string ,
eventtype string ,
domain string ,
path string ,
query string ,
refer string ,
title string ,
platform string ,
cs1 string ,
cs2 string ,
cs3 string ,
cs4 string ,
cs5 string ,
cs6 string ,
cs7 string ,
cs8 string ,
cs9 string ,
cs10 string ,
page_id string ,
visit_id string
) 
PARTITIONED BY (dt string) 
ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
WITH SERDEPROPERTIES (
  "separatorChar" = ",",
   "quoteChar"     = '"',
   "escapeChar"    = "\\"
)  
STORED AS TEXTFILE;



--visit
create table ods_gio_visit (
userid string ,
sessionid string ,
sendtime string ,
eventtime string ,
eventtype string ,
ip string ,
countryname string ,
region string ,
city string ,
domain string ,
path string ,
refer string ,
useragent string ,
appversion string ,
model string ,
manufacturer string ,
channel string ,
language string ,
osversion string ,
resolution string ,
platform string ,
visit_id string ) 
PARTITIONED BY (dt string) 
ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
WITH SERDEPROPERTIES (
  "separatorChar" = ",",
   "quoteChar"     = '"',
   "escapeChar"    = "\\"
)  
STORED AS TEXTFILE;




