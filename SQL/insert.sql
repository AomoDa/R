
create table temp_growingio_page (
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
visit_id string
) 
PARTITIONED BY (dt string) 
ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'



load data local inpath '/tmp/part-00000'  into table temp_growingio_page partition(dt='2016-07-24')

