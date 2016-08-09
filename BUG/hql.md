
#背景说明

我在使用hql查询一个语句的时候，发现查询出来的结果和预期的不一样，似乎是串列了。我的原始数据的结构是：
```csv
userid              	string              	from deserializer   
sessionid           	string              	from deserializer   
sendtime            	string              	from deserializer   
eventtime           	string              	from deserializer   
eventtype           	string              	from deserializer   
domain              	string              	from deserializer   
path                	string              	from deserializer   
query               	string              	from deserializer   
refer               	string              	from deserializer   
title               	string              	from deserializer   
platform            	string              	from deserializer   
cs1                 	string              	from deserializer   
cs2                 	string              	from deserializer   
cs3                 	string              	from deserializer   
cs4                 	string              	from deserializer   
cs5                 	string              	from deserializer   
cs6                 	string              	from deserializer   
cs7                 	string              	from deserializer   
cs8                 	string              	from deserializer   
cs9                 	string              	from deserializer   
cs10                	string              	from deserializer   
page_id             	string              	from deserializer   
visit_id            	string              	from deserializer   
dt                  	string              	                    
	 	 
# Partition Information	 	 
# col_name            	data_type           	comment             
	 	 
dt                  	string              	         
```




以下是我的hql语句。
```sql
select a.first_page,
count(distinct if(a.page_num=1,a.userid,null)) as uv,
count(distinct if(a.n=1,a.userid,null))  as drop_rate
from 
  (select userid,
   split(parse_url(refer,'HOST'),"\\.")[0] as refer_page,
   split(domain,"\\.")[0] as first_page,
   split(path,"\\/")[1] as www_path,
   ROW_NUMBER() OVER (PARTITION BY userid ORDER BY sendtime) AS page_num,
   count(1) OVER (PARTITION BY userid )  as n ,
   FIRST_VALUE(split(domain,"\\.")[0]) OVER (PARTITION BY userid ORDER BY sendtime) as first_ddd
   from ods_gio_page
   where dt='2016-08-07' and platform='Web'
    ) as a
group by a.first_page
```
