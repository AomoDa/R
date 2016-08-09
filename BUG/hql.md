
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

#使用hive的结果：

我的hive版本是：
$ hive --version
\n Hive 1.0.0
\n Subversion file:///Users/vikram/src/hive/commit/release-1.0.0-rc2 -r Unknown
\n Compiled by vikram on Thu Jan 29 16:48:20 PST 2015
\n From source with checksum cddf389e4cbba6b5e6aaee2e82a857f5


## 我的完整的hql语句为：
以下是我的hql语句，以及查询的结果如下图
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

## 错误地方和解析
###子查询的结果：
当我只运行子查询的部分的时候，结果是正确的，子查询的sql和正确的结果如下：
```sql
select userid,
   split(parse_url(refer,'HOST'),"\\.")[0] as refer_page,
   split(domain,"\\.")[0] as first_page,
   split(path,"\\/")[1] as www_path,
   ROW_NUMBER() OVER (PARTITION BY userid ORDER BY sendtime) AS page_num,
   count(1) OVER (PARTITION BY userid )  as n ,
   FIRST_VALUE(split(domain,"\\.")[0]) OVER (PARTITION BY userid ORDER BY sendtime) as first_ddd
   from ods_gio_page
   where dt='2016-08-07' and platform='Web' limit 100;
```
正确的结果:
```csv
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www		10	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www	course	1	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www	course	2	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	NULL	www	course	5	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	NULL	www	course	6	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www	course	7	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www	course	8	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www		9	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	search	course	11	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	search	course	12	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www		13	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	www		14	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	search	course	15	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	www	search	course	16	24	www
6c8e70b5-ebdb-443b-8487-9a5bfe17f599	search	www	course	17	24	www

```

###运行完整的hql：

输出的结果明显是有问题的，我嵌套了一个子查询，然后对子查询的first_page（位于子查询的第三列，是一些字符串）分组计算。
\n 结果是这样样子，貌似分组的是一个sendtime字段。
```csv
1470499200608	1	0
1470499200775	1	0
1470499201348	1	0
1470499201502	1	0
1470499201619	1	0
1470499201987	1	0
1470499202843	1	0
1470499204088	1	0
1470499204952	1	0
1470499205224	1	0
1470499205309	1	0
1470499205916	1	0
1470499205959	1	0

```

## 另外一种方式

我觉得是子查询的问题，所以我创建一个中间表，然后对中间表进行查询：
```sql
create table tmp_gio_drop as
select userid,
   split(parse_url(refer,'HOST'),"\\.")[0] as refer_page,
   split(domain,"\\.")[0] as first_page,
   split(path,"\\/")[1] as www_path,
   ROW_NUMBER() OVER (PARTITION BY userid ORDER BY sendtime) AS page_num,
   count(1) OVER (PARTITION BY userid )  as n ,
   FIRST_VALUE(split(domain,"\\.")[0]) OVER (PARTITION BY userid ORDER BY sendtime) as first_ddd
   from ods_gio_page
   where dt='2016-08-07' and platform='Web'
```
接下来我把完整的sql改成如下：
```sql
select first_page,
count(distinct if(page_num=1,userid,null )),
count(distinct if(n=1,userid,null))
from tmp_gio_drop
group by first_page
```
这下结果就正确了，正确的结果是：

```csv
blog	178	152
e	1047	551
j	1958	1801
jiuye	14017	0
ke	652	209
wenda	1313	1176
wiki	6804	3848
www	14360	4693
```
