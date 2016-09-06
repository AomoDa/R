
####hive一个 TAT为空的BUG，个人认为语法没有错误


```sql
select 
'2016-09-05' as dt,
 a.src_zhiye,
 a.zhiye,
 count(distinct userid) as uv
from
  (
   select
    userid,
    case when refer is null or refer='' 
              then '直接访问'
         when refer regexp 'baidu|sem|seo|google|360|youdao|bing|yahoo|sogou' 
              then '搜索引擎'
         when parse_url(refer,'HOST')='www.jikexueyuan.com' and 
              (parse_url(refer,'PATH') is null or parse_url(refer,'PATH') regexp '^[/]?$' ) 
              then '首页'
         when parse_url(refer,'HOST') like '%jikexueyuan.com%'  
              then  '极客站内'
         else 'Other' end as src_zhiye,
    split(path,'[/]')[2] as zhiye
  from ods_gio_page
  where dt='2016-09-05' and 
      domain ='www.jikexueyuan.com' and platform='Web' and
      path regexp '[/]zhiye([/][a-zA-Z]+[/]?)$'     
  )as a 
group by a.src_zhiye,a.zhiye 

union all
--SECOND

select 
'2016-09-05' as dt, 
b.src_goods,
c.goods_title,
count(distinct b.userid) as uv
from
(
 select
 userid,
 case when refer is null or refer='' 
           then '直接访问'
      when refer regexp 'baidu|sem|seo|google|360|youdao|bing|yahoo|sogou' 
           then '搜索引擎'
      when parse_url(refer,'HOST')='www.jikexueyuan.com' and 
           (parse_url(refer,'PATH') is null or parse_url(refer,'PATH') regexp '^[/]?$' ) 
           then '首页'
      when parse_url(refer,'HOST')='www.jikexueyuan.com' and 
           parse_url(refer,'PATH') regexp '[/]zhiye([/][a-zA-Z]+[/]?)$' 
           then split(parse_url(refer,'PATH'),'[/]')[2]     
      when parse_url(refer,'HOST') like '%jikexueyuan.com%'  
           then  '极客站内'
      else 'Other' end as src_goods,
 split(split(path,'[/]')[3],'[.]')[0] as goods_id
 from ods_gio_page 
 where dt='2016-09-05' and 
      domain ='www.jikexueyuan.com' and 
      path regexp '[/]zhiye[/]course[/][0-9]+[.]html' and
      platform='Web'    
) as b ,mds_v6_goods_releation as c
where c.app_id=3 and c.goods_id=b.goods_id
group by b.src_goods,c.goods_title  

```

##hive 1.0.0 报错
FAILED: SemanticException The abstract syntax tree is null

##hive2.0.0完美运行，没有问题：

```sql
Query ID = root_20160906232707_a7a36953-8e9a-4a0a-a1d9-d049b98ee9cb
Total jobs = 1
Launching Job 1 out of 1


Status: Running (Executing on YARN cluster with App id application_1471531268985_0416)

----------------------------------------------------------------------------------------------
        VERTICES      MODE        STATUS  TOTAL  COMPLETED  RUNNING  PENDING  FAILED  KILLED  
----------------------------------------------------------------------------------------------
Map 1 .......... container     SUCCEEDED      8          8        0        0       0       0  
Map 4 .......... container     SUCCEEDED      8          8        0        0       0       0  
Map 7 .......... container     SUCCEEDED      1          1        0        0       0       0  
Reducer 2 ...... container     SUCCEEDED      1          1        0        0       0       0  
Reducer 5 ...... container     SUCCEEDED      1          1        0        0       0       0  
Reducer 6 ...... container     SUCCEEDED      1          1        0        0       0       0  
----------------------------------------------------------------------------------------------
VERTICES: 06/06  [==========================>>] 100%  ELAPSED TIME: 8.90 s     
----------------------------------------------------------------------------------------------
OK
2016-09-05	Other	web	1
...
```

