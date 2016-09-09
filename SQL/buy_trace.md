




##效果
![image](https://raw.githubusercontent.com/AomoDa/R/master/SQL/jpg/1.jpg)
![image](https://raw.githubusercontent.com/AomoDa/R/master/SQL/jpg/2.jpg)
![image](https://raw.githubusercontent.com/AomoDa/R/master/SQL/jpg/3.jpg)



## 性能

代码执行过程是使用 HIVE 2.1.0，使用MR引擎。整体172秒。

如果使用TEZ，20秒完成。

分一张中间表、一张计算表。

###一张中间表，不要看起来代码多，其实就是2个job，整体25秒左右执行完成。

```sql

 select 
userid,
uid,
seq,
imp_seq,
is_order,
is_target_goods_id,
is_source_goods_id,
is_study_id,
is_try_id,
nvl(nvl(nvl(is_study_id,is_try_id),is_order),nvl(is_target_goods_id,is_source_goods_id))as goods_id,
source,
target,
max(imp_seq) over(partition by userid order by seq ROWS between UNBOUNDED PRECEDING and  UNBOUNDED FOLLOWING) as max_imp_seq
--求出最大重要时序。
from 
(
select userid,
       uid,
       seq,
       if(domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]order[/][0-9]+[.]html',split(`path`,'[/]|[.]')[3],null) as is_order,
       ---拆解出订单页的商品ID，为了解决搜索四大类商品的桑基图而派生。
       if(domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]course[/][0-9]+[.]html',split(`path`,'[/]|[.]')[3],null) as is_target_goods_id,
       --拆解target出商品详情页ID，
       if(refer_domain='www.jikexueyuan.com' and refer_path regexp '[/]zhiye[/]course[/][0-9]+[.]html',split(refer_path,'[/]|[.]')[3],null) as is_source_goods_id,
       --拆解出source商品详情页ID，
       if(domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query not regexp 'try=1', split(`path`,'[/]')[3],null) as is_study_id,
       if(domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query regexp 'try=1',split(`path`,'[/]')[3] ,null) as is_try_id,
       case when seq=1 and refer_domain is null then 'DIRECT'
            when seq=1 and refer_domain regexp 'baidu|google|360|youdao|bing|yahoo|sogou' then 'SO'
            when  refer_domain='www.jikexueyuan.com' and refer_path regexp '^[/]?$' 
                 then 'Homepage'
            when seq=1 and refer_domain not regexp 'jikexueyuan.com' then 'Other'
            when seq=1 and refer_domain  regexp 'jikexueyuan.com' then 'INNER'            
            when seq>1  and refer_domain='www.jikexueyuan.com' and refer_path regexp '[/]zhiye[/]([a-zA-Z]+[/]?)$' 
                 then split(refer_path,'[/]')[2]
            when seq>1  and refer_domain='www.jikexueyuan.com' and refer_path regexp '[/]zhiye[/]course[/][0-9]+[.]html' 
                 then split(refer_path,'[/]|[.]')[3]
            when refer_domain regexp 'jikexueyuan.com' then 'INNER'             
            else null end source,

            --只有第一页（seq=1）的refer才能判断用户的来源，分直接访问、搜索引擎，其他。
            --当seq 大于1的时候，判断用户是来自 职业页面、还是课程详情页、首页、内站等。
            --'Inner'来源可能有 wiki、course等，理论上第一页（seq=1）的来源不应该有'Inner'，但这里还是定义下吧。
            --'Inner'来源可能会重复，必须从订单页过来的，等等，后面用imp_seq来处理掉，
            --'Inner'可能会循环，用is_loop处理掉

       case when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye([/][a-zA-Z]+[/]?)$' 
                 then split(`path`,'[/]')[2] 
            when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]course[/][0-9]+[.]html' 
                 then split(`path`,'[/]|[.]')[3]
            when domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query regexp 'try=1'
                 then 'Try'
            when domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query not regexp 'try=1'
                 then 'Pay User Study'  
            when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]order[/][0-9]+[.]html'
                 then 'Signing Up'
            else null end as target,

            --目标页面，按照业务定义还有业务时序（imp_seq），以及将用户行为划分重点路径时序。
            --职业页面 （1）
            --商品详情页（2）
            --试学页面（3），付费用户学习（3）
            --订单页面（4）
            --订单成交页（5），退出页（5）

       case when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye([/][a-zA-Z]+[/]?)$' 
                 then 1 
            when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]course[/][0-9]+[.]html' 
                 then 2
            when domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query regexp 'try=1'
                 then 3
            when domain='xue.jikexueyuan.com' and `path` regexp '[/]zhiye/course/[0-9]+/ke' and query not regexp 'try=1'
                 then 3  
            when domain='www.jikexueyuan.com' and `path` regexp '[/]zhiye[/]order[/][0-9]+[.]html'
                 then 4
            else null end as imp_seq 
from 
(
    select userid, 
       split(cs1,':')[1] as uid,
       parse_url(refer,'HOST') as refer_domain,
       parse_url(refer,'PATH') as refer_path,
       if(parse_url(refer,'HOST') = domain and parse_url(refer,'PATH')=`path`,1,0) as is_loop, 
       --判断用户是否再当前页面循环（刷新），为了解决桑吉图会死循环，所以要将这类页面过滤掉
       domain,
       `path`,
       query,
       row_number() over(partition by userid order by sendtime ROWS between UNBOUNDED PRECEDING and  UNBOUNDED FOLLOWING) as seq
    from ods_gio_page 
    where dt='$dt' and domain regexp '(www|xue)[.]jikexueyuan[.]com' and `path` regexp 'zhiye'
) as a where is_loop=0 ) as b 
where source is not null and target is not null 
```



###结果表

因为要join 3张表，还有一个union all，所以job比较多，一共8个job。
用MR引擎的话 整体下来 150秒。速度还是可以的。


```sql


select * from (
select
'$dt' ,
'1',
yy.zhiye_type,
yy.source,
yy.target,
count(distinct yy.userid)
from
(select 
  a.userid,
  case when a.imp_seq=1 and a.target='go' then 3
       when a.imp_seq=1 and a.target='ios' then 5
       when a.imp_seq=1 and a.target='python' then 2
       when a.imp_seq=1 and a.target='web' then 4
       else b.zhiye_show_id end as zhiye_type,

  case when a.imp_seq=1 then if(a.source regexp '[1-9]' or a.source in('go','ios','web','python') ,'INNER',a.source)
       when a.imp_seq=4 and  a.is_try >0 then 'Try'
       when a.imp_seq between 2 and 4 then if(a.source regexp '[1-9]',b.goods_title,a.source)
       else null end source,
  case when a.imp_seq=2 then b.goods_title
       else a.target end target
from

       (select * ,
        count(is_try_id) over(partition by userid  ROWS between UNBOUNDED PRECEDING and  UNBOUNDED FOLLOWING) as is_try
         from mds_v6_trace_goods) as a 
           left join
        (select * 
          from mds_v6_goods_releation
          where app_id=3) as b on a.goods_id=b.goods_id and a.goods_id is not null
) as  yy 
group by yy.source,yy.target,yy.zhiye_type

union all

select
'$dt' ,
'1',
yy.zhiye_type,
yy.source,
yy.target,
count(distinct yy.userid)
from
(select 
  aaaa.userid,
  case when aaaa.imp_seq=1 and aaaa.target='go' then 3
       when aaaa.imp_seq=1 and aaaa.target='ios' then 5
       when aaaa.imp_seq=1 and aaaa.target='python' then 2
       when aaaa.imp_seq=1 and aaaa.target='web' then 4
       else aaaa.zhiye_show_id end as zhiye_type,
  case when aaaa.imp_seq=2 then aaaa.goods_title
       else aaaa.target end source,
  case when c.uid is not null and c.order_succ=0 then 'Order Failed'
       when c.uid is not null and c.order_succ>0 then 'Order Successful'
       else 'Exit' end target
from
     
    (select a.*,b.zhiye_show_id,b.goods_title from 
       (select * 
         from mds_v6_trace_goods where imp_seq-max_imp_seq=0 ) as a 
           left join
        (select * 
          from mds_v6_goods_releation
          where app_id=3) as b on a.goods_id=b.goods_id and a.goods_id is not null ) as aaaa
        left join      
        (select uid,
                  product_id as goods_id,
                  count(if(status=1,1,null)) as order_succ
          from db_jkxy_order_info
          where to_date(created_at)='$dt' and is_delete=0 and uid is not null and uid !=''
          group by uid,product_id)  as c on aaaa.uid is not null and aaaa.uid=c.uid and aaaa.is_order=c.goods_id
) as  yy  
group by yy.source,yy.target,yy.zhiye_type ) as z  
```

