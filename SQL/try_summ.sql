

select
'2016-08-21',
x.goods_id,
x.goods_title,
x.s_n,
z.uv,
x.try_n,
x.try_bm,
nvl(y.order_num,0),
nvl(y.order_succ,0),
nvl(y.order_moeny,0),
x.try_b
from
(select 
a.goods_id,
c.goods_title,
count(distinct if(a.query not like '%try=1%',a.cs1,null)) as s_n ,
count(distinct if(a.query like '%try=1%',a.cs1,null))  as try_n,
count(distinct if(b.eventvalue like '%立即报名%',a.cs1,null)) try_bm,
count(distinct if(a.query like '%try=1%',a.userid,null)) - count(distinct if(a.query like '%try=1%' and b.userid is not null,a.userid,null))  as try_b
from 
(select 
userid,
page_id,
split(path,'\\/')[3] as goods_id,
query,
split(cs1,':')[1] as cs1
from ods_gio_page 
where dt='2016-08-21' and domain='xue.jikexueyuan.com' 
     and path regexp 'zhiye[/]course[/][0-9]+[/]' and cs1 !='') as a
left join
(select 
userid,
page_id,
path,
eventvalue
from ods_gio_action
where dt='2016-08-21' and domain='xue.jikexueyuan.com' 
     and path regexp 'zhiye[/]course[/][0-9]+[/]') as b on  a.page_id=b.page_id
left join 
(select * from mds_v6_goods_releation) as c on a.goods_id=c.goods_id and c.app_id=3 
group by a.goods_id,c.goods_title) as x

left join
(select e.goods_id,
sum(d.n) as order_num,
sum(d.num_order) order_succ,
sum(d.succ_money) order_moeny
from
(select uid,
    product_id,
    count(distinct uid) as n,
    count(distinct if(status=1,uid,null)) as num_order,
    sum(if(status=1,total_fee,0)) as succ_money
from db_jkxy_order_info
where to_date(created_at)='2016-08-21'  and is_delete=0 and app_id=3 
group by uid,product_id
) as d
join 
(select 
    split(cs1,':')[1] as cs1,
    split(path,'\\/')[3] as goods_id
from ods_gio_page
where dt='2016-08-21' and domain='xue.jikexueyuan.com' 
     and path regexp 'zhiye[/]course[/][0-9]+[/]' and query like '%try=1%'
    ) as e on d.uid=e.cs1 and d.product_id=e.goods_id
group by e.goods_id) as y on x.goods_id=y.goods_id 

left join 
(select 
split(split(path,'\\/')[3],'\\.')[0] as goods_id,
count(distinct userid)  uv
from ods_gio_page
where dt='2016-08-21' and domain='www.jikexueyuan.com' 
     and path regexp 'zhiye[/]course[/][0-9]+[.]html'
group by split(split(path,'\\/')[3],'\\.')[0]  ) as z on x.goods_id=z.goods_id