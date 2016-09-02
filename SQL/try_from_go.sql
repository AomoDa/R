

---------
select  
b.uid,
c.subject,
a.total_fee, 
b.career_status,
b.work_experience,
b.career_type,
b.school_year_enter from 

(select uid ,product_id ,app_id ,total_fee from jkxy_pay_v1.jkxy_order_info 
where date(created_at) >='2016-08-02' and status=1 and is_delete=0) as a
join
(select * from jkxy_v4.jkxy_user_member_extra
where date(ca_at)>='2016-08-02' and ca_status=1) as b on a.uid=b.uid

join 
(select distinct product_id,subject,app_id ,now_price,vip_price from report.rpt_v6_order) as c on a.product_id=c.product_id and a.app_id=c.app_id







---------
select 
'2016-08-17',
a.goods_id,
c.goods_title,
a.uv,
b.uv,
round(1-b.uv/a.uv,2)
from
(select 
split(split(path,'\\/')[3],'\\.')[0] as goods_id,
count(1) as uv
from ods_gio_page
where dt = '2016-08-17'  and platform='Web' and 
      domain like '%www.jikexueyuan.com%' and path regexp 'zhiye[/]course[/][0-9]+[.]html'
group by split(split(path,'\\/')[3],'\\.')[0] ) as a

left join 

(select 
split(split(parse_url(refer,'PATH'),'\\/')[3],'\\.')[0] as goods_id,
count(1) as uv
from ods_gio_page
where dt = '2016-08-17' and platform='Web' and 
      refer regexp 'www[.]jikexueyuan[.]com[/]zhiye[/]course[/][0-9]+[.]html'
group by split(split(parse_url(refer,'PATH'),'\\/')[3],'\\.')[0] ) as b on a.goods_id=b.goods_id

left join mds_v6_goods_releation as c  on a.goods_id=c.goods_id and c.app_id=3





--------
select a.* from 

(select userid,sendtime,refer,domain,path ,rank() over(partition by userid order by sendtime ) as n
from ods_gio_page where dt = '2016-08-17'  and platform='Web') as a

join 

(select userid
from ods_gio_page
where dt = '2016-08-17'  and platform='Web' and 
      domain like '%www.jikexueyuan.com%' and path regexp 'zhiye[/]course[/]23[.]html') as b on a.userid=b.userid





----
select 
count(distinct a.userid) 
from (
select userid,
last_value(domain) over(partition by userid order by sendtime  ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING ) as ld,
last_value(path) over(partition by userid order by sendtime  ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING ) as lp
from ods_gio_page
where dt = '2016-08-17'  and platform='Web' ) as a
where  a.ld like '%www.jikexueyuan.com%' and a.lp regexp 'zhiye[/]course[/]23[.]html'













