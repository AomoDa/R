

--FIRST


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
      domain ='www.jikexueyuan.com' and 
      path regexp '[/]zhiye([/][a-zA-Z]+[/]?)$'     
  )as a 
group by a.src_zhiye,a.zhiye 




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

