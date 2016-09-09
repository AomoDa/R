



###中间表
```sql
insert overwrite table mds_v6_trace_seq
select
a.seq,
if(a.exit_pv>5,6, a.exit_pv) exit_pv ,
a.src_refer,
a.target,
count(a.sessionid) as uv
from
(
  select
     sessionid,
     case 
      when refer is null or refer='' 
           then 'DIRECT'
      when refer regexp 'baidu|sem|seo|google|360|youdao|bing|yahoo|sogou' 
           then 'SO'
      when parse_url(refer,'HOST')='www.jikexueyuan.com' and 
           (parse_url(refer,'PATH') is null or parse_url(refer,'PATH') regexp '^[/]?$' ) 
           then 'HOMEPAGE'  
       when (parse_url(refer,'HOST')='www.jikexueyuan.com'  and parse_url(refer,'PATH')  regexp '^[/]course') or parse_url(refer,'HOST') like '%ke.jikexueyuan.com%' 
            then 'course'
       when parse_url(refer,'HOST')='www.jikexueyuan.com'  and parse_url(refer,'PATH') regexp '^[/]zhiye'
            then 'zhiye'            
       when parse_url(refer,'HOST') like '%jikexueyuan.com%' and split(parse_url(refer,'HOST'),'[.]')[0] in ('download','qun','wenda','tiku','my')
            then 'Community'
       when parse_url(refer,'HOST')like '%jikexueyuan.com%' and split(parse_url(refer,'HOST'),'[.]')[0] in ('blog','help','mooc','saerch','pay','passport')
            then 'Jike_other'  
       when parse_url(refer,'HOST') like '%jikexueyuan.com%' and split(parse_url(refer,'HOST'),'[.]')[0] in ('jiuye','xue','xuexi')
            then 'Learn'  
       when parse_url(refer,'HOST') like '%jikexueyuan.com%' and split(parse_url(refer,'HOST'),'[.]')[0] in ('fuwu','press','yingxiao','mail')
            then 'Management'  
       when parse_url(refer,'HOST') like '%jikexueyuan.com%'  and split(parse_url(refer,'HOST'),'[.]')[0] in ('j','e','huodong','zt')
            then 'Sale'
       when parse_url(refer,'HOST') like '%jikexueyuan.com%'  and split(parse_url(refer,'HOST'),'[.]')[0] in ('wiki')
            then 'Wiki'              
      else 'Other' end as src_refer,
     case 
       when domain='www.jikexueyuan.com' and (path is null or path regexp '^[/]?$' ) 
            then 'HOMEPAGE'
       when (domain='www.jikexueyuan.com'  and path regexp '^[/]course') or domain like '%ke.jikexueyuan.com%'
            then 'course'
       when domain='www.jikexueyuan.com'  and path regexp '^[/]zhiye'
            then 'zhiye'            
       when domain like '%jikexueyuan.com%' and split(domain,'[.]')[0] in ('download','qun','wenda','tiku','my')
            then 'Community'
       when domain like '%jikexueyuan.com%'  and split(domain,'[.]')[0] in ('blog','help','mooc','saerch','pay','passport')
            then 'Jike_other'  
       when domain like '%jikexueyuan.com%'  and split(domain,'[.]')[0] in ('jiuye','xue','xuexi')
            then 'Learn'  
       when domain like '%jikexueyuan.com%' and split(domain,'[.]')[0] in ('fuwu','press','yingxiao','mail')
            then 'Management'  
       when domain like '%jikexueyuan.com%'   and split(domain,'[.]')[0] in ('j','e','huodong','zt')
            then 'Sale'  
        when domain like '%jikexueyuan.com%'   and split(domain,'[.]')[0] in ('wiki')
            then 'Wiki'             
       else 'unknown' end as target,

     row_number() over(partition by  sessionid  order by sendtime ROWS between UNBOUNDED PRECEDING and  UNBOUNDED FOLLOWING) as seq,
     count(1) over(partition by  sessionid  order by sendtime ROWS between UNBOUNDED PRECEDING and  UNBOUNDED FOLLOWING) as exit_pv
   from ods_gio_page
   where dt='$dt' and platform='Web' 
) as a 
where (a.seq=2  and a.src_refer not in ('SO','Other','DIRECT')) 
      or (a.seq=1 and a.src_refer in ('SO','Other','DIRECT'))
      or (a.seq=3 and a.src_refer  not in ('SO','Other','DIRECT'))
      or (a.seq=4 and a.src_refer  not in ('SO','Other','DIRECT'))
      or (a.seq=5 and a.src_refer  not in ('SO','Other','DIRECT'))
group by a.src_refer,a.target ,a.seq,if(a.exit_pv>5,6, a.exit_pv) 
```



### 计算表
```sql

select * from (
select 
'sdt' as dt,
seq,
case when seq=1 then src_refer 
     when seq=2 then concat(src_refer,'(1)')
     when seq=3 then concat(src_refer,'(2)')
     when seq=4 then concat(src_refer,'(3)')     
     else concat(src_refer,'(4)')  end as src_refer,
case when seq=1 then concat(target,'(1)') 
     when seq=2 then concat(target,'(2)') 
     when seq=3 then concat(target,'(3)')    
     when seq=4 then concat(target,'(4)')           
     else concat(target,'(5)') end as target,
sum(uv) as uv
from  mds_v6_trace_seq 
group by seq,case when seq=1 then src_refer 
     when seq=2 then concat(src_refer,'(1)')
     when seq=3 then concat(src_refer,'(2)')
     when seq=4 then concat(src_refer,'(3)')     
     else concat(src_refer,'(4)')  end ,
case when seq=1 then concat(target,'(1)') 
     when seq=2 then concat(target,'(2)') 
     when seq=3 then concat(target,'(3)')    
     when seq=4 then concat(target,'(4)')           
     else concat(target,'(5)') end 

union all
select
'$dt' as dt,
'6' as seq,
case when seq=1 then concat(target,'(1)') 
     when seq=2 then concat(target,'(2)') 
     when seq=3 then concat(target,'(3)') 
     when seq=4 then concat(target,'(4)') 
     else concat(target,'(5)')  end  as src_refer,
'exit' as target,
sum(uv) as uv
from mds_v6_trace_seq
where (seq=1 and exit_pv=1) or (seq=2 and exit_pv=2) or (seq=3 and exit_pv=3) or (seq=4 and exit_pv=4) or (seq=5 and exit_pv=5) 
group by case when seq=1 then concat(target,'(1)') 
     when seq=2 then concat(target,'(2)') 
     when seq=3 then concat(target,'(3)') 
     when seq=4 then concat(target,'(4)') 
     else concat(target,'(5)')  end
) as y



```
