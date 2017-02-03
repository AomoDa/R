
set hive.exec.dynamic.partition.mode=nonstrict;
insert overwrite table tkio.tkio_med_dau_detail_day partition(ds,appid)
select xwho,
       min(ds) over(partition by xwho) as first_ds,
       first_value(dau_segments) over(partition by xwho order by ds ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as first_dau_segments,
       datediff(ds,min(ds) over(partition by xwho)) as dau_diff,
       dau_segments,
       ds ,
       'bfa099f35fb3b8158846a24e7066ca34' as appid
from
 (select xwho, 
       ds,
       collect_set(xwhat) as dau_segments
    from default.segments2
    where ds between '2016-12-01'  and '2017-02-01' and 
      appid='bfa099f35fb3b8158846a24e7066ca34' and 
      xwho is not null and xwhat!=''
    group by xwho,ds
) as tt 




use tkio;
create EXTERNAL table tkio_med_dau_detail_day(
xwho string,
first_ds string,
first_dau_segments array<string>,
dau_diff int,
dau_segments array<string>
)
PARTITIONED BY ( 
ds string, 
appid string)
ROW FORMAT DELIMITED 
FIELDS TERMINATED BY '\t'
STORED AS ORC
location '/tmp/reyuntrack/warehouse/track/tkio.db/tkio_med_dau_detail_day'
;





set hive.exec.dynamic.partition.mode=nonstrict;
insert overwrite table tkio.tkio_med_dau_cross_day partition(ds,appid)
select xwho,
       dau_ds as  dau_ds_base,
       datediff(ds,dau_ds) as dau_diff,
        ds,
       'bfa099f35fb3b8158846a24e7066ca34' as appid
from 
(select xwho,
       ds,
       collect_list(ds) over(partition by xwho order by ds ROWS between 30 PRECEDING  and CURRENT ROW ) as dau_ds_array
  from tkio.tkio_med_dau_detail_day
  where ds between '2016-12-01' and '2017-02-01' and 
        appid='bfa099f35fb3b8158846a24e7066ca34'
) as t1 lateral view explode(t1.dau_ds_array) lat_view as dau_ds
where datediff(ds,dau_ds) in (0,1,2,3,4,5,6,7,14,30)


use tkio;
create EXTERNAL table tkio_med_dau_cross_day(
xwho string,
dau_ds_base string,
dau_diff int
)
PARTITIONED BY ( 
ds string, 
appid string)
ROW FORMAT DELIMITED 
FIELDS TERMINATED BY '\t'
STORED AS ORC
location '/tmp/reyuntrack/warehouse/track/tkio.db/tkio_med_dau_cross_day'
;


--------------------------------------------------------------

--- select next_day(date_sub('2017-02-13',7), 'MO');

insert overwrite table tkio.tkio_med_dau_detail_week partition(ds,appid)

select xwho,
       min(next_day(date_sub(week_start_ds,7), 'MO')) over(partition by xwho) as first_week_ds,
       min(week_num) over(partition by xwho) as first_week_num,
       first_value(week_dau) over(partition by xwho order by week_num ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as first_week_segments,
       week_num - min(week_num) over(partition by xwho)  as dau_diff_week,
       week_dau as week_dau_segments,
       week_num,
       next_day(date_sub(week_start_ds,7), 'MO'),
       'bfa099f35fb3b8158846a24e7066ca34' as appid
from 
(select xwho,
       weekofyear(ds) as week_num,
       min(ds) as week_start_ds,
       collect_list(dau_segments) as week_dau
    from tkio_med_dau_detail_day
    where ds between '2016-12-01' and '2017-02-01' and
          appid='bfa099f35fb3b8158846a24e7066ca34'
    group by xwho,weekofyear(ds)

) as tt




use tkio;
create EXTERNAL table tkio_med_dau_detail_week(
xwho string,
first_week_ds string,
first_week_num int,
first_week_segments array<array<string>>,
dau_diff_week int,
week_dau_segments array<array<string>>,
week_num int 
)
PARTITIONED BY ( 
ds string, 
appid string)
ROW FORMAT DELIMITED 
FIELDS TERMINATED BY '\t'
STORED AS ORC
location '/tmp/reyuntrack/warehouse/track/tkio.db/tkio_med_dau_detail_week'
;



---


insert overwrite table tkio.tkio_med_dau_detail_month partition(ds,appid)

select xwho,
       min(mon_start_ds) over(partition by xwho) as first_week_ds,
       min(mon_num) over(partition by xwho) as first_month_num,
       first_value(mon_dau) over(partition by xwho order by mon_num ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as first_month_segments,
       mon_num - min(mon_num) over(partition by xwho)  as dau_diff_month,
       mon_dau as month_dau_segments,
       mon_num,
       mon_start_ds,
       'bfa099f35fb3b8158846a24e7066ca34' as appid
from 
(select xwho,
       month(ds) as mon_num,
       concat(substr(ds,0,8),'01') as mon_start_ds,
       collect_list(dau_segments) as mon_dau
    from tkio_med_dau_detail_day
    where ds between '2016-12-01' and '2017-02-01' and
          appid='bfa099f35fb3b8158846a24e7066ca34'
    group by xwho,month(ds),concat(substr(ds,0,8),'01') 

) as tt



use tkio;
create EXTERNAL table tkio_med_dau_detail_month(
xwho string,
first_month_ds string,
first_month_num int,
first_month_segments array<array<string>>,
dau_diff_month int,
month_dau_segments array<array<string>>,
month_num int 
)
PARTITIONED BY ( 
ds string, 
appid string)
ROW FORMAT DELIMITED 
FIELDS TERMINATED BY '\t'
STORED AS ORC
location '/tmp/reyuntrack/warehouse/track/tkio.db/tkio_med_dau_detail_month'
;



