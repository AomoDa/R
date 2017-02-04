



---------------------------------
-- 日
---------------------------------

--新增/活跃 留存 

--不筛选事件类型
--StartDate (eg '2017-01-01')
--EndDate (eg '2017-01-01')
--Appid
set session processing_optimization='columnar';

select first_ds,
       dau_diff,
       count(1) as dau_retention
  from tkio_med_dau_detail_day
  where ds between '$StartDate' and date_format(date '$EndDate' + interval '30' day,'%Y-%m-%d') and 
        appid='$Appid' and dau_diff in(0,1,2,3,4,5,6,7,14,30) and 
        first_ds between  '$StartDate' and  '$EndDate' 
  group by first_ds,dau_diff


---------------------------------
-- 周
---------------------------------

--不筛选事件类型
--StartDate  一定必须是 自然周的周一(eg '2017-01-02')
--EndDate 一定必须是 自然周的周一 (eg '2017-01-02')
--Appid

select first_week_ds,
       dau_diff_week,
       count(1) as dau_retention
  from tkio_med_dau_detail_week
  where ds between '$StartDate' and date_format(date '$EndDate' + interval '56' day,'%Y-%m-%d') and 
        appid='$Appid'  and 
        first_week_ds between  '$StartDate' and  '$EndDate' 
  group by first_week_ds,dau_diff_week






---------------------------------
-- 月
---------------------------------

--不筛选事件类型
--StartDate  一定必须是 自然月的1号(eg '2017-01-01')
--EndDate 一定必须是 自然周的1号( (eg '2017-01-01')
--Appid

select first_month_ds,
       dau_diff_month,
       count(1) as dau_retention
  from tkio_med_dau_detail_month
  where ds between '$StartDate' and date_format(date '$EndDate' + interval '3' month,'%Y-%m-%d') and 
        appid='$Appid'  and 
        first_month_ds between  '$StartDate' and  '$EndDate' 
  group by first_month_ds,dau_diff_month




--#########################################################


-- 活跃/活跃 留存 

---------------------------------
-- 日
---------------------------------
--不筛选事件类型
--StartDate (eg '2017-01-01')
--EndDate (eg '2017-01-01')
--Appid

set session processing_optimization='columnar';

select dau_ds_base,
       dau_diff,
       count(1) as dau_retention
    from tkio.tkio_med_dau_cross_day
    where ds between '$StartDate' and date_format(date '$EndDate' + interval '30' day,'%Y-%m-%d') and 
          appid='$Appid'and 
          dau_ds_base between  '$StartDate' and  '$EndDate' and 
          dau_diff in(0,1,2,3,4,5,6,7,14,30)
    group by dau_ds_base, dau_diff





---------------------------------
-- 周
---------------------------------

--不筛选事件类型
--StartDate  一定必须是 自然周的周一(eg '2017-01-02')
--EndDate 一定必须是 自然周的周一 (eg '2017-01-02')
--Appid


select dau_ds_base,
       dau_diff_week,
       count(1) as dau_retention
    from tkio.tkio_med_dau_cross_week
    where ds between '$StartDate' and date_format(date '$EndDate' + interval '56' day,'%Y-%m-%d') and 
          appid='$Appid'and 
          dau_ds_base between  '$StartDate' and  '$EndDate'
    group by dau_ds_base, dau_diff_week






---------------------------------
-- 月
---------------------------------

--不筛选事件类型
--StartDate  一定必须是 自然月的1号(eg '2017-01-01')
--EndDate 一定必须是 自然周的1号( (eg '2017-01-01')
--Appid


select dau_ds_base,
       dau_diff_month,
       count(1) as dau_retention
    from tkio.tkio_med_dau_cross_month
    where ds between '$StartDate' and date_format(date '$EndDate' + interval '3' month,'%Y-%m-%d') and 
          appid='$Appid'and 
          dau_ds_base between  '$StartDate' and  '$EndDate'
    group by dau_ds_base, dau_diff_month
