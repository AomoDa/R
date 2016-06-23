select 

date_sub('$dt',6) as start_dt,
'$dt' as end_dt,
a.uid,
a.num_course,
a.num_lesson,
a.avtime,
nvl(b.num_topic,0) as num_topic,
nvl(c.num_reply,0) as num_reply,
RANK() OVER (ORDER BY a.num_course DESC) AS rank_course,
RANK() OVER (ORDER BY a.num_lesson DESC) AS rank_lesson,
RANK() OVER (ORDER BY a.avtime DESC )  AS rank_avtime,
count(1) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as num_all,
ceil(avg(a.num_course) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING)) as avg_course,
ceil(avg(a.num_lesson) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING))as avg_lesson,
round(avg(a.avtime) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING),2) as avg_avtime
max(a.num_course) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as max_course,
max(a.num_lesson) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING)as max_lesson,
max(a.avtime) OVER(ROWS between UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING) as max_avtime

from 
(select uid,
count(distinct acid ) as num_course,
count(distinct (acid*100 +avseq) )  as num_lesson ,
round(sum(avtime) /60,1)  as avtime
from mds_video_duration
where dt between date_sub('$dt',6) and '$dt'  and uid !=''
group by uid having round(sum(avtime) /60,1) >5 ) as a

left join
(select uid,count(distinct id) as num_topic 
from db_jkxy_learn_question_topic_info 
where to_date(created_at) between date_sub('$dt',6) and '$dt'
group by uid) as b on a.uid=b.uid

left join 
(select uid,count(1) as num_reply 
from db_jkxy_learn_question_reply_info
where to_date(created_at) between date_sub('$dt',6) and '$dt'
group by uid ) as c on a.uid=c.uid  


###################################################
create table rpt_api_uid_learn_weekly (
id int(10) unsigned NOT NULL AUTO_INCREMENT,
start_dt date comment '周期起始时间',
end_dt date comment '周期结束时间',
uid int comment '用户标识uid',
num_course int comment '该用户学习课程数量',
num_lesson int comment '该用户学习课时数量',
avtime float comment '该用户学习时长，单位分钟',
num_topic int comment '该用户学习发帖提问数量' ,
num_reply int comment '该用户学习回答帖子数量',
rank_course int comment '该用户学习课程数量排名',
rank_lesson int comment '该用户学习课时数量排名',
rank_avtime int comment '该用户学习时长排名',
num_all int comment '统计周期内所有的用户数量，去重。',
avg_course int comment '统计周期内所有的平均学习课程数量，向上取整',
avg_lesson int comment '统计周期内所有的平均学习课时数量，向上取整',
avg_avtime float comment '统计周期内所有的平均学习时长，单位分钟',
max_course int comment '统计周期内所有的最大学习课程数量，向上取整',
max_lesson int comment '统计周期内所有的最大学习课时数量，向上取整',
max_avtime float comment '统计周期内所有的最大学习时长，单位分钟',
PRIMARY KEY (`id`),
index(uid)
) COMMENT='用户周报接口数据'

###################################################
###################################################



-- 用户视频推荐算法


select 
0,
date_sub('$dt',6) as start_dt,	
'$dt' as end_dt,
xx.uid,
xx.acid,
zz.course_name,
case when xx.num_learn_lesson >= zz.lesson_count then '已完成' else concat('还差',cast( round(zz.lesson_count-xx.num_learn_lesson,0)  as string),'课时') end as learn_type,
zz.recommend_course_id,
zz.recommend_course_name,
zz.recommend_type
from
(select uid,acid ,count(distinct avseq) as num_learn_lesson
from mds_video_duration
where dt between date_sub('$dt',6) and '$dt' and uid!=''
group by uid,acid) as xx

join 

(select uid
from mds_video_duration
where dt between date_sub('$dt',6) and '$dt' and uid!=''
group by uid 
having round(sum(avtime) /60,1) >5) as yy on xx.uid=yy.uid

join

(select x.course_id,
x.course_name,
z.lesson_count,
nvl(y.recommend_course_id_xilie,z.recommend_course_id_info)  as recommend_course_id,
nvl(y.recommend_course_name_xilie,z.recommend_course_name_info)  as recommend_course_name,
case when y.recommend_course_id_xilie is not null then '系列课程推荐' 
     when y.recommend_course_id_xilie is null and z.recommend_course_id_info is not null then '职业课程库推荐'
     else '无推荐' end as recommend_type
from
(select id as course_id,
title as course_name
from db_jkxy_learn_course_info
where is_avaiable=1 and is_delete=0) as x

left join 

(select a.id as xilie_id,
c.id as course_id,
b.seq,
lead(c.id,1) OVER(partition by a.id ORDER by b.seq) as recommend_course_id_xilie,
lead(c.title,1) OVER(partition by a.id ORDER by b.seq) as recommend_course_name_xilie
from  
(select id,title from db_jkxy_learn_xilie_info where is_delete=0 and is_show=1) as a
left join 
(select xilie_info_id, course_id,seq from db_jkxy_learn_xilie_detail  where is_delete=0 and is_show=1) as b on a.id=b.xilie_info_id
left join 
(select id,title from db_jkxy_learn_course_info where is_delete=0) as c  on b.course_id=c.id
where c.id is not null ) as y on x.course_id=y.course_id

left join 

(select ac_id ,lesson_count,
lead(ac_id,1) OVER (partition by three_id ORDER by avaiable_at) as recommend_course_id_info,
lead(ac_name,1) OVER (partition by three_id ORDER by avaiable_at) as recommend_course_name_info
from mid_course_info 
where avaiable_at !='' and avaiable_at !='NULL' and avaiable_at !='null'  and is_delete=0 and is_avaiable=1) as z on x.course_id =z.ac_id ) as zz on xx.acid=zz.course_id


###################################################


create table rpt_api_uid_course_recommend_weekly (
id int(10) unsigned NOT NULL AUTO_INCREMENT,
start_dt date comment '周期起始时间',
end_dt date comment '周期结束时间',
uid int comment '用户标识uid',
course_id int comment '用户学习课程ID',
course_name  varchar(255) comment '用户学习课程名称',
learn_type varchar(255) comment '学习完成情况' ,
recommend_course_id int comment '推荐课程ID',
recommend_course_name varchar(255) comment'推荐课程名称',
recommend_type varchar(255) comment '推荐类型',
PRIMARY KEY (id),
index(uid)
) COMMENT='用户课程推荐周报接口数据'
