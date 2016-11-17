

#!/bin/bash

dt='2016-09-14'
ed='2016-11-16'
cnt=$[($(date -d "$ed" +%s)-$(date -d "$dt" +%s))/(24*60*60)]
for((i=0;i<$cnt;i++))
do

sql="
select /*+ mapjoin(b) */ a.ds,
count(1),
count(if(a.idfa='00000000-0000-0000-0000-000000000000',1,null))
from
(select ds,appid,xcontext['idfa'] idfa
from trackdefault.events2
where ds ='$dt' and xwhat='install'  and xcontext['idfa'] is not null
) a

join 

(select appid
from track_mid.db_track_app_info
where platform='iOS')  b on a.appid=b.appid

group by a.ds "

sudo -uhive hive -e "$sql" >> idfa0.csv

dt=`date -d "$dt +1 day" +%Y-%m-%d`
done





