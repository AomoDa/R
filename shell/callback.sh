

#!/bin/bash
bucketid="93b66b1f67b56a2fe8692cbd835bd8cd"
appid="6aecde04c49125c54a94bf2faf69c321"
ds_sub=7

dt='2016-10-31'
ed='2016-10-31'
cnt=$[($(date -d "$ed" +%s)-$(date -d "$dt" +%s))/(24*60*60)]

for((i=0;i<=$cnt;i++))
do
sql="
select distinct t0.appid as appkey, 
t0.installtime,
concat(unix_timestamp(t0.installtime), '000') as activetime, 
t1.xcontext['ryosversion'] as osversion, 
t1.xcontext['rydevicetype'] as devicetype, 
t0.deviceid as deviceid, 
t1.xcontext['mac'] as mac, 
case when t0.channelid = '_default_' then '' 
     else concat(unix_timestamp(t0.clicktime), '000') end as clicktime, 
case when t0.channelid = '_default_' then '' 
     else str_to_map(t2.xcontext['querystring'], '&', '=')['ryspreadurl'] end as spreadurl, 

case when t0.channelid = '_default_' then '' 
     else str_to_map(t2.xcontext['querystring'], '&', '=')['ryspreadname'] end as spreadname, 

case when t0.channelid = '_default_' then '' else t0.ip end as uip

from
 ( select *
        from trackinitiate.installchannel_bucket 
        where bucketid = '$bucketid' 
              and ds = '2016-10-13' and appid = '$appid' 
             and installdate = '2016-10-13' and is_normal = 1 and channelname != 'adwords'  
    ) t0 

join

    (select * 
        from trackdefault.events2 
        where appid = '$appid' 
              and ds = '2016-10-13' and xwhat = 'install' and xcontext['deviceid'] is not null 
    ) t1  on t0.deviceid = t1.xcontext['deviceid']

left outer join

    (select * 
        from trackdefault.click 
        where appid = '$appid' 
              and ds >= date_sub('$dt',$ds_sub) and ds <= '$dt' --track.setting_history.ciday ----mysql
    ) t2 on t0.channelid = t2.xcontext['channelid'] and t0.deviceid = t1.xcontext['deviceid'] 
            and t0.clicktime = t2.xwhen

"

xpath="$appid""_$dt"".csv"

echo $sql
echo '----------------------------------------'
sudo -uhive hive -e "$sql" > "$xpath"
dt=`date -d "$dt +1 day" +%Y-%m-%d`
done
