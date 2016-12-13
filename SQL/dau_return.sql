select  a.appid,
        '${ds}' ds,
        if(b.is_return_click=1,b.channelid,c.channelid) channelid,
        a.deviceid
from 

    (select appid,
            deviceid,
            channelid,
            hash(appid) h_appid, 
            hash(deviceid) h_deviceid
        from trackdimensions.dau_bucket
        where  bucketid='${bucketid}'  and 
              ds='${ds}' and 
              deviceid is not null and 
              deviceid !='00000000-0000-0000-0000-000000000000'
        group by appid,
            deviceid,
            channelid,
            hash(appid) , 
            hash(deviceid)
    ) a

left outer join

    (select appid,
           deviceid,
           installtime,
           channelid,
           clicktime,
           if((to_unix_timestamp(installtime)-to_unix_timestamp(clicktime))<=86400,1,0) is_return_click,
           hash(appid) h_appid, 
           hash(deviceid) h_deviceid
        from trackinitiate.todayinstallchannel_bucket
        where  bucketid='${bucketid}'  and 
              ds='${ds}' and
              deviceid is not null and 
              deviceid !='00000000-0000-0000-0000-000000000000'
    ) b on a.h_appid=b.h_appid and a.h_deviceid=b.h_deviceid

left outer join 

    (select hash(appid) h_appid, 
            hash(deviceid) h_deviceid,
            max(channelid)channelid,
            count(if(ds >=date_sub('${ds}',30),1,null)) is_old_user,
            count(if(ds <date_sub('${ds}',30),1,null)) is_return_user
        from trackdimensions.dau_bucket
        where  bucketid='${bucketid}'  and 
              ds between date_sub('${ds}',60) and date_sub('${ds}',1) and 
              deviceid is not null and 
              deviceid !='00000000-0000-0000-0000-000000000000' and 
              channelid!='_default_'
        group by hash(appid),hash(deviceid)
    ) c on a.h_appid=c.h_appid and a.h_deviceid=c.h_deviceid

where c.is_old_user =0 and c.is_return_user>0 and c.h_deviceid is not null 
