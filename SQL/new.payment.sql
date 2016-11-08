select a.appid, a.ds, a.channelid, '${ds}' as dtoday,
datediff(b.ds, a.ds) as ddiff, count(distinct b.deviceid) as payers, sum(b.amount) as amount
from 
(

select aa.appid,aa.deviceid,aa.ds,bb.channelid,bb.cid
from 
(select appid, deviceid, max(ds) ds,
from trackdimensions.install_bucket
where bucketid='${bucketid}' and 
      ds>=date_sub('${ds}', 60) and ds<='${ds}' 
      and is_normal='1'
      and deviceid is not null and deviceid !='' 
      and deviceid !='00000000-0000-0000-0000-000000000000'
group by appid,deviceid )  aa

left outer join 

(select appid, deviceid, channelid, ds, cid
from trackdimensions.install_bucket
where bucketid='${bucketid}' 
      and ds>=date_sub('${ds}', 60) 
      and ds<='${ds}' and is_normal='1')  bb

on aa.appid=bb.appid and aa.deviceid=bb.deviceid and aa.ds=bb.ds


) a left outer join
(
select appid, deviceid, channelid, ds, 
sum(case when amount is not null then cast(amount as double) else 0.0 end) as amount, cid
from trackdimensions.payment_bucket
where bucketid='${bucketid}' and ds='${ds}'
group by appid, deviceid, channelid, ds, cid
) b on a.deviceid=b.deviceid and a.channelid=b.channelid and a.appid=b.appid
where datediff(b.ds, a.ds) is not null
group by a.appid, a.ds, a.channelid, datediff(b.ds, a.ds)
