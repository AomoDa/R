

select *
from track_pay_type_report
where ds='2016-10-26' and appid='54f2aeb184126c9271915a449d620b9c'
       and channel='_default_'




select distinct ds from track_pay_type_report_new;





track_pay_type_report

alter table track_pay_type_report modify PAY_AMOUNT_NEW float


select channelid, deviceid,amount
from trackconceptions.payment_bucket
where ds='2016-10-26' and appid='54f2aeb184126c9271915a449d620b9c'
  



select deviceid,installdate,channelid
from trackinitiate.todayinstallchannel_bucket
where ds>='2016-10-26' and appid='54f2aeb184126c9271915a449d620b9c'
      and deviceid in (
      'CA72D222-CD3F-4B82-AF2D-B50E8DAA4D8B',
      '895AAA85-7F21-43BE-877E-79F848C467E8',
      'B4FC6190-59EC-4AF2-9D26-C632AB957F9D',
      'FA8A6AB2-5B57-474E-8052-A39B4D103980')



select deviceid
from trackinitiate.todayinstallchannel_bucket
where ds='2016-11-01' and appid='6aecde04c49125c54a94bf2faf69c321'  
and channelid='44479e93171389fce9e92d2d608fe994'




---------


select  bucketid,ds,count(1)
from trackdimensions.dau_bucket
where  ds between '2016-10-27' and '2016-10-30'
group by ds,bucketid




select xcontext['deviceid'] 
from trackdefault.events2
where ds='2016-11-02' and appid='2c56dd83a7ebc28f477262cb0691149a' and xwhat='reged'

select count(1)
from trackdimensions.reged_bucket
where ds='2016-11-02' and appid='2c56dd83a7ebc28f477262cb0691149a' 


-----

select count(1)
from trackinitiate.installchannel_bucket
where ds='2016-11-02' and appid='2c56dd83a7ebc28f477262cb0691149a' and installdate='2016-11-02'




----------------------------------------------------------

select a.appid,a.deviceid,
b.ds installdate,
a.ds payment_ds,
a.transactionid,
a.currencyamount,
c.campaign_name
from

(select distinct  appid,ds,xcontext['deviceid'] deviceid,
        xcontext['transactionid'] transactionid,
        xcontext['currencyamount'] currencyamount
from trackdefault.events2 
where ds between '2016-08-01' and '2016-10-31' 
      and appid in ('8ae913dc09ee01f826a38432e55e9c13','ac40be0e542306225f0f6db89f210dbe')
      and xwhat='payment') a 

left outer join 
(select deviceid,max(ds) ds , max(channelid) channelid,appid
from trackconceptions.install_bucket
where ds between '2016-08-01' and '2016-10-31' 
      and appid in ('8ae913dc09ee01f826a38432e55e9c13','ac40be0e542306225f0f6db89f210dbe')
group by deviceid, appid ) b on a.deviceid=b.deviceid and a.appid=b.appid
left outer join 
(select channelid,campaign_name
from track_mid.db_track_channel_info
where appid in ('8ae913dc09ee01f826a38432e55e9c13','ac40be0e542306225f0f6db89f210dbe') ) c 

on b.channelid=c.channelid








-----

####11-02

select a.deviceid,
a.account_id,
a.currencyamount,
b.channelid
from
(select xcontext['deviceid'] deviceid,
          xwho account_id,
          sum(xcontext['currencyamount']) currencyamount
from trackdefault.events2 
where ds='2016-11-02' 
      and appid='e3f3b95d0b03a710858bac9b5088f6fe'
      and xwhat='payment'
group by xwho, xcontext['deviceid'] ) a 
join 
(select deviceid,ds,channelid
from trackinitiate.todayinstallchannel_bucket
where appid='e3f3b95d0b03a710858bac9b5088f6fe'
      ) b on a.deviceid=b.deviceid




----
select xcontext['deviceid'] deviceid,
       xcontext['currencyamount'] currencyamount,
       xwhen
from trackdefault.events2 
where ds='2016-11-03' 
      and appid='39ce98b994607ebcce6f04a4cd65d517'
      and xwhat='payment'
      and xcontext['deviceid'] in ('860514030659717','860514030617863')








select a.deviceid,a.installdate,a.installtime,
 case when b.campaign_name is null then '自然量' else b.campaign_name end  as channe

from 
(select deviceid,installdate,installtime,channelid
from trackinitiate.todayinstallchannel_bucket
where ds='2016-11-03' and appid='1a8743c3dd3a13c6aad68c1d04be3864' ) a

left outer join 

(select * from track_mid.db_track_channel_info
where appid='1a8743c3dd3a13c6aad68c1d04be3864') 
b on a.channelid=b.channelid



select APPID, DS, CHANNEL, ACTIVE, REGED, DISTINCTREGED  
from trackoverview_report where appid = '41da8ff46a2372d23218890b388b319d' and ds>'2016-11-06'; 
