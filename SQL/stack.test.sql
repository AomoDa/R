select appid,
       h_appid,
       deviceid,
       ins_datetime,
       ins_ip,
       join_id
    from 
     (  select appid,
              hash(appid) as h_appid,
              deviceid,
              hash(deviceid) as h_deviceid,
              ins_datetime,
              ins_ip,
              hash(ins_ip) as h_ins_ip,
              hash(idfa) idfa,
              hash(upper(md5(upper(idfa)))) as md5_idfa,
              hash(upper(md5(lower(imei)))) as md5_imei,
              hash(upper(md5(lower(android_id)))) as md5_android_id,
              hash(upper(sha1(idfa))) as sha_idfa,
              hash(upper(sha1(android_id))) as sha_android_id,
              hash(imei) as imei,
              hash(if(mac='02:00:00:00:00:00',0,mac)) mac,
              hash(upper(android_id)) as android_id
           from tkio.tkio_tmp_install_day 
           where ds='2016-12-19' and 
                 appid='cefc2dcca8ac8c4b0ec6e8fe3861e1b1' and 
                 is_norm_install=1
       ) t1 LATERAL VIEW stack(9,h_deviceid,h_ins_ip,md5_idfa,
                               md5_imei,md5_android_id,
                               sha_idfa,sha_android_id,mac,android_id) lt_view as join_id
    where join_id!=hash('0')
