select
'$dt',
x.path,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_source'),'UTF-8') AS utm_source,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_medium'),'UTF-8')AS utm_medium,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_campaign'),'UTF-8') AS utm_campaign,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_term'),'UTF-8') AS utm_term ,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_content'),'UTF-8') AS utm_content ,
count(distinct userid) as uv,
count(1) as pv
from (
select 
path,
query,
userid,
concat('http://',domain,path,'?',query) as url
from ods_gio_page
where dt='$dt' and domain='j.jikexueyuan.com'  and  query regexp 'utm_' and platform='Web') as x
group by x.path,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_source'),'UTF-8') ,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_medium'),'UTF-8'),
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_campaign'),'UTF-8'),
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_term'),'UTF-8') ,
reflect('java.net.URLDecoder','decode',parse_url(url,'QUERY','utm_content'),'UTF-8')
