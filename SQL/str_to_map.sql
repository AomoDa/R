

select query,str_to_map(query,'&','=')
from ods_gio_page
where dt='2016-09-10' and domain='j.jikexueyuan.com'  and  query regexp 'utm_' and platform='Web'


--1
---utm_source=jkxy&utm_medium=www-index-a-cf&utm_term=discount&utm_content=jiuye-discount&utm_campaign=teachers-day	
---	{"utm_source":"jkxy","utm_medium":"www-index-a-cf","utm_term":"discount","utm_content":"jiuye-discount","utm_campaign":"teachers-day"}

--2
--utm_source=jkxy&utm_medium=www-index-a-cf&utm_term=discount&utm_content=jiuye-discount&utm_campaign=teachers-day	
--{"utm_source":"jkxy","utm_medium":"www-index-a-cf","utm_term":"discount","utm_content":"jiuye-discount","utm_campaign":"teachers-day"}
