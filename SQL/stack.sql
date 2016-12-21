select appid,a
from db_track_app_info lateral view stack(4,app_name,category_name,isgame,platform) lview as a
