



#!/bin/bash

#################################
#       Author:mengdongxing
#       Date:2016-04-01
#		预测每日UV
#################################

ssh hadoop02 "source /etc/profile;spark-submit /home/hadoop/r_script/test_predict_uv.R "

scp hadoop02:/home/hadoop/r_script/test123.dat hadoop03:/tmp/tmp123.dat

mysql  -e "delete from  report.rpt_predict_uv   "
mysql  -e "load data local infile '/tmp/tmp123.dat'   into   table report.rpt_predict_uv   "

