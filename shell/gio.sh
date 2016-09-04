#!/bin/bash

#################################
#         Author:
#         Date:2016-08-12
#         load gio data
#################################

source /home/hadoop/dw/config/etl_config.sh

hive -e " alter table ods_gio_action drop  IF EXISTS partition(dt='$dt')"
hive -e " alter table ods_gio_action_rule drop IF EXISTS partition(dt='$dt')"
hive -e " alter table ods_gio_action_tag drop IF EXISTS partition(dt='$dt')"
hive -e " alter table ods_gio_page drop IF EXISTS partition(dt='$dt')"
hive -e " alter table ods_gio_visit drop IF EXISTS partition(dt='$dt')"

hadoop dfs -rmr hdfs://hadoop01:54310/user/hive/warehouse/ods_gio_action/dt=$dt
hadoop dfs -rmr hdfs://hadoop01:54310/user/hive/warehouse/ods_gio_action_rule/dt=$dt
hadoop dfs -rmr hdfs://hadoop01:54310/user/hive/warehouse/ods_gio_action_tag/dt=$dt
hadoop dfs -rmr hdfs://hadoop01:54310/user/hive/warehouse/ods_gio_page/dt=$dt
hadoop dfs -rmr hdfs://hadoop01:54310/user/hive/warehouse/ods_gio_visit/dt=$dt

source /etc/profile && cd /home/hadoop/dw/shell/report/growingio && python growingio.py -d $dt >> /home/hadoop/logs/growingio.log 2>&1
source /etc/profile && cd /home/hadoop/dw/shell/report/growingio && python rules.py -d $dt >> /home/hadoop/logs/growingio.log 2>&1
