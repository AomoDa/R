

node=`hive -S -e "select id,node_name_en,rule_exp,node_is_guide,node_is_active,is_imp_node from trace.trace_node_info" | awk -v dt=$dt '
BEGIN{
  FS="\\t";
  id="";
  name="";
  is_guide="";
  is_active="";
  is_imp="";
}
{
  id=sprintf("%s    when %s then '\''%s'\''\\n",id,$3,$1);
  name=sprintf("%s    when %s then '\''%s'\''\\n",name,$3,$2);
  is_guide=sprintf("%s    when %s then '\''%s'\''\\n",is_guide,$3,$4);
  is_active=sprintf("%s    when %s then '\''%s'\''\\n",is_active,$3,$5);
  is_imp=sprintf("%s    when %s then '\''%s'\''\\n",is_imp,$3,$6);
}
END{
  printf("  case\\n%s  end node_id,\n",id);
  printf("  case\\n%s  end node_name,\n",name);
  printf("  case\\n%s  end node_is_guide,\n",is_guide);
  printf("  case\\n%s  end node_is_active,\n",is_active);
  printf("  case\\n%s  end node_is_imp\n",is_imp);
}'`

prev=`echo "$node" | sed 's/url/referer/g' | sed 's/node_/prev_node_/g'`
