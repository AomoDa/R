



library(ggplot2)
library(maps)
library(plyr)




# Fire Incident Table
# fireincident
# NOT_RES ： Not Residential Flag

usa_info <- data.frame(STATE=state.abb,STATE_NAME=tolower(state.name))
states_map <- map_data("state")

x <- read.csv('basicincident.txt',sep='^')
x$FF_DEATH[x$AID %in%c(3,4)]<- 0
x$FF_DEATH[is.na(x$FF_DEATH)] <- 0
x$OTH_DEATH[is.na(x$OTH_DEATH)] <- 0
x$DEATH <- x$FF_DEATH+x$OTH_DEATH

data_death <- x[x$DEATH!=0,c(1:5,8,10,33:34,42)]
data_death <- merge(data_death,usa_info,all = T)

data_death_sum <- ddply(.data = data_death,
	.variables = c('STATE_NAME'),
	.fun = summarise,
	FF_DEATH=sum(FF_DEATH),
	OTH_DEATH=sum(OTH_DEATH),
	DEATH=sum(DEATH))


ggplot(data_death_sum, aes(map_id = STATE_NAME)) +
    geom_map(aes(fill = DEATH), map = states_map,col=I('white')) +
    expand_limits(x = states_map$long, y = states_map$lat)+
    coord_map()+
    scale_fill_distiller(palette = "YlOrRd",direction =1)+
    theme(panel.background = element_blank())+theme_bw()



# Civilian Fire Casualty Table
# civiliancasualty
# Civilian casualties (i.e., injuries and deaths) are tallied in 
# the basic incident table and the details of each casualty
# are reported in the civilian fire casualty table.

# SEQ_NUMBER
# GENDER
# SEV
# Severity Scale Definition
#1 Minor
#2 Moderate
#3 Severe
#4 Life Threatening
#5 Death
#U Undetermined

#
#basicincident
