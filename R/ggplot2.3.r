x <- read.csv('mydf.csv')
xx<- ddply(.data = x,
	.variables = c('g1','g2'),
	.fun = summarise,
	avg=mean(v),
	low=t.test(v)$conf.int[1],
	upper=t.test(v)$conf.int[2])

pd <- position_dodge(0.1)

ggplot(data=xx,aes(x=g1,y=avg,group=g2))+
    geom_point(aes(col=g2,shape=g2),size=3,position = pd)+
    geom_line(aes(col=g2),position = pd,lty=2)+
    geom_errorbar(aes(ymin=low,ymax=upper),col=I('black'),width=0.05,position = pd)+
    geom_text(aes(label=round(avg,2)),nudge_x = 0, nudge_y = 0.1,check_overlap = TRUE)+
    theme(panel.background = element_blank())+theme_bw()+
    labs(x='xlab',y='Year',title='Study 1')
