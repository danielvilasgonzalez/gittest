d<-data.frame(year=c(2009:2020),
           dissimilarity=runif(12, 0.5, 1),
           centroid_distance=runif(12, 0.5, 1),
           volume_change=runif(12, 0.5, 1),
           SST=runif(12, 0, 1),
           SSS=runif(12, 0, 1),
           red_tides=runif(12, 0, 1),
           abundance=runif(12, 0, 1),
           occurrence=runif(12, 0, 1),
           effort=runif(12, 0, 1))

d<-reshape2::melt(d,id='year')
d$effect<-ifelse(d$variable %in% c('dissimilarity','centroid_distance','volume_change'),'fixed effects','factors')


library(ggplot2)
library(ggthemes)

p1<-ggplot()+
  #geom_line(data=subset(d,effect=='fixed effects'),aes(x=year,y=value,group=variable,color=variable),linetype='dashed')+
  geom_line(data=subset(d,effect=='factors'),aes(x=year,y=value,group=variable,color=variable),linetype='solid',size=0.8)+
  ggtitle(label='factors afffecting ENM estimation')+
  labs(color = "")+
  theme_bw()+
  xlab('')+
  ylab('standardized value')+
  theme(legend.title = element_blank(),strip.text.x = element_text(size = 9,color='black'), 
        #plot.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14),
        panel.grid.minor = element_line(color = col_grid,
                                        size = 0.3,
                                        linetype = 'dashed'),
        panel.grid.major = element_line(color = col_grid,
                                        size = 0.5))+
  expand_limits(y=0)+
  scale_color_tableau()+
  scale_x_continuous(breaks = c(2009:2020),minor_breaks = c(2009,2011:2014,2016:2019))

p2<-ggplot()+
  geom_line(data=subset(d,effect=='fixed effects'),aes(x=year,y=value,group=variable,color=variable),linetype='dashed',size=0.8)+
  ggtitle(label='hypervolume metrics')+
  labs(color = "")+
  theme_bw()+
  ylab('metric value')+
  theme(legend.title = element_blank(),strip.text.x = element_text(size = 9,color='black'), 
        #plot.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14),
        panel.grid.minor = element_line(color = col_grid,
                                        size = 0.3,
                                        linetype = 'dashed'),
        panel.grid.major = element_line(color = col_grid,
                                        size = 0.5))+
  expand_limits(y=0)+
  scale_color_tableau()+
  scale_x_continuous(breaks = c(2009:2020),minor_breaks = c(2009,2011:2014,2016:2019))
  #geom_line(data=subset(d,effect=='factors'),aes(x=year,y=value,group=variable,color=variable),linetype='solid')

plot_grid(p1,p2,nrow = 2,align = 'v')
