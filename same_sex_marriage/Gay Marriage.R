
require(ggplot2)
require(scales)


gm<-read.csv("gaym2.csv",strip.white=TRUE)
gm$Year<-as.factor(gm$Year)
gm$Group<-as.factor(gm$Group)


#create the dot plot gay marriage support
my.plot<-ggplot(gm, aes(y=reorder(Category, Support2013), x=Support)) + 
  geom_point(size=5, aes(colour=Year)) +
  geom_vline(xintercept=.5,color="black") +
  scale_colour_manual(values=c("#999999","Red")) + 
  scale_x_continuous(breaks=c(0,.25,.50,.75,1),limits=c(0,1),labels=c("January","February","March",
                                                                      "April","May","June","July",
                                                                      "August","September","October",
                                                                      "November","December"))+
  ylab("")+
  xlab("Changes in Percent of Support For Gay Marriage")+
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),text = element_text(size=14),
        axis.text.y = element_text(angle=0)) +
  facet_grid(Group~., scales="free_y", space="free_y")

py <-plotly()

r <- py$ggplotly(my.plot)
