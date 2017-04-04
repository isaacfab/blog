#script for making autism blog charts
require(datasets)
require(sp)
require(googleVis)
require(rworldmap)
require(ggplot2)
require(scales)
library(plotly)

#MMR coverage is reported through the CDC 
mmr<-read.csv("MMRCoverage.csv",strip.white=TRUE)

#the autism data file is created using two data sources, california state regional center incidence 
#and census reporting for popluation level fo the state of california
#I'm not claiming these rates represent all autism in california, just those reported in the referenced report
#I use linear interpolation to estimate population levels between census reports
#I use linear interpolation to estimate autism incidece before 1986 and after 2007
rates<-read.csv("autismdata.csv",strip.white=TRUE)
rates$DSM<-as.factor(rates$DSM)


#creating the google vis map with the mmr data
colnames(mmr)<-c("State","MMR Coverage Rate","MMR.Uncovered.Rate")
MmrStates <- gvisGeoChart(mmr, "State", "MMR.Uncovered.Rate",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(MmrStates)

#create the dot plot of california autism cases
my.plot<-ggplot(rates, aes(x=year, y=Per10K)) + 
  geom_point(size=5, aes(colour=DSM)) +
  scale_colour_brewer(palette="Set1", limits=c("DSM III","DSM III R","DSM IV","DSM IV TR"), guide=FALSE) + 
  geom_point(size=5,aes(y=MMR.Coverage))+
  scale_x_continuous(breaks=c(1980,1987,1994,2000,2010))+
  ylab("")+
  xlab("")+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),text = element_text(size=20),
        axis.text.x = element_text(angle=45, vjust=0)) +
  facet_grid(.~DSM, scales="free_x", space="free_x")

py <-plotly()

r <- py$ggplotly(my.plot)
