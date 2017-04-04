require(sp)
require(googleVis)
require(rworldmap)
require(ggplot2)
require(scales)
require(plotly)
require(foreign)

#Load the data
data<-read.csv("florida.csv",strip.white=TRUE)

#map for lightning fatalites 
l.data<-data[data$Lightning_Fatality>0,]
LgtStates <- gvisGeoChart(l.data, "State", "Lightning_Fatality",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       colors="['white','#CCCC00']",
                                       width=500, height=300))
plot(LgtStates)

#map for unprovoked shark attacks 
s.data<-data[data$Shark_Attacks>0,]
SharkStates <- gvisGeoChart(s.data, "State", "Shark_Attacks",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       colors="['white','blue']",
                                       width=500, height=300))
plot(SharkStates)

#map for gator attacks 
g.data<-data[data$Gator_Attacks>0,]
GatorStates <- gvisGeoChart(g.data, "State", "Gator_Attacks",
                            options=list(region="US", 
                                         displayMode="regions", 
                                         resolution="provinces",
                                         colors="['white','green']",
                                         width=500, height=300))
plot(GatorStates)
