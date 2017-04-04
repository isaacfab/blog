library(sp)
library(ggmap)
library(rgdal)
library(RgoogleMaps)
library(SpatialTools)
library(ggplot2)
library(RDSTK)

#prepare vis for baltimore riots
riot.data<-read.csv("BaltimoreRiots.csv",header=T)

#geo code the address for lat and long 
riot.data$Address<-as.character(riot.data$Address)

x<-data.frame()
for(i in 1:length(riot.data$Address)){
  hold<-geocode(riot.data$Address[i])
  x<-rbind(x,hold)
}

riot.data$Longitude<-x$lon
riot.data$Latitude<-x$lat

riot.data<-riot.data[!is.na(riot.data$Longitude),]

jack<-riot.data[86,]

#build some maps
point.breaks<-unique(riot.data$Category)
city.map<-get_map(location="Baltimore, MD",source="osm",maptype="watermark",zoom=12)
City.Map<-ggmap(city.map, extent = "device", legend = "topleft")

from<-c("New Hope Academy, Baltimore, MD 21201")
to<-c("Hillsdale Rd and Liberty Heights Ave, Baltimore MD")
legs_df <- route(from,to,alternatives = F)

#create density map
City.Map+
  stat_bin2d(
    aes(x = Longitude, y = Latitude, colour = Category,fill=Category),
    size = .1, bins = 25, alpha = .5,
    data = riot.data[1:85,]
  )+
scale_color_manual(values=c("darkgreen","yellow", "red","orange","black","black"), 
                     name="",
                     breaks=point.breaks,
                     labels=point.breaks)+
  scale_fill_manual(values=c("darkgreen","yellow", "red","orange","black","black"), 
                     name="",
                     breaks=point.breaks,
                     labels=point.breaks)+
  theme(legend.position="right")+
  geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat), 
           alpha = 3/4, size = 2, data = legs_df)
    


#create point plot
  City.Map+  
        geom_point(aes(x = Longitude, y = Latitude,color=Category)
                   ,size=5,data = riot.data)+
        geom_point(aes(x = Longitude, y = Latitude),
                   size=36,data = jack,color="red",alpha=.2)+
        scale_color_manual(values=c("darkgreen","yellow", "blue","red","orange","black"), 
                           name="",
                           breaks=point.breaks,
                           labels=point.breaks)+
        theme(legend.position="right")

#write out frequency based data
tab<-as.data.frame(table(riot.data$Category))
write.csv(tab,"riotdata.csv")
    