#this script is written by Isaac J. Faber
#produced on May 30 2015
#purpose is to create a set of visuals for c-section related information 
#some of this code is derived from flowingdata.com
#some of this code is adapted from googleVis templates

library(sp)
library(ggmap)
library(RgoogleMaps)
library(ggplot2)
library(RDSTK)
library(googleVis)
library(maptools)
library(stringdist)
library(maps)
library(scales)

#bring back in the county data
c.data<-read.csv("csection.csv",header=T)
c.data$County<-as.character(c.data$County)

#geocode the locations with google api
#geocode the counties in the US
x<-data.frame()
for(i in 1:length(c.data$County)){
  hold<-geocode(c.data$County[i])
  x<-rbind(x,hold)
}

c.data$Longitude<-x$lon
c.data$Latitude<-x$lat

#write data back out to a directory
data<-data[!is.na(data$Longitude),]
csection<-data
write.csv(csection,"csection.csv")

#fips codes for county data
fips<-read.csv("countyFIPSCodes.csv",header=T)
fips$County<-paste(fips$county,", ",fips$state,sep="")

#some minor formating of codes
fips$cfips <- as.character(fips$cfips)
addLeadingZeros <- function(x) {
  if (nchar(x)==2){
    return(paste("0",x,sep=""))
  }
  if (nchar(x)==1){
    return(paste("00",x,sep=""))
  }
  else {
    return(x)
  }
}
fips$cfips <- sapply(fips$cfips, addLeadingZeros)

fips$sfips <- as.character(fips$sfips)
addLeadingZeros2 <- function(x) {
  if (nchar(x)==1){
    return(paste("0",x,sep=""))
  }
  else {
    return(x)
  }
}
fips$sfips <- sapply(fips$sfips, addLeadingZeros2)

#matching codes from census file
fips$FIPS<-paste(fips$sfips,fips$cfips,sep="")

#bring in neonatal mortality data
n.data<-read.csv("neonatal.csv",header=T)
n.data<-n.data[n.data$Country!="Greenland"&n.data$Country!="Kosovo",]
#change the units to per 100K
n.data$Neonatal.Mortality.Rate<-n.data$Neonatal.Mortality.Rate*100
#bring in maternal mortality data
m.data<-read.csv("mmr.csv",header=T)
colnames(m.data)<-c("Country","Maternal.Mortality")
m.data<-m.data[m.data$Country!="Greenland"&m.data$Country!="Kosovo",]

####################create a couple of world maps#########################
#neonatal and maternal mortality data
Geo=gvisGeoChart(m.data, locationvar="Country", colorvar="Maternal.Mortality",
                 options=list(colors="['white','#dd1c77']",projection="kavrayskiy-vii"))

Geo2=gvisGeoChart(n.data, locationvar="Country", colorvar="Neonatal.Mortality.Rate",
                 options=list(colors="['white','#31a354']",projection="kavrayskiy-vii"))

GG <- gvisMerge(Geo,Geo2, horizontal=F) 
plot(GG)


#####################create county level plot #############################
counties <- readShapeSpatial("County_2010Census_DP1/County_2010Census_DP1.shp")
y<-unique(c.data$County)

#create a data frame with c section rates of counties in the US
county<-data.frame()
for(i in 1:length(y)){
  sub<-c.data[c.data$County==y[i],]
  sub2<-sub[sub$Delivery.Method=="Cesarean",]
  rate<-sub2$Births[1]/sum(sub$Births)
  hold<-data.frame(county=sub$County[1],rate=rate)
  county<-rbind(county,hold)
}

#subset the county data frame 
county$test<-grepl(c("Unidentified"),county$county)
county<-county[county$test==FALSE,]

county$FIPS<-"0"
for(i in 1:length(county$test)){
  hold<-stringdist(county$county[i],fips$County)
  y<-which.min(hold)
  county$FIPS[i]<-fips$FIPS[y]
}

#create a vecotor of matches for FIPS codes
csection.r <- county[match(counties@data$GEOID10, county$FIPS),]

#create color vector
findStampCol <- function(x) {
  if (is.na(x)) {
    col <- "#cccccc"
  } else if (x > 0.45) {
    col <- "#810f7c"
  } else if (x > 0.4) {
    col <- "#8856a7"
  } else if (x > 0.35) {
    col <- "#8c96c6"
  } else if (x > 0.3) {
    col <- "#9ebcda"
  } else if (x > 0.25) {
    col <- "#bfd3e6"
  } else if (x > .20) {
    col <- "#edf8fb"
  } else {
    col <- "#ffffff"
  }
  
  return(col)
}
stampCol <- sapply(csection.r$rate, findStampCol)

leg.txt <- c("< 20%", "20-25%", "25-30%", "30-35%", "35-40%"," > 40%")
colors<-(c("#ffffff","#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c"))

#build the map with set limits
png("vacancy-rate-higher.png", width=960, height=600)
plot(counties, col=stampCol, xlim=c(-125.97,-66.32), ylim=c(24.39, 49.7), lwd=0.01)
legend("bottomleft", leg.txt, bty="n", fill=colors)
dev.off()

#dot plot
dot<-read.csv("county.csv")
dot$cat<-factor(dot$cat,levels=c("rates of interest","bottom 10 counties","top 10 counties","other countries"))
  ggplot(dot, aes(y=reorder(obs,rate), x=rate)) + 
  geom_point(size=5, aes(colour=cat)) +
  #geom_vline(xintercept=.5,color="black") +
  scale_colour_brewer(palette="Set1",guide=F) + 
  scale_x_continuous(breaks=c(0,.1,.2,.3,.4,.5),limits=c(0,.5),labels=percent)+
  ylab("")+
  xlab("Rates of Cesarean Delivery in US Counties Comparison")+
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),text = element_text(size=14),
        axis.text.y = element_text(angle=0)) +
  facet_grid(cat~., scales="free_y", space="free_y")