#This script is to create visuals with for Heatstroke Death Data
#written by Isaac J. Faber

require(sp)
require(googleVis)
require(rworldmap)
require(ggmap)
require(ggplot2)
require(scales)
require(tm)
require(wordcloud)

data<-read.csv("heatstroke.csv",strip.white=T,header=T)
data$Age.1<-as.numeric(data$Age.1)

#calculate age in months
for(i in 1:length(data$Age.1)){
  if(data$Age.2[i]=="dy"){
    data$Age.m[i]<-data$Age.1[i]/30.2
  }
  if(data$Age.2[i]=="wk"){
    data$Age.m[i]<-(data$Age.1[i]/52)*12
  }
  if(data$Age.2[i]=="yr"){
    data$Age.m[i]<-data$Age.1[i]*12
  }
}

#enrich the data set for specific date data columns
data$Date<-as.Date(data$Date,format="%m/%d/%y")
data$weekday <- weekdays(data$Date)
data$month <- months(data$Date)
data$quarter <- quarters(data$Date)
d<-as.POSIXlt(data$Date)
data$year<- d$year+1900
data$mon<-d$mon+1
data$wday<-d$wday+1

#build a calendar heatmap
cal<-as.data.frame(table(data$Date))
colnames(cal)<-c("Date","Heatstroke.Death")
cal$Date<-as.Date(cal$Date)
Cal <- gvisCalendar(cal, 
                    datevar="Date", 
                    numvar="Heatstroke.Death",
                    options=list(
                      title="Heatstroke Deaths in Vehicles",
                      height=1600,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)

#geocode the locations of the data
data$Location<-paste(data$City,", ",data$State,sep="")
data$Location<-as.character(data$Location)
x<-data.frame()
for(i in 1:length(data$Location)){
  hold<-geocode(data$Location[i])
  x<-rbind(x,hold)
}

data$lon<-x$lon
data$lat<-x$lat

#create google vis map
data$latlong<-paste(data$lat,":",data$lon,sep="")
y<-as.data.frame(table(data$Location))

y$latlong<-"hold"
for(i in 1:length(y$Var1)){
  hold<-data[data$Location==y$Var1[i],]
  y$latlong[i]<-hold$latlong[1]
  
}

colnames(y)<-c("City","Heatstroke.Deaths","latlong")
new.data<-new.data[new.data$City!=new.data$City[1],]

map<- gvisGeoChart(y, locationvar="latlong", 
                 # sizevar='Heatstroke.Deaths',
                   colorvar='Heatstroke.Deaths', 
                  hovervar='City',
                   options=list(height=600,width=700,colorAxis= "{values:[1,10],
                                        colors:[\'red',\'black']}",
                                region="US",
                                resolution="provinces"))
plot(map)

#a few pie charts for catagorical variables
y<-table(data$Other.Location)
y<-as.data.frame(y)
y<-y[y$Freq!=y$Freq[1],]

Pie <- gvisPieChart(y)
plot(Pie)

#wordcloud
words<-Corpus(DirSource("temp/"))
words <- tm_map(words, removeWords, stopwords("english"))

wordcloud(words,min.freq=3,max.words=200,random.order=FALSE,
          rot.per=0, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#location related charts

  #pie chart of location#
for(i in 1:length(data$Other.Location)){
  if(grepl("Mother",data$Other.Location[i],ignore.case=T)==T){
    data$Location.Descrip[i]<-"Mother's Work"
  }
  if(grepl("Father",data$Other.Location[i],ignore.case=T)==T){
    data$Location.Descrip[i]<-"Father's Work"
  }
  if(grepl("Grand",data$Other.Location[i],ignore.case=T)==T){
    data$Location.Descrip[i]<-"Grandparent's"
  }
  if(grepl("Driveway",data$Other.Location[i],ignore.case=T)==T){
    data$Location.Descrip[i]<-"Driveway"
  }
  if(grepl("care",data$Other.Location[i],ignore.case=T)==T){
    data$Location.Descrip[i]<-"Daycare"
  }
}

#latitude and month#
lat.mon <- ggplot(data, aes(x=mon, y=lat))
  lat.mon + 
  stat_bin2d() +
  scale_fill_gradient(low="#ffeda0", high="#f03b20", limits=c(1, 20))+
  scale_x_continuous(breaks=c(1:12),limits=c(1,12),labels=c("January","February","March",
                                                            "April","May","June","July",
                                                            "August","September","October",
                                                            "November","December"))+
  scale_y_continuous(breaks=c(42.3,36,32,25),labels=c("Boston","Las Vegas","Dallas","Miami"))+
  ylab("Latitude")+xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=1))

#time related charts
  #Annual incident roll up# 
hold<-as.data.frame(table(data$year))
hold$Freq<-as.numeric(hold$Freq)
hold$Var1<-as.character(hold$Var1)
hold$Var1<-as.numeric(hold$Var1)

  ggplot(hold,aes(x=Var1,y=Freq))+geom_line()+
  scale_y_continuous(limit=c(0,50))+
  ylab("Deaths Per Year")+xlab("")+
  stat_smooth(method="lm")+
  theme_bw()

#last five year calendar heatmap#
  #Day of week#
hold<-data[data$Hours.2<24&!is.na(data$weekday),]
hold$weekday<-as.character(hold$weekday)
hold$weekday<-factor(hold$weekday,levels=c("Monday","Tuesday","Wednesday","Thursday",
                                              "Friday","Saturday","Sunday"))
ggplot(hold, aes(x=weekday, y=Hours.2)) + geom_boxplot(notch=T) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  ylab("Duration Left in Vehicle")+xlab("")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday",
                            "Friday","Saturday","Sunday"))+
  theme_bw()

#Duration left in car and age of child#
ggplot(hold, aes(x=Hours.2)) + stat_ecdf() +  
    geom_vline(xintercept=.5,color="black") +
    ylab("Cumulative Total")+xlab("Hours Left in Vehicle")+
    theme_bw()

dur.age<-ggplot(data,aes(x=Hours.2,y=Age.m))
dur.age +
  stat_bin2d()+
  scale_fill_gradient(low="red", high="black", limits=c(1, 10)) +
  scale_x_continuous(breaks=c(.5,6,12,18,24),limits=c(0,24),labels=c("Half Hour Death Line",6,12,18,24))+
  scale_y_continuous(breaks=c(1,12,24,36),limits=c(0,36))+
  ylab("Age in Months")+xlab("Hours Left in Vehicle")+
  geom_vline(xintercept=.5,color="black")+
  theme_bw()

#individual characteristics related charts
  #relationship and age of caregiver by gender
