#load required packages for analysis
#there are some calculations done in excel see those files for specifics
require(datasets)
require(sp)
require(googleVis)
require(rworldmap)
require(ggplot2)
require(scales)
require(stringdist)
library(plotly)
library(wordcloud)
library(tm)

#read data on police fatalites
police<-read.csv("sankey_race.csv",header=T)

dataSK<-police[,1:3]

#create sankey diagrams of percent contribution of killings
Sankey <- gvisSankey(dataSK, from="From", to="To", weight="Percent.Contribution",
                     options=list(
                       sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))

Tab <- gvisTable(police, 
               options=list(page='enable',fontSize="12"))

GT <- gvisMerge(Sankey,Tab, horizontal=FALSE)
plot(GT)

my.plot<-ggplot(police, aes(x=From, y=Percent.of.Killings)) + 
  geom_point(size=5, aes(colour=From)) +
  scale_colour_brewer(palette="Set1", limits=c("Killed By Police","Killed Police"), guide=F) + 
  geom_point(size=5,aes(y=Race.Percent.of.US.Population))+
  scale_y_continuous(breaks=c(0,25,50,75,100),limits=c(0,75))+
  ylab("Percent Contribution")+
  xlab("")+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),text = element_text(size=16),
        axis.text.x = element_text(angle=90, vjust=0)) +
  facet_grid(.~To, scales="free_x", space="free_x")

py <-plotly()

r <- py$ggplotly(my.plot)

#read data on police victims 
police.v<-read.csv("fatal_e.csv",header=T)

#need to consolidate some of the information 
#change the date field into something more useful
police.v$Date.of.injury.resulting.in.death..month.day.year.<-as.POSIXlt(police.v$Date.of.injury.resulting.in.death..month.day.year.,format="%m/%d/%y")

police.v$hour<-police.v$Date.of.injury.resulting.in.death..month.day.year.$hour
police.v$min<-police.v$Date.of.injury.resulting.in.death..month.day.year.$min
police.v$day<-(police.v$Date.of.injury.resulting.in.death..month.day.year.$mday)
police.v$wday<-as.factor(police.v$Date.of.injury.resulting.in.death..month.day.year.$wday)
police.v$month<-(police.v$Date.of.injury.resulting.in.death..month.day.year.$mon+1)
police.v$month<-as.factor(police.v$month)
police.v$year<-police.v$Date.of.injury.resulting.in.death..month.day.year.$year+1900

#need to map the victim race to something less specific to match FBI data
police.v$Subject.s.race<-as.character(police.v$Subject.s.race)
race<-c("African American","European American","Asain/Pacific Islander","Hispanic","Unknown","Native American/Alaska Native")
i<-amatch(police.v$Subject.s.race,race,maxDist=50)
police.v$race<-race[i]

#create a word cloud from the narrative
words<-as.data.frame(as.character(police.v$A.brief.description.of.the.circumstances.surrounding.the.death))
setwd("temp/")
write.csv(words,"words.csv")
#from this point I had to create a .txt file

#chage the directory back up one level setwd("")
words<-Corpus(DirSource("temp/"))
words <- tm_map(words, stripWhitespace)
words <- tm_map(words, tolower)
words <- tm_map(words, removeWords, stopwords("english"))

wordcloud(words,min.freq=3,max.words=200,random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


#create a subset of only the year 2013
sub.police<-police.v[police.v$year==2013,]


