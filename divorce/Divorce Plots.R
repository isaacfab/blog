#creating the google vis map with the mmr data
require(datasets)
require(sp)
require(googleVis)
require(rworldmap)
require(ggplot2)
require(scales)
require(plotly)
require(foreign)

################DATA PROCESSING#######################
#by state marriage and divorce rates for 2012
data<-read.csv("data.csv",strip.white=TRUE)
data$Distribution<-as.numeric(data$Distribution)

#remove states with no data and label divorce rates
data2<-data[!is.na(data$Seperation.Rate),]
data2$Sign<-c("")
data2$Sign[data2$Distribution>=0]<-c("Above 50%")
data2$Sign[data2$Distribution<0]<-c("Below 50%")
data2$Sign<-as.factor(data2$Sign)

#read in combined data for motion chart
comb.data<-read.csv("CombData.csv",strip.white=TRUE)
#loop to organize the data
comb<-data.frame()
t<-1999
c<-15
for(j in 1:14){ 
  for(i in 1:52){
    S<-as.character(comb.data[i,1])
    MR<-as.numeric(as.character(comb.data[i,c]))
    DR<-as.numeric(as.character(comb.data[(i+52),c]))
    DP<-DR/MR
    hold<-data.frame(Year=t,State=S,Marriage.Rate=MR,Divorce.Rate=DR,Divorce.Percent=DP)
    comb<-rbind(comb,hold)
  }
t<-t+1
c<-c-1
}
comb<-comb[!is.na(comb$Divorce.Rate),]
comb<-comb[comb$State!="Nevada",]
comb<-comb[comb$State!="Hawaii",]


#read the CDC data from the NSFG
#female survey data
f.cdc<-read.dta("Female_Data.dta")
#male survey data
m.cdc<-read.dta("Male_Data.dta")

######female analysis#######
#only view individuals that ever got married
f.cdc<-f.cdc[f.cdc$EVRMARRY=="EVER MARRIED",]
f.cdc.10yr<-f.cdc[f.cdc$CMINTVW-f.cdc$MARDAT01>=120,]
#calculate statistical divorce rates 
rate.data<-data.frame()
x<-(f.cdc.10yr[f.cdc.10yr$FMARNO!="1 TIME"|(f.cdc.10yr$FMARNO=="1 TIME"&(f.cdc.10yr$MARSTAT!=1&f.cdc.10yr$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(f.cdc.10yr$MARSTAT)

hold<-data.frame(Type=c("First Marriage Average"),Rate=rate,Gender=c("Female"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$FMARNO!="1 TIME"&(f.cdc.10yr$CMINTVW-f.cdc.10yr$MARDAT02>=120),]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="2 TIMES"|(temp.f.cdc$FMARNO=="2 TIMES"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Second Marriage Average"),Rate=rate,Gender=c("Female"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-temp.f.cdc[temp.f.cdc$FMARNO!="2 TIMES"&(temp.f.cdc$CMINTVW-temp.f.cdc$MARDAT03>=120),]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="3 TIMES"|(temp.f.cdc$FMARNO=="3 TIMES"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Third Marriage Average"),Rate=rate,Gender=c("Female"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$RSCRRACE==1|f.cdc.10yr$RSCRRACE==2|f.cdc.10yr$RSCRRACE==3,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Asain"),Rate=rate,Gender=c("Female"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$RSCRRACE==6,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Hispanic"),Rate=rate,Gender=c("Female"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$RSCRRACE==5,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("White"),Rate=rate,Gender=c("Female"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$RSCRRACE==4,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("African American"),Rate=rate,Gender=c("Female"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[is.na(f.cdc.10yr$DEGREES),]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("No Degree"),Rate=rate,Gender=c("Female"),SubType=c("Education"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[!is.na(f.cdc.10yr$DEGREES),]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Has College Degree"),Rate=rate,Gender=c("Female"),SubType=c("Education"))
rate.data<-rbind(rate.data,hold)

f.cdc.10yr$Age_At_M<-(f.cdc.10yr$MARDAT01-f.cdc.10yr$CMBIRTH)
f.cdc.10yr$Age_At_M<-round(f.cdc.10yr$Age_At_M/12)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$Age_At_M<20,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: Less than 20"),Rate=rate,Gender=c("Female"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$Age_At_M<25&f.cdc.10yr$Age_At_M>=20,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: 20-25"),Rate=rate,Gender=c("Female"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$Age_At_M<30&f.cdc.10yr$Age_At_M>=25,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: 26-30"),Rate=rate,Gender=c("Female"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.f.cdc<-f.cdc.10yr[f.cdc.10yr$Age_At_M>=30,]
x<-(temp.f.cdc[temp.f.cdc$FMARNO!="1 TIME"|(temp.f.cdc$FMARNO=="1 TIME"&(temp.f.cdc$MARSTAT!=1&temp.f.cdc$MARSTAT!=3)),])
rate<-length(x$MARSTAT)/length(temp.f.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: Greater than 30"),Rate=rate,Gender=c("Female"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)
#######male analysis########
#only view individuals that ever got married
m.cdc<-m.cdc[m.cdc$EVRMARRY=="EVER MARRIED",]

#only those with 10 years since first marriage
#interview date CMINTVW 1st marriage date MARDAT01
m.cdc.10yr<-m.cdc[m.cdc$CMINTVW-m.cdc$MARDAT01>=120,]
#code for how many have times been married FMARNO
#current status MARSTAT
#calculate statistical divorce rates 
x<-(m.cdc.10yr[m.cdc.10yr$FMARNO!="1 TIME"|(m.cdc.10yr$FMARNO=="1 TIME"&(m.cdc.10yr$MARSTAT!="Married"&m.cdc.10yr$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(m.cdc.10yr$MARSTAT)

hold<-data.frame(Type=c("First Marriage Average"),Rate=rate,Gender=c("Male"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[(m.cdc.10yr$FMARNO!="1 TIME"&(m.cdc.10yr$CMINTVW-m.cdc.10yr$MARDAT02>=120)),]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="2 TIMES"|(temp.m.cdc$FMARNO=="2 TIMES"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Second Marriage Average"),Rate=rate,Gender=c("Male"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-temp.m.cdc[temp.m.cdc$FMARNO!="2 TIMES"&(temp.m.cdc$CMINTVW-temp.m.cdc$MARDAT03>=120),]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="3 TIMES"|(temp.m.cdc$FMARNO=="3 TIMES"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Third Marriage Average"),Rate=rate,Gender=c("Male"),SubType=c("Number of Marriages"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$RSCRRACE==1|m.cdc.10yr$RSCRRACE==2|m.cdc.10yr$RSCRRACE==3,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Asain"),Rate=rate,Gender=c("Male"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$RSCRRACE==6,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Hispanic"),Rate=rate,Gender=c("Male"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$RSCRRACE==5,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("White"),Rate=rate,Gender=c("Male"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$RSCRRACE==4,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("African American"),Rate=rate,Gender=c("Male"),SubType=c("Race"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[is.na(m.cdc.10yr$DEGREES),]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("No Degree"),Rate=rate,Gender=c("Male"),SubType=c("Education"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[!is.na(m.cdc.10yr$DEGREES),]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Has College Degree"),Rate=rate,Gender=c("Male"),SubType=c("Education"))
rate.data<-rbind(rate.data,hold)
#code for age CMBIRTH
m.cdc.10yr$Age_At_M<-(m.cdc.10yr$MARDAT01-m.cdc.10yr$CMBIRTH)
m.cdc.10yr$Age_At_M<-round(m.cdc.10yr$Age_At_M/12)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$Age_At_M<20,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: Less than 20"),Rate=rate,Gender=c("Male"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$Age_At_M<25&m.cdc.10yr$Age_At_M>=20,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: 20-25"),Rate=rate,Gender=c("Male"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$Age_At_M<30&m.cdc.10yr$Age_At_M>=25,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: 26-30"),Rate=rate,Gender=c("Male"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

temp.m.cdc<-m.cdc.10yr[m.cdc.10yr$Age_At_M>=30,]
x<-(temp.m.cdc[temp.m.cdc$FMARNO!="1 TIME"|(temp.m.cdc$FMARNO=="1 TIME"&(temp.m.cdc$MARSTAT!="Married"&temp.m.cdc$MARSTAT!="Widowed")),])
rate<-length(x$MARSTAT)/length(temp.m.cdc$MARSTAT)

hold<-data.frame(Type=c("Age at Mar: Greater than 30"),Rate=rate,Gender=c("Male"),SubType=c("Age"))
rate.data<-rbind(rate.data,hold)

######################PLOT SCRIPTS######################
#create a google plot map of marriage and divorce rates

#marriage rate map
MarStates <- gvisGeoChart(data, "State", "Marriage.Rate",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       colors="['white','green']",
                                       width=500, height=300))

#divorce rate map
DivStates <- gvisGeoChart(data2, "State", "Seperation.Rate",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",                        
                                       colorAxis="{colors:['white', 'red']}",
                                       datalessRegionColor="grey",
                                       width=500, height=300))

MD<-gvisMerge(MarStates,DivStates, horizontal=TRUE) 
plot(MD)

#remove nevada becasue it is an obvious outlier of travel marriages 
data2<-data2[!data2$State=="Nevada",]

# Order the data frame by difference in divorce rates
data2<-data2[order(data2$Percent.Difference),]

#create plot of difference
my.plot<-ggplot(data2, aes(x=reorder(State,Percent.Difference), y=Percent.Difference)) + 
  geom_point(size=5,aes(colour=Sign)) + #if using plotly only use the first two lines
  scale_colour_brewer(palette="Set1", limits=c("Above 50%","Below 50%"), guide=FALSE) +
  scale_y_continuous(limits=c(.2,.8),breaks=c(.25,.3,.35,.4,.45,.5,.55,.60,.65,.7,.75),labels = percent) +
  geom_hline(yintercept=.5) +
  geom_hline(yintercept=.45, color="grey") +
  geom_hline(yintercept=.55, color="grey") +
  ylab("")+
  xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1,size=12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

py <-plotly()
r<-py$ggplotly(my.plot)

#Motion Chart with all states
p<-gvisMotionChart(comb, 
                idvar="State", 
                colorvar="Divorce.Percent",
                xvar="Marriage.Rate",
                yvar="Divorce.Rate",
                sizevar="Divorce.Percent",
                timevar="Year",
                options=list(height=600,width=700)) 
plot(p)

#plot stuff
rate.data$Gender<-as.factor(rate.data$Gender)
rate.data$SubType<-as.factor(rate.data$SubType)
#"Number of Marriages","Race","Education","Age"

my.plot<-ggplot(rate.data, aes(x=Type, y=Rate)) +
  #geom_segment(aes(yend=Type), xend=0, colour="grey50") +
  geom_point(size=5, aes(colour=Gender)) +
 # facet_wrap(~SubType,ncol=1)
  xlab("")+
  ylab("")+
  #scale_colour_brewer(palette="Set1", limits=c("Female","Male"), guide=FALSE) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  facet_grid(.~SubType, scales="free_x", space="free_x")

out <- py$ggplotly(my.plot, kwargs=list(filename="facet_grid", fileopt="overwrite"))
