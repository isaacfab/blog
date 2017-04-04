#script for water useage 
require(googleVis)

#make a google vis pie chart for California
data<-read.csv("CaliWater.csv",header=FALSE)
Pie <- gvisPieChart(data,options=list(width=700, height=300))

plot(Pie)
