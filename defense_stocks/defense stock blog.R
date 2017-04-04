# read in the budget data
library(ggplot2)
require(plotly)

budget<-read.csv("dodbudget.csv",header=TRUE)
budget2<-read.csv("dodbudget2.csv",header=TRUE)

budget<-rbind(budget,budget2)
#create a simple bar plot

my.plot <- ggplot(budget, aes(x=Year, y=Funding, fill=Type)) +
  geom_bar(stat="identity",position="dodge") +
  geom_line(data=budget2,aes(x=Year,y=Funding,color=Type))

py <-plotly()
r<-py$ggplotly(my.plot)

