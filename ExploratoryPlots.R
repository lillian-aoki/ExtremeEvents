library(readr)
library(tidyverse)
library(ggplot2)

EEdata <- read.csv("EEdata.csv") #load via readr

EEdata <- distinct(EEdata,UniqueAccession,.keep_all = TRUE)
EEdata <- EEdata[-52,]
EEdata$Extreme_Definition_Num

temp 
  
  
EEdata$`Continuous Monitoring Prior`
ProxEv <- EEdata %>%
  group_by(ProximateEvent_Type)%>%
  summarise(numPapers=length(subset(EEdata,Extreme_Definition=="Yes with a non-statistical context-specific definition")))
  spread(key=Extreme_Definition,value=Def)

ggplot(EEdata,aes(fill=Extreme_Definition,x=ProximateEvent_Type))+geom_bar()+
  theme_bw()

ggplot(EEdata,aes(fill=Extreme_Definition,x=DistalEvent_Type))+geom_bar()
ggplot(EEdata,aes(fill=Extreme_Definition,x=Type_system))+geom_bar()+
  theme_bw()
ggplot(EEdata,aes(x=EEdata$'Continuous Monitoring Prior'))+geom_bar()+
  theme_bw()
