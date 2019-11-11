library(readr)
library(tidyverse)
library(ggplot2)

EEdata <- read_csv("EEdata.csv",skip_empty_rows = TRUE)
EEdata <- distinct(EEdata,UniqueAccession,.keep_all = TRUE)
EEdata <- EEdata[-50,] #can't figure out a better way to get rid of the final NA row

### Adding some now data columns for more broad categories

EEdata$Type_system_broad <- factor(rep(NA,length(EEdata$Type_system)),
                                   levels=c("freshwater","coastal","marine"))
EEdata$Type_system_broad[EEdata$Type_system %in% c("catchment/watershed","floodplain","lake","pond","stream/river","wetland")] <- "freshwater"
EEdata$Type_system_broad[EEdata$Type_system %in% c("estuary","lagoon","marine intertidal")] <- "coastal" 
# question if "coastal ocean" is coastal or marine - the only paper in this category looked at subtidal algae beds
EEdata$Type_system_broad[EEdata$Type_system %in% c("coastal ocean","deep sea","open ocean")] <- "marine"
# given there are so few marine events, maybe better to combine coastal and marine categories

EEdata$ProximateEvent_Type_broad <- factor(rep(NA,length(EEdata$ProximateEvent_Type)),
                                           levels=c("Flood","Drought","Heatwave","Other"))
EEdata$ProximateEvent_Type_broad[EEdata$ProximateEvent_Type %in% c("Flood")] <- "Flood"
EEdata$ProximateEvent_Type_broad[EEdata$ProximateEvent_Type %in% c("Drought")] <- "Drought"
EEdata$ProximateEvent_Type_broad[EEdata$ProximateEvent_Type %in% c("Heatwave")] <- "Heatwave"
EEdata$ProximateEvent_Type_broad[is.na(EEdata$ProximateEvent_Type_broad)] <- "Other"

### Re-ordering the statistical definitions
EEdata$Extreme_Definition <- as.factor(EEdata$Extreme_Definition)
ExDef_order <- c("No - extreme event not mentioned but inferred",
                 "No - extreme event mentioned but not defined",
                 "Yes with a non-statistical general definition",
                 "Yes with a non-statistical context-specific definition",
                 "Yes with a statistical general definition",
                 "Yes with a statistical context-specific definition")
EEdata$Extreme_Definition <- ordered(EEdata$Extreme_Definition,levels=ExDef_order)
### Exploratory plots
ggplot(EEdata,aes(fill=Extreme_Definition,
                  x=reorder(ProximateEvent_Type,ProximateEvent_Type,function(x)-length(x))))+
  geom_bar()+
  ylab("# of papers")+
  xlab("Proximate Event Type - Specific")+
  labs(title="Papers sorted by Proximate Event - Specific")+
  theme_bw()
# Most papers look at flood, drought, or heatwave
ggplot(EEdata,aes(fill=Extreme_Definition,
                  x=reorder(ProximateEvent_Type_broad,ProximateEvent_Type_broad,function(x)-length(x))))+
  geom_bar()+
  ylab("# of papers")+
  xlab("Proximate Event Type - Broad")+
  labs(title="Papers sorted by Proximate Event - Broad")+
  theme_bw()
# Similar split of definitions across event types, about 1/3 use statistical defintion, 1/3 use non-statistical definition
# 1/3 don't define 'extreme event'
ggplot(EEdata,aes(fill=Extreme_Definition,x=reorder(DistalEvent_Type,DistalEvent_Type,function(x)-length(x))))+geom_bar()+
  ylab("# of papers")+
  xlab("Distal Event Type - Specific")+
  labs(title="Papers sorted by Distal Event - Specific")+
  theme_bw()
# Not sure how to use these categories compared to Proximate event - drought is in both??
ggplot(EEdata,aes(fill=Extreme_Definition,x=reorder(Type_system,Type_system,function(x)-length(x))))+geom_bar()+
  ylab("# of papers")+
  xlab("System type - Specific")+
  labs(title="Papers sorted by System - Specific")+
  theme_bw()
# Estuaries and streams/rivers are largest groups, catchment/watershed is also big group
ggplot(EEdata,aes(fill=Extreme_Definition,x=Type_system_broad))+geom_bar()+
  ylab("# of papers")+
  xlab("System type - Broad")+
  labs(title="Papers sorted by System - Broad")+
  theme_bw()
# Even split if we group coastal with marine
ggplot(EEdata,aes(x=EEdata$'Continuous Monitoring Prior',fill=Type_system_broad))+geom_bar()+
  xlab("Continuous monitoring prior to event")+
  ylab("# of papers")+
  labs(title="Papers split by prior monitoring")+
  theme_bw()
# ~2/3 had monitoring, 1/3 did not; similar split between fresh/marine