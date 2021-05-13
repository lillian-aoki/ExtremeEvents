## Figure_definitions.R
## This file creates a figure of extreme event definitions using the mini-review data
## Last updated 2021-05-13 by Lillian Aoki

library(readr)
library(tidyverse)
library(cowplot)

## set the theme ####
# this function can be used to get our figures synchronized - sets the global theme for all ggplot objects in the script
# copy paste the following line and run it before the any ggplot plotting functions
theme_set(theme_bw()+ theme(panel.grid = element_blank()))

# note, I also set the axis text sizes in my figure below, if we want consistent text sizes, use the full theme_set() below
theme_set(theme_bw()+ theme(panel.grid = element_blank(),
                            axis.text = element_text(size=11),
                            axis.title = element_text(size=12),
                            legend.text = element_text(size=10)))

## prepare data ####
EEdata <- read_csv("EEdata.csv",skip_empty_rows = TRUE)
EEdata <- distinct(EEdata,UniqueAccession,.keep_all = TRUE)
EEdata <- EEdata[-c(which(is.na(EEdata$UniqueAccession))),]

# create 'broad' categories of proximate event types - Flood, Drought, Heatwave, Other
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

# Create figure for paper ####
fig <- ggplot(EEdata,aes(fill=Extreme_Definition,
                  x=reorder(ProximateEvent_Type_broad,ProximateEvent_Type_broad,function(x)-length(x))))+
  geom_bar()+
  ylab("# of papers")+
  xlab("Extreme event type")+
  theme(legend.title = element_blank())
legend <- get_legend(fig)
fig_nol <- ggplot(EEdata,aes(fill=Extreme_Definition,
                  x=reorder(ProximateEvent_Type_broad,ProximateEvent_Type_broad,function(x)-length(x))))+
  geom_bar()+
  ylab("# of papers")+
  xlab("Extreme event type")+
  theme(legend.title = element_blank(),
        legend.position = "")
fig_leg <- cowplot::plot_grid(fig_nol,legend,ncol=1,rel_heights = c(1,.3))
fig_leg
# export the figure as a PDF for the submission
# BioScience doesn't give dimensions but 1-column is about 3.3 in in width...
ggsave(fig_leg, filename = "Figure_definitions/Fig_definitions.pdf",width=3.25, height=6.5)
