ExploratoryPlots
================
LRA
11/11/2019

Importing the EE dataset

``` r
EEdata <- read_csv("EEdata.csv",skip_empty_rows = TRUE)
EEdata <- distinct(EEdata,UniqueAccession,.keep_all = TRUE)
EEdata <- EEdata[-50,] #can't figure out a better way to get rid of the final NA row
```

Add new data columns to summarize event types and system types

``` r
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
```

Re-order the levels of the statistical definition question

``` r
EEdata$Extreme_Definition <- as.factor(EEdata$Extreme_Definition)
ExDef_order <- c("No - extreme event not mentioned but inferred",
                 "No - extreme event mentioned but not defined",
                 "Yes with a non-statistical general definition",
                 "Yes with a non-statistical context-specific definition",
                 "Yes with a statistical general definition",
                 "Yes with a statistical context-specific definition")
EEdata$Extreme_Definition <- ordered(EEdata$Extreme_Definition,levels=ExDef_order)
```

## Exploratory Plots

``` r
ggplot(EEdata,aes(fill=Extreme_Definition,x=ProximateEvent_Type_broad))+geom_bar()+
  ylab("# of papers")+
  theme_bw()
```

![](ExploratoryPlots_files/figure-gfm/exploratory%20plots-1.png)<!-- -->

``` r
ggplot(EEdata,aes(fill=Extreme_Definition,x=DistalEvent_Type))+geom_bar()+
  ylab("# of papers")+
  theme_bw()
```

![](ExploratoryPlots_files/figure-gfm/exploratory%20plots-2.png)<!-- -->

``` r
ggplot(EEdata,aes(fill=Extreme_Definition,x=Type_system_broad))+geom_bar()+
  ylab("# of papers")+
  theme_bw()
```

![](ExploratoryPlots_files/figure-gfm/exploratory%20plots-3.png)<!-- -->

``` r
ggplot(EEdata,aes(x=EEdata$'Continuous Monitoring Prior'))+geom_bar()+
  xlab("Continuous monitoring prior to event")+
  ylab("# of papers")+
  theme_bw()
```

![](ExploratoryPlots_files/figure-gfm/exploratory%20plots-4.png)<!-- -->