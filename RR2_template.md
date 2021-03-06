# Reproducible Research: Peer Assessment 2
#  Damage to population and economy from weather in the United States 

## Synopsis
I have used the Storm Data dataset provided by the National Weather Service to analyse which weather events are most harmful. The analysis looks at two aspectes of damage; damage to population health and economic damage. This analysis looks at damage from weather from year 2000-2011, identifying the events causing the largest numbers of fatalities and injuries as well as the events leading to the largest costs from property and crop damage. 

My analysis shows that the events that cause most population damage are excessive heat, tornados, floods and thunderstorm. 

When it comes to economic damage, floods cause by far the most damage when looking at all years. During 2011 tornados cause the most damage, but this pattern is not repeated for other years. 

## Data Processing
This study is based on the National Weather Services' Storm Data dataset, and can be downloaded from [here.](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) It includes data on damage from weather event in the United States from 1950 up to 2011. It is read and loaded to the variable *stormdata* Some documentation for the dataset can be found [here. ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

```r
if (!file.exists("stormdata.csv.bz2")){
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","stormdata.csv.bz2", method = "curl")}
stormdata.full<- read.csv(bzfile("stormdata.csv.bz2"), header = TRUE, na.strings = "")
```

The data is processed in the following steps: 

1. Column names have been transformed to lower case 
2. Only the columns of the data concerning damage caused by weather, i.e. the ones needed for this analysis, are kept
3. The column *year* is added
4. A new column called *evtype_cleaned* is created, this is based on the information from *evtype*,and interprets each reported event as one of the 48 event types listed in section 7 of the storm data documentation. To achieve this, the event types are modified in the following ways:
    + All event names are transformed to lower case and numbers, special characters and leading and trailing spaces are removed
    + Some common variations and abbrevations for events are replaced with a standard event name, e.g. "TSTM"" is replaced by "thunderstorm"
    + Words that do not contain a weather event, but rather a description or conjunction such as "and" and "severe" are excluded
    + The event definition from the documentation are read and stored in the vector *events* and the cleaned event types are compared to this vector using the function adist and partial matching.  
      + When there is a close enough match, the event name will be replaced by the corresponding entry from *events*. 
      + If no appropriate match can be found, the event is renamed to "other". 
      + If there are multiple matches the one with most matching words will be chosen. 
      + If there are still multiple matching, the one with the smallest number of mismatched words will be chosen. 
5. New columns with the calculated value of crop damage, property damage and total economic damage (crop and property damages combined) are added and stored in the columns *propdmgest*, *cropdmgest*, and *dmgest*.
6. The data is limited to data reported during year 2000 or later. The reason for this is twofold: 
    + Firstly, Events that took place more than ten years ago (relative to the last date of the dataset) is not relevant for assessing how to prevent damage from events that happened today. The measures we take to prevent damage from weather events have changed (and hopefully improved) greatly since the 1950s. For example, we build houses differently now than we did before, and our strategies for handling extreme weather and catastrophes have been updated. So, a large number of fatalities for a specific event in 1950 might not be at all relevant for precenting fatalities today.   
    + We do not have as much data from the early years of the dataset as we have for the later years (see exploratory analysis), for example, the first reports of economic damage are from 1995. 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(knitr)
names(stormdata.full)<-tolower(names(stormdata.full))
stormdata.full$year <- year(as.Date(stormdata.full$bgn_date,"%m/%d/%Y"))
stormdata <- select(stormdata.full, evtype, fatalities, injuries, propdmg, propdmgexp, cropdmg, cropdmgexp, year)
stormdata <- filter(stormdata, year>=2000)
```

Processing of the event types:

```r
# remove special characters and trailing/leading spaces 
stormdata$evtype<-tolower(stormdata$evtype)
stormdata$evtype_cleaned<- gsub("[0-9]*[\\.\\(\\)]*","", stormdata$evtype)
stormdata$evtype_cleaned<- gsub("[-/:]"," ", stormdata$evtype_cleaned)
stormdata$evtype_cleaned<- gsub("^[ ]","",  stormdata$evtype_cleaned)
stormdata$evtype_cleaned<- sub("[ ]$", "", stormdata$evtype_cleaned)

# I copied the 48 event types from the content list of the documentation
# read them from txt file in work directory
events<-read.csv("eventtypes.txt", header = FALSE, stringsAsFactors = FALSE)
events<-sapply(events, tolower)
events<-gsub("[0-9]*","", events)
events<-gsub("[-/:]"," ", events)
events<-gsub( " *\\(.*?\\) *", "", events)
events<-gsub("[ ]$","",  events)
events<-gsub("^[ ]","",  events)

# Replace the abbreviations and variations of event name with standard name
replace <- c("tstm", "winds", "storms", "flooding", "currents", "fires", "fld","wild fire", "hot", "snowfall", "warm","cool","wnd","windchill","storm surge","dry","landslide","precipitation","wintry mix")
with    <- c("thunderstorm", "wind", "storm", "flood", "current", "fire", "flood", "wildfire", "heat","snow","heat","cold","wind","wind chill","coastal flood","drought","debris flow","rain","winter weather")

for(i in seq_along(replace)){
    stormdata$evtype_cleaned<-gsub(replace[i], with[i], stormdata$evtype_cleaned, ignore.case = TRUE)
}
rm(i, replace, with)

# Remove words that do not contains information about the event we want to map
remove <- c("unseasonably","light ","and ","temperatures", "advisories","gusty","conditions", "prolong","very ", "unusually","extremely","abnormally","unseasonal","severe","record","advisory","hard ")
for(i in seq_along(remove)){
    stormdata$evtype_cleaned<-gsub(remove[i], "", stormdata$evtype_cleaned, ignore.case = TRUE)
}
rm(i, remove)

# store unique event names from stormdata and extract thjose that don't have an exact match in events
event_orgnames <- unique(stormdata$evtype_cleaned)
event_orgnames <- event_orgnames[!event_orgnames %in% events]

# map these event names to their closest match
for(i in seq_along(event_orgnames)){
    d1 <- adist(event_orgnames[i],events, partial = TRUE)
    d2 <- t(adist(events, event_orgnames[i], partial = TRUE))
    d <- apply(rbind(d1,d2), MARGIN=2,FUN=min)
    
    # allowed distance depends on length of string
    reldist <- 1.0*min(d)/nchar(event_orgnames[i])
    # identified matches
    pot.matches <-events[which(d==min(d))]
  
    if(reldist<=0.2){
      if(length(pot.matches)==1)  
        stormdata$evtype_cleaned[stormdata$evtype_cleaned==event_orgnames[i]] <- pot.matches
      else if (length(pot.matches)>1){
        a_split <- unlist(strsplit(event_orgnames[i], split=" ", fixed=TRUE))
        b_split <- strsplit(pot.matches, split=" ", fixed=TRUE)
        commonwords <- sapply(b_split, FUN = function(x) length(intersect(unlist(x),a_split)))
        diffwords <- sapply(b_split, FUN = function(x) length(setdiff(unlist(x),a_split)))
        
        if(sum(commonwords==max(commonwords))==1){
          stormdata$evtype_cleaned[stormdata$evtype_cleaned==event_orgnames[i]] <- pot.matches[commonwords==max(commonwords)]
        }
        else if (sum(diffwords==min(diffwords))==1){
          stormdata$evtype_cleaned[stormdata$evtype_cleaned==event_orgnames[i]] <- pot.matches[diffwords==min(diffwords)]
        }
        else stormdata$evtype_cleaned[stormdata$evtype_cleaned==event_orgnames[i]] <- "other"
      }
    }
    else stormdata$evtype_cleaned[stormdata$evtype_cleaned==event_orgnames[i]] <- "other"
}
```

Processing the values for the cost for crop and property damage: 

```r
# Assign a numeric value to the crop and property damage by translating the alphabetical magnuítudes to numeric values 
# Economic damage will be stored in million dollars
stormdata$propdmgexp<-tolower(stormdata$propdmgexp)
stormdata$cropdmgexp<-tolower(stormdata$cropdmgexp)

alphabetic <- c("k", "m", "b")
magnitude  <- c(0.001,1,1000)
stormdata$propdmgest <- 0;
stormdata$cropdmgest <- 0;

for(i in seq_along(alphabetic)){
    prop_ind <- which(stormdata$propdmgexp==alphabetic[i] & !is.na(stormdata$propdmg))
    stormdata$propdmgest[prop_ind] <-  magnitude[i]*stormdata$propdmg[prop_ind]
    
    crop_ind <- which(stormdata$cropdmgexp==alphabetic[i] & !is.na(stormdata$cropdmg))
    stormdata$cropdmgest[crop_ind] <-  magnitude[i]*stormdata$cropdmg[crop_ind]
}
stormdata$dmgest <- stormdata$cropdmgest+stormdata$propdmgest
rm(i,prop_ind,crop_ind,alphabetic,magnitude)
```

## Results
### Events causing population harm 
The data contains information on the number of fatalitites and injuries caused by the different types of weather. To assess how much harm each type of weather does I have counted the number of fatalities and injuries caused by each of the grouped weather types for the full data as well as restricted to the last year in the data. The rationale behind restricting the data to the last year is to better see which types of weather are problematic at the time of data collection, and where efforts to prevent harm should be directed in order to be most efficient.  


```r
library(ggplot2)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.2.2
```

```r
# sum fatalities and injuries by event type and sort to show events with most fatalities/injuries
fatal    <- stormdata %>% group_by(evtype_cleaned) %>% summarize(fatal = sum(fatalities)) %>% top_n(10, fatal)
recentfatal <- stormdata %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(rec_fatal = sum(fatalities)) %>% top_n(10, rec_fatal)
recentfatal <- recentfatal[order(recentfatal$rec_fatal, decreasing=TRUE),]
injuries <- stormdata %>% group_by(evtype_cleaned) %>% summarize(injuries = sum(injuries)) %>% top_n(10, injuries)
recentinjuries <- stormdata %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(rec_injuries = sum(injuries)) %>% top_n(10, rec_injuries)
recentinjuries <- recentinjuries[order(recentinjuries$rec_injuries, decreasing=TRUE),]

labeltheme <- theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())
geomstyle  <- geom_bar(stat="identity", fill="steelblue", col="darkblue")

g1 <- ggplot(data=fatal, aes(x=evtype_cleaned, y=fatal)) + geomstyle + ggtitle("10 events causing most fatalities")+labeltheme+ylab("Fatalities")
g2 <- ggplot(data=injuries, aes(x=evtype_cleaned, y=injuries)) + geomstyle + ggtitle("10 events causing most injuries")+labeltheme+ylab("Injuries")
grid.arrange(g1, g2, nrow=1, ncol=2)
```

![Figure 2](RR2_template_files/figure-html/identify_harm-1.png) 

```r
kable(recentfatal, digits=0, col.names = c("Event","Fatalities"),
      caption = "Table 1: Events causing most fatalities in 2011")
```



Table: Table 1: Events causing most fatalities in 2011

Event                Fatalities
------------------  -----------
tornado                     587
flash flood                  68
heat                         63
flood                        58
thunderstorm wind            54
excessive heat               36
rip current                  29
lightning                    26
cold wind chill              21
high surf                    11

```r
kable(recentinjuries, digits=0, col.names = c("Event","Injuries"),
      caption = "Table 2: Events causing most injuries in 2011")
```



Table: Table 2: Events causing most injuries in 2011

Event                Injuries
------------------  ---------
tornado                  6163
heat                      611
thunderstorm wind         373
lightning                 194
excessive heat            138
wildfire                  116
strong wind                33
hail                       31
flash flood                30
rip current                27

These graphs show the 10 events with the largest number of fatalities or injuries for the full data. Tables 1 and 2 show the same information when restricting the data to 2011. From the graphs we can see that tornados cause the largest number of injuries and also a large number of fatalities. The second biggest cause of fatalitites for data from 2000-2011 is excessive heat which causes a slightly smaller number of deaths. Other large causes of fatalitites and injuries are floods, thunderstorms, lightning, and rip currents.   

It can be noted that there is a large number of injuries and fatalites form tornados in 2011. Some [wikipedia reserach](https://en.wikipedia.org/wiki/April_25%E2%80%9328,_2011_tornado_outbreak) shows that this is due to an exceptionally large number of tornado outbreaks in the US in 2011. 

### Events with greatest economic consequences 

I have summed the damage to properties and the damage to crops to get the total amount of economic damage. The distribution of total economic damage by event is shown in the plots below. 


```r
# sum economic damage by event type and sort to show events with greatest economic consequences 
dmg <- stormdata %>% na.omit() %>% group_by(evtype_cleaned) %>% summarize(dmgest = sum(dmgest)) %>% top_n(10, dmgest)
dmg <- dmg[order(dmg$dmgest, decreasing=TRUE),]
recentdmg <- stormdata %>% na.omit() %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(dmgest = sum(dmgest)) %>% top_n(10, dmgest)
recentdmg <- recentdmg[order(recentdmg$dmgest, decreasing=TRUE),]
  
ggplot(data=dmg, aes(x=evtype_cleaned, y=dmgest)) + geomstyle + ggtitle("Total damage by event type")+ylab("Damage in million $")+labeltheme
```

<img src="RR2_template_files/figure-html/identify_eco-1.png" title="Figure 3" alt="Figure 3" style="display: block; margin: auto;" />

```r
kable(recentdmg, digits=0, col.names = c("Event","Estimated damage (million $)"),
      caption = "Table 6: Events causing largest economic damage in 2011")
```



Table: Table 6: Events causing largest economic damage in 2011

Event                Estimated damage (million $)
------------------  -----------------------------
tornado                                      9851
flood                                        7913
flash flood                                  1472
wildfire                                      658
hail                                          534
thunderstorm wind                             522
tropical storm                                163
high wind                                      86
tsunami                                        54
lightning                                      47

From the plots and table 3 we see that the event causing the largest damage in million dollars is floods followed by hurricane typhoons and tornados. If we only look at the 2011 data in the table the pattern is different, with tornados being the largest source of economic damage, closely followed by floods. 

## Discussion and conclusions 
The results from this analysis indicates that the events causing most damage (both economical and to the population) are tornados, floods and heat spells. Efforts for preventing weather related damage should be focused on these types of events. 

## Appendix; Exploratory analysis 
Here are some exploratory searches I performed to better understand the data. They are available for reference (and for my own sake), but are not essential in understanding and interpreting the analysis and results. Please feel free to ignore. 


```r
names(stormdata)
```

```
##  [1] "evtype"         "fatalities"     "injuries"       "propdmg"       
##  [5] "propdmgexp"     "cropdmg"        "cropdmgexp"     "year"          
##  [9] "evtype_cleaned" "propdmgest"     "cropdmgest"     "dmgest"
```

```r
summary(stormdata)
```

```
##     evtype            fatalities           injuries       
##  Length:523163      Min.   :  0.00000   Min.   :0.00e+00  
##  Class :character   1st Qu.:  0.00000   1st Qu.:0.00e+00  
##  Mode  :character   Median :  0.00000   Median :0.00e+00  
##                     Mean   :  0.01146   Mean   :6.72e-02  
##                     3rd Qu.:  0.00000   3rd Qu.:0.00e+00  
##                     Max.   :158.00000   Max.   :1.15e+03  
##     propdmg         propdmgexp           cropdmg         cropdmgexp       
##  Min.   :   0.00   Length:523163      Min.   :  0.000   Length:523163     
##  1st Qu.:   0.00   Class :character   1st Qu.:  0.000   Class :character  
##  Median :   0.00   Mode  :character   Median :  0.000   Mode  :character  
##  Mean   :  11.41                      Mean   :  1.708                     
##  3rd Qu.:   1.00                      3rd Qu.:  0.000                     
##  Max.   :5000.00                      Max.   :990.000                     
##       year      evtype_cleaned       propdmgest         cropdmgest      
##  Min.   :2000   Length:523163      Min.   :0.00e+00   Min.   :0.00e+00  
##  1st Qu.:2003   Class :character   1st Qu.:0.00e+00   1st Qu.:0.00e+00  
##  Median :2006   Mode  :character   Median :0.00e+00   Median :0.00e+00  
##  Mean   :2006                      Mean   :6.30e-01   Mean   :4.51e-02  
##  3rd Qu.:2009                      3rd Qu.:0.00e+00   3rd Qu.:0.00e+00  
##  Max.   :2011                      Max.   :1.15e+05   Max.   :1.51e+03  
##      dmgest        
##  Min.   :0.00e+00  
##  1st Qu.:0.00e+00  
##  Median :0.00e+00  
##  Mean   :6.80e-01  
##  3rd Qu.:0.00e+00  
##  Max.   :1.15e+05
```

```r
# which events are most common?
stormdata.common <- stormdata %>% count(evtype_cleaned)
stormdata.common[order(stormdata.common$n, decreasing = TRUE),]
```

```
## Source: local data frame [48 x 2]
## 
##               evtype_cleaned      n
##                        (chr)  (int)
## 1                       hail 166449
## 2          thunderstorm wind 166449
## 3                flash flood  40586
## 4                      flood  21478
## 5                    tornado  17688
## 6                  high wind  16514
## 7   marine thunderstorm wind  11987
## 8                 heavy snow  11226
## 9               winter storm   9774
## 10                 lightning   9686
## ..                       ...    ...
```

```r
# which of these are not mapped?
stormdata.common <- stormdata[!stormdata$evtype_cleaned %in% events,] %>% count(evtype_cleaned)
stormdata.common[order(stormdata.common$n, decreasing = TRUE),]
```

```
## Source: local data frame [1 x 2]
## 
##   evtype_cleaned     n
##            (chr) (int)
## 1          other   189
```

```r
# which are mapped?
stormdata.common <- stormdata[stormdata$evtype_cleaned %in% events,] %>% count(evtype_cleaned)
stormdata.common[order(stormdata.common$n, decreasing = TRUE),]
```

```
## Source: local data frame [47 x 2]
## 
##               evtype_cleaned      n
##                        (chr)  (int)
## 1                       hail 166449
## 2          thunderstorm wind 166449
## 3                flash flood  40586
## 4                      flood  21478
## 5                    tornado  17688
## 6                  high wind  16514
## 7   marine thunderstorm wind  11987
## 8                 heavy snow  11226
## 9               winter storm   9774
## 10                 lightning   9686
## ..                       ...    ...
```

```r
# sum fatalities by event type and sort to show events with most fatalities
stormdata.fatal <- stormdata %>% group_by(evtype_cleaned) %>% summarize(fatalities = sum(fatalities))
stormdata.fatal[order(stormdata.fatal$fatalities, decreasing = TRUE),]
```

```
## Source: local data frame [48 x 2]
## 
##              evtype_cleaned fatalities
##                       (chr)      (dbl)
## 1                   tornado       1193
## 2            excessive heat       1013
## 3               flash flood        600
## 4                 lightning        466
## 5               rip current        462
## 6                     flood        289
## 7         thunderstorm wind        248
## 8                      heat        246
## 9                 avalanche        179
## 10  extreme cold wind chill        154
## ..                      ...        ...
```

```r
# sum fatalities by event type and sort to show events with most fatalities
stormdata.injur <- stormdata %>% group_by(evtype_cleaned) %>% summarize(injuries = sum(injuries))
stormdata.injur[order(stormdata.injur$injuries, decreasing = TRUE),]
```

```
## Source: local data frame [48 x 2]
## 
##        evtype_cleaned injuries
##                 (chr)    (dbl)
## 1             tornado    15213
## 2      excessive heat     3708
## 3   thunderstorm wind     3157
## 4           lightning     2993
## 5   hurricane typhoon     1291
## 6                heat     1256
## 7      winter weather     1165
## 8            wildfire      911
## 9         flash flood      812
## 10          high wind      677
## ..                ...      ...
```

```r
# properties of the property and crop damage columns
summary(stormdata$propdmg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   11.41    1.00 5000.00
```

```r
summary(stormdata$cropdmg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.000   0.000   1.708   0.000 990.000
```

```r
summary(stormdata$propdmgexp)
```

```
##    Length     Class      Mode 
##    523163 character character
```

```r
summary(stormdata$cropdmgexp)
```

```
##    Length     Class      Mode 
##    523163 character character
```

```r
# economic damage from tornados over time
tornado <- stormdata %>% na.omit() %>% filter(evtype_cleaned=="tornado") %>% group_by(year, evtype_cleaned) %>% summarize(dmgest = sum(dmgest))%>% top_n(10, dmgest)
```

Some plots for which output is hidden to not exceed the limit of three plots


```r
# How many records per year do we have?
# Reporting seem to be taking of by 1995
plot(stormdata.full %>% count(year), pch=19)+abline(v=1995)

# no records of crop or prop damage before 95
plot(stormdata.full %>% group_by(year) %>%  summarise(dmg = sum(cropdmg)))
plot(stormdata.full %>% group_by(year) %>%  summarise(dmg = sum(propdmg)))

plot(stormdata.full %>% group_by(year) %>%  summarise(dmg = sum(fatalities)))
plot(stormdata.full %>% group_by(year) %>%  summarise(dmg = sum(injuries)))
```

