# Reproducible Research: Peer Assessment 2
#  Title: 

## Synopsis: max 10 sentences description of analysis and results

Answer the following questions: 
1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2) Across the United States, which types of events have the greatest economic consequences?

## Data Processing
The data is downloaded from [this repository: ](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) to the variable *stormdata* Some documentation for the dataset can be found [here: ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

```r
if (!file.exists("stormdata.csv.bz2")){
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","stormdata.csv.bz2", method = "curl")}
stormdata.full<- read.csv(bzfile("stormdata.csv.bz2"), header = TRUE, na.strings = "")
```

The data is processed in the following steps:
- Column names have been transformed to lower case 
- Only the columns of the data concerning damage caused by weather, i.e. the ones needed for this analysis, are kept
- The column *year* is added
- A new column called *evtype_cleaned* is created, this contains the information from *evtype* modified in the following way:
    - All event names are transformed to lower case and special characters and leading and trailing spaces are removed
    - Some common variations and abbrevations for events are replaced with a standard event name, e.g. "TSTM"" is replaced by "thunderstorm"
    - Similar events are grouped together, e.g. all event contained the word "flood" are mapped to the event type "floods (several types)" 
- New columns with the calculated value of crop damagae, property damage and total economic (crop and property combined) are added


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
names(stormdata.full)<-tolower(names(stormdata.full))
stormdata <- select(stormdata.full, evtype, fatalities, injuries, propdmg, propdmgexp, cropdmg, cropdmgexp)
stormdata$year <- year(as.Date(stormdata.full$bgn_date,"%m/%d/%Y"))
rm(stormdata.full)
```



```r
# remove special characters and trailing/leading spaces 
stormdata$evtype<-tolower(stormdata$evtype)
stormdata$evtype_cleaned<- gsub("[0-9]*[\\.\\(\\)]*","", stormdata$evtype)
stormdata$evtype_cleaned<- gsub("[-/:]"," ", stormdata$evtype_cleaned)
stormdata$evtype_cleaned<- gsub("^[ ]","",  stormdata$evtype_cleaned)
stormdata$evtype_cleaned<- sub("[ ]$", "", stormdata$evtype_cleaned)

# Replace the abbreviations and variations of event name with standard name
replace <- c("tstm", "winds", "storms", "flooding", "currents", "fires")
with    <- c("thunderstorm", "wind", "storm", "flood", "current", "fire")

for(i in seq_along(replace)){
    stormdata$evtype_cleaned<-gsub(replace[i], with[i], stormdata$evtype_cleaned, ignore.case = TRUE)
}
rm(i, replace, with)

# Rename events containing certain patterns 
pattern <- c("summary", "flood", "tide", "thunderstorm", "heat", "cold", "chill", "wild fire", "thunderstorm", "lightning", "winter weather", "hail")
newname <- c("summary", "flood (several types)", "tide (several types)", "thunderstorm", "heat (several types)", "cold (several types)", "wind chill", "wildfire", "thunderstorm/lightning", "thunderstorm/lightning", "winter weather", "hail")

for(i in seq_along(pattern)){
    stormdata$evtype_cleaned[grep(pattern[i],stormdata$evtype_cleaned,ignore.case = TRUE)]<-newname[i]
}
rm(i, pattern, newname)
```


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
rm(i,prop_ind,crop_ind,alphabetic,magnitude)

stormdata$dmgest <- stormdata$cropdmgest+stormdata$propdmgest
```

## Exploratory analysis

Which events are included in the data?

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
##     evtype            fatalities          injuries        
##  Length:902297      Min.   :  0.0000   Min.   :   0.0000  
##  Class :character   1st Qu.:  0.0000   1st Qu.:   0.0000  
##  Mode  :character   Median :  0.0000   Median :   0.0000  
##                     Mean   :  0.0168   Mean   :   0.1557  
##                     3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##                     Max.   :583.0000   Max.   :1700.0000  
##     propdmg         propdmgexp           cropdmg         cropdmgexp       
##  Min.   :   0.00   Length:902297      Min.   :  0.000   Length:902297     
##  1st Qu.:   0.00   Class :character   1st Qu.:  0.000   Class :character  
##  Median :   0.00   Mode  :character   Median :  0.000   Mode  :character  
##  Mean   :  12.06                      Mean   :  1.527                     
##  3rd Qu.:   0.50                      3rd Qu.:  0.000                     
##  Max.   :5000.00                      Max.   :990.000                     
##       year      evtype_cleaned       propdmgest         cropdmgest     
##  Min.   :1950   Length:902297      Min.   :0.00e+00   Min.   :0.0e+00  
##  1st Qu.:1995   Class :character   1st Qu.:0.00e+00   1st Qu.:0.0e+00  
##  Median :2002   Mode  :character   Median :0.00e+00   Median :0.0e+00  
##  Mean   :1999                      Mean   :4.70e-01   Mean   :5.4e-02  
##  3rd Qu.:2007                      3rd Qu.:0.00e+00   3rd Qu.:0.0e+00  
##  Max.   :2011                      Max.   :1.15e+05   Max.   :5.0e+03  
##      dmgest        
##  Min.   :0.00e+00  
##  1st Qu.:0.00e+00  
##  Median :0.00e+00  
##  Mean   :5.30e-01  
##  3rd Qu.:0.00e+00  
##  Max.   :1.15e+05
```

```r
stormdata.common <- stormdata %>% count(evtype_cleaned)
stormdata.common[order(stormdata.common$n, decreasing = TRUE),]
```

```
## Source: local data frame [460 x 2]
## 
##            evtype_cleaned      n
##                     (chr)  (int)
## 1  thunderstorm/lightning 352566
## 2                    hail 289282
## 3   flood (several types)  82731
## 4                 tornado  60652
## 5               high wind  21764
## 6              heavy snow  15708
## 7              heavy rain  11742
## 8            winter storm  11436
## 9          winter weather   8155
## 10           funnel cloud   6845
## ..                    ...    ...
```

```r
# sum fatalities by event type and sort to show events with most fatalities
stormdata.fatal <- stormdata %>% group_by(evtype_cleaned) %>% summarize(fatalities = sum(fatalities))
stormdata.fatal[order(stormdata.fatal$fatalities, decreasing = TRUE),]
```

```
## Source: local data frame [460 x 2]
## 
##            evtype_cleaned fatalities
##                     (chr)      (dbl)
## 1                 tornado       5633
## 2    heat (several types)       3138
## 3  thunderstorm/lightning       1571
## 4   flood (several types)       1525
## 5             rip current        572
## 6    cold (several types)        451
## 7               high wind        283
## 8               avalanche        224
## 9            winter storm        216
## 10             heavy snow        127
## ..                    ...        ...
```

```r
# sum fatalities by event type and sort to show events with most fatalities
stormdata.injur <- stormdata %>% group_by(evtype_cleaned) %>% summarize(injuries = sum(injuries))
stormdata.injur[order(stormdata.injur$injuries, decreasing = TRUE),]
```

```
## Source: local data frame [460 x 2]
## 
##            evtype_cleaned injuries
##                     (chr)    (dbl)
## 1                 tornado    91346
## 2  thunderstorm/lightning    14775
## 3    heat (several types)     9224
## 4   flood (several types)     8604
## 5               ice storm     1975
## 6               high wind     1440
## 7                    hail     1371
## 8            winter storm     1338
## 9       hurricane typhoon     1275
## 10               wildfire     1061
## ..                    ...      ...
```

```r
summary(stormdata$propdmg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   12.06    0.50 5000.00
```

```r
summary(stormdata$cropdmg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.000   0.000   1.527   0.000 990.000
```

```r
summary(stormdata$propdmgexp)
```

```
##    Length     Class      Mode 
##    902297 character character
```

```r
summary(stormdata$cropdmgexp)
```

```
##    Length     Class      Mode 
##    902297 character character
```

## Results
### Most harmful events 
The data conatins information on the number of fatalitites and injuries from the different types of weather. To assess how much harm each type of weather does I have counted the number of fatalities and injuries caused by each of the grouped weather types gor the full data and restricted to the last year in the data. The rational behind restricting the data to the last year is to better see which types of weather are problematic at the moment, and where efforts to prevent harm should be directed. 


```r
library(ggplot2)
library(gridExtra)

# sum fatalities and injuries by event type and sort to show events with most fatalities/injuries
rank.fatal    <- stormdata %>% group_by(evtype_cleaned) %>% summarize(fatal = sum(fatalities)) %>% top_n(10, fatal)
rank.recentfatal <- stormdata %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(rec_fatal = sum(fatalities)) %>% top_n(10, rec_fatal)
rank.injuries <- stormdata %>% group_by(evtype_cleaned) %>% summarize(injuries = sum(injuries)) %>% top_n(10, injuries)
rank.recentinjuries <- stormdata %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(rec_injuries = sum(injuries)) %>% top_n(10, rec_injuries)

labeltheme <- theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())
geomstyle  <- geom_bar(stat="identity", fill="steelblue", col="darkblue")

g1 <- ggplot(data=rank.fatal, aes(x=evtype_cleaned, y=fatal)) + geomstyle + ggtitle("10 events causing most fatalities")+labeltheme
g2 <- ggplot(data=rank.recentfatal, aes(x=evtype_cleaned, y=rec_fatal)) + geomstyle + ggtitle("Restricted to 2011 data")+labeltheme

g3 <- ggplot(data=rank.injuries, aes(x=evtype_cleaned, y=injuries)) + geomstyle + ggtitle("10 events causing most injuries")+labeltheme
g4 <- ggplot(data=rank.recentinjuries, aes(x=evtype_cleaned, y=rec_injuries)) + geomstyle + ggtitle("Restricted to 2011 data")+labeltheme

grid.arrange(g1, g2, g3, g4, nrow=2, ncol=2)
```

![](RR2_template_files/figure-html/identify_harm-1.png) 

### Events with greatest economic consequences 


```r
# sum economic damage by event type and sort to show events with greatest economic consequences 
rank.prop    <- stormdata %>% na.omit() %>% group_by(evtype_cleaned) %>% summarize(propdmgest = sum(propdmgest)) %>% top_n(10, propdmgest)
rank.recentprop <- stormdata %>% na.omit() %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(propdmgest = sum(propdmgest)) %>% top_n(10, propdmgest)

rank.crop <- stormdata %>% na.omit() %>% group_by(evtype_cleaned) %>% summarize(cropdmgest = sum(cropdmgest)) %>% top_n(10, cropdmgest)
rank.recentcrop <- stormdata %>% na.omit() %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(cropdmgest = sum(cropdmgest)) %>% top_n(10, cropdmgest)

rank.tot <- stormdata %>% na.omit() %>% group_by(evtype_cleaned) %>% summarize(dmgest = sum(dmgest)) %>% top_n(10, dmgest)
rank.recenttot <- stormdata %>% na.omit() %>% filter(year>=2011) %>% group_by(evtype_cleaned) %>% summarize(dmgest = sum(dmgest)) %>% top_n(10, dmgest)

g1 <- ggplot(data=rank.prop, aes(x=evtype_cleaned, y=propdmgest)) + geomstyle + ggtitle("Property damage by event type")+ylab("Damage in million $")+labeltheme
g2 <- ggplot(data=rank.recentprop, aes(x=evtype_cleaned, y=propdmgest)) + geomstyle + ggtitle("Restricted to 2011 data")+ylab("")+xlab("")+labeltheme

g3 <- ggplot(data=rank.crop, aes(x=evtype_cleaned, y=cropdmgest)) + geomstyle + ggtitle("Crop damage by event type")+ylab("Damage in million $")+labeltheme
g4 <- ggplot(data=rank.recentcrop, aes(x=evtype_cleaned, y=cropdmgest)) + geomstyle + ggtitle("Restricted to 2011 data")+ylab("")+labeltheme

g5 <- ggplot(data=rank.tot, aes(x=evtype_cleaned, y=dmgest)) + geomstyle + ggtitle("Total damage by event type")+ylab("Damage in million $")+labeltheme
g6 <- ggplot(data=rank.recenttot, aes(x=evtype_cleaned, y=dmgest)) + geomstyle + ggtitle("Restricted to 2011 data")+ylab("")+labeltheme

grid.arrange(g1, g2, g3, g4, g5, g6, nrow=3, ncol=2)
```

![](RR2_template_files/figure-html/identify_eco-1.png) 


Some things to consider: 
- which event has the largest total number of injuries/fatalities since the start of data collection
- which has the largest number of fatalities/injuries in a single instance?
- which has the largest number of injuries/fatalities in 'modern times' 

Furthermore: Not all events are properly described, some similar events are not grouped together, some are not spelled correctly etc.

## Discussion of results  


