library(dplyr)
library(ggplot2)

setwd("C:/Users/jkempke/Box Sync/Coursera/Storm-Data")

if (!file.exists("./StormData.csv.bz2")){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url,destfile = "./StormData.csv.bz2", method="auto")
    }

#read.csv() can handle files with bz2 compression and does not need an extra unzip() step
#I set stringsAsFactors = FALSE since the altenrative changes all the date-times into factors
storm <- read.csv("./StormData.csv.bz2", stringsAsFactors = FALSE)

str(storm)
head(storm)

#First, to change the date-times into an appropriate format
#Then extract the year so we can get at some trends.
storm$BGN_DATE <- as.POSIXlt(strptime(storm$BGN_DATE, format = "%m/%d/%Y %X"))
#the POSIXlt allows me to extract teh year
storm <- mutate(storm, year = BGN_DATE$year)
storm <- mutate(storm, year = year + 1900)
#dplyr doesn't seem to support POSIXlt anywhere else so I convert back to POSIXct
storm$BGN_DATE <- as.POSIXct(storm$BGN_DATE)

#Factor Event type
storm$EVTYPE <- as.factor(storm$EVTYPE)
#This produces 985 event types!
levels(storm$EVTYPE)

#Let's try some summation of deaths across event types

storm <- group_by(storm, EVTYPE)
human_impact <- summarise(storm, TotalDeath = sum(FATALITIES), TotalInjury = sum(INJURIES), TotalImpact = sum(TotalDeath, TotalInjury))
human_impact <- filter(human_impact, TotalImpact > 0)
human_impact <- arrange(human_impact, desc(TotalImpact, TotalDeath))
View(head(human_impact, n = nrow(human_impact)/5))

ggplot(head(human_impact, n = nrow(human_impact)/30), aes(x=EVTYPE, y = TotalImpact)) + 
    geom_bar(stat = "identity") +
    xlab("Event Type") +
    ylab("Total Number of Human Injuries + Deaths")+
    ggtitle("Sums of the Human Impact of US Storms by Storm Type, 1950-2011")


#Now to look at economic damage
# there is property damage and crop damage
#each is composed of a numeric value and an associated character value that serves as a multiplier
table(storm$PROPDMGEXP)
table(is.na(storm$PROPDMGEXP))
table(is.na(storm$CROPDMGEXP))


economic_impact <- mutate(storm, prop_exp = as.factor(PROPDMGEXP), crop_exp = as.factor(CROPDMGEXP))
    
    
    group_by(storm, as.factor(PROPDMGEXP))
    
    mutate(storm, prop_damage = if(PROPDMGEXP == "K") 1000)
    
    prop_damage = ifelse(PROPDMGEXP == "K", 1000 * PROPDMG}
                          else if (PROPDMGEXP == "k"){prop_damage = 1000 * PROPDMG}
                          else if (PROPDMGEXP == "B"){prop_damage = 1000000000 * PROPDMG}
                          else if (PROPDMGEXP == "M"){prop_damage = 1000000 * PROPDMG}
                          else if (PROPDMGEXP == "m"){prop_damage = 1000000 * PROPDMG}
                          else {prop_damage = NA})