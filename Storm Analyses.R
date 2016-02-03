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
storm <- select(storm, EVTYPE, FATALITIES, INJURIES, PROPDMGEXP, PROPDMG, CROPDMGEXP, CROPDMG)

table(is.na(storm$PROPDMGEXP))
table(is.na(storm$CROPDMGEXP))

table(storm$PROPDMGEXP)
table(storm$CROPDMGEXP)

summary(storm$CROPDMG)
table(is.na(storm$CROPDMGEXP))
table(storm$CROPDMGEXP)

storm <- storm %>% mutate(prop_damage = ifelse(PROPDMG == 0, 0,
                                        ifelse(PROPDMGEXP == "K", 1000 * PROPDMG,
                                        ifelse(PROPDMGEXP == "k", 1000 * PROPDMG,
                                        ifelse(PROPDMGEXP == "M", 1000000 * PROPDMG,
                                        ifelse(PROPDMGEXP == "m", 1000000 * PROPDMG, 
                                        ifelse(PROPDMGEXP == "B", 1000000000 * PROPDMG, NA)))))))


storm <- storm %>% mutate(crop_damage = ifelse(CROPDMG ==0, 0,
                                        ifelse(CROPDMGEXP == "K", 1000 * CROPDMG,
                                        ifelse(CROPDMGEXP == "k", 1000 * CROPDMG,
                                        ifelse(CROPDMGEXP == "M", 1000000 * CROPDMG,
                                        ifelse(CROPDMGEXP == "m", 1000000 * CROPDMG,
                                        ifelse(CROPDMGEXP == "B", 1000000000 * CROPDMG, NA)))))))

summary(storm$prop_damage)
summary(storm$crop_damage) 

str(storm)
head(storm)

#Factor Event type
storm$EVTYPE <- as.factor(storm$EVTYPE)
#This produces 985 event types!
levels(storm$EVTYPE)
storm <- group_by(storm, EVTYPE)

#Let's try some summation of deaths across event types

human_impact <- summarise(storm, TotalDeath = sum(FATALITIES), TotalInjury = sum(INJURIES), TotalImpact = sum(TotalDeath, TotalInjury))
human_impact <- filter(human_impact, TotalImpact > 0)
human_impact <- arrange(human_impact, desc(TotalImpact, TotalDeath))


View(head(human_impact, n = nrow(human_impact)/5))
ggplot(head(human_impact, n = nrow(human_impact)/10), aes(x=EVTYPE, y = TotalImpact)) + 
    geom_bar(stat = "identity") +
    xlab("Event Type") +
    ylab("Total Number of Human Injuries + Deaths")+
    ggtitle("Sums of the Human Impact of US Storms by Storm Type, 1950-2011")


#Now to look at economic damage
econ_impact <- summarise(storm, 
                         TotalProp = sum(prop_damage), 
                         TotalCrop = sum(crop_damage), 
                         TotalDamage = sum(TotalProp, TotalCrop))

econ_impact <- filter(econ_impact, TotalDamage > 0)
econ_impact <- arrange(econ_impact, desc(TotalDamage))


View(head(econ_impact, n=nrow(econ_impact)/5))
ggplot(head(econ_impact, n=10), aes(x=EVTYPE, y = TotalDamage)) +
    geom_bar(stat = 'identity') +
    xlab("Event Type") +
    ylab("Estimated Dollar Amount of Damages") +
    ggtitle("Sum of Estimated Dollar Amount of Crop and Property Damage by Storm Type, US 1950-2011")
