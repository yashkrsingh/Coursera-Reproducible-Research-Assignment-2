## Name         : Yash Kumar Singh
## Program Title: Reproducible Research Assignment 2 (Temporary)
## ------------------------------------------------------------------

## ------------------------------------------------------------------
## Loading Data
## ------------------------------------------------------------------
library(R.utils)
setwd("~/Reproducible-Research-Assignment-2-Coursera/")
if(!file.exists("stormdata.csv.bz2")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "~/Reproducible-Research-Assignment-2-Coursera/stormdata.csv.bz2")
}
bunzip2("stormdata.csv.bz2", "storm.csv", remove = FALSE)
data <- read.csv("storm.csv")

## ------------------------------------------------------------------
## Data Processing
## ------------------------------------------------------------------
reduced <- data[,c('EVTYPE','FATALITIES','INJURIES','PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]
rm(data)

reduced$evname <- NA
reduced[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|fog|wall cloud", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Precipitation & Fog"
reduced[grepl("wind|storm|wnd|hurricane|typhoon", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Wind & Storm"
reduced[grepl("slide|erosion|slump", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Landslide & Erosion"
reduced[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|record temperature|record high", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Heat & Drought"
reduced[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|blizzard|chill|freezing|avalanche|glaze|sleet", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Snow & Ice"
reduced[grepl("flood|surf|blow-out|swells|fld|dam break", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Flooding & High Surf"
reduced[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "High seas"
reduced[grepl("dust|saharan", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Dust & Saharan winds"  
reduced[grepl("tstm|thunderstorm|lightning", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Thunderstorm & Lightning"
reduced[grepl("tornado|spout|funnel|whirlwind", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Tornado"
reduced[grepl("fire|smoke|volcanic", reduced$EVTYPE, ignore.case = TRUE), "evname"] <- "Fire & Volcanic activity"
reduced <- reduced[complete.cases(reduced[,'evname']), ]
reduced$EVTYPE <- reduced$evname
reduced$EVTYPE <- as.factor(reduced$EVTYPE)
reduced$evname <- NULL

exponent <- function(x){
  if(is.numeric(x)) {
    x <- x
  }
  else if(grepl("h", x, ignore.case=TRUE)) {
    x <- 2
  }
  else if(grepl("k", x, ignore.case=TRUE)) {
    x <- 3
  }
  else if(grepl("m", x, ignore.case=TRUE)) {
    x <- 6
  }
  else if(grepl("b", x, ignore.case=TRUE)) {
    x <- 9
  }
  else if(x == "" || x == " "){
    x <- 0
  }
  else{
    x <- NA
  }
  x
}

calculateValue <- function(num, exp){
  pow <- exponent(exp)
  if(is.numeric(num)){
    num <- num * (10 ^ pow)
  }
  
  if(!is.numeric(num)){
    num <- 0
  }
  
  num
}

reduced$PROPERTY <- mapply(calculateValue, reduced$PROPDMG, reduced$PROPDMGEXP)
reduced$CROP <- mapply(calculateValue, reduced$CROPDMG, reduced$CROPDMGEXP)

reduced <- reduced[complete.cases(reduced[,'PROPERTY']), ]
reduced <- reduced[complete.cases(reduced[,'CROP']), ]

reduced$TOTALDMG = reduced$PROPERTY + reduced$CROP

reduced$PROPDMG <- NULL
reduced$PROPDMGEXP <- NULL
reduced$CROPDMG <- NULL
reduced$CROPDMGEXP <- NULL

## ------------------------------------------------------------------
## Aggregating Results
## ------------------------------------------------------------------
library(plyr)
aggr <- ddply(reduced, "EVTYPE", summarise, Fatalities = sum(FATALITIES), Injuries = sum(INJURIES), Property = sum(PROPERTY), Crop = sum(CROP), Total = sum(TOTALDMG))

population <- aggr[c(1,2,3)]
population$Total <- population$Fatalities + population$Injuries
population <- population[order(population$Total, population$Fatalities, population$Injuries, decreasing = TRUE), ]

rownames(population) <- 1:11

economic <- aggr[c(1,4,5,6)]
economic <- economic[order(economic$Total, economic$Property, economic$Crop, decreasing = TRUE), ]
rownames(economic) <- 1:11

## ------------------------------------------------------------------
## Graphing Results
## ------------------------------------------------------------------
library(ggplot2)

temp1 <- population[,c(1,2)]
temp1$Type <- "Fatalities"
temp2 <- population[,c(1,3)]
temp2$Type <- "Injuries"
colnames(temp1) <- c("x", "y", "type")
colnames(temp2) <- c("x", "y", "type")
temp <- rbind(temp1, temp2)
rm(temp1,temp2)
png(filename = "plot1.png", height = 480, width = 480, bg = "white")
g <- ggplot(data = temp, aes(x, y, fill = type)) 
g <- g + geom_bar(stat="identity", position="dodge", alpha=.3)
g <- g + theme(axis.text.x = element_text(angle=60, hjust = 1))
g <- g + coord_cartesian(ylim = c(0, 15000))
g <- g + labs(x = "Disaster Type", y = "Casualities")
g <- g + ggtitle("Effect of Disasters on Population")
print(g)
dev.off()
rm(temp)


temp1 <- economic[,c(1,2)]
temp1$Type <- "Property"
temp2 <- economic[,c(1,3)]
temp2$Type <- "Crop"
colnames(temp1) <- c("x", "y", "type")
colnames(temp2) <- c("x", "y", "type")
temp <- rbind(temp1, temp2)
rm(temp1,temp2)
png(filename = "plot2.png", height = 480, width = 480, bg = "white")
g <- ggplot(data = temp, aes(x, y, fill = type)) 
g <- g + geom_bar(stat="identity", position="dodge", alpha=.3)
g <- g + theme(axis.text.x = element_text(angle=60, hjust = 1))
g <- g + coord_cartesian(ylim = c(0, 30000000000))
g <- g + labs(x = "Disaster Type", y = "Economic Damage")
g <- g + ggtitle("Effect of Disasters on Economy")
print(g)
dev.off()
rm(temp)

## ------------------------------------------------------------------
