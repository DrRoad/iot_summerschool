# Summer School 2018: IoT
# Script for analysis of incoming sensor data
# 18-08-31
# Version 0.1



# libraries
library(readr)
library(lubridate)


### Import CSV Data ###
# TODO: change location

location <- "data/feed.csv"
ECOTRON <- read_csv(location)
##############################################################

### Name columns #############################################
# TODO: Change dummies: a,b,c... to variables
colnames(ECOTRON) <- c("Date","Entry_id", "a", "b", "c", "d", "e", "f", "g", "h")

### Date conversion ##########################################
ECOTRON$Date <- as.POSIXct(ECOTRON$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
# Change timezone
ECOTRON$Date <- with_tz(ECOTRON$Date, "CET")


### Slice data, define start date and end date
# start: 31.8.2018, 6pm
# end: 1.9.2018, 9am
# TODO: Change actual start and end date
plot(ECOTRON$a~ECOTRON$Date, type="l", col="green")
ECOTRONsub<- subset(ECOTRON, ECOTRON$Date >='2018-08-31 13:15:02' &
                      ECOTRON$Date <= '2018-08-31 14:00:36')

ECOTRONsub$Date <- as.POSIXct(ECOTRONsub$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
# Change timezone
ECOTRONsub$Date <- with_tz(ECOTRONsub$Date, "CET")

### Plot

# Temperatur
plot(ECOTRONsub$a ~ ECOTRONsub$Date, type="l", col="green")
add(subset_ECOTRON$a~subset_ECOTRON$Date, type="l", col="red")


