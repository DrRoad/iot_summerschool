# Summer School 2018: IoT
# Script for analysis of incoming sensor data
# 18-08-31
# Version 0.1

install.packages("lubridate")

# libraries
library(readr)
library(lubridate)

### Import CSV Data ###
# TODO: change location

readData <-  function(channel) {
  # Get CSV File
  ECOTRON <- read.csv(url(paste("http://thingspeak.umwelt-campus.de/channels/", channel, "/feed.csv", sep="")))
  
  ### Name columns #############################################
  # TODO: Change dummies: a,b,c... to variables
  colnames(ECOTRON) <- c("Date","Entry_id", "a", "b", "c", "d", "e", "f", "g", "h")
  
  ### Date conversion ##########################################
  ECOTRON$Date <- as.POSIXct(ECOTRON$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
  # Change timezone
  ECOTRON$Date <- with_tz(ECOTRON$Date, "CET")
  
  return(ECOTRON)
}

# location <- "data/feed_Kathi.csv"
ECOTRON1 <- readData(318)
ECOTRON2 <- readData(319)
ECOTRON3 <- readData(320)

##############################################################

### Slice data, define start date and end date
# start: 31.8.2018, 6pm
# end: 1.9.2018, 9am
# TODO: Change actual start and end date
plot(ECOTRON$a~ECOTRON$Date, type="l", col="green")
ECOTRONsub<- subset(ECOTRON, ECOTRON$Date >='2018-08-31 13:15:02' &
                      ECOTRON$Date <= '2018-08-31 14:00:36')

### Plot

max_date <- max(ECOTRONsub$Date)
min_date <- min(ECOTRONsub$Date)

# Temperatur
plot(ECOTRONsub$a ~ ECOTRONsub$Date, type="l", 
     col="green", xlim=c(min_date, max_date),format="%Y-%m-%d %H:%M:%S")
lines(x,y, col="red")
lines(x,y, col="blue")
legend("topright", title="CO2 Measurement", c("ECO1", "ECO2", "ECO3"), fill=c("green","red","blue"), 
        bg = "gray90", cex=0.8, inset=.05)




