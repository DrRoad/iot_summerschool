# Summer School 2018: IoT
# Script for analysis of incoming sensor data
# 18-08-31
# Version 0.1

# libraries
library(readr)
library(lubridate)

### Import CSV Data ###
# TODO: change location

getCSVData <-  function(channel) {
  # Get CSV File
  ECOTRON <- read_csv(url(paste("http://thingspeak.umwelt-campus.de/channels/", channel, "/feed.csv", sep="")))
  
  ### Name columns #############################################
  # TODO: Change dummies: a,b,c... to variables
  colnames(ECOTRON) <- c("Date","Entry_id", "a", "b", "c", "d", "e", "f", "g", "h")
  
  ### Date conversion ##########################################
  ECOTRON$Date <- as.POSIXct(ECOTRON$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
  # Change timezone
  ECOTRON$Date <- with_tz(ECOTRON$Date, "CET")
  ECOTRONsub<- subset(ECOTRON, ECOTRON$Date >='2018-08-31 13:15:02' &
                        ECOTRON$Date <= '2018-08-31 14:00:36')

  return(ECOTRON)
}

plotEcotronData <- function(ECOTRON1,ECOTRON2, ECOTRON3) {
  ### Slice data, define start date and end date
  # start: 31.8.2018, 6pm
  # end: 1.9.2018, 9am
  # TODO: Change actual start and end date
  
  # Temperatur
  plot(ECOTRON1$b ~ ECOTRON1$Date, type="l", 
       col="green",format="%Y-%m-%d %H:%M:%S")
  lines(ECOTRON2$b~ECOTRON2$Date, col="red")
  lines(ECOTRON3$b~ECOTRON3$Date, col="blue")
  legend("topright", title="CO2 Measurement", c("ECO1", "ECO2", "ECO3"), fill=c("green","red","blue"), 
         bg = "gray90", cex=0.8, inset=.05)
}

ECOTRON1 <- getCSVData(318)
ECOTRON2 <- getCSVData(319)
ECOTRON3 <- getCSVData(320)


plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3)


processEcotronData(ECOTRON2)


processEcotronData(ECOTRON3)

