# Summer School 2018: IoT
# Script for analysis of incoming sensor data
# 18-08-31
# Version 0.1

# libraries
library(readr)
library(lubridate)

### Import CSV Data ###
# TODO: change location

getCSVData <-  function(channel,api_key) {
  # Get CSV File
  ECOTRON <- read_csv(url(paste("http://thingspeak.umwelt-campus.de/stream/channels/", channel, "/feeds?api_key=", api_key, sep="")))
  
  ### Name columns #############################################
  # TODO: Change dummies: a,b,c... to variables
  colnames(ECOTRON) <- c("Date","Entry_id", "Dummy", "Temperature", "Humidity", "Pressure", "CO2", "Light", "SoilMoisture", "AirQuality")
  
  ### Date conversion ##########################################
  ECOTRON$Date <- as.POSIXct(ECOTRON$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
  # Change timezone
  ECOTRON$Date <- with_tz(ECOTRON$Date, "CET")
  
  ### Slice data, define start date and end date
  # start: 31.8.2018, 6pm
  # end: 1.9.2018, 9am
  # TODO: Change actual start and end date
  
  
  ECOTRONsub<- subset(ECOTRON, ECOTRON$Date >='2018-08-31 13:15:02' &
                        ECOTRON$Date <= '2018-08-31 14:00:36')
  
  # Fix temperatures
  ECOTRON$Temperature1 <- ECOTRON$b[c(T,T,F)]
  ECOTRON$Temperature2 <- ECOTRON$b[c(F,F,T)]

  return(ECOTRON)
}

plotEcotronData <- function(ECOTRON1, ECOTRON2, ECOTRON3, variable, start, end) {

  ### Slice data, define start date and end date
  # TODO: Change actual start and end date
  
  
  ECOTRON1<- subset(ECOTRON1, ECOTRON1$Date >=start &
                        ECOTRON1$Date <= end)
  ECOTRON2<- subset(ECOTRON2, ECOTRON2$Date >=start &
                      ECOTRON2$Date <= end)
  ECOTRON3<- subset(ECOTRON3, ECOTRON3$Date >=start &
                      ECOTRON3$Date <= end)
  
  # Temperatur
  plot(ECOTRON1[[variable]] ~ ECOTRON1$Date, type="l", 
       col="green",format="%Y-%m-%d %H:%M:%S")
  lines(ECOTRON2[[variable]]~ECOTRON2$Date, col="red")
  lines(ECOTRON3[[variable]]~ECOTRON3$Date, col="blue")
  legend("topright", title=paste(variable, " Measurement", sep=""), c("ECO1", "ECO2", "ECO3"), fill=c("green","red","blue"), 
         bg = "gray90", cex=0.8, inset=.05)
}

ECOTRON1 <- getCSVData(318, "KYYV1D41TADHWA6X")
ECOTRON2 <- getCSVData(319, "BJT4PQDOKAWQVGAD")
ECOTRON3 <- getCSVData(320, "G03E8ZGJNU37MVMZ")


plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "Dummy")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "Temperature",'2018-08-31 18:00:00', '2018-08-31 22:00:00')
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "Humidity")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "Pressure")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "CO2")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "Light")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "SoilMoisture")
plotEcotronData(ECOTRON1, ECOTRON2, ECOTRON3, "AirQuality")





