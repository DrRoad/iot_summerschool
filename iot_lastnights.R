# plot two last nights
# libraries
library(readr)
library(lubridate)


# get data


# new: 
ECOTRONnew <- read.csv("data/new_data.csv")
  
colnames(ECOTRONnew) <- c("Date","Entry_id", "Dummy", "Temperature", "Humidity", "Pressure", "CO2", "Light", "SoilMoisture", "AirQuality")

### Date conversion ##########################################
ECOTRONnew$Date <- as.POSIXct(ECOTRONnew$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
# Change timezone
ECOTRONnew$Date <- with_tz(ECOTRONnew$Date, "CET") 

#old:
ECOTRONold <- read.csv("data/old_data.csv")
colnames(ECOTRONold) <- c("Date","Entry_id", "Dummy", "Temperature", "Humidity", "Pressure", "CO2", "Light", "SoilMoisture", "AirQuality")

### Date conversion ##########################################
ECOTRONold$Date <- as.POSIXct(ECOTRONold$Date, "%Y-%m-%d %H:%M:%S", tz="UTC")
# Change timezone
ECOTRONold$Date <- with_tz(ECOTRONold$Date, "CET")


plotEcotronData <- function(ECOTRONold, ECOTRONnew, variable, unit) {
  
  ### Slice data, define start date and end date
  # TODO: Change actual start and end date
  
  
  ECOTRONold<- subset(ECOTRONold, ECOTRONold$Date >="2018-08-30 18:00:00" &
                        ECOTRONold$Date <= "2018-08-31 08:00:00")
  ECOTRONnew<- subset(ECOTRONnew, ECOTRONnew$Date >="2018-08-31 18:00:00" &
                        ECOTRONnew$Date <= "2018-09-01 08:00:00")
  
  # vaiable
  par(mfrow=c(2,1))
  plot(ECOTRONold[[variable]] ~ ECOTRONold$Date, type="l", 
       main=paste(variable, " (night of 30.8.2018)", sep=""), xlab="", ylab=paste(variable," ", unit, sep=""),
       col="blue",format="%Y-%m-%d %H:%M:%S")
  
  plot(ECOTRONnew[[variable]] ~ ECOTRONnew$Date, type="l", 
       main=paste(variable, " (night of 31.8.2018)", sep=""), xlab="", ylab=paste(variable,unit, sep=""),
       col="blue",format="%Y-%m-%d %H:%M:%S")
  
}

png('plot/Temperature.png')
plotEcotronData(ECOTRONold, ECOTRONnew, "Temperature", "[°C]")
dev.off()
png('plot/Humidity.png')
plotEcotronData(ECOTRONold, ECOTRONnew, "Humidity", "[]")
dev.off()


png('plot/Pressure.png')
par(mfrow=c(1,1))
plot(ECOTRONnew$Pressure ~ ECOTRONnew$Date, type="l", 
     main=paste("Pressure", " (night of 31.8.2018)", sep=""), xlab="", ylab="Pressure [hPa]",
     col="blue",format="%Y-%m-%d %H:%M:%S")
dev.off()


png('plot/CO2.png')
plotEcotronData(ECOTRONold, ECOTRONnew, "CO2", "[ppm]")
dev.off()

png('plot/SoilMoisture.png')
par(mfrow=c(1,1))
ECOTRONold<- subset(ECOTRONold, ECOTRONold$Date >="2018-08-30 18:00:00" &
                      ECOTRONold$Date <= "2018-08-31 08:00:00")

plot(ECOTRONold$SoilMoisture ~ ECOTRONold$Date, type="l", 
     main=paste("SoilMoisture", " (night of 30.8.2018)", sep=""), xlab="", ylab=paste("SoilMoisture [kPa]", sep=""),
     col="blue",format="%Y-%m-%d %H:%M:%S")

dev.off()

png('plot/AirQuality.png')
par(mfrow=c(1,1))
ECOTRONnew<- subset(ECOTRONnew, ECOTRONnew$Date >="2018-08-31 18:00:00" &
                      ECOTRONnew$Date <= "2018-09-01 08:00:00")
plot(ECOTRONnew$AirQuality ~ ECOTRONnew$Date, type="l", 
     main=paste("AirQuality", " (night of 31.8.2018)", sep=""), xlab="", ylab=paste("AirQuality [ppmVOC]", sep=""),
     col="blue",format="%Y-%m-%d %H:%M:%S")

dev.off()


png('plot/Light.png')
ECOTRONnew$Light[is.na(ECOTRONnew$Light)] <- 0
plotEcotronData(ECOTRONold, ECOTRONnew, "Light", "[W/m²]")
dev.off()
