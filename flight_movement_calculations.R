#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

# Description #########
#This script analyses various flight paramaters, including
#reanalysing weather data extracted from NCEP reanalysis I 
#(see 'weather_details.R'), using standar wind-shear equations
#to calculate wind-speed at height (it does not make any allowance
#for variation in wind-direction at different heights) - it should
#only be trusted probably for lower altitudes.



# Packages required #########
#Required packages
library(RODBC) 


#Database queries #################################

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r

tm <- as.POSIXlt(flights$start_time)
tm[1:10]
attr(tm,"tzone") <- "UTC"
tm[1:10]
flights$start_time <- tm

tm <- as.POSIXlt(flights$end_time)
tm[1:10]
attr(tm,"tzone") <- "UTC"
tm[1:10]
flights$end_time <- tm


flights$start_time[1:10]

#str(flights)  #check structure

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights_weather AS f
ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- as.POSIXlt(flights.weather$start_time)
#Check how this appears (i.e. time zone)
tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
tm[1:10]
flights.weather$start_time <- tm

#str(flights.weather)  #check structure



#Wind shear#############################################
#Making aproximate calculation of wind speed at median flight height
#using equation one from:
#Hsu SA, Meindl EA, Gilhousen DB (1994) Determining the power-law wind-profile exponent under near-neutral stability conditions at sea. Journal of Applied Meteorology 33:757–772.
#Formular:
# u2 = u1 (Z2 / Z1) ^ P
# They suggest using a value of 0.11 for P
# u1 and u2 are the reference and desired wind speed measure
# Z1 and Z2 are the reference and desired heights respectively.
#
#Here we will use the values at 10m (uwnd.10m and vwnd.10m)
#Calculating first uwnd.10m.flt.ht and vwn.10m.flt.ht
#Then with simple Pythagoras theorem, the wind velocity
#(square root of sum of the squared values for u and v)


wind.shear <- function(uwind10, vwind10, ht.med){
  
  # Remove negative altitude values, if x is 1 m or less, replace with 1 m
  # Leave NA values as is.
  rem.neg <- function(x){
    if(is.na(NA) == FALSE){
      if(x < 1) x <- 1
    }
    return(x)
  }
  
  #For median flight height, remove values less than 1, and replace with
  # 1. See function 'rem.neg' above.
  ht.med <- sapply(ht.med, rem.neg)
  
  #Calculate the new wind speeds for both u and v wind vectors
  uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
  vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
  
  #New wind speed, using hypotenuse rule to calculate wind speed
  wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
  
  #Vairables to export
  vairables <- cbind(uwind.new, vwind.new, wind.new)
  
  return(vairables)
}        #end of wind.shear function

# Calculate wind-speed at flight height
# Get u and v vector at flight height, and scalar wind speed
wind.calculated    <- wind.shear(flights.weather$uwnd10m,
                                 flights.weather$vwnd10m,
                                 flights$alt_med)

# Make into dataframe
wind.calculated    <- as.data.frame(wind.calculated)

# Give column names
names(wind.calculated) <- c("uwind.10m.flt.ht", "vwind.10m.flt.ht",
                            "wind.10m.flt.ht")

# Add to new dataframe for newly calculated flight characteristics,
# later to be exported to the database.
flights.characteristics <- cbind(flights.weather$flight_id,
                                 flights.weather$start_time,
                                 flights$device_info_serial,
                                 wind.calculated)

new.names <- names(flights.characteristics)
new.names <- new.names[4:length(new.names)]
new.names <- c("flight_id", "start_time", "device_info_serial", new.names)

names(flights.characteristics) <- new.names


# names(flights.characteristics)
# str(flights.characteristics)
# flights.characteristics$start_time




#Wind direction and speed#############

#uwind10 <- 1
#vwind10 <- -1

wind.dir.speed <- function(uwind10, vwind10){
  wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
  wind.dir   <- atan(abs(uwind10/vwind10))
  wind.dir   <- wind.dir * 180 / pi
  
  bear.correction<-  function(x,y){
    if(x > 0 && y < 0){
      return(90)
    }else if(x < 0 && y < 0){
      return(180)
    }else  if(x < 0 && y > 0){
      return(270)
    }else   return(0)
  }
  z <- bear.correction(uwind10,vwind10)
  wind.dir <- wind.dir + z
  
  x <- cbind(wind.speed,wind.dir)
  return(x)
}

#I recieved help with mapply from StackOverflow: http://stackoverflow.com/questions/14196696/sapply-with-custom-function-series-of-if-statements
wind.info <- t(mapply (wind.dir.speed, flights_weather$uwnd.10m,
                       flights_weather$vwnd.10m))

# drops <- c("wind.origin")
# flights_weather <- flights_weather[,!(names(flights_weather) %in% drops)]


wind.info <- as.data.frame(wind.info)
names(wind.info) <- c("wind.speed","wind.dir")
flights_weather <- cbind(flights_weather, wind.info)

wind.head <- ((flights_weather$wind.dir+180) %% 360)
flights_weather <- cbind(flights_weather, wind.head)



#Fast directional flight classificiation############
#Sepperate out fast directional flight from not obviously
#directional flight. Required for drift analysis where we
#only want to analyse directional flight.






#Wind effect/ drift analysis########################

#Wind drift analysis:
#Equation:  y = (w.sin(b))/Va
#y  - angle between track and heading (drift)
#w  - wind speed
#b  - angle between track and wind (with 0 a tail wind)
#Va - air speed (need to assume this)

#need to calculate 'b', and look up 'Va'.

names(flights)

hist(flights$straigtness[flights$straigtness < 1.1], xlim = c(0,2))
hist(flights$rho)
hist(flights$ang_dev)
hist(flights$ang_var)
plot(flights$rho[flights$straigtness < 1.1]~flights$straigtness[flights$straigtness < 1.1])
names(flights)
plot(flights$rho ~ flights$speed_inst_med,xlim=c(3,25))
plot(flights$rho ~ flights$points)
plot(flights$rho ~ flights$ang_dev)
plot(flights$rho ~ flights$ang_var)
names(flights)

par(mfrow = c(2,2))
summary(as.factor(flights$trip_flight_type))
hist(flights$straigtness[flights$straigtness < 1.5], breaks = 20, main = "all")

hist(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "inward"], freq = FALSE, main = "inward")
mean(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "inward"], na.rm = TRUE)

hist(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "outward"], freq = FALSE, main = "outward")
mean(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "outward"], na.rm = TRUE)

hist(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "normal"], freq = FALSE, main = "other")
mean(flights$straigtness[flights$straigtness < 1.5 & flights$trip_flight_type == "normal"], na.rm = TRUE)

mean(flights$straigtness[flights$straigtness < 1.5 & flights$straigtness > .3], na.rm = TRUE)

?hist
plot(flights$straigtness ~ flights$points, ylim = c(0,1.5))
plot(flights$straigtness ~ flights$interval_mean, xlim = c(0,1000), ylim = c(0,1.5))
plot(flights$straigtness ~ flights$duration, ylim = c(0,1.5), xlim = c(0,1000))

names(flights)


names(flights)
plot(flights$straigtness[flights$straigtness < 1.5 &  flights$rho < 1.5]
     ~ flights$rho[flights$straigtness < 1.5 &  flights$rho < 1.5])
abline(lm(flights$straigtness[flights$straigtness < 1.5 &  flights$rho < 1.5]
          ~ flights$rho[flights$straigtness < 1.5 &  flights$rho < 1.5]))
hist(flights$rho[flights$rho < 1.5])

par(mfrow = c(2,2))
hist(flights$rho)
hist(flights$rho[flights$trip_flight_type == "outward"], main = "Outward")
hist(flights$rho[flights$trip_flight_type == "inward"],  main = "Inward")
hist(flights$rho[flights$trip_flight_type == "normal"],  main = "Others")
par(mfrow = c(1,1))

#Speed graphs####################

names(flights_weather)

names(flights)

hist(flights$speed_inst_med[flights$trip_flight_type == "inward"])
mean(flights$speed_inst_med[flights$trip_flight_type == "inward"], na.rm = TRUE)
sd(flights$speed_inst_med[flights$trip_flight_type == "inward"], na.rm = TRUE)
hist(flights$speed_a_b[flights$trip_flight_type == "inward"])
mean(flights$speed_a_b[flights$trip_flight_type == "inward"], na.rm = TRUE)
sd(flights$speed_a_b[flights$trip_flight_type == "inward"], na.rm = TRUE)
unique(as.factor(flights$trip_flight_type))

hist(flights$speed_inst_med[flights$trip_flight_type == "inward" & flights$speed_a_b > 3])
mean(flights$speed_inst_med[flights$trip_flight_type == "inward" & flights$speed_a_b > 3], na.rm = TRUE)
sd(flights$speed_inst_med[flights$trip_flight_type == "inward" & flights$speed_a_b > 3], na.rm = TRUE)