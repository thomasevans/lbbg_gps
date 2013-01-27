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
  # This function calculates the wind speed and direction based the u
  # v wind vectors
  
  #Wind speed Pythanogras theorem
  wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
  
  # Calculate direction in radians (0 - 90 deg)
  dir <- atan(abs(uwind10/ vwind10))
  
  # Direction in degrees (0 - 90)
  dir <- dir * 180 / pi
  
  # Make into bearing from North
  if(uwind10 > 0 && vwind10 < 0){
    wind.dir <- (dir + 90)
  }else if(uwind10 < 0 && vwind10 < 0){
    wind.dir <- (dir + 180)
  }else  if(uwind10 < 0 && vwind10 > 0){
    wind.dir <- (dir + 270)
  }else   wind.dir <- (dir)
  
  x <- cbind(wind.speed, wind.dir)
  return(x)
}

# Testing
# wind.dir.speed(1,1)
# wind.dir.speed(1,-1)
# wind.dir.speed(-1,-1)
# wind.dir.speed(-1,1)

# Calculate wind speed (at 10m) and direction (bearing from north)
# for all flights
# I recieved help with mapply from StackOverflow: http://stackoverflow.com/questions/14196696/sapply-with-custom-function-series-of-if-statements
wind.info <- t(mapply(wind.dir.speed, flights.weather$uwnd10m,
                       flights.weather$vwnd10m))

# Make dataframe
wind.info <- as.data.frame(wind.info)

# Give names to columns
names(wind.info) <- c("wind.speed", "wind.dir")

# Add calculated wind info to flight.characteristics 
flights.characteristics <- cbind(flights.characteristics, wind.info)

wind.origin <- ((flights.characteristics$wind.dir+180) %% 360)

# hist(wind.head)
# hist(flights.characteristics$wind.dir)

# Add wind origin direction to table.
flights.characteristics <- cbind(flights.characteristics, wind.origin)







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