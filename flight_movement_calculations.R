# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Description #########
# This script analyses various flight paramaters, including
# reanalysing weather data extracted from NCEP reanalysis I 
# (see 'weather_details_parallel.R'), using standard wind-shear equations
# to calculate wind-speed at height (it does not make any allowance
# for variation in wind-direction at different heights) - it should
# probably only to be trusted  for lower altitudes.



# Packages required #########
#Required packages
library(RODBC) 


#Database queries #################################

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

#Get a copy of the flights DB table.
# flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
#   FROM lund_flights AS f
#   ORDER BY f.flight_id ASC ;"
#                     ,as.is=TRUE)

#commuting version
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
  FROM lund_flights_commuting_par AS f
  ORDER BY f.flight_id ASC ;"
                    ,as.is=TRUE)

# flights$start_time
flights$start_time  <- as.POSIXct(flights$start_time,
                                  tz="GMT",
                                  format="%Y-%m-%d %H:%M:%S")


flights$end_time  <- as.POSIXct(flights$end_time,
                                tz="GMT",
                                format="%Y-%m-%d %H:%M:%S")


q1 <- "SELECT DISTINCT f.*
FROM lund_flights_weather AS f
WHERE f.flight_id IN ("

fun <- function(x){
  x <- as.numeric(as.character(x))
  paste(" ", x, ",", sep = "")
}
# ?paste
q2 <- paste(sapply(flights$flight_id, fun), collapse = "")                            
q3 <- ")
ORDER BY f.flight_id ASC;"

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query =
                              gsub("\n", " ",
                                   paste(q1, q2, q3, sep="")),
                            as.is = TRUE)




flights.weather$start_time  <- as.POSIXct(flights.weather$start_time,
                                tz="GMT",
                                format="%Y-%m-%d %H:%M:%S")


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
# (3^0.11)*5.69
# 
# ((43.5/10)^0.11)
# ((17.5/10)^0.11)

wind.shear <- function(uwind10, vwind10, ht.med){
  
  # Remove negative altitude values, if x is 1 m or less, replace with 1 m
  # Leave NA values as is.
  rem.neg <- function(x){
    if(is.na(x) == FALSE){
      if(x < 1) x <- 1
    }
    return(x)
  }
  
  #   rem.neg(-1)
  
  
  #For median flight height, remove values less than 1, and replace with
  # 1. See function 'rem.neg' above.
  ht.med <- sapply(ht.med, rem.neg)
  
  #Calculate the new wind speeds for both u and v wind vectors
  uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
  vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
  
  
  #New wind speed, using Pythagoras theorem to calculate wind speed
  wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
  
  #Vairables to export
  vairables <- cbind(uwind.new, vwind.new, wind.new)
  
  return(vairables)
}        #end of wind.shear function

# Remove negative or <0.1 m altitudes with 0.1 value
# First define function
rep.neg.alt <- function(x){
  if(is.na(x)){
    return(NA)} else {
      if(x < 0.1){
        return(0.1)} else {
          return(x)}
    }
}


# rep.neg.alt(NA)

# Then do for all flights
alt.cor <- sapply(flights$alt_med, rep.neg.alt)


# Calculate wind-speed at flight height
# Get u and v vector at flight height, and scalar wind speed
wind.calculated    <- wind.shear(flights.weather$uwnd10m,
                                 flights.weather$vwnd10m,
                                 alt.cor)

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


# Calculation wind direction and speed
# Function defenition
wind.dir.speed <- function(uwind10, vwind10){
  # This function calculates the wind speed and direction based the u
  # v wind vectors
  
  #Wind speed Pythagoras theorem
  wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
  
  # Calculate direction in radians (0 - 90 deg)
  dir <- atan(abs(uwind10/ vwind10))
  
  #   atan(1)
  #   atan(0.5)
  #   dir <- atan(0.5)
  #   ?atan
  # Direction in degrees (0 - 90)
  dir <- dir * 180 / pi
  
  # Make into bearing from North
  if(uwind10 > 0 && vwind10 < 0){
    wind.dir <- (180 - dir)
  }else if(uwind10 < 0 && vwind10 < 0){
    wind.dir <- (dir + 180)
  }else  if(uwind10 < 0 && vwind10 > 0){
    wind.dir <- (360 - dir)
  }else   wind.dir <- (dir)
  
  x <- cbind(wind.speed, wind.dir)
  return(x)
}

# wind.dir.speed(-10,-100)



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

wind.origin <- ((flights.characteristics$wind.dir + 180) %% 360)


# Add wind origin direction to table.
flights.characteristics <- cbind(flights.characteristics, wind.origin)



#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights.characteristics, tablename = "lund_flight_com_weather_par",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(start_time = "datetime")
        )

odbcCloseAll()  # close any database connections
