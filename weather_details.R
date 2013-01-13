#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#This script gets weather data for each flight, using the NCEP/NCAR Reanalysis.
#For each flight, the position at the start and the date-time then is used.
#Values are extracted for various weather factors, mainly at ground level.
#Calculations are then made on these values. For example to calculate the
#aproximate wind speed at the bird flight height using wind shear equations.


#Required packages
library(RODBC) 
library(RNCEP)

#Database queries #################################


#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")

#str(flights)  #check structure


#Testing##########################
#make subset of the data
flights_original <- flights
flights <- flights_original
flights <- flights[1:10,]


#Get weather data #################################
#library(RNCEP)  #package to extract weather data

flights_weather <- NULL

  #Total cloud cover 'tcdc.eatm' (total cloud cover %, guassian grid)
  cloud <- NCEP.interp(variable = "tcdc.eatm", level = "gaussian",
              lat = flights$start_lat, lon = flights$start_long,  
              dt = flights$start_time,
              reanalysis2 = FALSE, keep.unpacking.info = TRUE,
              interp = 'linear')

  #Add values to flights_weather table
  tcdc.eatm <- (as.numeric(cloud))
  tcdc.eatm.sd <- (attr(cloud, which = "standard deviation"))
  flights_weather <- cbind(flights_weather, tcdc.eatm, tcdc.eatm.sd)


  #Air tempreture at surface 'air.sig995' (Air tempreture at surface in deg K)
  air.temp <- NCEP.interp(variable = "air.sig995", level = "surface",
                       lat = flights$start_lat, lon = flights$start_long,  
                       dt = flights$start_time,
                       reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                       interp = 'linear')

  #Add values to flights_weather table
  air.sig995 <- (as.numeric(air.temp))
  air.sig995.sd <- (attr(air.temp, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,air.sig995,air.sig995.sd)



#Precipitation rate at surface 'prate.sfc' (Precipitation Kg/m^2/s)
ppt <- NCEP.interp(variable = "prate.sfc", level = "gaussian",
                        lat = flights$start_lat, lon = flights$start_long,  
                        dt = flights$start_time,
                        reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                        interp = 'linear')

  #Add values to flights_weather table
  prate.sfc <- (as.numeric(ppt))
  prate.sfc.sd <- (attr(ppt, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,prate.sfc,prate.sfc.sd)

#Pressure at surface 'pres.sfc' (Pa)
sur.pres <- NCEP.interp(variable = "pres.sfc", level = "surface",
                   lat = flights$start_lat, lon = flights$start_long,  
                   dt = flights$start_time,
                   reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                   interp = 'linear')

  #Add values to flights_weather table
  pres.sfc <- (as.numeric(sur.pres))
  pres.sfc.sd <- (attr(sur.pres, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,pres.sfc,pres.sfc.sd)



#Pressure at sea level 'slp' (Pa)
sea.pres <- NCEP.interp(variable = "slp", level = "surface",
                      lat = flights$start_lat, lon = flights$start_long,  
                      dt = flights$start_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')

#Add values to flights_weather table
slp <- (as.numeric(sea.pres))
slp.sd <- (attr(sea.pres, which = "standard deviation"))
flights_weather <- cbind(flights_weather,slp,slp.sd)



#Wind Speed in E-W direction 'uwnd.sig995' (ms^-1) 'near surface'
uwnd <- NCEP.interp(variable = "uwnd.sig995", level = "surface",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
uwnd.sig995 <- (as.numeric(uwnd))
uwnd.sig995.sd <- (attr(uwnd, which = "standard deviation"))
flights_weather <- cbind(flights_weather,uwnd.sig995,uwnd.sig995.sd)


#Wind Speed in N-S direction 'vwnd.sig995' (ms^-1) 'near surface'
vwnd <- NCEP.interp(variable = "vwnd.sig995", level = "surface",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
vwnd.sig995 <- (as.numeric(vwnd))
vwnd.sig995.sd <- (attr(vwnd, which = "standard deviation"))
flights_weather <- cbind(flights_weather,vwnd.sig995,vwnd.sig995.sd)



#Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
uwnd10 <- NCEP.interp(variable = "uwnd.10m", level = "gaussian",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
uwnd.10m <- (as.numeric(uwnd10))
uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
flights_weather <- cbind(flights_weather,uwnd.10m,uwnd.10m.sd)


#Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
vwnd10 <- NCEP.interp(variable = "vwnd.10m", level = "gaussian",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
vwnd.10m <- (as.numeric(vwnd10))
vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
flights_weather <- cbind(flights_weather,vwnd.10m,vwnd.10m.sd)




flights_weather <- as.data.frame(flights_weather)



#Wind shear#############################################
#Making aproximate calculation of wind speed at median flight height
#using equation one from:
#Hsu SA, Meindl EA, Gilhousen DB (1994) Determining the power-law wind-profile exponent under near-neutral stability conditions at sea. Journal of Applied Meteorology 33:757–772.
#Formular:
#u2 = u1 (Z2 / Z1) ^P
#They suggest using value 0.11 for P
#u1 and u2 are the reference and desired wind speed measure
#Z1 and Z2 are the reference and desired heights respectively.
#
#Here we will use the values at 10m (uwnd.10m and vwnd.10m)
#Calculating first uwnd.10m.flt.ht and vwn.10m.flt.ht
#Then with simple Pythagoras theorem, the wind velocity
#(square root of sum of the squared values for u and v)

wind.shear <- function(uwind10, vwind10, ht.med){
  rem.neg <- function(x){
    if(is.na(NA) == FALSE){
      if(x < 1) x <- 1
    }
    return(x)
  }
  ht.med <- sapply(ht.med, rem.neg)
  uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
  vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
  wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
  
  vairables <- cbind(uwind.new, vwind.new, wind.new)
  
  return(vairables)
}        #end of wind.shear function
  
wind.calculated    <- wind.shear(flights_weather$uwnd.10m, flights_weather$vwnd.10m,  flights$alt_med)
wind.calculated    <- as.data.frame(wind.calculated)
names(wind.calculated) <- c("uwind.10m.flt.ht","vwind.10m.flt.ht","wind.10m.flt.ht")
flights_weather <- cbind(flights_weather, wind.calculated)

#junk test code########################
#drops <- c("uwind.new", "vwind.new", "wind.new")
#flights_weather <- flights_weather[,!(names(flights_weather) %in% drops)]

# ht.med  <- flights$alt_med[x]
# uwind10 <- flights_weather$uwnd.10m[x]
# vwind10 <- flights_weather$vwnd.10m[x]
# 
# rem.neg <- function(x){
#   if(x > -99){
#   if(x < 1) x <- 1}
#   return(x)
# }
# rem.neg(NA)
# if(is.na(NA) == FALSE)
# 
# 
# str(ht.med)
# ht.med2 <- sapply(ht.med,rem.neg)
# uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
# vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
# wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
# 
# vairables <- cbind(uwind.new, vwind.new, wind.new)
# 
# 
# 
# 
# 
# 
# 
# wind.calculated <- NULL
# 
# ?traceback
# 
# ht.med.test <- sapply(flights$alt_med,rem.neg)
# test2 <- flights_weather$uwnd.10m * ((ht.med.test / 10) ^ 0.11)
# summary(flights$alt_med)
# summary(flights_weather$uwnd.10m)
# hist(flights_weather$vwnd.10m)
# 
# ((flights$alt_med[1:200] / 10) ^ 0.11)
# 
# uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
# vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
# wind.new  <- sqrt((test2 * test2) + (test2 * test2))
# 
# 
# uwind10x <- flights_weather$uwnd.10m[1:200]
# vwind10x <- flights_weather$vwnd.10m[1:200]
# ht.medx  <- flights$alt_med[1:200]
# 
# rem.neg <- function(x){
#   if(x < 1) x <- 1
#   return(x)
# }
# ht.med <- sapply(ht.med,rem.neg)
# uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
# vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
# wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
# 
# vairables <- cbind(uwind.new, vwind.new, wind.new)
# 
# wind.shear
# wind.shear(uwind10x, vwind10x, ht.medx)

#Then will add this to the flights_weather data frame


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