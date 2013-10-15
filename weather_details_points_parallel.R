# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
 # You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# Required packages #####
#To link to database
library(RODBC)
require(foreach)
require(doParallel)


# Database functions - get data from the database ####
#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


#for all of data_base, except pre-deployment and null records
#excluding individuals with start north of 59 (i.e. those birds
#from Fågelsundet)
# gps <- sqlQuery(gps.db, query="SELECT DISTINCT t.device_info_serial, t.date_time, g.latitude, g.longitude, g.altitude, g.vnorth, g.veast, t.trip_id 
#   FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS t
#   WHERE g.device_info_serial = t.device_info_serial
#     AND g.date_time = t.date_time
#     AND t.trip_id > 0
#   ORDER BY t.device_info_serial ASC, t.date_time ASC ;"
#                 ,as.is=TRUE)

# save("gps", file = "gps_wind_drift_analysis.RData")
# To speed things up - cached copy of GPS table above.
load("gps_wind_drift_analysis.RData")

# str(gps)


# Correct date_time
gps$date_time <- as.POSIXct(gps$date_time,
                                  tz="GMT",
                                  format="%Y-%m-%d %H:%M:%S")

#Make cluster of 16 instances
cl <- makeCluster(16)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


# clusterExport(cl, c("gps"))


# Make a list of available devices
devices <- sort(unique(gps$device_info_serial))

#export the gps data and trip list

clusterExport(cl, c("gps","devices"))  


#make a list object to recieve the data
lst <- list()



#get weather data for each flight of each trip
#Use system.time to time how long this takes.
# On 2013-09-30 took 24875 s (<7 h)
system.time({lst <- foreach(i = seq(along = devices )) %dopar%{
  
  # Package to extract weather data from NOAA
  require(RNCEP)
  
  d <- devices[i]
  sub01 <- subset(gps, gps$device_info_serial == d)
  
  
#   Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
#       uwnd10 <- NCEP.interp(
#         variable = "uwnd.10m",
#         level = "gaussian",
#         lat = sub01$latitude,
#         lon = sub01$longitude,
#         dt = sub01$date_time,
#         reanalysis2 = FALSE,
#         keep.unpacking.info = TRUE,
#         interp = 'linear'
#       )
  uwnd.10m <-  1  #test
  uwnd.10m.sd  <-  2  #test
  
  
  #Add values to points.weather table
  #     uwnd.10m <- (as.numeric(uwnd10))
  #     uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
  #     points.weather <- cbind(uwnd.10m,uwnd.10m.sd)
  
  
  #Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
  #     vwnd10 <- NCEP.interp(
  #       variable = "vwnd.10m", level = "gaussian",
  #       lat = sub01$latitude, lon = sub01$longitude,  
  #       dt = sub01$date_time,
  #       reanalysis2 = FALSE, keep.unpacking.info = TRUE,
  #       interp = 'linear'
  #     )
  #     
  #     #Add values to points.weather table
  #     vwnd.10m <- (as.numeric(vwnd10))
  #     vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
  
  vwnd.10m <- 3  #test
  vwnd.10m.sd  <- 4  #test
  
  
  x <- cbind(sub01$device_info_serial,sub01$date_time, uwnd.10m,uwnd.10m.sd, vwnd.10m,vwnd.10m.sd)
  
  
  
  #output data as list (this will be appended to the global list, lst.
  list(x)   
} #end of foreach functions
}) #end of things being timed by system.time





z <- 0
  for (i in 1: length(lst)){
    y <- matrix(unlist(lst[i]), ncol = 6, byrow = F)
    z <- rbind(z,y)
  }

dz <- dim(z)
z2 <- z[2:dz[1],]
weather.data <- as.data.frame(z2)
row.names(weather.data)<-NULL
names(weather.data) <- c("device_info_serial","date_time","uwnd.10m","uwnd.10m.sd","vwnd.10m", "vwnd.10m.sd")

weather.data$date_time <- as.POSIXct(weather.data$date_time, tz = "UTC", origin = "1970-01-01")




#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, weather.data, tablename = "lund_points_weather",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(date_time = "datetime"))















#Copied from 'flight_movement_calculations'









#Wind effect/ drift analysis########################

#Wind drift analysis:
#Equation:  y = (w.sin(b))/Va
#y  - angle between track and heading (drift)
#w  - wind speed
#b  - angle between track and wind (with 0 a tail wind)
#Va - air speed (need to assume this)

#need to calculate 'b', and look up 'Va'.

# names(flights.characteristics)
# names(flights.weather)
# names(flights)

# Airspeed - we can try a range from 10, 12, 14 perhaps, covering likely value range.
# Values for Karlsö, according to current analysis appear a bit low.


# Calculate angle between Track and Wind vector (beta)

#Difference between wind direction and flight direction
beta  <- (flights.characteristics$wind.dir - flights$bearing_a_b)
hist(beta)

# Absolute difference
beta  <- abs(beta)
hist(beta)

angle.dif <- function(x){
  #Function to find difference between flight and wind direction
  #Range from 0 - 180
  if(is.na(x)) return (NA)
  else{
    if(x > 180) {return (360 - x)}
    else return (x)
  }
}

angle.dif(NA)



#Use 'angle.dif' function to calculate actual alpha value
beta  <- sapply(beta, angle.dif)
hist(beta)

#y = (w.sin(b))/Va
#Air speed
Va <- 12

y  <- (flights.characteristics$wind.10m.flt.ht * sin(rad(beta))) / Va
y.rad <- y
y.deg <- deg(y)
alpha <- y.deg

hist(y.deg)

wind.vec <-  sapply(flights$bearing_a_b, angle.dif)
hist(wind.vec)

track.vec <-  sapply(flights.characteristics$wind.dir, angle.dif)
hist(track.vec)


# Output to database #####
plot(alpha , track.vec)
points(alpha, wind.vec, col = "red")
reg1 <- lm(alpha ~ track.vec)
reg2 <- lm(alpha ~ wind.vec)
abline(reg1)
abline(reg2, col = "red")


#Inward flights
plot(alpha[inward] , flights$bearing_a_b[inward])
points(alpha[inward], beta[inward], col = "red")





#Fast directional flight classificiation############
#**Perhaps save this for a sepperate script doing direct comparisons.

#Sepperate out fast directional flight from not obviously
#directional flight. Required for drift analysis where we
#only want to analyse directional flight.
