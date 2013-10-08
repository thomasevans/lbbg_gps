# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# Required packages #####
#To link to database
library(RODBC)


# Database functions - get data from the database
#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


#for all of data_base, except pre-deployment and null records
#excluding individuals with start north of 59 (i.e. those birds
#from Fågelsundet)
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude, g.altitude, g.vnorth, g.veast, t.trip_id 
  FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS t
  WHERE g.device_info_serial = t.device_info_serial
    AND g.date_time = t.date_time
    AND t.trip_id > 0
  ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)


# Load in requried data (GPS points, device_info_serial, date_time - anything else??)

gps.points <- ...
only points within foraging trips (trip_id >= 1??)

# Calculate number of trips
length(unique(...))

# Make ten lists of foraging trips - to be used in for loop thing, so that we can save each one out to file/ workspace
x <- unique(foraging_trips...)

trip.list <- list()

a <- 1
b <- floor(length(x)/10)

for( i in 1:9){
  trip.list[i] <- list(x[a:(a+b)])
  a <- a+b+1
}
trip.list[10] <- list(x[a:length(x)])


# For each list of foraging trips...
weather.points <- list()

for (i in 1:10){
  inialise foreach thing...
  
  foreach flight in list do this...{
    get wind data
      east-west
    
      north-south
    
    save to data.frame thing, then add to list
    
  }
  
  
  weather.points[i] <- info
  save(weather.points, file = "weather.points.RData")
}

# Reassemble data from weather.points list into dataframe or gps points,
# containing columns for: device_info_serial, date_time, wind_date stuff...









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
