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

# Load in requried data (GPS points, device_info_serial, date_time - anything else??)

# gps.points <- ...
# only points within foraging trips (trip_id >= 1??)


#Make list of trips ####
# Calculate number of trips
n_trips <- length(unique(gps$trip_id))

# Make ten lists of foraging trips - to be used in for loop thing, so that we can save each one out to file/ workspace
x <- unique(gps$trip_id)

trip.list <- list()

a <- 1
b <- floor(length(x)/10)

for( i in 1:9){
  trip.list[i] <- list(x[a:(a+b)])
  a <- a+b+1
}
trip.list[10] <- list(x[a:length(x)])



#Make cluster of 16 instances
cl <- makeCluster(16)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


#export the gps data and trip list
clusterExport(cl, c("gps","trip.list"))

# For each list of foraging trips do ####
# weather.points <- list()
weather.points <-  NA
for (i in 1:10){
  

  
  #make a list object to recieve the data
  lst <- list()
  
#   str(lst)
  
  lst <- foreach(z = seq(along = unlist(trip.list[x]))) %dopar%{
    require(RNCEP)
  
#     z <- 5
    
    #get trip id
    d <- trip.list[[i]][z]
    
    #make a subset of GPS data for only this trip
    sub01 <- subset(gps, gps$trip_id == d)
      
    
    
    #Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
#     uwnd10 <- NCEP.interp(
#       variable = "uwnd.10m",
#       level = "gaussian",
#       lat = sub01$latitude,
#       lon = sub01$longitude,
#       dt = sub01$date_time,
#       reanalysis2 = FALSE,
#       keep.unpacking.info = TRUE,
#       interp = 'linear'
#     )
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
    points.weather <- cbind(sub01$device_info_serial,sub01$date_time, uwnd.10m,uwnd.10m.sd, vwnd.10m,vwnd.10m.sd)
    
    list(points.weather)
    
  }
  
  
  #Get length of above - number of GPS points - how to count?
  #Then unlist, and reshape into data.frame, add to previously existing dataframe if present.
# lst[2]
  
  #   x <- 3
  n <- length(gps$trip_id[(gps$trip_id >= trip.list[[i]][1]) & (gps$trip_id <= trip.list[[i]][length(trip.list[[i]])])])

#   length(unlist(lst))/(n+1)
  
#    weather.data <-  data.frame(matrix(unlist(lst), nrow = n, byrow = T))
  
  length(lst)
  
  test <-  data.frame(unlist(lst))
  test <-  data.frame(matrix(unlist(lst), ncol = 6, byrow = F))
  
  test <-  data.frame(matrix(unlist(lst), nrow = n, byrow = T))
#   test <- data.frame(as.matrix(lst))
  
  
  ?matrix
  
   weather.data <-  data.frame(matrix(unlist(lst), nrow = n))
  
  
  weather.points <- rbind(weather.points,weather.data)
  weather.data <- NA
  save(weather.points, file = "weather.points.RData")
}


#close cluster
stopCluster(cl)

# load("weather.points.RData")
#  weather.points[2,]
# weather.points[1,]
# weather.points[2,]
# weather.points[100,]
# weather.points[2000,]

#drop first row (NAs)
weather.points <- weather.points[2:length(weather.points$X1),]

names(weather.points) <- c("device_info_serial","date_time","uwnd.10m","uwnd.10m.sd","vwnd.10m", "vwnd.10m.sd")

summary(unique(as.factor(weather.points$device_info_serial)))
str(weather.points)


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
