# Debugging of the code.

# Take some example tracks, to calculate values manually.

# Select some flights (maybe 5 of outward, 5 of inward)
# Ouput to csv with flights numbered

# Plus filter out calculated values for these flights.



#Extract the GPS points for filtered flights#####
# names(flights.sub)

#To get GPS points
#1. Get GPS data
#2. Filter by device id and start and end time - do for earch flight
#3. Combine into single dataframe

#Get GPS data #####
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')



#See what tables are available
sqlTables(gps.db)

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query=
                "SELECT DISTINCT g.device_info_serial,
                  g.date_time, g.longitude, g.latitude, g.x_speed,
                  g.y_speed, g.z_speed, g.positiondop, g.speed_accuracy,
                  c.bearing_next, c.bearing_prev, c.nest_gc_dist,
                  c.nest_bear, c.inst_ground_speed, c.p2p_dist,
                  c.time_interval_s, c.turning_angle, c.flight_class,
                  c.flight_id, g.altitude
                FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
                WHERE g.device_info_serial = c.device_info_serial
                AND g.date_time = c.date_time
                ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)

#Get the flight data too
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")



#Previously accessed table of GPS data:
save(gps, flights, file = "debugging_init.RData")

load("debugging_init.RData")


gps$date_time[1:10]

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

flights$start_time  <-  as.POSIXct(as.character(flights$start_time),
                                   tz="GMT",format="%Y-%m-%d %H:%M:%S")

flights$end_time    <-  as.POSIXct(as.character(flights$end_time),
                                   tz="GMT",format="%Y-%m-%d %H:%M:%S")


gps.sub <- gps[1,]
gps.sub <- gps.sub[-1,]
# 
# i <- 5
# i <- 1
# i <- 9




#names(flights)

#Filter flights####
#Get a list of flight_id for outward flights of between 30 and 60 minutes
out.flight.sub <- flights$flight_id[flights$duration > 1800 & 
                                     flights$duration < 3200 &
                                     flights$trip_flight_type ==
                                     "outward"][1:5]

#Do the same for inward flights
in.flight.sub <-  flights$flight_id[flights$duration > 1800 &
                                     flights$duration < 3200 &
                                     flights$trip_flight_type ==
                                     "inward"][1:5]

out.flight.sub
in.flight.sub

#Extract just these flights
flight.sub <- c(out.flight.sub, in.flight.sub)

flights.sub <- flights[flights$flight_id %in% flight.sub ,]

flights_weather.sub <- flights_weather[flights$flight_id %in% flight.sub ,]

flights.sub <- cbind(flights.sub, flights_weather.sub)

flights.sub.original <- flights.sub
flights.sub <- flights.sub.original


#Filter GPS data ####
for(i in seq(along = flight.sub)){
  device <- flights.sub$device_info_serial[flights.sub$flight_id ==
                                             flight.sub[i]]
  start.time <- flights.sub$start_time[flights.sub$flight_id ==
                                                 flight.sub[i]]
  end.time <-flights.sub$end_time[flights.sub$flight_id ==
                                      flight.sub[i]]
  filter <- (gps$device_info_serial == device & 
    gps$date_time >= start.time &
    gps$date_time <= end.time)
  gps.f <- gps[filter,]
  gps.sub <- rbind(gps.sub, gps.f)
}



summary(filter)

length(gps.sub$z_speed)

str(flights)
str(gps.sub)
str(gps)



# Output the data to csv#####

# First flight summary information for the flights
write.csv(flights.sub, "flights.sub.csv")

# Then the GPS points for the flights
write.csv(gps.sub, "gps.sub.csv")


names(gps.sub)
# Map tracks ###########
plot(gps.sub$longitude, gps.sub$latitude, col = as.numeric(gps.sub$flight_id))

#Then do dot-to-dot lines
for( i in seq ( along = (unique(gps.sub$flight_id)))){
  y <- unique(gps.sub$flight_id)[i]
  
  x <- subset(gps.sub, flight_id == y, select=c(longitude,latitude))
  z <- length(x$longitude)
  
  segments(x$longitude[-1], x$latitude[-1], x$longitude[1:z-1], x$latitude[1:z-1],
           col = as.numeric(y))
}




# Re-calculating summary stats####

#Required packages:
#required for distance calculations
library(fossil)   

#required for some circular calculations
library(circular) 

#Go though each of the trips, calculating values
for( i in seq ( along = (unique(gps.sub$flight_id)))){

  #Get flight_id
  y <- unique(gps.sub$flight_id)[i]
  
  #Subset the gps data, to just get the points for this flight
  x <- subset(gps.sub, flight_id == y)
  
  #Check the number of points (number of data rows)
  z <- length(x$longitude)
  


}

a <- flight.info(7, gps = gps.sub)


flight.sub

#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number, n.nest_id, n.latitude, n.longitude, t.device_info_serial
FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t
WHERE n.ring_number = t.ring_number
ORDER BY n.ring_number ASC;")

a

lookup_nest <- function(device_info){
  x     <- nest_loc$device_info_serial == device_info
  lat   <- nest_loc$latitude[x]
  long  <- nest_loc$longitude[x]
  return(c(lat,long))
}


t <- 7


flight.info <- function(t, gps = gps.sub){
  
  library(fossil)   #required for distance calculations
  library(circular) #required for some circular calculations
  #make a subset of 'gps' containing just data for flight, t.
  
  sub01 <- subset(gps.sub,flight_id == t)
  
  
  n <- length(sub01$date_time)         #the number of gps points for this flight
  
  #calculate various paramaters for flights#################
  start_time <- min(sub01$date_time)   #start time
  end_time  <-  max(sub01$date_time)  #end time
  duration <- as.numeric(difftime(end_time,start_time,units="secs"))   #get flight duration in seconds
  dist_max  <-  max(sub01$nest_gc_dist)   #greatest distance reached from nest
  dist_total <- sum(sub01$p2p_dist[1:n])   #total distance travelled, exclude first point p2p distance, as this includes distance to point before flight 'started'.
  interval_mean <- mean(sub01$time_interval_s)   #mean log interval
  interval_min  <- min(sub01$time_interval_s)     #min log interval, may be useful for highlighting flights where high resolution GPS data is available (i.e. where the conditional log mode was used). It might make more sense to floor or round this value.
  device_info_serial <- sub01$device_info_serial[1]  #get device_info_serial
  
  start_long   <-  sub01$longitude[1]
  start_lat   <-   sub01$latitude[1]
  end_long    <-   sub01$longitude[n]
  end_lat    <-    sub01$latitude[n]
  nest <- lookup_nest(device_info_serial)
  
  dist_nest_start    <-   1000*deg.dist(nest[2],nest[1],start_long,start_lat)
  dist_nest_end      <-   1000*deg.dist(nest[2],nest[1],end_long,end_lat)
  
  dist_nest_start    <-   1000*sub01$nest_gc_dist[1]
  dist_nest_end      <-   1000*sub01$nest_gc_dist[n]
    names(sub01)
  
#   sub01
#   ?deg.dist
#   long,lat
  #Displacement relative to colony/ nest, i.e. difference between final and first distance from nest.
  dist_nest_dif <- dist_nest_end - dist_nest_start
  
  
  #Some summaries of various values useful in drift analysis and similar calculations#######
  dist_a_b    <-    1000*deg.dist(start_long,start_lat,end_long,end_lat)              #require a p2p distance function
  straigtness <-    dist_a_b/dist_total              #use total distance travelled, and straight-line distance
  bearing_a_b <-     earth.bear(start_long,start_lat,end_long,end_lat)             #bearing from start position to final position
  
  
  #Some calculations regarding speed###################
  speed_a_b  <-  dist_a_b/duration     #resultant speed for distance travelled over time
  #flight_class == 3
  speed_inst_mean <-   mean(sub01$inst_ground_speed[sub01$flight_class == 3],na.rm = TRUE) #excluding the non-flight points (usually the first and final point
  speed_inst_med <-   median(sub01$inst_ground_speed[sub01$flight_class == 3],na.rm = TRUE)
  speed_inst_var <-   var(sub01$inst_ground_speed[sub01$flight_class == 3],na.rm = TRUE)
  #names(gps)
  
  #Altitude, max, mean, median
  alt_max    <- max(sub01$altitude[sub01$flight_class == 3],
                    na.rm = TRUE)
  alt_min    <- min(median(sub01$altitude[sub01$flight_class == 3],
                           na.rm = TRUE))
  alt_mean   <- mean(median(sub01$altitude[sub01$flight_class == 3],
                            na.rm = TRUE))
  alt_med    <- median(sub01$altitude[sub01$flight_class == 3],
                       na.rm = TRUE)
  
  # 'circular' package functions#############
  #get value of rho
  rho        <- rho.circular(sub01$bearing_next, na.rm = TRUE)
  ang_dev    <- angular.deviation(sub01$bearing_next, na.rm = TRUE)
  ang_var    <- angular.variance(sub01$bearing_next, na.rm = TRUE)
  
  #make a vector containing all this data
  data.out <- c(t,n,start_time,end_time,duration,dist_max,
                dist_total,interval_mean,interval_min,
                device_info_serial,start_long,start_lat,end_long,
                end_lat,dist_nest_start,dist_nest_end,
                dist_nest_dif,dist_a_b,straigtness,
                bearing_a_b,speed_a_b,speed_inst_mean,
                speed_inst_med,speed_inst_var,alt_max,
                alt_min,alt_mean,alt_med, rho, ang_dev, ang_var)  
  
  return(data.out)            #output a vector for the bird of flight id
}






#for each gps point lookup the nest location  
nest_pos <- sapply(sub01$device_info_serial, lookup_nest)

#to transpose this generated matrix, so that dimentions are correct
nest_pos <- t(nest_pos)
nest_pos <- as.data.frame(nest_pos)
colnames(nest_pos) <- c("lat","long")


#calculate grand circle distance from nest for each GPS location             
gc_dist <- deg.dist(sub01$latitude, sub01$longitude, nest_pos$lat, nest_pos$long)



dist_nest_start    <-   1000*deg.dist(nest[2],nest[1],start_long,start_lat)
dist_nest_end      <-   1000*deg.dist(nest[2],nest[1],end_long,end_lat)


dist_nest_start    <-   1000*deg.dist(nest[1],nest[2],start_lat,start_long)
dist_nest_end      <-   1000*deg.dist(nest[1],nest[2],end_long,end_lat)

