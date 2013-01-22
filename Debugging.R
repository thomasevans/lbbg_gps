# Debugging of the code.

# Take some example tracks, to calculate values manually.

# Select some flights (maybe 5 of outward, 5 of inward)
# Ouput to csv with flights numbered

# Plus filter out calculated values for these flights.

names(flights)

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




#Extract the GPS points for filtered flights#####
names(flights.sub)
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
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude, g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop, g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist, c.nest_bear, c.inst_ground_speed, c.p2p_dist, c.time_interval_s, c.turning_angle, c.flight_class,c.flight_id, g.altitude
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

#save(gps, file = "gps.data")

#to test
#i <- 3

gps.sub <- NULL
names(gps.sub) <- names(gps)

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
  gps.f <- rbind(gps.f, gps.f)
}


gps.sub

gps.sub

?save.csv