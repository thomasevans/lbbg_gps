#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Description#######
#This script produces figures and calculations, to show example foraging
#With paramater values calculated etc.



#Datbase extract trips table#########
# test <- "This is text"
# test2 <- paste(test)
# test2


#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')



#See what tables are available
sqlTables(gps.db)

# rm(gps)
#for all of data_base, except pre-deployment and null records

#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

str(trips)


# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
trips$start_time[1:10]
tm <- as.POSIXlt(trips$start_time)
#Check how this appears (i.e. time zone)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# tm[1:10]
trips$start_time <- tm

trips$end_time[1:10]
tm <- as.POSIXlt(trips$end_time)
#Check how this appears (i.e. time zone)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# tm[1:10]
trips$end_time <- tm




#Subset trips ####
#Choose a subset of trips, that we will use as examples.
trips.cond   <-  subset(trips, trips$interval_min < 800 &
                          trips$duration_s > (1.5 * 60 * 60))

names(trips.cond)
length(trips.cond$trip_id)

#If we want repeatable results, we choose where to start our pseudo random series
set.seed(1)

x <- sample(1:length(trips.cond$trip_id), 5, replace=F)

trips.sample <- trips.cond[x,]

str(trips.sample)

#Extract flights and gps points ######

#For testing, get some initial values
id     <-  4
i      <-  trips.sample$device_info_serial[id]
start.t  <-  trips.sample$start_time[id]
end.t    <-  trips.sample$end_time[id]

#Extract GPS points
gps.extract <- function(i, start.t, end.t){
#Function to extract required GPS data
  q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
            g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop,
            g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist,
            c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
            c.turning_angle, c.flight_class,  c.flight_id, g.altitude
            FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
            WHERE g.device_info_serial = c.device_info_serial
            AND g.date_time = c.date_time
            AND "

  q1b <-  " ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
  

  
  q1c <- paste(" g.device_info_serial = ", i, " AND ",
              "g.date_time >= #", start.t, 
              "# AND g.date_time <= #", end.t, "# ", sep = "")
  
  
  gps.sub <- sqlQuery(gps.db, query= gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                  ,as.is=TRUE)
  
  return(gps.sub)
}


#Extract flight points
flights.extract <- function(i, start.t, end.t){
  #Function to extract required flights data
  
  #Parts of query
  q1a <- "SELECT DISTINCT lf.*, lfc.*, lfw.*
            FROM lund_flights AS lf, lund_flights_characteristics AS lfc,
              lund_flights_weather AS lfw
            WHERE lf.flight_id = lfc.flight_id AND  lf.flight_id = lfw.flight_id
            AND "
  
  q1b <-  " ORDER BY lf.device_info_serial ASC, lf.flight_id ASC ;"
  
  
  #Accepts the aguments given to the function, to extract the
  #values requested.
  q1c <- paste(" lf.device_info_serial = ", i, " AND ",
               "lf.start_time >= #", start.t, 
               "# AND lf.start_time <= #", end.t, "# ", sep = "")
  
  #Get flight information
  flight.sub <- sqlQuery(gps.db, query= gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                      ,as.is=TRUE)  
  drops <- c("flight_id.1","start_time.1", "device_info_serial.1", "flight_id.2", "start_time.2")
  flight.sub <- flight.sub[,!(names(flight.sub) %in% drops)]

  return(flight.sub)
}


# test <- flights.extract(i, start.t, end.t)

gps.sub <- gps.extract(i, start.t, end.t)
flights.sub <- flights.extract(i, start.t, end.t)


#Mapping trip #####

#For testing, get some initial values
id     <-  2
i      <-  trips.sample$device_info_serial[id]
start.t  <-  trips.sample$start_time[id]
end.t    <-  trips.sample$end_time[id]

gps.sub <- gps.extract(i, start.t, end.t)
flights.sub <- flights.extract(i, start.t, end.t)


# Map tracks ###########
plot(gps.sub$longitude, gps.sub$latitude, col = as.numeric(gps.sub$flight_id))

#Non-flight points
points(gps.sub$longitude[gps.sub$flight_id == 0], gps.sub$latitude[gps.sub$flight_id == 0], col = "black")

#Then do dot-to-dot lines
for( i in seq ( along = (unique(gps.sub$flight_id)))){
  y <- unique(gps.sub$flight_id)[i]
  
  x <- subset(gps.sub, flight_id == y, select=c(longitude,latitude))
  z <- length(x$longitude)
  
  segments(x$longitude[-1], x$latitude[-1], x$longitude[1:z-1], x$latitude[1:z-1],
           col = as.numeric(y))
}

names(gps.sub)
unique(gps.sub$flight_class)

#Ideas ####
#Could use map thing, like did for GLS data before (then would
#have coastlines etc.

