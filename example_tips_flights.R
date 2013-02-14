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
# 
# str(trips)


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
# 
# names(trips.cond)
# length(trips.cond$trip_id)

# If we want repeatable results, we choose where to start our
# pseudo random series
set.seed(1)
stop("***Warning, using fixed seed for random number generation***")

x <- sample(1:length(trips.cond$trip_id), 100, replace=F)

trips.sample <- trips.cond[x,]

# str(trips.sample)

#Extract flights and gps points ######

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
               "(lf.start_time >= #", start.t, 
               "# OR lf.start_time <= #", end.t, "#) ", sep = "")
  
  #Get flight information
  flight.sub <- sqlQuery(gps.db, query= gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                      ,as.is=TRUE)  
  drops <- c("flight_id.1","start_time.1", "device_info_serial.1", "flight_id.2", "start_time.2")
  flight.sub <- flight.sub[,!(names(flight.sub) %in% drops)]

  return(flight.sub)
}


# test <- flights.extract(i, start.t, end.t)
i <- 100
id <- 100
map.trip(100)


# Mapping trip #####
# Function defenition
map.trip <- function(id){
  #Function to make a map for foraging trip
  
  library(maps)
  
  #First subset the data that we require  
  i      <-  trips.sample$device_info_serial[id]
  start.t  <-  trips.sample$start_time[id]
  end.t    <-  trips.sample$end_time[id]
  
  gps.sub <- gps.extract(i, start.t, end.t)
  flights.sub <- flights.extract(i, start.t, end.t)
  
  
  # Set map limits
  c.xlim <- range(gps.sub$longitude)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(gps.sub$latitude)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par( mar = c(5, 4, 4, 2))
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="light green", bg = "white")
  # ?par
  
  # names(flights.sub)
  # Add points
  
  #Flight points
  # i <- 2
  for( i in seq(along = flights.sub$trip_flight_type)){
    flight.type <- flights.sub$trip_flight_type[i]
    if(flight.type == "outward"){
      points(gps.sub$longitude[gps.sub$flight_id ==
                                 flights.sub$flight_id[i]],
             gps.sub$latitude[gps.sub$flight_id ==
                                flights.sub$flight_id[i]],
             col = "blue", pch = 19)} else 
               if(flight.type == "inward"){
                 points(gps.sub$longitude[gps.sub$flight_id ==
                                            flights.sub$flight_id[i]],
                        gps.sub$latitude[gps.sub$flight_id ==
                                           flights.sub$flight_id[i]],
                        col = "red", pch = 19)}  else 
                        {
                          points(gps.sub$longitude[gps.sub$flight_id ==
                                                     flights.sub$flight_id[i]],
                                 gps.sub$latitude[gps.sub$flight_id ==
                                                    flights.sub$flight_id[i]],
                                 col = "dark grey", pch = 19)}
  }
  
  # Other points
  points(gps.sub$longitude[gps.sub$flight_id == 0],
         gps.sub$latitude[gps.sub$flight_id == 0],
         col = "black")
  
  
  # Add lines
  
  #First grey for all
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = "grey")
  
  for( i in seq ( along = (unique(gps.sub$flight_id)))){
  
    y <- unique(gps.sub$flight_id)[i]
    if(y != 0){
    x <- subset(gps.sub, flight_id == y,
                select=c(longitude, latitude))
    z <- length(x$longitude)
    n <- length(gps.sub$longitude)
    segments(x$longitude[-1], x$latitude[-1],
             x$longitude[1:z-1], x$latitude[1:z-1],
             col = "black")
    }
  }
  
  # Scale bar and axis
  map.scale(ratio = FALSE)
  box()
  axis(side=(1),las=1)
  axis(side=(2),las=1)
#   ?text
  mtext(paste("Device: ", trips.sample$device_info_serial[id],
                  "    Trip: ", trips.sample$trip_id[id])
       , side = 3, line = 2, cex = 1)
  mtext(paste("Departure time: ", min(gps.sub$date_time), " UTC")
        , side = 3, line = 1, cex = 1)
  
    dur <- as.difftime(trips.sample$duration_s[id], units= "secs")
    dur <- as.numeric(dur, units="hours")
    mtext(paste("Trip duration: ",
                format(round(dur, 2), nsmall = 2) , " hours")
          , side = 3, line = 0, cex = 1)
#   ?grconvertX
  legend("topleft", pch=c(19, 19, 19, 1),
         c("IN", "OUT", "OTHER", "NON-FLIGHT"),
         col = c("red", "blue", "dark grey", "black"),
         )
#   ?legend
#   ?legend
  
}
#   names(trips.sample)
#   ?difftime
  
#   ?mtext
#   ?main
# plot

# For testing, get some initial values
# i     <-  1

pdf("example_trips_4.pdf")
for(i in seq(along = (trips.sample$trip_id))){
map.trip(i)
}
dev.off()



pdf("example_trips_3.pdf")
for(i in 20:30){
  map.trip(i)
}
dev.off()



