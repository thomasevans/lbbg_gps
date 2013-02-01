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
tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
tm[1:10]
trips$start_time <- tm

trips$end_time[1:10]
tm <- as.POSIXlt(trips$end_time)
#Check how this appears (i.e. time zone)
tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
tm[1:10]
trips$end_time <- tm




#Subset trips ####
#Choose a subset of trips, that we will use as examples.
trips.cond   <-  subset(trips, trips$interval_min < 800 &
                          trips$duration_s > (1.5 * 60 * 60))

x <- sample(1:length(trips.cond), 5, replace=F)
x <- c(x)
x <- c(1, 5, 6, 8)

trips.cond[x]
trips.cond[]


str(trips)


hist(trips$interval_min)




q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
          g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop,
          g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist,
          c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
          c.turning_angle, c.flight_class,  c.flight_id, g.altitude
          FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
          WHERE g.device_info_serial = c.device_info_serial
          AND g.date_time = c.date_time
          AND "

q1b <-  "ORDER BY g.device_info_serial ASC, g.date_time ASC ;"

qc <- " g.device_info_serial = 519 " 

gps <- sqlQuery(gps.db, query= gsub("\n", " ", paste(q1a, qc, q1b, sep="")) ,as.is=TRUE)

?sprintf




#Ideas ####
#Could use map thing, like did for GLS data before (then would
#have coastlines etc.

