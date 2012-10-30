#********************************
#This script extracts data from the database, then for each trip, produces a summary, for example, greatest distance and bearing at that point, trip duration, start time, end time, number of points, gps interval


#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query="SELECT DISTINCT c.*, g.longitude, g.latitude, g.altitude
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY c.device_info_serial ASC, c.date_time ASC ;"
                ,as.is=TRUE)

#for testing purposes we only take the first x lines
gps <- gps[1:10000,]

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

#if we want to check which columns are present
names(gps)






