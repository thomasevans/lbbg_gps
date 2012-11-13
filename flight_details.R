#'A script to process data on individual flights.
#'It will summarise flights by distance travelled, altitude, speed etc.
#'They will also be labelled by foraging trip and device_info_serial.
#'The flights should be numbered for each foraging trip. Perhaps also labelling the first and final flight of a foraging trip - which may contitute 'commuting' flight.


#'First we workout a sensible cutoff value, beyond which gps points will be classified as flight. Clearly some flight points are below this, but this will hopfully include most points
hist(gps$inst_ground_speed[gps$inst_ground_speed < 30  & gps$inst_ground_speed > 1 ],breaks=200, xlim=c(0,30),main="Histogram of instantaneous recored speed",xlab="Instaneous ground speed (GPS) - m/s")
abline(v=3.5,lwd=3,lty=3)



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

#preserve a copy of original, before taking a subset for testing.
gps.original <- gps

#for testing purposes we only take the first x lines
gps <- gps[1:50000,]