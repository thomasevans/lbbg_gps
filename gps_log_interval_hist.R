# To link to database
library(RODBC)




#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)


#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db,
                query = "SELECT DISTINCT g.device_info_serial, g.date_time, c.time_interval_s
                FROM gps_uva_tracking_limited AS g,
                cal_mov_paramaters AS c
                WHERE g.device_info_serial = c.device_info_serial
                AND g.date_time = c.date_time;"
                ,as.is=TRUE)


hist(gps$time_interval_s[gps$time_interval_s < 200])
