#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Trip information

#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')



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


#Trip duratiion ####
hist(trips$duration_s[trips$duration_s < 100 *60 * 60] /60/60, xlim = c(0, 100))
hist(trips$duration_s[trips$duration_s < 50 *60 * 60] /60/60, xlim = c(0, 50))
hist(trips$duration_s[trips$duration_s < 30 *60 * 60] /60/60, xlim = c(0, 30))

