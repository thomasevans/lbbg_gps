# Get an example migration track...

# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Looking at trips performned to Gotland - proportion of trips by period
# Number of trips etc.



#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

str(trips)
summary(as.factor(trips$trip_type))


# Subset migration trips
trip.mig <- subset(trips, trip_type == 1)

# Parallel foreach loop to get GPS data for each trip
library(doParallel)
library(foreach)

cl <- makeCluster(8)
registerDoParallel(cl)

clusterExport(cl, c("trip.mig"))  


n <- length(trip.mig[,1])

gps.mig <- foreach (i = 1:n, .combine = rbind) %dopar% {
  source("gps_extract.R")
  d <- trip.mig$device_info_serial[i]
  t1 <- trip.mig$start_time[i]
  t2 <- trip.mig$end_time[i]
  gps.dat <- gps.extract(d, t1, t2)
  gps.dat <- cbind(i, gps.dat)
  return(gps.dat)
}

stopCluster(cl)

str(gps.mig)

# Get datetime in correct format.
gps.mig$date_time <- as.POSIXct(gps.mig$date_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S")
head(gps.mig$date_time)
summary(as.factor(gps.mig$i))

gps.mig <- gps.mig[order(gps.mig$i, gps.mig$date_time),]


save(gps.mig, file = "migration_GPS.Rdata")

