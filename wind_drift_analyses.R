# For commuting fligths calculate airspeed, heading etc...


# Get data from database -----

# Connect to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

# Get Flight data
#' Get all 'lund_flights_commuting' and 'lund_flights_commuting_par'.
flights <- sqlQuery(
  gps.db, query =
      "SELECT DISTINCT lf.*, lfc.*
       FROM lund_flights_commuting AS lf, lund_flights_commuting_par AS lfc
       WHERE lf.flight_id = lfc.flight_id
       ORDER BY lf.flight_id ASC;"
  , as.is = TRUE)


# Get GPS point data in two steps
# First get GPS points and paramaters
source("gps_extract.R")

# Function to get GPS points for each flight and add a column for flight_id
gps.wrap <- function(flight_id, flights){
  idx <- flights$flight_id == flight_id
  x <- gps.extract(flights$device_info_serial[idx], flights$start_time[idx], flights$end_time[idx])
  return(cbind(flight_id,x))
#   return(list(n))
}


# Testing
# gps.wrap(flights$flight_id[10], flights)

# Get the data (took ca. 1 h for <5000 flights - after had
# 'pooled' driver setting)
gps.data.list <- list()
system.time({
  gps.data.list <- lapply(X = flights$flight_id, gps.wrap, flights = flights)
})

save(gps.data.list, file = "gps.data.list.RData")

# Merge to dataframe
gps.data <- do.call( rbind , gps.data.list)

# warnings()

# Then get GPS point weather data and (?) add to gps.data table
gps.data$device_info_serial[1:10]
gps.data$date_time[1:10]  #still in character format







# Close connection
odbcCloseAll()


