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
  x <- gps.extract(flights$device_info_serial[idx], flights$start_time[idx], flights$end_time[idx], weather = TRUE)
  if(length(x[,1]) == 0) return(cbind(flight_id,rep(NA,51)))
  else return(cbind(flight_id,x))
#   return(list(n))
}


# x <- gps.extract(flights$device_info_serial[1], flights$start_time[1], flights$end_time[1], weather = TRUE)

# Testing
#  gps.wrap(flights$flight_id[idx.wrong[1]], flights)

# Get the data (took ca. 1 h for <5000 flights - after had
# 'pooled' driver setting)
# gps.data.list <- list()
# system.time({
#   gps.data.list <- lapply(X = flights$flight_id, gps.wrap, flights = flights)
# })

# save(gps.data.list, file = "gps.data.list.weather.RData")

load(file = "gps.data.list.weather.RData")

# Determin which flights failed to return data

x <- NULL
for(i in 1:4655){
  x[i] <- length(gps.data.list[[i]])
}

# summary(as.factor(x))

idx <- c(1:4655)

idx.wrong <- idx[x == 102]
# gps.data.list[[idx.wrong[1]]]
# gps.wrap(flights$flight_id[idx.wrong[1]], flights)

# x <- gps.extract(flights$device_info_serial[idx.wrong[1]],
#                  flights$start_time[idx.wrong[1]],
#                  flights$end_time[idx.wrong[1]],
#                  weather = FALSE)

# Remove list items for flights that didn't return data
gps.data.list.ok <- gps.data.list[(!(idx %in% idx.wrong))]


# Merge to dataframe
gps.data <- do.call( rbind , gps.data.list.ok)
# str(gps.data)
flights.ok <- flights[(flights$flight_id %in% unique(gps.data$flight_id)),]


# Clearn workspace so we only have the data we want
flights <- flights.ok
rm(list=setdiff(ls(), c("flights","gps.data")))

# Close connection
odbcCloseAll()






