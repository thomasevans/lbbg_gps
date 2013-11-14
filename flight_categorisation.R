# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script is to test different methods to categorise flight to sepperate out commuting (directed) flight from other types of flight (mainly expected to be searching).


#'  1. Get a subset of flights to use in testing - get 50 of each
#'   i.  For outward flights
#'   ii. For inwward flights

# Get data - database  ####

# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

#Get a copy of the flights DB table.
flights.all <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r

flights.all$start_time <- as.POSIXct(flights.all$start_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

flights.all$end_time <- as.POSIXct(flights.all$end_time,
                               tz="GMT",
                               format="%Y-%m-%d %H:%M:%S")

# Make flight type a factor rather than character
flights.all$trip_flight_type <- as.factor(flights.all$trip_flight_type)
# summary(flights.all$trip_flight_type)


# Subsetting flight data ####
flights.com <- subset(flights.all, trip_flight_type == "inward"  | trip_flight_type == "outward")

# Fixed seed
set.seed(25)
# Sample of commuting flights
fs <- sample(flights.com$flight_id,100)

# Get GPS points for these flights
source("gps_extract.R")

# Index vector for flights.com
flights.com.ind <- 1:length(flights.com$flight_id)
# i <- 5
# Initialise 'points', which will hold all point data
points <- NULL
for( i in 1:length(fs)){
  id  <- fs[i]
  ind <- flights.com.ind[flights.com$flight_id == id]
  points.temp <- gps.extract(flights.com$device_info_serial[ind],flights.com$start_time[ind],flights.com$end_time[ind])
  points <- rbind(points,points.temp)
}

points$date_time <- as.POSIXct(points$date_time,
                                   tz="GMT",
                                   format="%Y-%m-%d %H:%M:%S")

#'  2. Plot maps for non categorised outward and inward flights

# Map non-categorised flights ####
# Get mapping function
source("maps_flights.R")

# map specified
maps.flights(points, all.flights = FALSE, flight.id = fs[20:25])

maps.flights(points, all.flights = TRUE)


#'  3. Make categorisation alogrithm
#'  
#'  4. Test alorithm on a few individual flights
#'  
#'  5. Test algorithm on full sample (50 of each flight type)
#'  
#'  6. When satisfied with algorithm apply to all 'commuting' flights, flights classified as outward or inward.
#'  
#'  7. Re-analyse all these flights to get summary statistics
#'  
#'  8. Re-run previous statistical analyses on newly analysed data.
