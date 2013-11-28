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
  return(cbind(flight_id,x))
#   return(list(n))
}


# x <- gps.extract(flights$device_info_serial[1], flights$start_time[1], flights$end_time[1], weather = TRUE)

# Testing
#  gps.wrap(flights$flight_id[10], flights)

# Get the data (took ca. 1 h for <5000 flights - after had
# 'pooled' driver setting)
gps.data.list <- list()
system.time({
  gps.data.list <- lapply(X = flights$flight_id, gps.wrap, flights = flights)
})

save(gps.data.list, file = "gps.data.list.weather.RData")

# load(file = "gps.data.list.RData")

# Merge to dataframe
gps.data <- do.call( rbind , gps.data.list)

# warnings()

# Then get GPS point weather data and (?) add to gps.data table
gps.data$device_info_serial[1:10]
gps.data$date_time[1:10]  #still in character format


gps.data$date_time <- as.POSIXct(gps.data$date_time,
                                         tz="GMT",
                                         format="%Y-%m-%d %H:%M:%S")



weather.data.all <- sqlQuery(
  gps.db, query =
    paste0("SELECT DISTINCT t.*
           FROM lund_points_weather AS t
           ORDER BY t.device_info_serial, t.date_time ASC;", collapse = NULL)
          , as.is = TRUE)

weather.data.all$date_time <- as.POSIXct(weather.data.all$date_time,
                            tz="GMT",
                            format="%Y-%m-%d %H:%M:%S")


get.data <- function(device, date_time, data.set = weather.data.all){
  x <- data.set[data.set$device_info_serial == device  &
                  data.set$date_time == date_time,]
  if(length(x[,1]) == 0) return(as.numeric(rep(NA,6)))
  else return(x)
}

# 
# str(gps.data$date_time[3])
# str(weather.data.all$date_time)
get.data(gps.data$device_info_serial[3],
         date_time = gps.data$date_time[3],
         data.set = weather.data.all)



test <- mapply(FUN = get.data, 
               device = gps.data$device_info_serial[1:10],
               date_time = gps.data$date_time[1:10])

test2 <- t(test)
weather.points <- as.data.frame(test2)
summary(is.na(as.data.frame(test2$device_info_serial)))
str(weather.points)
?mapply
test2 <- do.call( rbind , test)
?lapply

# Close connection
odbcCloseAll()


