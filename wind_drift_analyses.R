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
rm(list = setdiff(ls(), c("flights","gps.data")))

# Close connection
odbcCloseAll()


# Calculations ----

# Calculate wind.speed at flight height for all points
source("wind_shear.R")
wind.data <- mapply(wind.shear,
                    uwind10 = gps.data$uwnd_10m,
                    vwind10 = gps.data$vwnd_10m,
                    height = gps.data$altitude)
wind.data <- t(wind.data)
wind.data <- as.data.frame(wind.data)
row.names(wind.data) <- NULL
names(wind.data) <- c("wind_u", "wind_v", "wind")

# Have a look at these data
wind_u.rat <- wind.data$wind_u/ gps.data$uwnd_10m
hist(wind_u.rat)

wind_v.rat <- wind.data$wind_v/ gps.data$vwnd_10m
hist(wind_v.rat)
max(wind_v.rat, na.rm = TRUE)
min(wind_v.rat, na.rm = TRUE)


# Remove negative altitude values
rep.neg.alt <- function(x){
  if(is.na(x)) return(x) else{
    if(x < -20) return(NA) else{
      if(x < 0.1){
        return(0.1)} else {
          return(x)}
    }
  }
}

# Remove negative or <0.1 m altitudes with 0.1 value
alt.new <- sapply(gps.data$altitude, rep.neg.alt)
hist(alt.new[alt.new < 100])
sort(gps.data$altitude)[1:1000]

# hist(alt.new[alt.new < 100  & gps.data$positiondop < 2 ])
# hist(gps.data$positiondop)


# Dataframe of calculated paramaters ------------
gps.data.par <- cbind(gps.data[,1:3], wind.data, alt.new)
# str(wind.data)

# Calculate wind speed and direction (bearing from gN) ---------
source("wind_dir_speed.R")

# Calculate at flight height
wind.temp <- mapply(FUN = wind.dir.speed,
                    uwind10 = gps.data.par$wind_u,
                    vwind10 = gps.data.par$wind_v)
wind.temp <- t(wind.temp)
names.df <- names(gps.data.par)
gps.data.par <- cbind(gps.data.par,wind.temp)
names(gps.data.par) <- c(names.df, "wind_sc", "wind_dir_deg")


?sapply

str(gps.data.par)
gc()



