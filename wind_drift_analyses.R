# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


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

# Calculate wind speed and direction at flight height
wind.temp <- mapply(FUN = wind.dir.speed,
                    uwind10 = gps.data.par$wind_u,
                    vwind10 = gps.data.par$wind_v)
# Transpose
wind.temp <- t(wind.temp)

# Add this to data frame
names.df <- names(gps.data.par)
gps.data.par <- cbind(gps.data.par,wind.temp)
names(gps.data.par) <- c(names.df, "wind_sc", "wind_dir_deg")

# Clean up a bit
rm(wind.temp)
rm(wind.data)


# Direction from which wind is coming from (origin)
wind_origin <- ((gps.data.par$wind_dir_deg + 180) %% 360)
# hist(wind_origin)
gps.data.par <- cbind(gps.data.par,wind_origin)


# Flight vector components ------------------------

#' Calculating heading vector (we already have the wind vector
#' and the track vector).
#' In principle this is simple vector addition.
#' 

# Calculating u and v component of heading vector
# Subtract wind vector from ground vector, leaving
# heading vector
# names(points)
# names(points.weather)
head_u <- gps.data$veast  - gps.data.par$wind_u
head_v <- gps.data$vnorth - gps.data.par$wind_v

head.info <- t(mapply(wind.dir.speed, head_u,
                      head_v))
# names(points.weather)
# Make dataframe
head.info <- as.data.frame(cbind(head.info, head_u, head_v))

# Give names to columns
names(head.info) <- c("head_speed", "head_dir", "head_u", "head_v")

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, head.info)

# Ground speed + track heading
ground_speed <- t(mapply(wind.dir.speed, gps.data$veast,
                         gps.data$vnorth))
ground_heading <- ground_speed[,2]
ground_speed   <- ground_speed[,1]

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, ground_speed, ground_heading)


median(ground_speed[ground_speed < 50], na.rm = TRUE)
median(gps.data.par$head_speed[gps.data.par$head_speed < 50], na.rm = TRUE)

# Heading track vs. ground heading, 0 - 180 indicates 'drift' to right
# Values 180 - 360 indicate 'drift' to left.
hist((gps.data.par$ground_heading - gps.data.par$head_dir) %% 360)



# Flight vectors - relative to track ----------

bear_cor <- function(x, track){
  out <- x - track
  out <- out %% 360
#   if(out > 180) return(360 - out) else return(out)
}

head_dir <-  mapply(bear_cor,
                 x = gps.data.par$head_dir,
                 track = gps.data.par$ground_heading)


wind_dir <-   mapply(bear_cor,
                      x = gps.data.par$wind_dir_deg,
                      track = gps.data.par$ground_heading)



par <- cbind(wind_dir,gps.data.par$wind_sc,head_dir, gps.data.par$head_speed,gps.data.par$ground_speed)
par <- as.data.frame(par)
names(par) <- c("wind_dir","wind_speed","head_dir","head_speed","track_speed")

head(par)


# Function to get alpha or beta, if angle is reflex then subtract it from 360
ang.cor <- function(x){
  if(is.na(x)) return(x) else{
    if(x > 180) return(360 - x)
    else return(x)}
}

# alpha <- NULL
alpha <- sapply(X = par$head_dir, FUN = ang.cor)

beta <- sapply(X = par$wind_dir, FUN = ang.cor)

par <- cbind(par, alpha, beta)

# side and head wind components
wind.comp <- function(beta, wind_speed, wind_dir){
  if(is.na(beta) | is.na(wind_speed) | is.na(wind_dir)){
    return(c(NA,NA))} else {
      # Package needed to convert degrees to radians
      require(CircStats)
      # If beta angle is more than 90 do these calculations
      if(beta > 90){
        beta <- 180 - beta
        beta.rad <- rad(beta)
        
        wind_head_tail <- (cos(beta.rad))*wind_speed
        # As beta >90 wind must be tails wind, make negative
        wind_head_tail <- wind_head_tail * -1
        
        wind_side <- (sin(beta.rad))*wind_speed
        # If wind comes from left make negative
        if(wind_dir < 180)  wind_side <- wind_side * -1
      } else {
        beta.rad <- rad(beta)
        wind_head_tail <- (cos(beta.rad))*wind_speed
        wind_side <- (sin(beta.rad))*wind_speed
        if(wind_dir < 180)  wind_side <- wind_side * -1
      }
      # For testing, check that calculated side wind and head wind components would add up to original wind vector (i.e. wind speed)
    #   test_var <- sqrt((wind_side*wind_side) + (wind_head_tail*wind_head_tail))
    #   return(c(wind_side, wind_head_tail, test_var))
      return(c(wind_side, wind_head_tail))
}
}

wind.comp_calc <- mapply(wind.comp,
                         beta = par$beta,
                         wind_speed = par$wind_speed,
                         wind_dir = par$wind_dir) 
# str(wind.comp_calc)

# Merge to dataframe
# temp <- do.call( rbind , wind.comp_calc)


# Merge to dataframe
temp <- as.data.frame(t(wind.comp_calc))
names(temp) <- c("wind_side", "wind_head_tail")
par <- cbind(par, temp)
names(par)
new.par <- par[,c(1,3,6:9)]
names(new.par) <- c("wind_dir_track",
                    "head_dir_track", "alpha",
                    "beta", "wind_side",
                    "wind_head_tail")

gps.data.par.out <- cbind(gps.data.par, new.par)
head(gps.data.par.out)
# length(unique(names(gps.data.par.out))) == length(names(gps.data.par.out))
# Save data to database -------
odbcCloseAll()

gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
# names(gps.data.par)

#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps.data.par.out, tablename = "lund_flight_points_wind_par",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(device_info_serial = "integer",
                      date_time = "datetime"))

odbcCloseAll()
