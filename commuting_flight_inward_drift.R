# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Description -----
# This script goes through all inward flights calculating for each flight point various paramaters associated with wind drift. Once calculated these are then output to a new database table.


# Database data downloand ----

# Connect to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)


# Get nest data
#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number,
                     n.nest_id, n.latitude, n.longitude, t.device_info_serial
                     FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t
                     WHERE n.ring_number = t.ring_number
                     ORDER BY n.ring_number ASC;")

# Get Flight data
#' Get all 'lund_flights_commuting' and 'lund_flights_commuting_par'.
flights <- sqlQuery(
  gps.db, query =
    "SELECT DISTINCT lf.*, lfc.*
  FROM lund_flights_commuting AS lf, lund_flights AS lfc
  WHERE lf.flight_id = lfc.flight_id
  AND   lfc.trip_flight_type = 'inward'
  ORDER BY lf.flight_id ASC;"
  , as.is = TRUE)

str(flights)

# Correct data structure - factors, datetimes etc...
flights$device_info_serial <- as.factor(flights$device_info_serial)


# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- NULL
tm <- as.POSIXlt(flights$start_time)
#Check how this appears (i.e. time zone)
# head(tm)
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# head(tm)
flights$start_time <- tm

tm <- NULL
tm <- as.POSIXlt(flights$end_time)
#Check how this appears (i.e. time zone)
# head(tm)
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# head(tm)
flights$end_time <- tm


str(flights)



# Initial test with one flight
i <- 1

# Get GPS data function
source("gps_extract.R")

require("fossil")

# Nest location
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}

# Flight start and end time
time.s  <- flights$start_time[i]  
time.e  <- flights$end_time[i] 

# Nest location, aka. goal location
nest_site <- lookup_nest(flights$device_info_serial[i])
goal.lat   <- nest_site[1]
goal.long  <- nest_site[2]



# Get GPS data
flight.points <- gps.extract(flights$device_info_serial[i],
                             start.t = time.s,
                             end.t = time.e,
                             weather = TRUE,
                             ECMWF = TRUE
                             )

# Correct date_time format
# str(flight.points)
tm <- NULL
tm <- as.POSIXlt(flight.points$date_time)
#Check how this appears (i.e. time zone)
# head(tm)
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# head(tm)
flight.points$date_time <- tm


# Distance in metres from first point to goal point
start_to_goal_dist <- 1000* deg.dist(flight.points$longitude[1],
                               flight.points$latitude[1],
                               goal.long,
                               goal.lat)

# number of points
n_points   <- length(flight.points$device_info_serial)

# Flight total duration in seconds
total_time <- as.numeric(difftime(flight.points$date_time[n_points],
  flight.points$date_time[1], units = "secs"))

# Flight_id
flight_id <- flights$flight_id[i] 
device_info_serial <- flights$device_info_serial[i]
bear_first_to_goal <- earth.bear(flight.points$longitude[1],
                        flight.points$latitude[1],
                        goal.long,
                        goal.lat)



wind_dir_fun <- function(wind, goal){
  x <- wind - goal
  x <- x %% 360
  if(x > 180){x <- -1*(360 - x)}
  return(x)  
}



# names(flights)
# For each points calculate a bunch of different things
# j <- 2
for(j in 1:n_points){
  date_time <- flight.points$date_time[j]
  
  goal_dist <- 1000* deg.dist(flight.points$longitude[j],
                              flight.points$latitude[j],
                              goal.long,
                              goal.lat)
  
  start_dist <- 1000* deg.dist(flight.points$longitude[j],
                              flight.points$latitude[j],
                               flight.points$longitude[1],
                               flight.points$latitude[1])
  
  bear_goal <- earth.bear(flight.points$longitude[j],
                              flight.points$latitude[j],
                              goal.long,
                              goal.lat)
  
  bear_dif <- abs(bear_goal - bear_first_to_goal)
  
  # Not sure that this is correct - need to think about it a bit more/ test it on more examples.
  dist_straight_line <- goal_dist * sin(rad(bear_dif))
  
  # Proportional drift, distance from straight-line distance at current point, and the total distance if the straight-line (as the crow flys) path were taken
  dist_drift_prop <- dist_straight_line/start_to_goal_dist
  
  # Distance along straight-line route from start - not sure that it gives correct answer
  dist_along_staight_line_from_start <- sqrt(
    (start_dist*start_dist) +
      (dist_straight_line*dist_straight_line))
  
  # Distance to goal along straghtline, if negative beyond goal (?)
  dist_to_goal_straight_line <- start_to_goal_dist - 
    dist_along_staight_line_from_start
  
  # Proportion of distance travelled along straight-line path, if >1 beyond goal
  dist_prop_to_goal <- dist_along_staight_line_from_start/start_to_goal_dist
  
  # Time from start
  time_from_start <- as.numeric(difftime(flight.points$date_time[j],
                                        flight.points$date_time[1],
                                        units = "secs"))
  # Time from end
  time_from_end <- as.numeric(difftime(flight.points$date_time[n_points],
                                        flight.points$date_time[j],
                                        units = "secs"))
  
  # Proportion of time (time from start over total flight time)
  prop_time   <-  time_from_start/total_time 
  
  
  
  
  
  -20 %% 360
  
}

