# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# A function to extract information on a flight for a given flight id.





# Function 'flight.info' produces a list of lists
# of information on flights.
#
# id - flight_id, the flight number
# gps - the gps dataframe
# 
# Testing
# 
# id <- 59
# names(gps)
# gps$positiondop[1:100]
# gps$speed_accuracy[1:100]
# gps$bearing_next[1:100]
# 
# id = 1
# type = "com"
#   install.packages("circular")


flight.info <- function(id, type = c("com","default")){
# id - flight_id
# type - whether you want to analyse flight as 'commuting' flight or basic flight classification
#   ?require
  
#   id <- 3005
#   type <- "com"
  id <- as.numeric(as.character(id))
  
  require(fossil)   #required for distance calculations
  require(circular) #required for some circular calculations
  source("gps_extract.R")   # Get function to extract GPS data
  require(RODBC)
  

  gps.db3 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  
  
#   gps.db2 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  #   class(gps.db2)
  if(!inherits(gps.db3,"RODBC")){
    for(i in 1:4){
      gps.db3 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
      if(inherits(gps.db3,"RODBC")) break
    }
  }
  
  # Nest query  
  nq1 <- "SELECT DISTINCT n.ring_number, n.nest_id,
  n.latitude, n.longitude, t.device_info_serial
  FROM gps_uva_nest_limited AS n,
    gps_uva_track_session_limited AS t
  WHERE n.ring_number = t.ring_number
  AND t.device_info_serial = "
  
  nq2 <- " ORDER BY n.ring_number ASC;"
  
    

  # flights info
  q1 <- "SELECT DISTINCT f.*
  FROM lund_flights_commuting AS f
  WHERE f.flight_id = "

  q2c <- " ORDER BY f.flight_id ASC;"
  
  q2d <- " ORDER BY f.flight_id ASC;"
  
  # For info on 'tryCatch' error handling function, see: http://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  out <- tryCatch({
#   type = "com"
  if( type == "com"){
  flight_par <- sqlQuery(gps.db3,
                         query = gsub("\n", " ", paste(q1, id, q2c, sep=""))
                      ,as.is=TRUE)
  } else {
    flight_par <- sqlQuery(gps.db3,
                           query = gsub("\n", " ", paste(q1, id, q2d, sep=""))
                           ,as.is=TRUE)
    }
  
#   str(flights.com)
  
  sub01 <- gps.extract(flight_par$device_info_serial,
                       flight_par$start_time,
                       flight_par$end_time)
    
  # Query the gull db to extract bird_id, nest_id, and nest locations
  nest_loc <- sqlQuery(gps.db3, query= 
                         gsub("\n", " ", paste(nq1, sub01$device_info_serial[1], nq2, sep=""))
                       ,as.is=TRUE)
  
  odbcClose(gps.db3)
  
#   sub01 <- subset(gps, flight_id == id)
  
  # The number of gps points for this flight.
  n <- length(sub01$date_time)
  
  # Calculate various paramaters for flights#################
  
  # Start time
  start_time <- min(sub01$date_time)   
  
  # End time
  end_time  <-  max(sub01$date_time)
  
  # Flight duration in seconds
  duration <- as.numeric(difftime(end_time, 
                                  start_time, units="secs")) 
  
  # Greatest distance reached from nest.
  dist_max  <-  max(sub01$nest_gc_dist)
  
  # Total distance travelled, exclude first point
  # p2p distance, as this includes distance to point
  # before flight 'started'.
  dist_total <- sum(sub01$p2p_dist[2:n])
  
  # Mean log interval.
  interval_mean <- mean(sub01$time_interval_s) 
  
  # Min log interval.
  # This may be useful for highlighting flights
  # where high resolution GPS data is available
  # (i.e. where the conditional log mode was used).
  # It might make more sense to floor or round this
  # value.
  interval_min  <- min(sub01$time_interval_s)
  
  # Device info serial
  device_info_serial <- sub01$device_info_serial[1]
  
  
  # Location at start, end, and for nest.
  start_long   <-  sub01$longitude[1]
  start_lat   <-   sub01$latitude[1]
  end_long    <-   sub01$longitude[n]
  end_lat    <-    sub01$latitude[n]
  nest <- c(nest_loc[1,3], nest_loc[1,4])
  
  # Distance from nest at start of flight.
  dist_nest_start    <-   1000 * 
    deg.dist(nest[2], nest[1], start_long, start_lat)
  
  # Distance from nest at end of flight.
  dist_nest_end      <-   1000 * 
    deg.dist(nest[2], nest[1], end_long, end_lat)
  
#   ?deg.dist
  
  # Displacement relative to colony/ nest,
  # i.e. difference between final and first distance
  # from nest.
  dist_nest_dif <- dist_nest_end - dist_nest_start
  
  # Drift analysis paramaters etc. ####    
  # Some summaries of various values useful in drift
  # analysis and similar calculations.
  
  # Straight-line distance from start to end of flight.
  dist_a_b    <-   1000 * deg.dist(
    start_long, start_lat, end_long, end_lat)     
  #   ?deg.dist
  
  # Straightness of flight.
  straigtness <-   dist_a_b/dist_total
  
  # Bearing from start to end.
  bearing_a_b <-   earth.bear(start_long, start_lat,
                              end_long, end_lat)             
  
  
  #Some calculations regarding speed###################
  
  # Resultant speed for distance travelled over time.
  speed_a_b  <-  dist_a_b/duration
  
  # Excluding the non-flight points (usually the first
  # and final point. So only using flight-class == 3.
  
  speed_inst_mean <-   mean(sub01$inst_ground_speed
                            [sub01$flight_class == 3],
                            na.rm = TRUE) 
  
  # Median of instantaneous speed.
  speed_inst_med <-   median(sub01$inst_ground_speed
                             [sub01$flight_class == 3],
                             na.rm = TRUE)
  
  # Variance of instaneous speed.
  speed_inst_var <-   var(sub01$inst_ground_speed
                          [sub01$flight_class == 3],
                          na.rm = TRUE)
  
  #Altitude, max, mean, median ####
  alt_max    <- max(sub01$altitude
                    [sub01$flight_class == 3],
                    na.rm = TRUE)
  
  alt_min    <- min(sub01$altitude
                    [sub01$flight_class == 3],
                    na.rm = TRUE)
  
  alt_mean   <- mean(sub01$altitude
                     [sub01$flight_class == 3],
                     na.rm = TRUE)
  
  alt_med    <- median(sub01$altitude
                       [sub01$flight_class == 3],
                       na.rm = TRUE)
  
  # 'circular' package functions#############
  
  #   vignette("Circular")
  
  # Change to circular object, and exclude final point - bearing then is to following (i.e. not in flight) point.
  bear.circ <- circular(sub01$bearing_next[-length(sub01$bearing_next)],units="degrees")
  
  # Value of rho
  rho        <- rho.circular(bear.circ,
                             na.rm = TRUE)
  
  ang_dev    <- angular.deviation(bear.circ,
                                  na.rm = TRUE)
  
  ang_var    <- angular.variance(bear.circ,
                                 na.rm = TRUE)
  
  #make a vector containing all this data
  data.out <- c(id, n, start_time, end_time, duration,
                dist_max, dist_total, interval_mean,
                interval_min, device_info_serial,
                start_long, start_lat, end_long,
                end_lat, dist_nest_start, dist_nest_end,
                dist_nest_dif, dist_a_b, straigtness,
                bearing_a_b, speed_a_b, speed_inst_mean,
                speed_inst_med, speed_inst_var, alt_max,
                alt_min, alt_mean, alt_med, rho, ang_dev,
                ang_var)  
  
  
  # Output a vector for the bird of flight id
#   x <- function(data.out){
#     return(data.out)
#   }
#   x(data.out)
  
  data.out
  },
                  error = function(cond){rep(NA, 31)}
  )
#   odbcClose(gps.db3)  # close any database connections
  return(out)
  # End function
}
#**********End of this function: flight.info
