# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# A script to analyse each flight, with various
# paramaters, such as maximum altitude, calculated.
# First the database will be queried to pull out
# the data columns we needd for our analysis.
# Second. Various paramaters and information about
# the flights will be calculated.
# Third. This will be ouput to a new database table
# specifially for flights.


#database functions###################
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
# sqlTables(gps.db)




#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query=
  "SELECT DISTINCT n.ring_number, n.nest_id,
  n.latitude, n.longitude, t.device_info_serial
  FROM gps_uva_nest_limited AS n,
    gps_uva_track_session_limited AS t
  WHERE n.ring_number = t.ring_number
  ORDER BY n.ring_number ASC;")


#get all the GPS info that we require

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query =
  "SELECT DISTINCT g.device_info_serial, g.date_time,
  g.longitude, g.latitude, 
  c.bearing_next, c.bearing_prev, c.nest_gc_dist,
  c.nest_bear, c.inst_ground_speed, c.p2p_dist,
  c.time_interval_s, c.flight_class,
  c.flight_id, g.altitude
  FROM gps_uva_tracking_speed_3d_limited AS g,
    lund_gps_parameters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)


#  rm(list=setdiff(ls(), c("gps","nest_loc")))

# load("flight_gps_data.Rdata")

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",
                            format = "%Y-%m-%d %H:%M:%S")



#preserve a copy of original, before taking a subset for testing.
# gps.original <- gps
#gps <- gps.original
#for testing purposes we only take the first x lines
# gps <- gps[1:50000,]



# nest postion#######################
# function to produce two vectors of latitude and longitude positions
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}


#produce a vector of flight numbers##############
flight_id <- sort(unique(gps$flight_id))

f <- length(flight_id)

#remove zero (i.e. non flight points)
flight_id <- flight_id[2:f]   



# start of function: flight.info######################
# Function 'flight.info' produces a list of lists
# of information on flights.
#
# t - flight_id, the flight number
# gps - the gps dataframe
# 
# Testing
# 
# t <- 59
# names(gps)
# gps$positiondop[1:100]
# gps$speed_accuracy[1:100]
# gps$bearing_next[1:100]
# 
# t = 5


flight.info <- function(t, gps=gps){
  
  library(fossil)   #required for distance calculations
  library(circular) #required for some circular calculations
  #make a subset of 'gps' containing just data for flight, t.
#   install.packages("circular")
  
  sub01 <- subset(gps, flight_id == t)
  
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
  nest <- lookup_nest(device_info_serial)

  # Distance from nest at start of flight.
  dist_nest_start    <-   1000 * 
    deg.dist(nest[2], nest[1], start_long, start_lat)
  
  # Distance from nest at end of flight.
  dist_nest_end      <-   1000 * 
    deg.dist(nest[2], nest[1], end_long, end_lat)
    

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
  
#   library("circular")
#   x <- c(100,90,120)
#   x <- circular(x,units = "degrees")
#   rho.circular(x)
  
#   library(circular)
  bear.circ <- circular(sub01$bearing_next,units="degrees")
  
#   x <- c(100,100,100,100)
#   x <- c(90,180,90,180)
#   x2 <- circular(x, units = "degrees")
#   x2
#   rho.circular(x2)
#   angular.deviation(x2)
#   angular.variance(x2)
#   ?angular.variance
  # Value of rho
  rho        <- rho.circular(bear.circ,
                             na.rm = TRUE)
  
  ang_dev    <- angular.deviation(bear.circ,
                                  na.rm = TRUE)
  
  ang_var    <- angular.variance(bear.circ,
                                 na.rm = TRUE)
  
  #make a vector containing all this data
  data.out <- c(t, n, start_time, end_time, duration,
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
  return(data.out)  
  
  # End function
}
#**********End of this function: flight.info



#Run function 'flight.info' in parallel########
require(foreach)
require(doParallel)

#use x cores, general solution for any windows machine.
cl <- makeCluster(parallel::detectCores())     

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)   

#this maybe neccessary so that the clustered instances or R have the
#required vairables/ functions in their scope, i.e. those functions
#and vairables which are referred to within the 'foreach' function.
clusterExport(cl, c("gps","flight.info"))   

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#make a list object to recieve the data
lst <- list()


#get paramaters for each flight
#Use system.time to time how long this takes.
system.time({lst <- foreach(i = seq(along = flight_id )) %dopar%{
  
  #calculate the trip numbers for the device i. i.e. the function 
  #which we wish to run for each device.     
  x <- flight.info(flight_id[i],gps)
  x <- t(x)
  
  #output data as list (this will be appended to the global list, lst.
  list(x)   
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)
#Time taken one time 4006.31 s (67 mins), 3105s another time
#Time taken on 20130929 (now including data from 2011, 2012, 2013): 7425 seconds (124 minutes).
 


#Create dataframe of flights########
#names for the dataframe
names.flights <- c("flight_id",
                   "points", "start_time",
                   "end_time", "duration",
                   "dist_max", "dist_total",
                   "interval_mean", "interval_min",
                   "device_info_serial",
                   "start_long", "start_lat",
                   "end_long", "end_lat",
                   "dist_nest_start",
                   "dist_nest_end",
                   "dist_nest_dif", "dist_a_b",
                   "straigtness", "bearing_a_b",
                   "speed_a_b", "speed_inst_mean",
                   "speed_inst_med",
                   "speed_inst_var", "alt_max",
                   "alt_min", "alt_mean", "alt_med",
                   "rho", "ang_dev", "ang_var")

#make a dataframe from the list generated by the above function.
flights <- data.frame(matrix(unlist(lst), nrow = length(flight_id), byrow = T))


names(flights) <- names.flights

#origin of UNIX date_time, required for coversion back to datetime objects for start_time and end_time
startdate <- "1970-01-01"
startdate <- as.Date(startdate)

#convert the end_time back to datetime format
flights$end_time <- as.POSIXct(
  as.POSIXlt(flights$end_time, origin=startdate,
             tz= "GMT",format="%Y-%m-%d %H:%M:%S"))

#conver the start_time back to datetime format
flights$start_time <- as.POSIXct(
  as.POSIXlt(flights$start_time, origin=startdate,
             tz= "GMT",format="%Y-%m-%d %H:%M:%S"))

# save(flights, file = "flights_part1.Rdata")

#Label flight type and number for each trip######

# For each trip, look at flights, label with number
# flight per that trip, and whether first or final,
# or inbetween. 
# Querry database to get trip information:
trips <- sqlQuery(gps.db, query="SELECT DISTINCT l.*
  FROM lund_trips AS l
  ORDER BY l.trip_id ASC ;"
                ,as.is=TRUE)

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
trips$start_time <- as.POSIXct(
  trips$start_time, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")

trips$end_time <- as.POSIXct(
  trips$end_time, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")



# Make vector to label trip_id for each flight
flights$trip_id <- flights$trip_flight_n <- 
  rep(NA,length(flights$device_info_serial))

# Flight type vector
flights$trip_flight_type <- 0

# # For testing
# i <- 1665
# i <- 4
# sub01$start_time

# Exclude flights which might have problems with them - those at the end of a bird record
excl <- rep(TRUE, length(flights$device_info_serial))
 trip.excl <- NA
devices <- unique(flights$device_info_serial)
for(x in seq(along = devices)){
#   x <- 2
  excl[max(flights$flight_id[
    flights$device_info_serial == devices[x]])] <- FALSE

  trip.excl[x] <- max(trips$trip_id[
    trips$device_info_serial == devices[x]], na.rm = TRUE)
}
  


# For all trips, do the following
system.time(for(i in seq(along = trips$trip_id)){
  
    id <- trips$trip_id[i]
  
  if( any( trip.excl == id)) {test <- 1} else{
      
      #Get device id
      device <- trips$device_info_serial[i]
    
      
      trip.filter <- ((flights$start_time < trips$end_time[i])
                      & (flights$end_time > trips$start_time[i])
                      & (flights$device_info_serial == device )
                      & excl)
      
      #Make subset of flights for each trip
      sub01 <- subset(flights, trip.filter)
                     
      # Make an index for flights subset above
      x <- seq(along = sub01$flight_id)
      
      # Give them the trip_id from which they come
      # Note however, given our filtering algorithm, 
      # it is possible for a flight to occur in more
      # than one trip. The later trip will take
      # precedence, overwriting details of earlier
      # trip. This should be quite unusual though.
      flights$trip_id[trip.filter]  <- id
      
      # Lable flight number within trip
      flights$trip_flight_n[trip.filter]  <- x
      
      
    #   Then label flights within each trip, by 'outward'
    #   for flight #1, 'inward' for final flight and 'normal'
    #   for all others.
      
      # First label all flights 'normal', then will relabel first 
      # and final trip.
      flights$trip_flight_type[trip.filter] <- "normal"
      
      # Label flight 1 as outward - note if we are missing the 
      # start of the trip then this might not actually be the
      # first flight - but the first recorded flight.
      flights$trip_flight_type[trip.filter &                
                            flights$trip_flight_n == 1] <-
                            "outward"
      
    
      # Label final flight.
      flights$trip_flight_type[trip.filter &
                            flights$trip_flight_n == max(x)] <-
                            "inward"
      
      }
})
#about 340 s




#output data to database##################

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(
  gps.db, flights, tablename = "lund_flights2",
  append = FALSE, rownames = FALSE,
  colnames = FALSE, verbose = FALSE,
  safer = TRUE, addPK = FALSE,
  fast = TRUE, test = FALSE,
  nastring = NULL,
  varTypes = c(start_time = "Date", end_time= "Date"))
  
message("After exporting table to DB, edit table in Access to define data-types and primary keys")

