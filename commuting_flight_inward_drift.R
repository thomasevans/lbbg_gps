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
    "SELECT DISTINCT lf.start_time, lf.end_time, lf.device_info_serial, lf.flight_id
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


# For testing take only a subset of the flights table.
# flights <- flights[1:10,]


# Initial test with one flight
# i <- 901
# i <- 50

# Drift calculation function -----



# Wrap statistics into a function
flight.drift.fun <- function(i, nest_loc. = nest_loc, flights. = flights){
  
  # Get GPS data function
#   source("gps_extract.R")
  # Connect to database
  require(RODBC)
  require("fossil")
  
  # Nest location
  lookup_nest <- function(device_info){
    x <- nest_loc.$device_info_serial == device_info
    lat <- nest_loc.$latitude[x]
    long <- nest_loc.$longitude[x]
    return(c(lat,long))
  }
  
  
  # Flight start and end time
  time.s  <- flights.$start_time[i]  
  time.e  <- flights.$end_time[i] 
  
  # Nest location, aka. goal location
  nest_site <- lookup_nest(flights.$device_info_serial[i])
  goal.lat   <- nest_site[1]
  goal.long  <- nest_site[2]
  
  
  
  # Get GPS data
  #     flight.points <- gps.extract(flights.$device_info_serial[i],
  #                                  start.t = time.s,
  #                                  end.t = time.e,
  #                                  weather = TRUE,
  #                                  ECMWF = TRUE
  #                                  )
  
  
  gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  
  # Somtimes above fails for some reason - if it fails try again some more times
  if(!inherits(gps.db,"RODBC")){
    for(i in 1:4){
      gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
      if(inherits(gps.db,"RODBC")) break
    }
  }
  
  
  
  
  flight.points <- sqlQuery(gps.db,
                            query = gsub("\n", " ",
                              paste("SELECT DISTINCT
                                g.device_info_serial, g.date_time,
                                g.longitude, g.latitude,
                                w.wind_speed_flt_ht_ecmwf,
                                w.wind_dir_ecmwf
                                FROM gps_uva_tracking_speed_3d_limited AS g,
                                lund_points_wind_ECMWF as w
                                WHERE g.device_info_serial = w.device_info_serial
                                AND g.date_time = w.date_time
                                AND ",
                                paste(" g.device_info_serial = ",
                                      flights.$device_info_serial[i], " AND ",
                                      "g.date_time >= #", time.s, 
                                      "# AND g.date_time <= #", time.e,
                                      "# ", sep = ""),
                                " ORDER BY g.device_info_serial ASC, g.date_time ASC ;", sep=""))
                            ,as.is=TRUE)
  
#   flight.points
  if(is.data.frame(flight.points)){
    if(length(flight.points$device_info_serial) > 2){
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
      start_to_goal_dist <- 1000 * deg.dist(flight.points$longitude[1],
                                             flight.points$latitude[1],
                                             goal.long,
                                             goal.lat)
      
      # number of points
      n_points   <- length(flight.points$device_info_serial)
      
      # Flight total duration in seconds
      total_time <- as.numeric(difftime(flight.points$date_time[n_points],
                                        flight.points$date_time[1], units = "secs"))
      
      # Flight_id
      flight_id <- flights.$flight_id[i] 
      device_info_serial <- flight.points$device_info_serial[1]
      bear_first_to_goal <- earth.bear(flight.points$longitude[1],
                                       flight.points$latitude[1],
                                       goal.long,
                                       goal.lat)
      
      
      # Function to give wind direction relative to goal direction
      wind_dir_fun <- function(wind, goal){
        x <- wind - goal
        x <- x %% 360
        if(!is.na(x)){
          if(x > 180){x <- -1*(360 - x)}
        } else x <- NA
        return(x)  
      }
      
      
      
      # Variable initiation
      date_time <- goal_dist <- start_dist <- bear_goal <-
        bear_dif <- dist_straight_line <- dist_drift_prop <-
        dist_along_staight_line_from_start <-
        dist_to_goal_straight_line <- dist_prop_to_goal <-
        time_from_start <- time_from_end <- prop_time <-
        wind_dir_rel <- side_wind <- time_previous <-
        full_drift_exp_dist <- act_drift_from_last_point <- 
        drift_prop <- wind_mean_segment <-  wind_dir_rel_seg <-
        side_wind_segment <-  bear_dif_seg <-
        drift_dist_segment <-  expected_drift_segment <-
        drift_relative_segment  <- NULL
      
      
      # cicurlar package for functions such as 'rad', convert degrees to radians
      require("circular")
      
      # names(flights.)
      # For each points calculate a bunch of different things
      # j <- 2
      for(j in 1:n_points){
        date_time[j] <- as.character(flight.points$date_time[j])
        
        
        goal_dist[j] <- 1000* deg.dist(flight.points$longitude[j],
                                       flight.points$latitude[j],
                                       goal.long,
                                       goal.lat)
        
        start_dist[j] <- 1000* deg.dist(flight.points$longitude[j],
                                        flight.points$latitude[j],
                                        flight.points$longitude[1],
                                        flight.points$latitude[1])
        
        bear_goal[j] <- earth.bear(flight.points$longitude[j],
                                   flight.points$latitude[j],
                                   goal.long,
                                   goal.lat)
        
        bear_dif[j] <- abs(bear_goal[j] - bear_first_to_goal)
        
        # Not sure that this is correct - need to think about it a bit more/ test it on more examples.
        dist_straight_line[j] <- goal_dist[j] * sin(rad(bear_dif[j]))
        
        # Proportional drift, distance from straight-line distance at current point, and the total distance if the straight-line (as the crow flys) path were taken
        dist_drift_prop[j] <- dist_straight_line[j]/start_to_goal_dist
        
        # Distance along straight-line route from start - not sure that it gives correct answer
        dist_along_staight_line_from_start[j] <- sqrt(
          (start_dist[j]*start_dist[j]) +
            (dist_straight_line[j]*dist_straight_line[j]))
        
        # Distance to goal along straghtline, if negative beyond goal (?)
        dist_to_goal_straight_line[j] <- start_to_goal_dist - 
          dist_along_staight_line_from_start[j]
        
        # Proportion of distance travelled along straight-line path, if >1 beyond goal
        dist_prop_to_goal[j] <- dist_along_staight_line_from_start[j]/start_to_goal_dist
        
        # Time from start
        time_from_start[j] <- as.numeric(difftime(flight.points$date_time[j],
                                                  flight.points$date_time[1],
                                                  units = "secs"))
        
        # Time from end
        time_from_end[j] <- as.numeric(difftime(flight.points$date_time[n_points],
                                                flight.points$date_time[j],
                                                units = "secs"))
        
        # Proportion of time (time from start over total flight time)
        prop_time[j]   <-  time_from_start[j]/total_time 
        
        # Wind direction relative to goal direction
        wind_dir_rel[j] <- wind_dir_fun(flight.points$wind_dir_ecmwf[j],
                                        bear_first_to_goal)
        
        # Side wind component at flight-height, negative if to left, positive if to right
        side_wind[j] <- flight.points$wind_speed_flt_ht_ecmwf[j] * 
          sin(rad(wind_dir_rel[j]))
        
        # Time from previous point
        x <- 1
        if(j > 1) x <- j - 1
        
        time_previous[j] <- as.numeric(difftime(flight.points$date_time[j],
                                                flight.points$date_time[x],
                                                units = "secs"))
        
        # Expected drift under no compensation following a fixed heading
        # Negative indicates drift to left (anticlockwise) of heading
        full_drift_exp_dist[j] <- side_wind[j]*time_previous[j]
        
        # If negative drift is reduced, if positive, increased.
        act_drift_from_last_point[j] <- (dist_straight_line[j]) - (dist_straight_line[x])
        
        # Prop drift
        drift_prop[j] <- act_drift_from_last_point[j]/full_drift_exp_dist[j]
      
      
      
      
      ###### Drift from last point, based on new goal direction at each point
      
      # Distance to goal from current point
      # calculated above:
#       goal_dist[j]
      
      # wind at previous point:
#       flight.points$wind_speed_flt_ht_ecmwf[x]
      
      # wind at current point:
#       flight.points$wind_speed_flt_ht_ecmwf[j]
      
      # average wind over flight segment
      wind_mean_segment[j]  <- (flight.points$wind_speed_flt_ht_ecmwf[x] +
                          flight.points$wind_speed_flt_ht_ecmwf[j])/ 2
      
      # Wind side wind component
          # Direction relative to goal at previous point:
          wind_dir_rel_seg[j] <- wind_dir_fun(
            flight.points$wind_dir_ecmwf[j],
            bear_goal[j])
      
      
          # Side component
          side_wind_segment[j] <-  wind_mean_segment[j] * 
            sin(rad(wind_dir_rel_seg[x]))
      
      # Drift from last point
#       goal_dist[j]*sin
      bear_dif_seg[j] <- abs(bear_goal[j] - bear_goal[x])


      drift_dist_segment[j] <- goal_dist[j] * sin(rad(bear_dif[j]))

      
      # expected drift per segment (i.e. from last point)
        # Time from last point
#         time_previous[j]

        # Expected drift
        expected_drift_segment[j] <-
            time_previous[j] * side_wind_segment[j]

      

      # Relative drift per segment
      drift_relative_segment[j] <- 
            drift_dist_segment[j]/expected_drift_segment[j]



#     *****Add vairables to output
#     *****Comment out lines that don't do an opperation
#     ***** E.g. those lines that just call previously made variable
#     ***** Then run the code to generate the data values
# 
#     ***** Do graphs....

      }




      # Combine data from all points into a single dataframe object to output
      data.var <- cbind(flight_id,device_info_serial,date_time,goal_dist,
                        start_dist,bear_goal,bear_dif,dist_straight_line,
                        dist_drift_prop,dist_along_staight_line_from_start,
                        dist_to_goal_straight_line,dist_prop_to_goal,
                        time_from_start,time_from_end,prop_time,
                        wind_dir_rel,side_wind,time_previous,
                        full_drift_exp_dist,act_drift_from_last_point,
                        drift_prop, wind_mean_segment ,  wind_dir_rel_seg ,
                        side_wind_segment ,  bear_dif_seg ,
                        drift_dist_segment ,  expected_drift_segment ,
                        drift_relative_segment)
      
      data.var <- as.data.frame(data.var)
      
      return(data.var)
    } else return(NULL)
  } else return(NULL)

}




# Check that it gives sensible output
# flight.drift.fun(i = 6)
# flight.drift.fun(i = 50)
# flight.drift.fun(i = 278)
# flight.drift.fun(i = 901)
# test.data <- flight.drift.fun(i = 50)


# Now run the function in parallel for all flights ----

# Require packages
library(foreach)
library(doParallel)

#Make cluster of number of devices instances
cl <- makeCluster(detectCores())

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  

#export needed data
clusterExport(cl, c("flights","nest_loc","flight.drift.fun"))  


# str(lst)

#make a list object to recieve the data
lst <- list()
f <- length(flights$flight_id)

# Doesn't take long  - ca. 40 s
system.time({lst <- foreach(i = 1:f ) %dopar%{
  flight.drift.fun(i = i)
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)

# Combine info for all flights into a single matrix - quite slow ----
flights.par <- do.call(rbind , lst)
# Matrix to data.frame conversion
flights.par <- as.data.frame(flights.par)
# Make a backup - save the data to R binnary file
save(flights.par, file = "commuting_flight_inward_drift_data_out.RData")




# Save to a new Database table
# Save data to database -------
odbcCloseAll()

gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
# names(gps.data.par)

str(flights.par)

# getSqlTypeInfo("ACCESS")


# Correct structure in flights.par table
str(flights.par)

as.num.fun <- function(x){
  x <- as.character(x)
  as.numeric(x)
}

as.int.fun <- function(x){
  x <- as.character(x)
  as.integer(x)
}

flights.par$flight_id <- as.int.fun(flights.par$flight_id)
flights.par$device_info_serial <- as.int.fun(flights.par$device_info_serial)
  
tm <- as.character(flights.par$date_time)
tm <- as.POSIXlt(tm)
attr(tm, "tzone") <- "UTC"
flights.par$date_time <- tm

flights.par$goal_dist <- as.num.fun(flights.par$goal_dist)
flights.par$start_dist <- as.num.fun(flights.par$start_dist)
flights.par$bear_goal <- as.num.fun(flights.par$bear_goal)
flights.par$bear_dif <- as.num.fun(flights.par$bear_dif)
flights.par$dist_straight_line <- as.num.fun(flights.par$dist_straight_line)
flights.par$dist_drift_prop <- as.num.fun(flights.par$dist_drift_prop)
flights.par$dist_along_staight_line_from_start <- as.num.fun(flights.par$dist_along_staight_line_from_start)
flights.par$dist_to_goal_straight_line <- as.num.fun(flights.par$dist_to_goal_straight_line)
flights.par$dist_prop_to_goal <- as.num.fun(flights.par$dist_prop_to_goal)
flights.par$time_from_start <- as.num.fun(flights.par$time_from_start)
flights.par$time_from_end <- as.num.fun(flights.par$time_from_end)
flights.par$prop_time <- as.num.fun(flights.par$prop_time)
flights.par$wind_dir_rel <- as.num.fun(flights.par$wind_dir_rel)
flights.par$side_wind <- as.num.fun(flights.par$side_wind)
flights.par$time_previous <- as.num.fun(flights.par$time_previous)
flights.par$full_drift_exp_dist <- as.num.fun(flights.par$full_drift_exp_dist)
flights.par$act_drift_from_last_point <- as.num.fun(flights.par$act_drift_from_last_point)
flights.par$drift_prop <- as.num.fun(flights.par$drift_prop)



flights.par$side_wind_segment <- as.num.fun(flights.par$side_wind_segment)
flights.par$bear_dif_seg <- as.num.fun(flights.par$bear_dif_seg)
flights.par$drift_dist_segment <- as.num.fun(flights.par$drift_dist_segment)
flights.par$expected_drift_segment <- as.num.fun(flights.par$expected_drift_segment)
flights.par$drift_relative_segment <- as.num.fun(flights.par$drift_relative_segment)

str(flights.par)

#Output flight wind par data to database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights.par, tablename = "lund_flight_com_in_drift_points",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(device_info_serial = "integer",
                      date_time = "datetime",
                      flight_id = "integer",
                      goal_dist = "double",
                      start_dist = "double",
                      bear_goal = "double",
                      bear_dif = "double",
                      dist_straight_line = "double",
                      dist_drift_prop = "double",
                      dist_along_staight_line_from_start = "double",
                      dist_to_goal_straight_line = "double",
                      dist_prop_to_goal = "double",
                      time_from_start = "double",
                      time_from_end = "double",
                      prop_time = "double",
                      wind_dir_rel = "double",
                      side_wind = "double",
                      time_previous = "double",
                      full_drift_exp_dist = "double",
                      act_drift_from_last_point = "double",
                      drift_prop = "double"
                      )
        )


# ?sqlSave
odbcCloseAll()
# names(flights.par)


