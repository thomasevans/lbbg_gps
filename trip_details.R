# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps



#********************************
#This script extracts data from the database, then for each trip, produces a summary, for example, greatest distance, trip duration, start time, end time, number of points, gps interval

# install.packages("RODBC")
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query=
  "SELECT DISTINCT c.*, g.longitude, g.latitude, g.altitude
  FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS c
  WHERE g.device_info_serial = c.device_info_serial
  AND g.date_time = c.date_time
  ORDER BY c.device_info_serial ASC, c.date_time ASC ;"
                ,as.is=TRUE)

#for testing purposes we only take the first x lines
#gps <- gps[1:100000,]

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- 
  as.POSIXct(gps$date_time,
             tz="GMT", format="%Y-%m-%d %H:%M:%S")

#gps$date_time[1:10]

#if we want to check which columns are present
#names(gps)
# str(gps)
#produce a vector of trip numbers
trip_id <- sort(unique(gps$trip_id))
f <- length(trip_id)
trip_id <- trip_id[2:f]   #remove zero (i.e. non trip points)

# trip_id <- as.numeric(trip_id)
# str(gps$trip_id)
# test <- trip.info(t = 1088, gps = gps)
#rm(t)
#t <- trip_id[100]

#***********start of function: trip.info
#Function 'trip.info' produces a lists of lists of information on trips
#t - trip_id, the trip number
#gps - the gps dataframe
trip.info <- function(t, gps = gps){

  #make a subset of 'gps' containing just data for trip, t.
  sub01 <- subset(gps, trip_id == t)
  n <- length(sub01$date_time)         #the number of gps points for this trip
#   names(sub01)
  #calculate various paramaters for trips
  start_time <- min(sub01$date_time)   #start time
  end_time  <-  max(sub01$date_time)  #end time
  duration <- as.numeric(difftime(end_time,start_time,units="secs"))   #get trip duration in seconds
  dist_max  <-  max(sub01$nest_gc_dist)   #greatest distance reached from nest
  dist_total <- sum(sub01$p2p_dist[2:n])   #total distance travelled, exclude first point p2p distance, as this includes distance to point before trip 'started'.
  interval_mean <- mean(sub01$time_interval_s)   #mean log interval
  interval_min <- min(sub01$time_interval_s)     #min log interval, may be useful for highlighting trips where high resolution GPS data is available (i.e. where the conditional log mode was used). It might make more sense to floor or round this value.
  interval_max <- max(sub01$time_interval_s)   # To find the largest gap in the trip track data
  trip_type <- ifelse(min(sub01$latitude) < 50, 1, 0) #label trips, zero if non-migratory, and 1 if migratory.
  device_info_serial <- sub01$device_info_serial[1]  #get device_info_serial

  dir_max_dist <- sub01$nest_bear[sub01$nest_gc_dist == dist_max]

  #' Aproximately define a polygon for Gotland, then determine
  #' if trip includes points within this area.
  
  gotland_long   <-  c(18.69902, 18.63378, 18.61279, 18.56489, 18.49318, 18.44077, 18.30666, 18.2061, 18.09348, 18.10016, 18.17383, 18.15944, 18.08482, 18.09232, 18.13166, 18.15832, 18.13748, 18.16773, 18.20654, 18.19873, 18.23659, 18.28966, 18.28202, 18.23032, 18.19989, 18.17564, 18.12695, 18.19584, 18.31607, 18.38137, 18.41146, 18.37254, 18.35202, 18.42005, 18.4743, 18.53251, 18.53104, 18.51167, 18.40778, 18.69513, 18.78849, 18.69069, 18.8719, 18.94208, 19.02026, 18.9025, 18.80552, 18.83831, 18.82224, 19.04768, 19.19782, 19.39945, 19.32853, 19.2397, 19.1699, 19.02679, 18.84733, 18.81656, 18.77957, 18.69902)
    
  gotland_lat    <-  c(57.94159, 57.89255, 57.85448, 57.84177, 57.83054, 57.82949, 57.65937, 57.61784, 57.54344, 57.4375, 57.38398, 57.318, 57.28085, 57.26383, 57.23233, 57.18662, 57.16356, 57.13989, 57.12689, 57.05876, 57.06099, 57.09378, 57.04784, 57.03813, 57.02055, 56.98026, 56.91038, 56.89589, 56.92512, 56.97002, 56.98702, 57.01864, 57.05858, 57.09776, 57.11539, 57.11611, 57.1273, 57.13386, 57.12617, 57.22258, 57.26402, 57.29327, 57.38018, 57.40429, 57.43351, 57.45097, 57.45049, 57.60669, 57.66888, 57.73525, 57.88962, 57.94424, 57.97617, 57.99029, 57.99399, 57.93041, 57.92847, 57.87778, 57.92259, 57.94159)
  
  library(sp)  
  #Number of GPS fixes from within the Gotland polygon
  on_gotland <- 
    point.in.polygon(sub01$longitude,
                         sub01$latitude,
                         gotland_long ,
                         gotland_lat)
  
  gotland <- sum(on_gotland == 1)

#  v <- c(0,0,1,4,2,1,1)
#   sum(v == TRUE)

  #Define as a Gotland trip if at least 2 points are within Gotland polygon.

# install.packages("sp")
# ?point.in.polygon


  gotland_points <- gotland
  gotland_points_prop <- gotland/n

  gotland_time <- sum(sub01$time_interval_s[on_gotland == 1])
  gotland_time_prop <- gotland_time/duration

  gotland <- if(gotland > 2){TRUE}  else{FALSE}
  
  #make a vector containing all this data
  data.out <- c(t, device_info_serial, 
                trip_type,n, start_time,
                end_time, duration,
                dist_max, dist_total,
                interval_mean, interval_min,
                gotland, gotland_points,
                gotland_points_prop, gotland_time,
                gotland_time_prop,
                dir_max_dist,
                interval_max)  
  
  return(data.out)        #output a vector for the bird of trip id
}
#**********End of this function: trip.info

# install.packages(c("foreach","doParallel"))
library(foreach)
library(doParallel)
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.

registerDoParallel(cl)   #start the parellel session of R; the 'slaves', which will run the analysis.

clusterExport(cl, c("gps","trip.info"))   #this maybe neccessary so that the clustered instances or R have the required vairables/ functions in their scope, i.e. those functions and vairables which are referred to within the 'foreach' function.

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#mast a list object to recieve the data
lst <- list()


#get paramaters for each trip
#Use system.time to time how long this takes.
# On 2014-07-02 took: 818 s
system.time({lst <- foreach(i = seq(along = trip_id )) %dopar%{
  #calculate the trip numbers for the device i. i.e. the function which we wish to run for each device.     
  x <- trip.info(trip_id[i],gps)
  x <- t(x)
  list(x)   #output data as list (this will be appended to the global list, lst.
} #end of foreach functions
}) #end of things being timed by system.time

# Back-up the new list object
save(lst, file = "trip_details_list.R")

#close cluster
stopCluster(cl)


# x <- trip.info(trip_id[5],gps)
# x
#names for the dataframe
names.trips <- c("trip_id","device_info_serial",
                 "trip_type","fix_n",
                 "start_time","end_time",
                 "duration_s","dist_max",
                 "dist_total","interval_mean",
                 "interval_min","gotland",
                 "gotland_points",
                 "gotland_points_prop",
                 "gotland_time",
                 "gotland_time_prop",
                 "dir_max_dist",
                 "interval_max")

#make a dataframe from the list generated by the above function.
trips <- data.frame(matrix(unlist(lst),
                           nrow = length(trip_id), byrow = T))
names(trips) <- names.trips

#origin of UNIX date_time, required for coversion back to datetime objects for start_time and end_time
startdate <- "1970-01-01"
startdate <- as.Date(startdate)

#convert the end_time back to datetime format
trips$end_time <- as.POSIXct(as.POSIXlt(
  trips$end_time, origin = startdate,
  tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

#conver the start_time back to datetime format
trips$start_time <- as.POSIXct(as.POSIXlt(
  trips$start_time,origin = startdate,
  tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))


#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, trips, tablename = "lund_trips", append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(start_time = "Date", end_time = "Date"))

message("After output of new table to the database it is neccessary to open the table in the database software (Access) and define primary keys, and possibly modify data types.")

close(gps.db)