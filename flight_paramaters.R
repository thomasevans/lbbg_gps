#'A script to analyse each flight, with various paramaters, such as maximum altitude.
#'First the database will be queried too pull out the data columns we needd for our analysis.
#'Second. Various paramateerrs and information about the flights will be calculated.
#'Third. This will be ouput to a new database table specifially for flights.

#'***********
#'Owing to some error in making an ODBC connection to the remote PostgreSQL Amsterdam 'flysafe' database with my Windows 8 installation, I am instead working with the binary datafile of object 'gps' produced previouly in 'flight_details'.

#save(gps, file = "gps.RData")
#'load in previousl saved GPS data (R data object)
#load("gps.RData")

#'Inspect this dataframe
#str(gps)

#*****************************In the eventual version, I want to do this using the database connection instead. In the mean time I use the previously saved GPS data.

#database functions
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)




#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number, n.nest_id, n.latitude, n.longitude, t.device_info_serial
FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t
WHERE n.ring_number = t.ring_number
ORDER BY n.ring_number ASC;")



#get all the GPS info that we require

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude, g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop, g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist, c.nest_bear, c.inst_ground_speed, c.p2p_dist, c.time_interval_s, c.turning_angle, c.flight_class,c.flight_id
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)




#function to produce two vectors of latitude and longitude positions
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}


#produce a vector of flight numbers
flight_id <- sort(unique(gps$flight_id))
f <- length(flight_id)
flight_id <- flight_id[2:f]   #remove zero (i.e. non flight points)


#***********start of function: flight.info
#Function 'flight.info' produces a list of lists of information on flights
#t - flight_id, the flight number
#gps - the gps dataframe
flight.info <- function(t, gps=gps){
  
  library(fossil)   #required for distance calculations
  #make a subset of 'gps' containing just data for flight, t.
  sub01 <- subset(gps,flight_id == t)
  n <- length(sub01$date_time)         #the number of gps points for this flight
  
  #calculate various paramaters for flights
  start_time <- min(sub01$date_time)   #start time
  end_time  <-  max(sub01$date_time)  #end time
  duration <- as.numeric(difftime(end_time,start_time,units="secs"))   #get flight duration in seconds
  dist_max  <-  max(sub01$nest_gc_dist)   #greatest distance reached from nest
  dist_total <- sum(sub01$p2p_dist[2:n])   #total distance travelled, exclude first point p2p distance, as this includes distance to point before flight 'started'.
  interval_mean <- mean(sub01$time_interval_s)   #mean log interval
  interval_min <- min(sub01$time_interval_s)     #min log interval, may be useful for highlighting flights where high resolution GPS data is available (i.e. where the conditional log mode was used). It might make more sense to floor or round this value.
  device_info_serial <- sub01$device_info_serial[1]  #get device_info_serial
  
  start_long   <-  sub01$longitude[1]
  start_lat   <-   sub01$latitude[1]
  eng_long    <-   sub01$longitude[n]
  end_lat    <-    sub01$latitude[n]
  
  #these will only work once the database link is working - alternative could be to input the nest data myself, from my excel records
  dist_nest_start    <-   deg.dist(lookup_nest(device_info_serial),start_long,start_lat)
  dist_nest_end      <-   deg.dist(lookup_nest(device_info_serial),start_long,start_lat)
    

  #Displacement relative to colony/ nest, i.e. difference between final and first distance from nest.
  dist_nest_dif <- dist_nest_end - dist_nest_start
  
    
    #Some summaries of various values useful in drift analysis and similar calculations
     
    dist_a_b    <-    deg.dist(start_long,start_lat,end_long,end_lat)              #require a p2p distance function
    straigtness <-    dist_total/dist_a_b              #use total distance travelled, and straight-line distance
    bearing_a_b <-     earth.bear(start_long,start_lat,end_long,end_lat)             #bearing from start position to final position
    
    
    #Some calculations regarding speed
    speed_a_b  <-  dist_a_b/duration     #resultant speed for distance travelled over time
  flight_class == 3
    speed_inst_mean <-   gps$inst_ground_speed[gps$flight_class == 3]/length(  gps$flight_class) #excluding the non-flight points (usually the first and final point
    speed_inst_med <-   median(gps$inst_ground_speed[gps$flight_class == 3],na.rm = TRUE)
    speed_inst_var <-   var(gps$inst_ground_speed[gps$flight_class == 3],na.rm = TRUE)

  #Altitude, max, mean, median
  #Altitude requires database too - so will have to wait
  alt_max    <- max(gps$altitude[gps$flight_class == 3],na.rm = TRUE)
  alt_min    <- min(median(gps$altitude[gps$flight_class == 3],na.rm = TRUE))
  alt_mean   <- mean(median(gps$altitude[gps$flight_class == 3],na.rm = TRUE))
  alt_med    <- median(gps$altitude[gps$flight_class == 3],na.rm = TRUE)
    
  
  #make a vector containing all this data
  data.out <- c(t,n,start_time,end_time,duration,dist_max,dist_total,interval_mean,interval_min,device_info_serial,start_long,start_lat,eng_long,end_lat,dist_nest_start,dist_nest_end,dist_nest_dif,dist_a_b,straigtness,bearing_a_b,speed_a_b,flight_class,speed_inst_mean,speed_inst_med,speed_inst_var,alt_max,alt_min,alt_mean,alt_med)  
  
  return(data.out)            #output a vector for the bird of flight id
}
#**********End of this function: flight.info




require(foreach)
require(doParallel)
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.
registerDoParallel(cl)   #start the parellel session of R; the 'slaves', which will run the analysis.
clusterExport(cl, c("gps","flight.info"))   #this maybe neccessary so that the clustered instances or R have the required vairables/ functions in their scope, i.e. those functions and vairables which are referred to within the 'foreach' function.

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#mast a list object to recieve the data
lst <- list()


#get paramaters for each flight
#Use system.time to time how long this takes.
system.time({lst <- foreach(i = seq(along = flight_id )) %dopar%{
  #calculate the trip numbers for the device i. i.e. the function which we wish to run for each device.     
  x <- flight.info(flight_id[i],gps)
  x <- t(x)
  list(x)   #output data as list (this will be appended to the global list, lst.
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)

#names for the dataframe
names.flights <- c("flight_id","points","start_time","end_time","duration","dist_max","dist_total","interval_mean","interval_min","device_info_serial","start_long","start_lat","eng_long","end_lat","dist_nest_start","dist_nest_end","dist_nest_dif","dist_a_b","straigtness","bearing_a_b","speed_a_b","flight_class","speed_inst_mean","speed_inst_med","speed_inst_var","alt_max","alt_min","alt_mean","alt_med")

#make a dataframe from the list generated by the above function.
flights <- data.frame(matrix(unlist(lst), nrow=length(trip_id), byrow=T))
names(flights) <- names.flights


#origin of UNIX date_time, required for coversion back to datetime objects for start_time and end_time
startdate <- "1970-01-01"
startdate <- as.Date(startdate)

#convert the end_time back to datetime format
flights$end_time <- as.POSIXct(as.POSIXlt(flights$end_time,origin=startdate, tz= "GMT",format="%Y-%m-%d %H:%M:%S"))

#conver the start_time back to datetime format
flights$start_time <- as.POSIXct(as.POSIXlt(flights$start_time,origin=startdate, tz= "GMT",format="%Y-%m-%d %H:%M:%S"))



#for each trip, look at flights, label with number flight per that trip, and whether first or final, or inbetween. 
#Querry database to get trip information:
trips <- sqlQuery(gps.db, query="SELECT DISTINCT l.*
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY c.device_info_serial ASC, c.date_time ASC ;"
                ,as.is=TRUE)

flights$trip_id <- 0

for(i in seq= along(trips$trip_id)){
  sub01 <- subset(flights,start_time >= trips$start_time & start_time <= trips$end_time)
  x <- seq(along(sub01$flight_id))
  flights$trip_id[flights,start_time >= trips$start_time & start_time <= trips$end_time)] <- i
  flights$trip_flight_n <- x
}

#output this data to the database

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights, tablename = "lund_flights", append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,varTypes=c(start_time="Date",end_time="Date"))
  


