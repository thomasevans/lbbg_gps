#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

# Description ####
#'A script to process data on individual flights.
#'It will summarise flights by distance travelled, altitude, speed etc.
#'They will also be labelled by foraging trip and device_info_serial.
#'The flights should be numbered for each foraging trip. Perhaps also labelling the first and final flight of a foraging trip - which may contitute 'commuting' flight.


# Database functions ####
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude, g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop, g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist, c.nest_bear, c.inst_ground_speed, c.p2p_dist, c.time_interval_s, c.turning_angle
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)


#check structure of object - does it contain what we expect it to?
#str(gps)

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")



# Cutoff for flight ####
# First we workout a sensible cutoff value, beyond which gps points
# will be classified as flight. Clearly some flight points are below
# this, but this will hopfully include most points.
# hist(gps$inst_ground_speed[gps$inst_ground_speed < 30  & gps$inst_ground_speed > 1 ],breaks=200, xlim=c(0,30),main="Histogram of instantaneous recored speed",xlab="Instaneous ground speed (GPS) - m/s")
# abline(v=3.5,lwd=3,lty=3)


# Testing with subset ####
# Preserve a copy of original, before taking a subset for testing.
gps.original <- gps
gps <- gps.original
#for testing purposes we take the first x lines
gps <- gps[1:50000,]


# Recognise and label flight ####
# Code to recognise flight, and adds this column,
# labelling with 0 if not flight and 1 if over 3.5 ms-1 (flight)
gps$flight_class <- ifelse(gps$inst_ground_speed > 3.5, 1, 0)


# Give flights unique id ####
# We want to label flights with a unique id
# First we make some vectors of next, previous point etc,
# to find start and end points of flights.

flight1 <- gps$flight_class + 1

#make vector of next point value
flight2 <- (2* c(gps$flight_class[2:length(gps$flight_class)], 0)) + 1

#make vector of prev point value
flight3 <- (3* c(0,gps$flight_class[1:(length(gps$flight_class)-1)]))+1



# Label by type of point: 0 - not flight, 1 - start, 2 - end, 3 - flight
# Product of above three vectors, produces unique values for each
# possible point type.
gps$flight_class_2 <- flight1 * flight2 * flight3
#summary(as.factor(gps$flight_class_2))  #inspect this

# Keep a copy of above calculation
fly_type <- gps$flight_class_2        


# Reduce to the four possibilties 
# (not flight, flight, or start or end points of flight).
gps$flight_class_2[(fly_type == 1)  ]          <- 0
gps$flight_class_2[fly_type == 3 ]             <- 1
gps$flight_class_2[(fly_type == 24) |
                     (fly_type == 6) | (fly_type == 8) |
                     (fly_type == 2) | (fly_type == 12)] <- 3
gps$flight_class_2[fly_type == 4] <- 2

#make column for flight id, start with value 0, which will be null value - i.e. not a trip (points at the nest)
gps$flight_id <- 0








#***********start of function: flight.lab
#Function 'flight.lab' will produce a vector of flight number for each device, thus allowing the for loop to be ran in parallel (quicker).
#d - device_info_serial
#gps - the gps dataframe

#summary(as.factor(gps$device_info_serial))
#for testing
#d <- 519

flight.lab <- function(d, gps=get("gps", envir=environment(trip.lab))){
  
  #first make a subset of 'gps' containing just data for device, d, away from the nest.
  sub01 <- subset(gps, flight_class_2 != 0 & device_info_serial == d,select=c(flight_class_2,flight_id))
  
  n <- length(sub01$flight_class_2) #the number of gps positions
  
  
  #a windows progress bar to monitor progress
  pb <- winProgressBar(title = paste( "progress bar for device",d), min = 0,max = n, width = 300)
  
  x <- 0   #x will keep note of trip number, we start at zero.
  #loop through all gps points, labelling them with trip id (x)
  for(i in 1:n){
    setWinProgressBar(pb, i, title=paste("device ", d, " is ", round(i/n*100, 0),"% done")) #refresh the progress bar, so that we can keep note of progress.
    if(sub01$flight_class_2[i] == 1) x <- x+1      #if start of a trip, increment x by one
    sub01$flight_id[i] <- x                    #allocated value of x for flight_id for position 'i'.
  }
  
  close(pb)    #close the windows progress bar
  return(sub01$flight_id)            #output a vector for the bird of flight id
}
#**********End of this function: flight.lab




#cribbed form trip_lab script

#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))

#*Calculate trip id for each device
#do this in parallel
#first load neccessary packages
require(foreach)
require(doParallel)
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.
registerDoParallel(cl)   #start the parellel session of R; the 'slaves', which will run the analysis.

clusterExport(cl, c("devices", "gps"))   #this maybe neccessary so that the clustered instances or R have the required vairables/ functions in their scope, i.e. those functions and vairables which are referred to within the 'foreach' function.

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#work out flight numbers for each device id, then add these to a list of lists (lst)
#Use system.time to time how long this takes - so far has taken around 10-15 minutes on 8 thread machine.
system.time({lst <- foreach(i = seq(along = devices )) %dopar%{
  #calculate the trip numbers for the device i. i.e. the function which we wish to run for each device.     
  x <- flight.lab(devices[i],gps)
  list(x) #output x as list
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)

#make a vector to which the trip numbers will be added for all individuals.
all.points <- rep(0,length(gps$device_info_serial))   #make this all zero (i.e. the default value - where there is no flight)
z <- 0    #a vairable which will store the maximum flight number from the previous device
x <- 0    #a new vector of flight numbers

#Loop through all devices, making flight numbers unique, by adding number of highest numbered existing flight, then adding this to the all.points vector, this being filtered, so that only points in real trips get value.
for(i in seq(along = devices )){
  z <- max(x)    #highest current flight_id number
  x <- unlist(lst[[i]]) + z   #add z to vector of current device trip numbers
  d <- devices[i]       #device id
  all.points[gps$flight_class_2 != 0 & (is.na(gps$flight_class_2) != TRUE) & gps$device_info_serial == d] <- x    #add vector x to the vector of all gps points
}

#then add the vector 'all.points' to the 'gps' dataframe.
gps$flight_id <- all.points




#now we have labelled all gps points with unique flight numbers we should now calculate various summary information for each flight.
#This could either be done in the same script, or we could first add the flight number and flight status columns to the 'cal_mov_paramaters' DB table, then make a new script which queries this table and calculates flight information, then outputs this data to a new db table specifically for flight information.
#To avoid over long and complicated scripts it is probably best to break this down, so first output the newly calculated column to the db, then start a new script for the flight calculations.


# names(gps)
# 
# gps$flight_class_2[1:100]
# gps$trip_id[1:100]
# gps$flight_id[1:100]

#output the new data columns to the 'cal_mov_paramaters' database table
#add neccessary columns to db table first. Here 'flight_class' and 'flight_id', which are both integers.
#put it all together in data_frame, with device_info_serial and date_time for primary keys.
export_table <- as.data.frame(cbind(
  gps$device_info_serial, gps$date_time, gps$flight_class_2,
  gps$flight_id))
#give names for columns
names(export_table) <- c("device_info_serial","date_time","flight_class","flight_id")
#add date_time to dataframe, somehow datetime loses its class in the above opperation - this is a workaround.
export_table$date_time <- gps$date_time
#export these calculated values to the database, updating existing 'cal_mov_paramaters' table.
#first added the three new columns to the table useing access
sqlUpdate(gps.db, export_table, tablename = "cal_mov_paramaters",index = c("device_info_serial","date_time"),fast=TRUE)


# names(export_table)
# 
# odbcCloseAll()
# 
# ?RODBC