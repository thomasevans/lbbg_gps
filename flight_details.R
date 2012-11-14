#'A script to process data on individual flights.
#'It will summarise flights by distance travelled, altitude, speed etc.
#'They will also be labelled by foraging trip and device_info_serial.
#'The flights should be numbered for each foraging trip. Perhaps also labelling the first and final flight of a foraging trip - which may contitute 'commuting' flight.



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
str(gps)

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")


#'First we workout a sensible cutoff value, beyond which gps points will be classified as flight. Clearly some flight points are below this, but this will hopfully include most points
hist(gps$inst_ground_speed[gps$inst_ground_speed < 30  & gps$inst_ground_speed > 1 ],breaks=200, xlim=c(0,30),main="Histogram of instantaneous recored speed",xlab="Instaneous ground speed (GPS) - m/s")
abline(v=3.5,lwd=3,lty=3)



#preserve a copy of original, before taking a subset for testing.
gps.original <- gps

#for testing purposes we only take the first x lines
gps <- gps[1:50000,]


#code to recognise flight, and adds this column, labelling with 0 if not flight and 1 if over 3.5 ms-1 (flight)
gps$flight_class <- ifelse(gps$inst_ground_speed > 3.5, 1,0)
#gps$flight_class <- as.factor(gps$flight_class)
#gps$flight_class <- as.numeric(gps$flight_class)

#'We want to label flights with a unique id
#first we make some vectors of next, previous point etc, to find start and end points of flights
flight1 <- gps$flight_class +1
#make vector of next point value
flight2 <- (2* c(gps$flight_class[2:length(gps$flight_class)],0))+1
#make vector of prev point value
flight3 <- (3* c(0,gps$flight_class[1:(length(gps$flight_class)-1)]))+1


#label by type of point: 0 - not flight, 1 - start, 2 - end, 3 - flight
gps$flight_class_2 <- flight1*flight2*flight3   #product of above three vectors, produces unique values for each possible point type.

summary(as.factor(gps$flight_class_2))  #inspect this


fly_type <- gps$flight_class_2        #keep a copy of above calculation

#Reduce to the four possibilties
gps$flight_class_2[(fly_type == 1)  ] <- 0
gps$flight_class_2[fly_type == 3 ] <- 1
gps$flight_class_2[(fly_type == 24) | (fly_type == 6) | (fly_type == 8) | (fly_type == 2) | (fly_type == 12)]<- 3
gps$flight_class_2[fly_type == 4] <- 2

#make column for trip id, start with value 0, which will be null value - i.e. not a trip (points at the nest)
gps$flight_id <- 0







#prototype function from trip details. Need to modify now for flights.

#***********start of function: flight.lab
#Function 'flight.lab' will produce a vector of flight number for each device, thus allowing the for loop to be ran in parallel (quicker).
#d - device_info_serial
#gps - the gps dataframe

#summary(as.factor(gps$device_info_serial))
#for testing
d <- 519

trip.lab <- function(d, gps=get("gps", envir=environment(trip.lab))){
  
  #first make a subset of 'gps' containing just data for device, d, away from the nest.
  sub01 <- subset(gps, flight_class_2 != 0 & device_info_serial == d,select=c(flight_class_2,flight_id))
  
  n <- length(sub01$flight_class_2) #the number of gps positions
  
  
  #a windows progress bar to monitor progress
  pb <- winProgressBar(title = paste( "progress bar for device",d), min = 0,max = n, width = 300)
  
  x <- 0   #x will keep note of trip number, we start at zero.
  #loop through all gps points, labelling them with trip id (x)
  for(i in 1:n){
    setWinProgressBar(pb, i, title=paste( round(i/n*100, 0),"% done for ", d)) #refresh the progress bar, so that we can keep note of progress.
    if(sub01$loc_type[i] == 1) x <- x+1      #if start of a trip, increment x by one
    sub01$trip_id[i] <- x                    #allocated value of x for trip_id for position 'i'.
  }
  
  close(pb)    #close the windows progress bar
  return(sub01$trip_id)            #output a vector for the bird of trip id
}
#**********End of this function: trip.lab




