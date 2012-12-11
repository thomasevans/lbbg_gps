#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

#NB Some of following code is copied/ edited from a script by Michael Kemp for a course held in Amsterdam in 2010

#Set the work directory
setwd("F:/Documents/Work/GPS_DB")

#To link to database
library(RODBC)

#to get spatial functions
library(fossil)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('GPS_db.accdb')


#See what tables are available
sqlTables(gps.db)

#Query the gull db to get gps data
#For testing, start with 15 days data from two birds
#gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
#  FROM gps_uva_tracking_limited AS g, gps_uva_track_session_limited AS t
#  WHERE g.device_info_serial = t.device_info_serial
#    AND g.date_time >= t.start_date
#    AND g.latitude IS NOT NULL
#    AND (g.device_info_serial = 602 OR g.device_info_serial = 603)
#    AND g.date_time > #2012-06-15#
#    AND g.date_time < #2012-07-01#
#  ORDER BY g.device_info_serial ASC, g.date_time ASC ;")

#for all of data_base, except pre-deployment and null records
#excluding individuals with start north of 59 (i.e. those birds from F?gelsundet)
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
  FROM gps_uva_tracking_limited AS g, gps_uva_track_session_limited AS t
  WHERE g.device_info_serial = t.device_info_serial
    AND g.date_time >= t.start_date
    AND g.latitude IS NOT NULL
    AND t.start_latitude < 59 
  ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)

gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")



#check structure of resulting dataframe
#str(gps)

#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number, n.nest_id, n.latitude, n.longitude, t.device_info_serial
FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t
WHERE n.ring_number = t.ring_number
ORDER BY n.ring_number ASC;")

#make a list of available devices
gps_devices <- sort(unique(nest_loc$device_info_serial))
#make data.frame
#gps_calculations <- data.frame(device_info_serial,date_time,gc_dist)
#probably better to make these as three vectors, then combine into dataframe and output


#go through gps data, place device_info_serial, date_time in table and then calculate gc_dist, and other variables??
#new.device_info_serial <- gps$device_info_serial
#new.date_time   <- gps$date_time

#load in some functions to calculate info about points
#source('F:/Documents/Work/R_functions_scripts/pt2pt_fxns.R')

#use tapply, sapply etc to apply a function to all which get nest long and lat for each gps point
#for each gps point need to lookup nest_lat, based on what device_id is, then 


#function to produce two vectors of latitude and longitude positions
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}
#sort(unique(gps$device_info_serial))
#device_info <- 294
#lookup_nest(603)

#for each gps point lookup the nest location  
nest_pos <- sapply(gps$device_info_serial, lookup_nest)

#to transpose this generated matrix, so that dimentions are correct
nest_pos <- t(nest_pos)
nest_pos <- as.data.frame(nest_pos)
colnames(nest_pos) <- c("lat","long")

#calculate grand circle distance from nest for each GPS location             
gc_dist <- deg.dist(gps$latitude,gps$longitude,nest_pos$lat,nest_pos$long)


hist(gc_dist)
nest_pos[1:100]



#calculating distance between consequative points
lat.next <- gps$latitude[-1]
long.next <- gps$longitude[-1]
p2p_dist <- deg.dist(gps$latitude[-length(gps$latitude)],gps$longitude[-length(gps$latitude)],lat.next,long.next)*1000
p2p_dist<- c(0,p2p_dist)  #adding a line at the beggining with 0 for first point, and so producing vector with correct length.
#NB this will calculate distance between all consequtive points, irrespective if these are from different birds.


#calculate time between fixes in seconds
time_interval_s <- as.numeric(difftime(gps$date_time[2:length(gps$date_time)],gps$date_time[(1:length(gps$date_time)-1)],units="s"))
time_interval_s <- c(0,time_interval_s)

#caculate speed based on time distance from last fix - i.e. not instantaneous gound speed
calculated_speed <- p2p_dist/time_interval_s

#calculate instaneous ground_speed recorded by GPS using the X and Y speeds from the GPS (Pythagoras theorem)
inst_ground_speed <- sqrt((gps$x_speed*gps$x_speed) + (gps$y_speed*gps$y_speed))                       

#have a look at the data
par(mfrow=c(2,1))
hist(inst_ground_speed,breaks=40)
hist(calculated_speed,breaks=40)

#differance between instaneous speed and calculated speed. Indicates turning, if turning a lot (e.g. soaring in a thermal) instaneous speed >> calculated speed.
diff_speed <- (inst_ground_speed-calculated_speed)


#bearing to next point
bearing_next <- c(earth.bear(gps$longitude[-length(gps$longitude)],gps$latitude[-length(gps$longitude)],gps$longitude[-1],gps$latitude[-1]),0)
#bearing_next[1:10]

#bearing from previous point
bearing_prev <- c(0,earth.bear(gps$longitude[-1],gps$latitude[-1],gps$longitude[-length(gps$longitude)],gps$latitude[-length(gps$longitude)]))
#bearing_prev[1:100]

#turning angle
turning_angle <- c(0,(abs(bearing_next[-c(1,length(bearing_next))] - bearing_next[-c(length(bearing_next)-1,length(bearing_next))]) %%180),0)

par(mfrow=c(2, 2))
hist(turning_angle)
hist(inst_ground_speed)
plot(turning_angle~inst_ground_speed)

plot(turning_angle~calculated_speed)

par(mfrow=c(1,1))
plot(turning_angle~time_interval_s,xlim=c(0,1500))

#turning angle is scale dependent, for with long time intervals larger turning angles must be expected than those for small time intervals. Here we compare turning angle frequency according to intervals <500s (primarily 100 s and below), and >500 s (primarily 600s).
#There a small peak of turning angles near 180 for over 500s, this may be pseudo pattern, see:
#Hurford A (2009) GPS Measurement Error Gives Rise to Spurious 180? Turning Angles and Strong Directional Biases in Animal Movement Data. PLoS ONE 4:e5632.

u500s<- time_interval_s <500
o500s<- time_interval_s >500
par(mfrow=c(2,1))
hist(turning_angle[u500s],xlim=c(0,180),breaks=20)
hist(turning_angle[o500s],xlim=c(0,180),breaks=20)

#for 'flight' fixes only
#this gives quite different results, and suggests the importance of excluding non flight data when looking at this.
u500s<- time_interval_s <500 & calculated_speed >5
o500s<- time_interval_s >500 & calculated_speed >5
par(mfrow=c(2,1))
hist(turning_angle[u500s],xlim=c(0,180),breaks=20)
hist(turning_angle[o500s],xlim=c(0,180),breaks=20)
length(turning_angle[o500s])
hist(time_interval_s[time_interval_s < 1000],xlim=c(0,800),breaks=20)
summary(time_interval_s)
#time_interval_s[1:100]
plot(turning_angle[u500s]~calculated_speed[u500s])


#see: Awkerman JA, Fukuda A, Higuchi H, Anderson DJ (2005) Foraging activity and submesoscale habitat use of waved albatrosses Phoebastria irrorata during chick-brooding period. Marine Ecology Progress Series 291:289-300.
#they have a graph of turning angle for waved albatross - though for far fewer foraging trips (though of course these are generally of greater duration and distance)

#bearing from nest, uses Haversine formula
nest_bear <- earth.bear(gps$latitude,gps$longitude,nest_pos$lat,nest_pos$long)
#hist(nest_bear)



#replace first values for each bird with zero, where calculations were made between birds
#need to put this in here
x <- 1  #for some reason seems neccessary to declare this vairable

#make a list of devices and put this in assending order
devices <- sort(unique(gps$device_info_serial))

#for each device present in data_set get the value for the minimum date_time value (i.e. the first record for that bird)
index <- seq(along = gps$device_info_serial)

for(i in seq(along = devices )){
  x[i] <- min(gps$date_time[gps$device_info_serial==devices[i]])
  r <- gps$device_info_serial == devices[i] & gps$date_time == x[i]
  ind <- index[r]
  bearing_prev[ind] <- calculated_speed[ind] <- p2p_dist[ind] <- time_interval_s[ind] <- turning_angle[c(ind, ind + 1)] <- 0
  #and now for final record
  x[i] <- max(gps$date_time[gps$device_info_serial==devices[i]])
  r <- gps$device_info_serial == devices[i] & gps$date_time == x[i]
  ind <- index[r]
  bearing_next[ind] <- turning_angle[c(ind,ind - 1)] <- 0
}



#put it all together in data_frame, with device_info_serial and date_time for primary keys. They either export direct to access, or to CSV etc, and 
export_table <- as.data.frame(cbind(gps$device_info_serial,gps$date_time,bearing_next,bearing_prev,calculated_speed,diff_speed,gc_dist,nest_bear,inst_ground_speed,p2p_dist,time_interval_s,turning_angle))
names(export_table) <- c("device_info_serial","date_time","bearing_next","bearing_prev","calculated_speed","diff_speed","nest_gc_dist","nest_bear","inst_ground_speed","p2p_dist","time_interval_s","turning_angle")
export_table$date_time <- gps$date_time
#str(export_table)
#export these calculated values to the database
#will be neccessary to edit table in Access after to define data-types and primary keys
sqlSave(gps.db, export_table, tablename = "cal_mov_paramaters", append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,varTypes=c(date_time="Date"))

