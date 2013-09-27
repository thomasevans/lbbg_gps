# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

#NB Some of following code is copied/ edited from a script by Michael Kemp for a course held in Amsterdam in 2010

# Description ##############
# Get GPS data from Amsterdam database. Then calculate various
# paramaters for each GPS location, such as turning angle,
# speed, etc.
# Data is then exported to an Access database.


# Libraries and database ################
#To link to database
library(RODBC)

#For spatial functions
library(fossil)

#More spatial functions
library(maptools)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# ?odbcConnectAccess2007
#See what tables are available
# sqlTables(gps.db)


#for all of data_base, except pre-deployment and null records
#excluding individuals with start north of 59 (i.e. those birds
#from Fågelsundet)
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
  FROM gps_uva_tracking_speed_3d_limited AS g, gps_uva_track_session_limited AS t, gps_uva_individual_limited as i
  WHERE g.device_info_serial = t.device_info_serial
    AND t.ring_number = i.ring_number
    AND i.species = 'Larus fuscus'
    AND g.date_time >= t. start_date
    AND t.start_latitude < 59 
    AND g.latitude IS NOT NULL
  ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)

# To get date_time into date-time object
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")


#check structure of resulting dataframe
#str(gps)

#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number,
      n.nest_id, n.latitude, n.longitude, t.device_info_serial
      FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t,  gps_uva_individual_limited as i
      WHERE n.ring_number = t.ring_number
      AND t.ring_number = i.ring_number
      AND i.species = 'Larus fuscus'
      ORDER BY n.ring_number ASC;")

#list available devices###########
#Not used.
#gps_devices <- sort(unique(nest_loc$device_info_serial))


#Nest location #############
#function to produce two vectors of latitude and longitude positions
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}

#for each gps point lookup the nest location  
nest_pos <- sapply(gps$device_info_serial, lookup_nest)

#to transpose this generated matrix, so that dimentions are correct
nest_pos <- t(nest_pos)
nest_pos <- as.data.frame(nest_pos)
colnames(nest_pos) <- c("lat","long")


#calculate grand circle distance from nest for each GPS location             
gc_dist <- deg.dist(gps$longitude, gps$latitude, nest_pos$long, nest_pos$lat)

#calculating distance between consecutive points#####
lat.next  <- gps$latitude[-1]
long.next <- gps$longitude[-1]

p2p_dist <- deg.dist(gps$longitude[-length(gps$latitude)], gps$latitude[-length(gps$latitude)], long.next, lat.next)*1000
p2p_dist<- c(0,p2p_dist)  #adding a line at the beggining with 0 for first point, and so producing vector with correct length.
#NB this will calculate distance between all consecutive points,
#irrespective of whether these are from different birds.


#calculate time between fixes in seconds############
time_interval_s <- 
  as.numeric(difftime(gps$date_time[2:length(gps$date_time)],
                      gps$date_time[(1:length(gps$date_time)-1)],
                      units="s"))

#Add 0 at beggining for first point.
time_interval_s <- c(0,time_interval_s)


# Speed calculations ##############
# Caculate speed based on time and distance from last fix
# i.e. not instantaneous gound speed, but calculated speed
calculated_speed  <- p2p_dist/time_interval_s

# Calculate instaneous ground_speed recorded by GPS using the
# veast and vnorth speeds from the GPS (Pythagoras theorem)
#
# **2013-09-21 corrected this to noth and east movment,
# not x and y, as these are different - earth coordinates
# rather than perpendicular to geoid (which we require here.)

inst_ground_speed <- sqrt((gps$veast*gps$veast) 
                          + (gps$vnorth*gps$vnorth))                       



# Differance between instaneous speed and calculated speed.
# Indicates turning, if turning a lot (e.g. soaring in a thermal)
# instaneous speed >> calculated speed.
diff_speed <- (inst_ground_speed - calculated_speed)

# To view this
hist(diff_speed[diff_speed < 12 & diff_speed > -7])

# Angles/ bearing calculations #######

# ?earth.bear

#bearing to next point
bearing_next <- c(earth.bear(
  gps$longitude[-length(gps$longitude)]
 ,gps$latitude[-length(gps$longitude)]
 ,gps$longitude[-1],
  gps$latitude[-1]), 0)


# bearing_next[1:10]

#bearing from previous point
bearing_prev <- c(0, earth.bear(
  gps$longitude[-1], gps$latitude[-1]
  ,gps$longitude[-length(gps$longitude)]
  ,gps$latitude[-length(gps$longitude)]))
# bearing_prev[1:100]

# a <- c(1,2,3,4)
# a[-1]
# a[-length(a)]

#turning angle
# turning_angle <- 
#   c(0, (abs(
#           bearing_next[-c(1,length(bearing_next))]
#          - bearing_next[-c(length(bearing_next)-1,
#          length(bearing_next))])
#   %%180), 0)

#install.packages('maptools')



#using code from http://stackoverflow.com/a/11185378/1172358
trackAngle <- function(xy) {
  angles <- abs(c(trackAzimuth(xy), 0) -
                  c(0, rev(trackAzimuth(xy[nrow(xy):1, ]))))
  angles <- ifelse(angles > 180, 360 - angles, angles)
  angles[is.na(angles)] <- 180
  angles[-c(1, length(angles))]
}

# ?trackAzimuth 

# long <- c(15,20,25,25,25)
# lat  <- c(50,51,52,52,52)
# pos <- cbind(long,lat)

turning_angle <- c(NA,trackAngle(cbind(gps$longitude,gps$latitude)),NA)

# hist(turning_angle)

# Turning angle is scale dependent, for with long time intervals larger
# turning angles must be expected than those for small time intervals.
# Here we compare turning angle frequency according to intervals <500s
# (primarily 100 s and below), and >500 s (primarily 600s).
# There a small peak of turning angles near 180 for over 500s,
# this may be pseudo pattern, see:
# Hurford A (2009) GPS Measurement Error Gives Rise to Spurious 180 Turning Angles and Strong Directional Biases in Animal Movement Data. PLoS ONE 4:e5632.

# u500s<- time_interval_s <500
# o500s<- time_interval_s >500
# par(mfrow=c(2,1))
# hist(turning_angle[u500s],xlim=c(0,180),breaks=20)
# hist(turning_angle[o500s],xlim=c(0,180),breaks=20)

#for 'flight' fixes only
#this gives quite different results, and suggests the importance of excluding non flight data when looking at this.
# u500s<- time_interval_s <500 & calculated_speed >5
# o500s<- time_interval_s >500 & calculated_speed >5
# par(mfrow=c(2,1))
# hist(turning_angle[u500s],xlim=c(0,180),breaks=20)
# hist(turning_angle[o500s],xlim=c(0,180),breaks=20)
# length(turning_angle[o500s])
# hist(time_interval_s[time_interval_s < 1000],xlim=c(0,800),breaks=20)
# summary(time_interval_s)
# #time_interval_s[1:100]
# plot(turning_angle[u500s]~calculated_speed[u500s])


#see: Awkerman JA, Fukuda A, Higuchi H, Anderson DJ (2005) Foraging activity and submesoscale habitat use of waved albatrosses Phoebastria irrorata during chick-brooding period. Marine Ecology Progress Series 291:289-300.
#they have a graph of turning angle for waved albatross - though for far fewer foraging trips (though of course these are generally of greater duration and distance)


#bearing from nest, uses Haversine formula
nest_bear <- earth.bear(gps$longitude,gps$latitude,nest_pos$long,nest_pos$lat)
#Corrected above on 2013-08-28

#replace first values for each bird with zero, where calculations were made between birds
#Not done

#make a list of devices and put this in assending order
devices <- sort(unique(gps$device_info_serial))

#for each device present in data_set get the value for the minimum
#date_time value (i.e. the first record for that bird)
index <- seq(along = gps$device_info_serial)

x <- NA  #initialise vairable to be used within loop


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


# Export to database ######
# Put it all together in data_frame, with device_info_serial and
# date_time for primary keys. Then export direct to access. 
export_table <- as.data.frame(
  cbind(
    gps$device_info_serial,
    gps$date_time, bearing_next,
    bearing_prev, calculated_speed
    ,diff_speed, gc_dist,nest_bear
    ,inst_ground_speed, p2p_dist,
    time_interval_s,turning_angle))

names(export_table) <- c("device_info_serial", "date_time", 
                         "bearing_next", "bearing_prev", 
                         "calculated_speed", "diff_speed",
                         "nest_gc_dist", "nest_bear",
                         "inst_ground_speed", "p2p_dist",
                         "time_interval_s","turning_angle")

export_table$date_time <- gps$date_time
#str(export_table)
#export these calculated values to the database
#will be neccessary to edit table in Access after to define data-types and primary keys

sqlSave(gps.db, export_table, tablename = "lund_gps_parameters",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date"))

# After exporting, neccessary to open table in Access and specify which columns are primary keys (date_time and device_info_serial - i.e. a combined primary key)

message("**After exporting, neccessary to open table in Access and specify which columns are primary keys (date_time and device_info_serial - i.e. a combined primary key)!!")

