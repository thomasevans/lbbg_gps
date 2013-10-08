#' Pilot project on GPS tagging of guillemots
#' Here some code to analyse, summarise, visualise this data.


#' Load in GPS data for both birds (combined?)
#' 

# Set working direction to guillemot data directory
setwd("D:/Dropbox/Guillemots/analysis")

# Read in data
gps <- read.csv("gps_data_1500_1502.csv",na.strings = "\\N", header = TRUE)

gps$date_time <- as.POSIXct(gps$date_time,
           tz="GMT",
           format="%Y-%m-%d %H:%M:%S")

start_1502 <- as.POSIXct("2013-06-17 12:30",
                         tz="GMT",
                         format="%Y-%m-%d %H:%M")
start_1500 <- as.POSIXct("2013-06-30 16:06",
                         tz="GMT",
                         format="%Y-%m-%d %H:%M")

#label deployment periods
deployment <- function(dev, dat){
        if(dev == 1500){if(dat > start_1500) return(TRUE) else return(FALSE)} else {if(dev == 1502){if(dat > start_1502) return(TRUE) else return(FALSE)} else return(NA)}
}

dep.time <- mapply(deployment,gps$device_info_serial,dat = gps$date_time)
# summary(dep.time)


# str(gps)
#' 
#' Do some summary analyses (distance from nest, inst speed etc)
#' 

library(fossil)

nest.long <- 17.9584114

nest.lat  <- 57.2896727  


#calculate grand circle distance from nest for each GPS location             
gc_dist <- deg.dist(gps$longitude, gps$latitude, nest.long, nest.lat)



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





#' 
#' Histogram of flights to determine speed cut off for flight (or
#' use value from previous paper)
#' 
hist(inst_ground_speed[inst_ground_speed < 30  & dep.time == TRUE], ylim = c(0,100))
hist(calculated_speed[(calculated_speed < 25) & (calculated_speed > 5) & dep.time == TRUE], ylim = c(0,100))
hist(inst_ground_speed[(inst_ground_speed < 25) & (inst_ground_speed > 5) & dep.time == TRUE], ylim = c(0,100))
#' Previous paper used 5 ms-1, based on this dataset that looks ok.

library('maptools')

#using code from http://stackoverflow.com/a/11185378/1172358
trackAngle <- function(xy) {
  angles <- abs(c(trackAzimuth(xy), 0) -
                  c(0, rev(trackAzimuth(xy[nrow(xy):1, ]))))
  angles <- ifelse(angles > 180, 360 - angles, angles)
  angles[is.na(angles)] <- 180
  angles[-c(1, length(angles))]
}


turning_angle <- c(NA,trackAngle(cbind(gps$longitude,gps$latitude)),NA)


# Put calculated variables into a dataframe:
gps_var <- as.data.frame(
  cbind(
    gps$device_info_serial,
    gps$date_time,  calculated_speed,
    gc_dist,dep.time
    ,inst_ground_speed, p2p_dist,
    time_interval_s,turning_angle))

gps_var_names <- c("device_info_serial", "date_time", 
                         "calculated_speed", 
                         "nest_gc_dist", "deploy",
                         "inst_ground_speed", "p2p_dist",
                         "time_interval_s","turning_angle")
names(gps_var) <- gps_var_names

#' 
#' Label flights
#' 
#use code from flight_paramaters

#label flight points
gps_var$flight_class <- ifelse(gps_var$inst_ground_speed > 5, 1, 0)



# Give flights unique id ####
# We want to label flights with a unique id
# First we make some vectors of next, previous point etc,
# to find start and end points of flights.

flight1 <- gps_var$flight_class + 1

#make vector of next point value
flight2 <- 
  (2* c(gps_var$flight_class[2:length(gps_var$flight_class)], 0)) + 1

#make vector of prev point value
flight3 <- 
  (3* c(0,gps_var$flight_class[1:(length(gps_var$flight_class)-1)])) + 1



# Label by type of point: 0 - not flight, 1 - start, 2 - end, 3 - flight
# Product of above three vectors, produces unique values for each
# possible point type.
gps_var$flight_class_2 <- flight1 * flight2 * flight3

#unique(gps$flight_class_2)
#summary(as.factor(gps$flight_class_2))  #inspect this

# Keep a copy of above calculation
fly_type <- gps_var$flight_class_2        


# Reduce to the four possibilties 
# (not flight, flight, or start or end points of flight).
# 0 - not flight, 1 - start, 2 - end, 3 - flight
gps_var$flight_class_2[ fly_type == 1 ]            <- 0
gps_var$flight_class_2[ fly_type == 3 ]            <- 1
gps_var$flight_class_2[ (fly_type == 2) | (fly_type == 6)
                    | (fly_type == 24)| (fly_type == 8)
                    | (fly_type == 12)]     <- 3
gps_var$flight_class_2[ fly_type == 4 ]       <- 2


# Number flights ####
# Make column for flight id, start with value 0, which
# will be null value - i.e. not a trip (points at the nest)
gps_var$flight_id <- 0

n <- length(gps_var$calculated_speed)
y <- 0
x <- 0

for(i in 1:n){
  if(is.na(gps_var$flight_class_2[i])){gps_var$flight_id[i] <- NA}else{
    if(gps_var$flight_class_2[i] != 0){
    # If start of a flight, increment x by one
  if(gps_var$flight_class_2[i] == 1) x <- x+1      
      # Allocated value of x for flight_id for position 'i'.
  gps_var$flight_id[i] <- x}}  
}



#produce a vector of flight numbers##############
flight_id <- sort(unique(gps_var$flight_id))

f <- length(flight_id)

#remove zero (i.e. non flight points)
flight_id <- flight_id[2:f]



# f <- 5
#t - flight id
#gps table
flight.info <- function(f, gps=gps, gps_var=gps_var){
  
  library(fossil)   #required for distance calculations
  library(circular) #required for some circular calculations
  #make a subset of 'gps' containing just data for flight, t.
  #   install.packages("circular")
  
  nest.long <- 17.9584114
  
  nest.lat  <- 57.2896727  
  
  
  sub01 <- subset(gps, gps_var$flight_id == f)
  sub02 <- subset(gps_var, flight_id == f)
  
  dev <- sub01$device_info_serial[1]  
  dep <- sub02$deploy[1]
  
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
  dist_max  <-  max(sub02$nest_gc_dist)
  
  # Total distance travelled, exclude first point
  # p2p distance, as this includes distance to point
  # before flight 'started'.
  dist_total <- sum(sub02$p2p_dist[2:n])
  
  # Mean log interval.
  interval_mean <- mean(sub02$time_interval_s) 
  
  # Min log interval.
  # This may be useful for highlighting flights
  # where high resolution GPS data is available
  # (i.e. where the conditional log mode was used).
  # It might make more sense to floor or round this
  # value.
  interval_min  <- min(sub02$time_interval_s)
  
  # Device info serial
  device_info_serial <- sub02$device_info_serial[1]
  
  
  # Location at start, end, and for nest.
  start_long   <-  sub01$longitude[1]
  start_lat   <-   sub01$latitude[1]
  end_long    <-   sub01$longitude[n]
  end_lat    <-    sub01$latitude[n]
  
  # Distance from nest at start of flight.
  dist_nest_start    <-   1000 * 
    deg.dist(nest.long, nest.lat, start_long, start_lat)
  
  # Distance from nest at end of flight.
  dist_nest_end      <-   1000 * 
    deg.dist(nest.long, nest.lat, end_long, end_lat)
  
  
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
  
  speed_inst_mean <-   mean(sub02$inst_ground_speed
                            ,
                            na.rm = TRUE) 
  
  # Median of instantaneous speed.
  speed_inst_med <-   median(sub02$inst_ground_speed
                             ,
                             na.rm = TRUE)
  
  # Variance of instaneous speed.
  speed_inst_var <-   var(sub02$inst_ground_speed
                          ,
                          na.rm = TRUE)
  
  #Altitude, max, mean, median ####
  alt_max    <- max(sub01$altitude
                    ,
                    na.rm = TRUE)
  
  alt_min    <- min(median(sub01$altitude
                           ,
                           na.rm = TRUE))
  
  alt_mean   <- mean(median(sub01$altitude
                            ,
                            na.rm = TRUE))
  
  alt_med    <- median(sub01$altitude
                       ,
                       na.rm = TRUE)
  summary(as.factor(sub02$flight_class))
  # 'circular' package functions#############

  
  #make a vector containing all this data
  data.out <- c(dev,f, n,dep, start_time, end_time, duration,
                dist_max, dist_total, interval_mean,
                interval_min, device_info_serial,
                start_long, start_lat, end_long,
                end_lat, dist_nest_start, dist_nest_end,
                dist_nest_dif, dist_a_b, straigtness,
                bearing_a_b, speed_a_b, speed_inst_mean,
                speed_inst_med, speed_inst_var, alt_max,
                alt_min, alt_mean, alt_med)  
  
  
  # Output a vector for the bird of flight id
  return(data.out)  
  
  # End function
}
#**********End of this function: flight.info


lst <- list()
lst[1] <- NA
x <- NA
# i <- 5


for(i in seq(along = flight_id )){
  
  #calculate the trip numbers for the device i. i.e. the function 
  #which we wish to run for each device.     
  x <- flight.info(flight_id[i],gps,gps_var)
  x <- t(x)
  
  #output data as list (this will be appended to the global list, lst.
  lst[i] <- list(x)  
  x <- NA
} #end of for loop



names.flights <- c("device_info_serial", "flight_id",
                   "points","deploy",
                   "start_time",
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
                   "alt_min", "alt_mean", "alt_med"
                   )

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


#' 
#' 
#' Summary statistics for flights
#' 
#' Tabulate and graph data on flights
#' 
flights.old <- flights
flights <-   subset(flights,flight_id != 64)


str(flights)
f_1500 <- flights$device_info_serial == 1500 & flights$deploy == 1
plot(flights$start_time[f_1500],flights$duration[f_1500]/60,ylab="Minutes in flight")
hist(flights$duration[f_1500]/60)

f_1502 <- flights$device_info_serial == 1502 & flights$deploy == 1
plot(flights$start_time[f_1502],flights$duration[f_1502]/60, ylim = c(0,100), ylab="Minutes in flight")
hist(flights$duration[f_1502]/60)
#' 
#' Map foraging trips for both birds sepprately
#' Perhaps also combined, and zooming in on some areas of interest?

write.csv(gps_var, file = 'gps_var.csv')



f_1502_gps <- gps$device_info_serial == 1502

col.fun <- function(x){if(is.na(x)) return ("grey") else{
  if(x==1) return("red") else return("black")}}

p.col <- sapply(gps_var$flight_class[f_1502_gps],col.fun)
plot(gps$long[f_1502_gps],gps$lat[f_1502_gps], col= p.col)



f_1500_gps <- gps$device_info_serial == 1500

col.fun <- function(x){if(is.na(x)) return ("grey") else{
  if(x==1) return("red") else return("blue")}}

p.col <- sapply(gps_var$flight_class[f_1500_gps],col.fun)
plot(gps$long[f_1500_gps],gps$lat[f_1500_gps], col= p.col)


library(maps)
load("SWE_adm0.RData")


p.col <- sapply(gps_var$flight_class[f_1500_gps],col.fun)
plot(gadm,xlim = range(gps$long[f_1500_gps]), ylim=range(gps$lat[f_1500_gps]), col="dark grey", bg = "white")
points(gps$long[f_1500_gps],gps$lat[f_1500_gps], col= p.col)
n <- length(gps$long[f_1500_gps])
segments(gps$long[f_1500_gps][-1], gps$lat[f_1500_gps][-1],
         gps$long[f_1500_gps][1:n-1],gps$lat[f_1500_gps][1:n-1],
         col = "black", lwd = 1.5)
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)


p.col <- sapply(gps_var$flight_class[f_1502_gps],col.fun)
plot(gadm,xlim = range(gps$long[f_1502_gps]), ylim=range(gps$lat[f_1502_gps]), col="dark grey", bg = "white")
n <- length(gps$long[f_1502_gps])
points(gps$long[f_1502_gps],gps$lat[f_1502_gps], col= p.col)
segments(gps$long[f_1502_gps][-1], gps$lat[f_1502_gps][-1],
         gps$long[f_1502_gps][1:n-1],gps$lat[f_1502_gps][1:n-1],
         col = "black", lwd = 1.5)
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)




#zoom in on one area of bird 1500
p.col <- sapply(gps_var$flight_class[f_1500_gps],col.fun)
plot(gadm,xlim = c(16.9,17.3), ylim=c(57.58,57.75), col="dark grey", bg = "white")
points(gps$long[f_1500_gps],gps$lat[f_1500_gps], col= p.col)
n <- length(gps$long[f_1500_gps])
segments(gps$long[f_1500_gps][-1], gps$lat[f_1500_gps][-1],
         gps$long[f_1500_gps][1:n-1],gps$lat[f_1500_gps][1:n-1],
         col = "black", lwd = 1.5)
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)




#zoom in on part of 1502
p.col <- sapply(gps_var$flight_class[f_1502_gps],col.fun)
plot(gadm,xlim = c(17.2,17.4), ylim=c(57.40,57.50), col="dark grey", bg = "white")
n <- length(gps$long[f_1502_gps])
points(gps$long[f_1502_gps],gps$lat[f_1502_gps], col= p.col)
segments(gps$long[f_1502_gps][-1], gps$lat[f_1502_gps][-1],
         gps$long[f_1502_gps][1:n-1],gps$lat[f_1502_gps][1:n-1],
         col = "black", lwd = 1.5)
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)
