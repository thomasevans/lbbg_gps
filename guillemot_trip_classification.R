# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# Classification of guillemot GPS data into foraging trips
# Will base this largely on the script used to do this for the LBBG data,
# adapting slightly the various thresholds etc.
# Purpose of this script is simply to identify foraging trips,
# Then get start-time, end-time, and number each trip with unique ID
# Summary statistics for each foraging trip will be extracted in a
# second sepperate script.


# DB function to extract data for each device_ID/ ring number? for deployment
# period.

# Will need to do sepprately for the IGU and UvA devices
# Get IGU GPS data

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get deployments table
deployments <- sqlQuery(gps.db,
                   query = "SELECT DISTINCT d.*
          FROM guillemots_track_session AS d
          ORDER BY d.device_info_serial ASC;",
                   as.is = TRUE)

# Get data into correct format
str(deployments)
deployments$device_info_serial <- as.factor(deployments$device_info_serial)
deployments$ring_number <- as.factor(deployments$ring_number)
deployments$start_date <- as.POSIXct(deployments$start_date, tz = "UTC")
deployments$end_date <- as.POSIXct(deployments$end_date, tz = "UTC")
deployments$successful_deployment <- as.factor(deployments$successful_deployment)



# Get UvA GPS data
# Get data for all devices attached to guillemots
# First get a list of device numbers from deployments table.
uva_dep_dev <- deployments$device_info_serial[
  deployments$device_type == "uva"]

igu_dep_dev <- deployments$device_info_serial[
  deployments$device_type == "igu"]

points_uva <- sqlQuery(gps.db,
                        query = paste("SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude, g.speed
          FROM gps_ee_tracking_speed_limited AS g
          WHERE g.device_info_serial IN (", paste(unique(uva_dep_dev), collapse = ", "),
          ") 
          ORDER BY g.device_info_serial ASC, g.date_time ASC ;", sep = ""),
                        as.is = TRUE)

str(points_uva)
points_uva$device_info_serial <- as.factor(points_uva$device_info_serial)
points_uva$date_time <- as.POSIXct(points_uva$date_time, tz = "UTC")



# Get IGU data along with point classification
points_igu <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude, c.coldist, c.diveprev, c.divenext, c.type, g.speed_ms
          FROM guillemots_gps_points_igu AS g, guillemots_gps_points_igu_class AS C
          WHERE g.device_info_serial = c.device_info_serial
          AND g.date_time = c.date_time
          ORDER BY g.device_info_serial ASC, g.date_time ASC ;",
                       as.is = TRUE)


# Fix data structure ----
str(points_igu)
points_igu$device_info_serial <- as.factor(points_igu$device_info_serial)
points_igu$date_time <- as.POSIXct(points_igu$date_time, tz = "UTC")
points_igu$type <- as.factor(points_igu$type)

names(points_igu)[9] <- "speed"

# Filter data by deployment status ------
# Filter data to only include data when devices are deployed.

hours_2 <- as.difftime(2, units = "hours")

# str(deployments)
# deployments$device_type <- as.factor(deployments$device_type)

# For IGU devices
dep_igu <- deployments[(deployments$device_type == "igu") &
                         deployments$successful_deployment == 1,]
n <- length(dep_igu[,1])

points_igu_f <- NULL
# i <- 4
for(i in 1:n){
  ring_number <- dep_igu$ring_number[i]
  t_start <- dep_igu$start_date[i] - hours_2
  t_end <- dep_igu$end_date[i] - hours_2
  device <- dep_igu$device_info_serial[i]
  
  f <- ((points_igu$device_info_serial == as.character(device)) &
    (points_igu$date_time > t_start) &
    (points_igu$date_time < t_end))
  # summary(f)
#   summary(points_igu$device_info_serial == as.character(device))
  
  
  points_s <- points_igu[f,]
  
  str(points_s)
  
  points_s <- cbind(points_s, ring_number)
  points_igu_f <- rbind(points_igu_f, points_s)
}



# For UVA devices
dep_uva <- deployments[(deployments$device_type == "uva") &
                         deployments$successful_deployment == 1,]

n <- length(dep_uva[,1])

points_uva_f <- NULL

for(i in 1:n){
  ring_number <- dep_uva$ring_number[i]
  t_start <- dep_uva$start_date[i] - hours_2
  t_end <- dep_uva$end_date[i] - hours_2
  device <- dep_uva$device_info_serial[i]
  
  f <- (points_uva$device_info_serial == as.character(device)) &
    (points_uva$date_time > t_start) &
    (points_uva$date_time < t_end)
  # summary(f)
  
  
  points_s <- points_uva[f,]
  
  str(points_s)
  
  points_s <- cbind(points_s, ring_number)
  points_uva_f <- rbind(points_uva_f, points_s)
}

# Add some empty vairables to UVA table to allow to be combined with
# data from IGU tags
str(points_uva_f)
str(points_igu_f)
coldist <- diveprev <- divenext <- type <- NA
points_uva_f <- cbind(points_uva_f, coldist , diveprev , divenext , type)

str(points_uva_f)
points_uva_f <- points_uva_f[,c(1:4,7:10,5,6)]

device_type <- "uva"
points_uva_f <- cbind(points_uva_f,device_type )

device_type <- "igu"
points_igu_f <- cbind(points_igu_f,device_type )


points_all <- rbind(points_igu_f,points_uva_f )

str(points_igu_f)



# Make some calculations on GPS data ------
# - distance from colony


# For each GPS position get distance from colony
# If GPS location is 0,0 give NA
fun.dist <- function(lat,long){
  # Define collony location
  lat.c  <- 57.289848
  long.c <- 17.958252
  
  # Get function to calculate distances
  source("deg.dist.R")
  
  if(lat == 0 | long == 0) {x <- NA} else {
    x <- deg.dist(long.c,lat.c, long,lat) 
  }
  return(x)
}

# ?difftime

# Get distance for each point
col.dist <- mapply(fun.dist,
                   points_all$latitude, points_all$longitude)

# Convert distance from km to m
col.dist <- col.dist * 1000

# Have a look at this data
hist(col.dist)



hist(col.dist[col.dist < 400], breaks = 50)



str(points_all)
# head(points_all$coldist)
points_all$coldist <- col.dist



# Output data for use in bathymetric analysis ------
# # Output all points
# save(points_all, file = "points_all.Rdata")
# 
# 
# # All surface points
# good <- points_all$type != "bad_location"
# na_good <- is.na(good)
# good[na_good] <- TRUE 
# # summary(good)
# p.surface <- ((good) &
#                 (col.dist > 300) &
#                 (points_all$speed < 5) &
#                 (!is.na(points_all$longitude)) &
#                 (!is.na(points_all$latitude)))
# surface_points <- points_all[p.surface,]
# surface_points <- surface_points[!is.na(surface_points$date_time),]
# save(surface_points, file = "surface_points.Rdata")
# 
# # All surface minus 5113
# good <- points_all$type != "bad_location"
# na_good <- is.na(good)
# good[na_good] <- TRUE 
# # summary(good)
# p.surface2 <- ((good) &
#                 (col.dist > 300) &
#                 (points_all$speed < 5) &
#                 (!is.na(points_all$longitude)) &
#                 (!is.na(points_all$latitude)) &
#                 points_all$device_info_serial != "5113")
# surface_points2 <- points_all[p.surface2,]
# surface_points2 <- surface_points2[!is.na(surface_points2$date_time),]
# save(surface_points2, file = "surface_points2.Rdata")
# 
# # All dive
# good <- points_all$type != "bad_location"
# na_good <- is.na(good)
# good[na_good] <- TRUE 
# dive <- points_all$diveprev == TRUE
# na_dive <- is.na(dive)
# dive[na_dive] <- FALSE 
# 
# 
# # summary(good)
# p.dive <- ((good) &
#                  dive &
#                (col.dist > 300) &
#                  (!is.na(points_all$longitude)) &
#                  (!is.na(points_all$latitude)))
# dive_points <- points_all[p.dive,]
# dive_points <- dive_points[!is.na(dive_points$date_time),]
# save(dive_points, file = "dive_points.Rdata")
# str(dive_points)



# If running from this point only, can load R binnary data file
load("guillemot_trip_classification_data.RData")

# Classify into foraging trips -------
# (see previous
# analysis for the LBBG in 'export_files.R' lines 75 onwards)

# Label points by on foraging trip or not
on_trip <- ifelse(points_all$coldist < 250, 0,1)

# For 'bad_points' also label as 'NA', as colony distance cannot be
# relied on.
x <- points_all$type == "bad_location"
x[is.na(x)] <- FALSE
on_trip[x] <- NA

# Index for 'bad_locations'
# Index for all location first
id <- c(1:length(points_all$type))

id.bad_location <- id[x]

# i <- 34

# For each bad_location do:
for(i in 1:length(id.bad_location)){
  ind.loc <- id.bad_location[i]
  
  # Get index for last vallid location (!== "bad_location" | is.na is TRUE)
  # Work backward from current location
  
  xb <- "bad_location"
  n <- ind.loc
  while((xb == "bad_location") | is.na(xb)| is.na(on_trip[n])){
    n <- n -1
    xb <- points_all$type[n]
    if(points_all$device_info_serial[ind.loc] !=
         points_all$device_info_serial[n]) break
  }
  
  last_vallid <- on_trip[n]
  
  # Get index for next vallid location (as above)
  # Work forward from current location
  
  xb <- "bad_location"
  n <- ind.loc
  while((xb == "bad_location") | is.na(xb) | is.na(on_trip[n])){
    n <- n +1
    xb <- points_all$type[n]
    if(points_all$device_info_serial[ind.loc] !=
         points_all$device_info_serial[n]) break
  }
  
  next_vallid <- on_trip[n]
  
  # If on a transition, assume that it's not on a trip
  if(next_vallid == last_vallid) {on_trip[ind.loc] <- next_vallid} else {
    on_trip[ind.loc] <- 0
  }
  
    
}




summary(as.factor(on_trip))

on_trip_f <- on_trip[!is.na(on_trip)]
summary(as.factor(on_trip_f))


# We want to label the positions for each trip with a unique trip id
# first we make some vectors of next, previous point etc, to find start
# and end points of trips
trip1 <- on_trip_f +1

#make vector of next point value
trip2 <- (2* c(on_trip_f[2:length(on_trip_f)],0))+1

#make vector of prev point value
trip3 <- (3* c(0,on_trip_f[1:(length(on_trip_f)-1)]))+1


#label by type of point: 0 - trip, 1 - start, 2 - end, 3 - nest
loc_type <- trip1*trip2*trip3   #product of above three vectors
loc_calc     <- loc_type        #keep a copy of above calculation

summary(as.factor(loc_type))
head(loc_calc)
summary(as.factor(loc_calc))


#label by type of point: 0 - nest, 1 - start, 2 - end, 3 - trip

#Reduce to the four possibilties
loc_type[(loc_type == 1)  ]  <- 0
loc_type[loc_type == 3 | (loc_type == 12)] <- 1
loc_type[(loc_type == 24) | (loc_type == 6) 
             | (loc_type == 8) | (loc_type == 2)]<- 3
loc_type[loc_type == 4] <- 2
summary(as.factor(loc_type))

trip_id <- rep(0,length(loc_type))


# Loop through all pints (except for NAs)
#x will keep note of trip number, we start at zero.
x <- 0
n <- length(loc_type)
for(i in 1:n){
  if(loc_type[i] != 0){
    #if start of a trip, increment x by one
    if(loc_type[i] == 1) x <- x+1 
    
    #allocated value of x for trip_id for position 'i'.
    trip_id[i] <- x    
  }
}

summary(trip_id)

# Give trip_id for all points (including invallid and dives)
trip_id_all <- rep(9999,length(on_trip))
trip_id_all[!is.na(on_trip)] <- trip_id
summary(as.factor(trip_id_all))
head(trip_id_all)

n <- length(trip_id_all)
x <- 0
for(i in 1:n){
  if(trip_id_all[i] == 9999)  trip_id_all[i] <- x else{
    x <- trip_id_all[i]
  }
}

summary(trip_id_all == 9999)

# 124 'trips', not all neccessarilly foraging trips, will
# probably need to filter more.
max(trip_id_all)

# Assemble table of foraging trips
# - trip_id
# - start_time
# - end_time
# - device_id

length(trip_id_all)
length(col.dist)
length(points_all$date_time)

# Distribution of 'trip' start dates
hist(points_all$date_time[loc_type == 1], breaks = "day",
     freq = TRUE)
# ?hist
# length(loc_calc)
# Assemble table of GPS point info
test <- cbind(trip_id_all, col.dist,
              as.character(points_all$date_time),
              as.character(points_all$type))

names(points_all)


# Output details to DB
# Trip classification
# Also output combined table of GPS points (for easier analysis)





# Figures for Stockholm meeting ------
# August 2014

# Histogram of point speeds
hist(points_all$speed, xlim = c(0,40),
     ylim = c(0,1000), breaks = 100,
     col = "grey40",
     xlab = "Speed (ms-1)",
     main = "",
     cex.lab = 1.6,
     cex.axis = 1.4)

# Histogram of distances from colony centre
hist(col.dist/1000,
     ylim = c(0,1000), breaks = 50,
     col = "grey40",
     xlab = "Distance from colony (km)",
     main = "",
     cex.lab = 1.6,
     cex.axis = 1.4)

hist(col.dist[col.dist < 500],
     ylim = c(0,300), breaks = 50,
     col = "grey40",
     xlab = "Distance from colony (m)",
     main = "",
     cex.lab = 1.6,
     cex.axis = 1.4)


hist(col.dist[col.dist < 2000],
     ylim = c(0,300), breaks = 100,
     col = "grey40",
     xlab = "Distance from colony (m)",
     main = "",
     cex.lab = 1.6,
     cex.axis = 1.4)

# Maybe also map location near the colony??
str(points_all)

points <- cbind(points_all$latitude,points_all$longitude, col.dist, points_all$type, points_all$device_info_serial, points_all$speed)
str(points)
points <- as.data.frame(points)
names(points) <- c("lat", "long", "coldist",
                   "type", "device_info_serial",
                   "speed")

summary(as.factor(points$type))
summary(points_all$type)
f <- (points$type != 1) | (points$type != 5)
map.trip(points = points[f,],xlim = c(17.9,18.0),
         ylim = c(57.28,57.31))

map.trip(points = points[f,],xlim = c(17.93,17.98),
         ylim = c(57.285,57.30))


f <- (points$type != 1) & (points$type != 5)
map.trip(points = points[f,],xlim = c(17.9,18.0),
         ylim = c(57.28,57.31))
map.trip(points = points[f,],xlim = c(17.93,17.98),
         ylim = c(57.285,57.30))


f2 <- (points$type != 1) & (points$type != 5) &
  (points$coldist < 300) &
  (points$coldist > 100)
points(points$lat[f2]~points$long[f2],
       col = "orange", pch = 8)



f2 <- (points$lat != 0 ) & (points$ehpe > 50) & !f
points(points$lat[f2 ]~points$long[f2 ],
       col = "green", pch = 8, cex = 1.2)




plot(col.dist[col.dist < 500]~points_all$longitude[col.dist < 500],
#      ylim = c(0,300), breaks = 50,
     col = "grey40",
#      xlab = "Distance from colony (m)",
     main = "",
     cex.lab = 1.6,
     cex.axis = 1.4)


# Diving locations


points <- cbind(points_all$latitude,points_all$longitude, col.dist, points_all$type, points_all$device_info_serial, points_all$speed, points_all$diveprev)
str(points_all)
str(points)
points <- as.data.frame(points)
names(points) <- c("lat", "long", "coldist",
                   "type", "device_info_serial",
                   "speed_ms", "dive_previous")
points$long <- as.numeric(as.character(points$long))
points$lat <- as.numeric(as.character(points$lat))

summary(as.factor(points$type))
summary(points_all$type)
summary(points_all$device_info_serial)

f <- (points$type != 1) & (points$type != 5) &
  points$device_info_serial == "4"
summary(f)
str(points_igu)
str(points$device_info_serial)
str(points)
map.trip(points = points[f,])

f2 <- (points$type != 1) & (points$type != 5) &
  points$device_info_serial == "4" &
  points$dive_previous == TRUE


points(points$lat[f2 ]~points$long[f2 ],
       col = "red", pch = 8, cex = 1.2)


# Plot all
f <- (points$type != 1) & (points$type != 5) &
  points$device_info_serial %in% c("1","2","3",
                                 "4", "5", "6",
                                 "7", "8",
                                 "9")
summary(f)
str(points_igu)
str(points$device_info_serial)
str(points)
map.trip(points = points[f,])

f2 <- (points$type != 1) & (points$type != 5) &
  points$device_info_serial %in% c("1","2","3",
                                   "4", "5", "6",
                                   "7", "8",
                                   "9") &
points$dive_previous == TRUE


points(points$lat[f2 ]~points$long[f2 ],
       col = "red", pch = 8, cex = .8)


# Time of day
diving <- (points$type != 1) & (points$type != 5) &
  points$device_info_serial %in% c("1","2","3",
                                   "4", "5", "6",
                                   "7", "8",
                                   "9") &
  points$dive_previous == TRUE
# View number of diving fixes by hour of day
hours <- as.POSIXlt(points_all$date_time[diving],
                    tz = "UTC")
hours.loc <- as.POSIXlt(hours, tz = "Europe/Stockholm")


hist(((as.numeric(as.POSIXlt(hours.loc)$hour)+2) %%24), xlim = c(0,24), breaks = 24,
     xlab = "Hour of day (CEST)",
     ylab = "Diving GPS fixes",
     main = "",
     col = "grey60")

# KDE