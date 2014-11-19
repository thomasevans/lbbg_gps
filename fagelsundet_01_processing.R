# Calculate various paramaters for the GPS tracking data for
# all Fågelsundet gulls for 2014
# Including:
# - Distance from nest
# - Whether on foraging trip (distance from nest threshhold)
# - Type of point ('flight', 'non-flight') using speed threshold
# - Distance to nearest coast
# - Whether on land/ sea
# - Combined, signed distance to coast
# Then output this data to DB with combined primary key of
# device_info_serial and date_time

# Required packages ------
library("RODBC")
library("sp")
library("rgdal")
library("rgeos")
library("raster")
library("compiler")

# Read in data from DB -----
# Make connection to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# GPS data
gps.points <- sqlQuery(gps.db,
                    query = "SELECT DISTINCT g.*
                    FROM fagelsundet_gulls_2014_gps_data AS g
                    ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                    ,as.is=TRUE)

# Check data structure
str(gps.points)

# hist(gps.points$altitude, breaks = 4000, xlim = c(-200,500))

# Fix date_time
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = "UTC"))



# Nest location data (combine device_info_serial with ring_number)
# Make list of devices
device.ids <- unique(gps.points$device_info_serial)

# Querry DB and combine tables for bird ID (containing nest location)
# and device_info_serial

nest.info <- sqlQuery(gps.db,
                       query = paste("SELECT DISTINCT t.track_session_id, t.key_name, t.device_info_serial, t.ring_number, t.start_latitude, t.start_longitude, i.species, n.latitude, n.longitude, n.nest_id
                    FROM gps_ee_track_session_limited_local AS t, gps_ee_individual_limited AS i, gps_ee_nest_limited AS n
WHERE t.device_info_serial IN (", paste(device.ids, collapse = ", "),
                      ")
AND t.ring_number = i.ring_number
AND i.ring_number = n.ring_number
                    ORDER BY t.device_info_serial ASC;", sep = "")
                       , as.is=TRUE)


# Calculate various paramaters ----

# Distance from nest -----

# Distance from nest function
bird.dist <- function(device_id,lat,long, z, ...){
  source("deg.dist.R")
  x <- as.data.frame(z[1])
  lat.1 <- as.numeric(x[x$device_info_serial == device_id,8])
  long.1 <- as.numeric(x[x$device_info_serial == device_id,9])
  dist <- deg.dist(long.1,lat.1,
                   long, lat, km = FALSE) 
  return(unlist(dist)[1])
}

# Initialise empty vector for distance data
col.dist <- NULL

# Run for all points
col.dist <- mapply(bird.dist,
                   lat = gps.points$latitude,
                   long = gps.points$longitude,
                   device_id = gps.points$device_info_serial,
                   MoreArgs = (list(z = list(nest.info))))

# Convert to metres
col.dist <- col.dist*1000




# Check that this looks ok
# head(col.dist)
# sample(col.dist, 10)

# Whether on trip (distance threshold) ----
# have a look at the distribution of points close to the nest.
# hist(col.dist[col.dist < 3000], breaks = 400)
# hist(col.dist[col.dist < 1000], breaks = 200)

# Threshold of ca. 200 m?
# summary(col.dist < 200)
# More on trip than at nest - but this makes sense as at least
# for some cases a fence was used to gain higher resolution data
# when the bird was away from the island.

trip.fun <- function(x, threshold = 200){
  a <- NULL
  if(is.na(x)) {a <- NA} else{
    if(x < threshold){
      a <- FALSE
    } else a <- TRUE
  }
  return(a)
}

on.trip <- sapply(col.dist, trip.fun)
# on.trip <- as.factor(on.trip)

# See how this looks
summary(on.trip)




# Time intervals (time from last point) -----

# First time differences
n         <- length(gps.points$date_time)
time_pre  <- gps.points$date_time[1:(n-1)]
time_next <- gps.points$date_time[2:(n)]

t_diff <- as.numeric(difftime(time_next, time_pre,
                              units = "secs"))

# Give value to first point
t_diff <- c(NA,t_diff)


id_pre <- gps.points$device_info_serial[1:(n-1)]
id_next <- gps.points$device_info_serial[2:(n)]

id.fun <- function(id1, id2){
  if(id1 == id2) return(1) else return(0)
}

id_test <- mapply(id.fun, id1 = id_pre, id2 = id_next)
# summary(as.factor(id_test))
id_test <- c(0,id_test)

t_diff[id_test == 0] <- NA





# See how this looks
head(t_diff)
sample(t_diff, 100)
hist(t_diff)
hist(t_diff[t_diff < 1000])
range(t_diff, na.rm = TRUE)



# Label species -----

gps.ind.comb <- merge(gps.points, nest.info, by = "device_info_serial")

sp.lat <- as.factor(gps.ind.comb$species)
summary(sp.lat)



# Type of point (flight/ non-flight, speed threshold) ----
# names(gps.points)
# hist(gps.points$speed)
# hist(gps.points$speed[gps.points$speed < 20], ylim = c(0,50000),
#      breaks = 40)
# 
# sp <- sp.lat == "Larus marinus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], ylim = c(0,10000),
#      breaks = 40)
# sp <- sp.lat == "Larus canus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], ylim = c(0,5000),
#      breaks = 40)
# sp <- sp.lat == "Larus argentatus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], ylim = c(0,10000),
#      breaks = 40)
# 
# # Three combined
# hist(gps.points$speed[gps.points$speed < 20], ylim = c(0,2000),
#      border = "dark grey", breaks = 80)
# sp <- sp.lat == "Larus argentatus"
# hist(gps.points$speed[gps.points$speed < 20 & sp],
#      add = TRUE,
#      breaks = 80)
# sp <- sp.lat == "Larus canus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], add = TRUE,
#      border = "red", breaks = 80)
# sp <- sp.lat == "Larus marinus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], add = TRUE,
#      border = "blue", breaks = 80)
# abline(v=3, lty = 2)
# abline(v=3.5, lty = 3)
# abline(v=4, lty = 4)
# 
# # Look closer at common gulls
# sp <- sp.lat == "Larus canus"
# hist(gps.points$speed[gps.points$speed < 20 & sp], add = FALSE,
#      border = "red", breaks = 80,
#      ylim = c(0,300))
# abline(v=c(3,3.5,4,4.5,5), lty = c(2:5))


# Problematic, as although bimodal, there is apparently quite
# a lot of overlap. A cut-off around 4.5 looks 'ok', though
# will have quite high levels of mis-clasification

# Label if in flight -----
# hg.thresh <- 3.25
# gbbg.thresh <- 4.25
# cg.thresh <- 4.5

flight.fun <- function(speed, sp){
   if(is.na(sp)|is.na(speed)) return(NA) else{
    if(sp == "Larus argentatus") thresh <- 3.25 else{
      if(sp == "Larus canus") thresh <- 4.5 else{
        if(sp == "Larus marinus") thresh <- 4.25
      }
    }
    
    if(speed < thresh) return(FALSE) else return(TRUE)  
  }
}

flight.point <- mapply(FUN = flight.fun, speed = gps.points$speed, sp = sp.lat)

# point.type <- as.factor(point.type)

summary(flight.point)


# Distance from coast -----
# Load coast-line data
load("openstreetmap_coast_polyline.RData")

# Re-project coast-line
# Transform points and coastline to Swedish map projection ()
openstreetmap_polyline_SWEREF_99 <- spTransform(openstreetmap_coast_polyline, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))



# Make points object
xy.loc <- cbind(gps.points$longitude,
                gps.points$latitude)

# Make spatial points object from xy locations
xy.sp.points <- SpatialPoints(xy.loc,
                              proj4string = CRS(
                                "+proj=longlat +datum=WGS84"))

# Re-project point data
xy.sp.points_SWEREF_99 <- spTransform(xy.sp.points, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))

# coast.dist.orig <- coast.dist
# Calculate distances



# *******Resume from here -------
load("20141118_temp.RData")





coast.dist <- NA
# Eventuall ran the following on Amazon EC2 (cloud)
# instance of RStudio, which completed in ca. 20 hours
system.time(
for(i in 1:n){
  # Distance calculated in metres
  coast.dist[i] <- gDistance(xy.sp.points_SWEREF_99[i,],
                         spgeom2 = openstreetmap_polyline_SWEREF_99,
                         byid = FALSE, hausdorff = FALSE,
                         densifyFrac = NULL)
}
)
# 




# # Make a map to see how this looks
# pdf("test.map.pdf")
# plot(openstreetmap_polyline_SWEREF_99)
# points(xy.sp.points_SWEREF_99, col = "red")
# dev.off()



# On land/ sea -----
# Load coast-line data
load("openstreetmap_coast_polygon.RData")

# Re-project coast-line polygon
# Transform points and coastline to Swedish map projection ()
openstreetmap_polygon_SWEREF_99 <- spTransform(openstreetmap_coast_polygon, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))


# Calculate if in polygon
# Useing package 'sp'
points.on.land <- over(xy.sp.points_SWEREF_99,
                       openstreetmap_polygon_SWEREF_99,
                       returnList = FALSE, fn = NULL)



# Label 'land' or 'sea' depending on above output
on_land <- !is.na(points.on.land)
summary(on_land)





# Signed distance from coast ----
sign.dist <- (1 - 2*on_land)*coast.dist



# Load data processed on Amazon EC2 cloud machine -----
load("coast.dist.RData")
load("sign.dist.RData")
load("on_land.RData")

# Combine into data frame ------
db.tab <- data.frame(gps.points$device_info_serial,
                gps.points$date_time,
                col.dist,
                on.trip,
                t_diff,
                sp.lat,
                flight.point,
                coast.dist,
                on_land,
                sign.dist            
)

names(db.tab) <- c("device_info_serial",
                   "date_time",
                   "col_dist",
                   "on_trip",
                   "t_diff",
                   "sp_lat",
                   "flight_point",
                   "coast_dist",
                   "on_land",
                   "coast_dist_sign")

# Output to Database ------
