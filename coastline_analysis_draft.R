# Coastline data - on land/sea - distance to coast, etc.


# Required libraries ------
# Spatial library
library("sp")
library("rgdal")
library("rgeos")
library("raster")
# install.packages("spatstat")
# library("spatstat")
# library("geosphere")

# Make some example GPS locations ------
x <- c(17.91, 17.91, 17.91)
y <- c(60.605, 60.595, 60.585)
xy.loc <- cbind(x,y)

xy.loc <- obs.xy.for


# Make spatial points object from xy locations
xy.sp.points <- SpatialPoints(xy.loc,
                              proj4string = CRS("+proj=longlat +datum=WGS84"))


# Spatial extent for analysis -----
lat.ext <- c(58, 63)
long.ext <- c(13, 23)

# Load coastline data (polyline) -----
# coast_line <- readOGR("D:/Dropbox/R_projects/lbbg_gps/coastline_polyline/coastlines-split-4326",
#                       "lines")

# Save as R binnary for quicker access next time
# save(coast_line, file = "coast_line_openstreetmap.RData")

# Load coastline data
# load("coast_line_openstreetmap.RData")


# Clip coastline data to smaller spatial extent
# - crop an area
# crop.ext <- extent(raster(xmn = long.ext[1], xmx = long.ext[2],
#                           ymn = lat.ext[1], ymx = lat.ext[2]))
# 
# coast_line_local <-  crop(coast_line, crop.ext)

# Compare object sizes, clipped version ca. 15x smaller
# object.size(coast_line)/object.size(coast_line_local)

# Save to R binnary object
# save(coast_line_local, file = "coast_line_openstreetmap_local.RData")

# Load coast_line data
load("coast_line_openstreetmap_local.RData")


lat.ext <- c(60.3, 60.9)
long.ext <- c(17, 19)

crop.ext <- extent(raster(xmn = long.ext[1], xmx = long.ext[2],
                          ymn = lat.ext[1], ymx = lat.ext[2]))

coast_line_local_2 <-  crop(coast_line_local, crop.ext)
plot(coast_line_local_2)
points(xy.sp.points, col = "red")

# Calculating distances with geosphere package ----
# dist2Line(xy.sp.points, coast_line_local_2,
#           distfun = distHaversine)
# Very slow - but works!



# Distance with rgeos ----
xy.sp.points

# Transform points and coastline to Swedish map projection ()
coast_line_local_2_SWEREF_99 <- spTransform(coast_line_local_2, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))

xy.sp.points_SWEREF_99 <- spTransform(xy.sp.points, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))


# ?gDistance

# Will have to use for loop I think - ideally
# an apply function, but can't see how to do
# that in this case.
n <- length(xy.sp.points_SWEREF_99)
# i <- 1
x.dist <- NA

for(i in 1:n){
    # Distance calculated in metres
    x.dist[i] <- gDistance(xy.sp.points_SWEREF_99[i,],
                        spgeom2 = coast_line_local_2_SWEREF_99,
                  byid = FALSE, hausdorff = FALSE, densifyFrac = NULL)
}
hist(x.dist/1000, breaks = 100,
     xlab = "Distance from coast (km)",
     main = "CG - distance from coast (foraging locations)")


# Land polygons - points on land (else sea) -----
# coast_line <- readOGR("D:/Dropbox/R_projects/lbbg_gps/land_poly",
#                       "land_polygons")

# save(coast_line, file = "coast_line_openstreetmap_world.RData")

load("coast_line_openstreetmap_world.RData")

lat.ext <- c(60.3, 60.9)
long.ext <- c(17, 19)

crop.ext <- extent(raster(xmn = long.ext[1], xmx = long.ext[2],
                          ymn = lat.ext[1], ymx = lat.ext[2]))

coast_line_polygon_local_2 <-  crop(coast_line, crop.ext)

save(coast_line_polygon_local_2, file = "coast_line_openstreetmap_local2.RData")

load("coast_line_openstreetmap_local2.RData")

# View how this looks
plot(coast_line_polygon_local_2, col = "grey")

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('blue','red'))

#This adds a column of color values
# based on the y values
col.dist <- rbPal(10)[as.numeric(cut(x.dist,breaks = 20))]
points(xy.sp.points, col = col.dist)



coast_line_polygon_local_2_SWEREF_99 <- spTransform(coast_line_polygon_local_2, CRS("+proj=tmerc +lat_0=0 +lon_0=15.8062845294444 +k=1.00000561024 +x_0=1500064.274 +y_0=-667.711 +ellps=GRS80 +pm=15.8062845294444 +units=m +no_defs"))


# Points in polygons ----
# Useing package 'sp'
points.on.land <- over(xy.sp.points_SWEREF_99,
                       coast_line_polygon_local_2_SWEREF_99,
                       returnList = FALSE, fn = NULL)

on_land <- !is.na(points.on.land)
summary(on_land)


plot(coast_line_polygon_local_2, col = "grey")

points(xy.sp.points, col = (1 - 1*on_land))

points(xy.sp.points, col = (1 - 1*on_land))


sign.dist <- (1 - 2*on_land)*x.dist
hist(sign.dist/1000, xlab = "Inland (negative) ------- at sea (positive) (km)",
     breaks = 40,
     main = "Distance to coast",
     col = "grey")

rbPal <- colorRampPalette(c('blue','yellow', 'red'))


rev.rbPal <- rev(rbPal(10))
col.dist <- (rev.rbPal[as.numeric(cut((sign.dist),breaks = 20))])

plot(coast_line_polygon_local_2, col = "grey")
points(xy.sp.points, col = col.dist)

plot(sign.dist,(1 - 2*on_land))

# Get NA if point is not over a polygon, and FID of polgon if it is
# So if ID - on land, if no ID, at sea



# Bathymetry ------
# Get bathymetry data
bath_raster <- raster("bsbd-0.9.3_full.grd")

# Local
bath_raster_local_2 <-  crop(bath_raster, crop.ext)

pdf("test.map.pdf")
plot(coast_line_local_2, xlim = c(17.5,18.5), ylim = c(60.5,60.7))
plot(bath_raster_local_2,xlim = c(17.5,18.5), ylim = c(60.5,60.7), add = TRUE)
plot(coast_line_local_2, add = TRUE, bg = NA)
points(xy.sp.points, col = "red")
dev.off()


# Get depth for each GPS location (NA if on land/ not on raster)
gps.bath <- extract(bath_raster,xy.loc)





# Example analysis for CG -----

# Read in CG data -----
hg.points <- read.csv("fagelsundet_cg_data.csv",
                      header = TRUE)

# Fix date-time
hg.points$date_time <-  as.POSIXct(strptime(hg.points$date_time,
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = "UTC"))


hg.points$latitude <- as.numeric(as.character(hg.points$latitude))
hg.points$longitude <- as.numeric(as.character(hg.points$longitude))
hg.points$speed     <- as.numeric(as.character(hg.points$speed))


# Sort data-frame by device_info_serial, then by date_time
hg.points <- hg.points[order(hg.points$device_info_serial,
                             hg.points$date_time), ]

# Calculate time intervals ------
# Calculat the time intervals between GPS points

# First time differences
n         <- length(hg.points$date_time)
time_pre  <- hg.points$date_time[1:(n-1)]
time_next <- hg.points$date_time[2:(n)]

t_diff <- as.numeric(difftime(time_next, time_pre,
                              units = "secs"))

# Give value to first point
t_diff <- c(NA,t_diff)


id_pre <- hg.points$device_info_serial[1:(n-1)]
id_next <- hg.points$device_info_serial[2:(n)]

id.fun <- function(id1, id2){
  if(id1 == id2) return(1) else return(0)
}

id_test <- mapply(id.fun, id1 = id_pre, id2 = id_next)
# summary(as.factor(id_test))
id_test <- c(0,id_test)

t_diff[id_test == 0] <- NA


hist(t_diff)
range(t_diff, na.rm = TRUE)



# median(t_diff, na.rm = TRUE)


# sort(t_diff, decreasing = FALSE)[1:100]

# There are some longer time intervals (not many)
# Here I replace time intervals longer than 1850 s (just over 30 minutes)
# with NA values - i.e. giving these points zero weight
sort(t_diff, decreasing = TRUE)[1:100]
time_interval <- t_diff
time_interval[t_diff > 1850] <- NA
summary(time_interval)
length(time_interval)
hist(time_interval)


time_interval_narm <- time_interval

# Replace NAs with 0 (i.e. zero weight)
time_interval_narm[is.na(time_interval)] <- 0







# Calculate distances -----
# distance from colony


# If want to use actual nest location
# Get nest locations
device_id <- unique(hg.points$device_info_serial)


library("RODBC")
# vignette("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Nest location (actually start location)
g <- data.frame(device_info_serial=numeric(),
                start_latitude = numeric(),
                start_longitude = numeric())
for(i in 1:length(device_id)){
  g[i,] <- sqlQuery(gps.db,
                    query = paste("SELECT gps_ee_track_session_limited_local.device_info_serial, gps_ee_track_session_limited_local.start_latitude, gps_ee_track_session_limited_local.start_longitude
                                  FROM gps_ee_track_session_limited_local
                                  WHERE (((gps_ee_track_session_limited_local.device_info_serial)= ",
                                  device_id[i], "));", sep = ""),
                    as.is = TRUE)
}

close(gps.db)

# g
# s_long <- as.numeric(g$start_longitude[1])
# s_lat  <- as.numeric(g$start_latitude[1])

bird.dist <- function(device_id,lat,long, z, ...){
  source("deg.dist.R")
  x <- as.data.frame(z[1])
  lat.1 <- as.numeric(x[x$device_info_serial == device_id,2])
  long.1 <- as.numeric(x[x$device_info_serial == device_id,3])
  dist <- deg.dist(long.1,lat.1,
                   long, lat, km = FALSE) 
  return(unlist(dist)[1])
}

col.dist <- NULL

col.dist <- mapply(bird.dist,
                   lat = as.numeric(hg.points$latitude),
                   long = as.numeric(hg.points$longitude),
                   device_id = hg.points$device_info_serial,
                   MoreArgs = (list(z = list(g))))

col.dist <- col.dist*1000


# Filter data -----

# Non-flight and not-colony
# summary(speed)
f <- (hg.points$speed < 4) & (col.dist > 1000)
# summary(speed < 4)
# summary(col.dist > 1000)
f[is.na(f)] <- FALSE

summary(f)


# Extract XY location data
obs.xy <- cbind(hg.points$longitude, hg.points$latitude)


obs.xy.for <- obs.xy[f,]


time.weight.hg.all.for <- 100*((time_interval_narm[f]) /
                                 (sum(time_interval_narm[f])))

head(time.weight.hg.all.for)




# Distance from coast ----







# END -----