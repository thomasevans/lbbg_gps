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
length(xy.sp.points_SWEREF_99)
i <- 1
# Distance calculated in metres
x.dist <- gDistance(xy.sp.points_SWEREF_99[i,],
                    spgeom2 = coast_line_local_2_SWEREF_99,
              byid = FALSE, hausdorff = FALSE, densifyFrac = NULL)







# plot(xy.sp.points)



# END -----