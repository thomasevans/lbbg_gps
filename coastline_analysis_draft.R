# Coastline data - on land/sea - distance to coast, etc.


# Required libraries ------
# Spatial library
library("sp")
library("rgdal")
library("rgeos")
library("raster")


# Make some example GPS locations ------
x <- c(17.91, 17.91, 17.91)
y <- c(60.605, 60.595, 60.585)
xy.loc <- cbind(x,y)



# Make spatial points object from xy locations
xy.sp.points <- SpatialPoints(xy.loc)


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

# coast_line_local <-  crop(coast_line, crop.ext)

# Compare object sizes, clipped version ca. 15x smaller
# object.size(coast_line)/object.size(coast_line_local)

# Save to R binnary object
# save(coast_line_local, file = "coast_line_openstreetmap_local.RData")

# Load coast_line data
load("coast_line_openstreetmap_local.RData")


# END -----