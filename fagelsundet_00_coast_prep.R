# This script takes in openstreetmap coastline data
# then crops this to the spatial extent appropriate
# for the Fågelsundet gull dataset

# See note at bottom of script for origin of coastline data used here

# Required packages ----
library("rgdal")
library("raster")



# Coastline -----

# * Load coastline data (polyline) -----
coast_line <- readOGR("D:/Dropbox/R_projects/lbbg_gps/coastline_polyline/coastlines-split-4326",
                      "lines")




# * Clip coastline data to smaller spatial extent -----
# - crop an area
crop.ext <- extent(raster(xmn = 14, xmx = 20.5,
                          ymn = 58, ymx = 62))


openstreetmap_coast_polyline <-  crop(coast_line, crop.ext)


# * Save to R binnary object   --------
save(openstreetmap_coast_polyline,
     file = "openstreetmap_coast_polyline.RData")


# Land data ----

# * Land polygons - points on land (else sea) -----
coast_land <- readOGR("D:/Dropbox/R_projects/lbbg_gps/land_poly",
                      "land_polygons")

# Use previously save R binnary file of above (whole world)
# load("coast_line_openstreetmap_world.RData")
# coast_land <- coast_line


# * Crop land data ------
openstreetmap_coast_polygon <-  crop(coast_land, crop.ext)

# Check this looks ok
plot(openstreetmap_coast_polygon)

plot(openstreetmap_coast_polyline, col = "red", lty = 2, add = TRUE)

# * Save to R binnary -----
save(openstreetmap_coast_polygon,
     file = "openstreetmap_coast_polygon.RData")


# OpenStreetMap data attribution ------
# Info from README file:

# This data was downloaded from openstreetmapdata.com which offers
# extracts of OpenStreetMap data.
# 
# See http://openstreetmapdata.com/data for details.
# 
# LICENSE
# =======
#   
#   This data is Copyright 2012 OpenStreetMap contributors. It is
# available under the Open Database License (ODbL).
# 
# For more information see http://www.openstreetmap.org/copyright

