# Script to produce raster layers from gull GPS data
# After producing raster layer, layer is output to kml
# for vieweing in Google Earth


# Load/ install require packages -------
# install.packages(c("plotKML","raster","rJava","animation"))

# Load packages
library("plotKML")
library("raster")
library("rJava")
library("animation")

# Load GPS data -----

# From csv
# HG data
hg.points <- read.csv("fagelsundet_hg_data.csv",
                      header = TRUE)


# Calculate time intervals ------
# Calculat the time intervals between GPS points


# Calculate distances -----
# distance from colony

# colony location
col_loc <- cbind(17.929104, 60.630572)

# Previously saved function
source("deg.dist.R")

# Run for all points
col.dist <- 1000*deg.dist(col_loc[1],col_loc[2], obs.xy[,1], obs.xy[,2])


# Set-up base-raster layer -----
# Encompases most of range of GPS points
# Aprox. a 500m grid
base_raster <- raster(nrows = 305, ncols = 563,
                      xmn = 15, xmx = 20.5,
                      ymn = 59.5, ymx = 61.0,
)


# Extract XY location data
obs.xy <- cbind(hg.points$longitude, hg.points$latitude)


# Filter data -----

# Make speed into numeric vector
speed <- as.numeric(as.character(hg.points$speed))


# Non-flight and not-colony
summary(speed)
f <- (speed < 4) & (col.dist > 1000)
f[is.na(f)] <- FALSE

obs.xy.for <- obs.xy[f,]




# Make raster layers -----

# All 'foraging' locations
point_raster_forage <- rasterize(obs.xy.for,
                                 base_raster,
                                 1,
                                 fun = sum)


# 6120 HG example -----
f2 <- (speed < 4) & (col.dist > 1000) & hg.points$device_info_serial == "6120"
f2[is.na(f2)] <- FALSE
# summary(f2)
obs.xy.for.6120 <- obs.xy[f2,]
a <- rep(1, length(obs.xy.for.6120[,1]))
# str(a)
point_raster_forage_6120 <- rasterize(obs.xy.for.6120,
                                      base_raster,
                                      fun=function(x, ...) {length(unique(na.omit(x)))})

plotKML(point_raster_forage_6120, file = "point_raster_forage_6120.kml",
        min.png.width = (10*612))

point_raster_forage_6120_log <- point_raster_forage_6120
values(point_raster_forage_6120_log) <- log(values(point_raster_forage_6120_log))

plotKML(point_raster_forage_6120_log, file = "point_raster_forage_6120_log.kml",
        min.png.width = (10*612))


# Output raster layers to kml files -----