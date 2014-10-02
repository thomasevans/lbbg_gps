# Comparing different bathymetric data to choose grid for
# analysis.

# Load GPS point data for guillemots -----

# 'surface_points' contains all GPS locations for 2014 where
# speed is < 5 ms-1 (not flying), and where distances are
# > 300 m from the colony. In addition there is limited
# GPS quality filtering to remove the most innacurate
# GPS locations.
load(url("https://dl.dropboxusercontent.com/u/3745544/guillemot_bathy/surface_points.Rdata"))



# 'surface_points2' is above but with one indivdual bird
# removed, that which appeared highly disturbed by the device
# spending ca. 5 days at sea, mostly apparently swimming on
# the sea surface
load(url("https://dl.dropboxusercontent.com/u/3745544/guillemot_bathy/surface_points2.Rdata"))


# 'dive_points' contains a subset of 'surface_points', only
# including locations from the 'homemade' (not Amsterdam)
# GPS tags. For these devices it contains locations where
# the previous GPS fix attempt was during a dive.
load(url("https://dl.dropboxusercontent.com/u/3745544/guillemot_bathy/dive_points.Rdata"))


# # Get guillemot obs data
# alk <- read.delim(file.choose())
# 
# # Get bathymetric data
# bsbd_raster <- raster(file.choose())




# Bathymetric data examples ----

# First load required packages ----
library(RColorBrewer)
library(raster)
# ?brewer.pal
# Colour palette to diplay bathymetric data
col.bath <- c(rev(brewer.pal(9,"Reds"))[1:5], rev(brewer.pal(9,"Blues")), "black")

col.obs <- brewer.pal(9,"Greys")

karls_x <- 17.958252
karls_y <- 57.289848

# 1. BSBD data -----
# Interpolated bathymetric data from 'BSBD'
# the Baltic Sea Bathymetric Database
# "The resolution is 2 minutes with respect
# to longitude, and 1 minute to latitude. 
# This is approximately 1 nautical mile, or
# 2 km resp."
# For full meta-data see:
# http://www.io-warnemuende.de/topography-of-the-baltic-sea.html


# # I downloaded the data in the 'NetCDF' format
# bsbd_raster <- raster("D:/Dropbox/R_projects/lbbg_gps/bathy/iowtopo.nc/iowtopo2_rev03.nc", varname = "Z_WATER")
# # Then output as a raster file including only the
# # bathymetric, depth data - no topographic data
# writeRaster(bsbd_raster, filename = "bsbd_raster.grd")

# First download the files and place in a directory
# Files required 'bsbd_raster.grd', and 'bsbd_raster.gri'
# Files at: https://www.dropbox.com/sh/laglwoqn8tjpi8g/AAAxGndSiOjzw0oLytEZGJnta?dl=0

# bsbd_raster <- raster("D:/Dropbox/Public/guillemot_bathy/bsbd_raster.grd")
# bsbd_raster <- raster(file.choose())

save(bsbd_raster, file = "bsbd_raster.RData")

load("bsbd_raster.RData")

# View raster layer
# plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black")

# Save plots to pdf
pdf("bsbd_figs.pdf")

plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath only")
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)

# Generate raster layer for surface_points2
xy <- cbind(surface_points2$longitude,surface_points2$latitude)
surface_points2_ras <- rasterize(xy,bsbd_raster, fun=function(x,...)length(x))

plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath + surface GPS")
plot(surface_points2_ras, col = col.obs, add = T)
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)

# dev.off()

# Do same but just for diving points
xy <- cbind(dive_points$longitude,dive_points$latitude)
dive_points_ras <- rasterize(xy,bsbd_raster, fun=function(x,...)length(x))
# pdf("dive_points_bsbd.pdf")
plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath + dive GPS")
plot(dive_points_ras, col = col.obs, add = T)
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)

# dev.off()


# Do same for guillemot observations
# ALKER AT SEA
# alk <- read.delim("D:/Dropbox/guillemot_2014_data/observation_survey/Karlso 2014/Alker_AtSea.TXT")
alk <- read.delim(file.choose())
xy <- alk[,3:2]
sigri <- rasterize(xy, bsbd_raster, field = alk[,4])
plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath + 'alker at sea'")
plot(sigri, col = col.obs, add = T)
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)

dev.off()