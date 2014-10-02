
# Load file 'bsbd_raster.RData'
load(file.choose())
# See end of script for original source and how I got this file.


# Load required packages ----
library(RColorBrewer)
library(raster)


# Example figure -----
# Colour palette to diplay bathymetric data
col.bath <- c(rev(brewer.pal(9,"Reds"))[1:5], rev(brewer.pal(9,"Blues")), "black")

col.obs <- brewer.pal(9,"Greys")

karls_x <- 17.958252
karls_y <- 57.289848



plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath only")
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)


# If you would like to add a scale you can use function 'map.scale'
# from the 'maps' package
library(maps)
map.scale(x= 17, y = 56.9, ratio = FALSE)









# BSBD data -----
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

# save(bsbd_raster, file = "bsbd_raster.RData")

