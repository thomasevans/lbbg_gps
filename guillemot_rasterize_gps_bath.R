
# Required files -----
# You should have the following files in the folder/ directory
# from which you will work.
# bsbd_raster.RData
# points.2014.all.RData
# guillemot_gps_data_all_2009.RData
# SWE_adm0.RData

# Set working-directory -----
# Set working-directory to where above files are located
setwd("...")

# Load in required files ----
# Bathymetry
load('bsbd_raster.RData')

library("raster")
bath_raster <- raster("bsbd-0.9.3.grd")


# 2014 GPS data
load('points.2014.all.RData')

# 2009 GPS data
load('guillemot_gps_data_all_2009.RData')


# Caculate time intevals for 2014 GPS locations -----

# First filter GPS locations, so we only include
# higher quality (not 'bad_location') GPS locations
# and exclude those with no location ('dive' and 'no_location')
f <- (points.2014.all$type != "bad_location") &
  (points.2014.all$type != "dive") &
  (points.2014.all$type != "no_location") &
  (points.2014.all$ring_number != "AAZ972")
f[is.na(f)] <- TRUE

# New datafame with only GPS locations meeting above criteria
points.2014 <- points.2014.all[f,]

# Calculate time between locations
time_interval <- NULL

# Set first to zero (no preceding points)
time_interval[1] <- 0


# Calculate time between points
# If final or first point from bird (ring_number)
# Set to zero
for(i in 2:(length(points.2014$type)-1)){
  if(points.2014$ring_number[i] !=
       points.2014$ring_number[i-1]){
    time_interval[i] <- 0
  } else if(points.2014$ring_number[i] !=
              points.2014$ring_number[i+1]){
    time_interval[i] <- 0} else{
      time_interval[i] <- as.numeric(
        difftime(
          points.2014$date_time[i-1],
          points.2014$date_time[i],
          units = "secs") * -1
      )
    }
}
# Set final point to zero otherwise will get
# error with above code where compares current
# and next point
time_interval[length(points.2014$type)] <- 0

# Add 'time_interval' to the dataframe
points.2014 <- cbind(points.2014, time_interval)

# See what the time intervals look like
# Time interval is in seconds
range(time_interval)
# In seconds
sort(time_interval, decreasing = TRUE)[1:100]
# In hours
sort(time_interval/(60*60), decreasing = TRUE)[1:100]
# There are some larger time intervals ('gaps') in the
# GPS data.
# Looking into these it turned out that the longest intervals
# are nearly all of points either at the colony or the gap
# between the last position at the colony, and the first point
# away from the colony.
# This is most likely owing to poor GPS reception at the
# colony, with the high cliff, and the murre-lab above both
# obscurring the view of the sky - leading to more failed
# fixes.

# When weighting data by time (total time in cell), suggest
# that we use mean time after removing time intervals greater
# than some threshold for points with longer time - so as
# not to give greater weight to these points




# Caculate time intevals for 2009 GPS locations -----



# Calculate time between locations
time_interval <- NULL

# Set first to zero (no preceding points)
time_interval[1] <- 0


# Calculate time between points
# If final or first point from bird (ring_number)
# Set to zero
for(i in 2:(length(points.2009.all$ring_number)-1)){
  if(points.2009.all$ring_number[i] !=
       points.2009.all$ring_number[i-1]){
    time_interval[i] <- 0
  } else if(points.2009.all$ring_number[i] !=
              points.2009.all$ring_number[i+1]){
    time_interval[i] <- 0} else{
      time_interval[i] <- as.numeric(
        difftime(
          points.2009.all$date_time[i-1],
          points.2009.all$date_time[i],
          units = "secs") * -1
      )
    }
}
# Set final point to zero otherwise will get
# error with above code where compares current
# and next point
time_interval[length(points.2009.all$ring_number)] <- 0

# Add 'time_interval' to the dataframe
points.2009.all <- cbind(points.2009.all, time_interval)

# See what the time intervals look like
# Time interval is in seconds
range(time_interval[points.2009.all$coldist > 1000])

# In seconds
sort(time_interval, decreasing = TRUE)[1:100]
# In hours
sort(time_interval/(60*60), decreasing = TRUE)[1:100]


# 23453/(60*60)



# Sub-set 2014 data for 'foraging' locations only ----
# Sub-set 2014 data for:
# 1. non-colony location (i.e. points during foraging trips only)
# 2. Non-colony & speed < 5 ms-1 (i.e. non commuting points -
# more likely actual foraging)

# 1. non-colony locations
# Only points > 1 km from colony
points.2014.non_col <- subset(points.2014, coldist > 1000)

# Only points > 1 km from colony & where speed is < 5 ms-1
points.2014.surface <- subset(points.2014, (coldist > 1000) & (
  speed < 5))




# Sub-set 2009 data for 'foraging' locations only ----

# 1. non-colony locations
# Only points > 1 km from colony
points.2009.non_col <- subset(points.2009.all, coldist > 1000)

# Only points > 1 km from colony & where speed is < 5 ms-1
points.2009.surface <- subset(points.2009.all, (coldist > 1000) & (
  speed_ms < 5))


# Prepare raster layers for GPS point subsets -----
# 2014 + surface points
xy.2014.surface <- cbind(points.2014.surface$long,
                         points.2014.surface$lat)

time.weight.2014.surface <- points.2014.surface$time_interval

# Mean of time_intervals for points with <35 minute time interval
t.mean <- mean(time.weight.2014.surface[time.weight.2014.surface <
                                35*60])

# Replace long time intervals with the mean time interval to prevent
# excessive weights on these points
time.weight.2014.surface[time.weight.2014.surface >
                           35*60] <- t.mean
time.weight.2014.surface.prop <- 100*time.weight.2014.surface /
  sum(time.weight.2014.surface)


# 2014 surface points raster layer
# rasterize(xy_coords, raster_parent, weight_vector, function_for_weight_vector)
time.weight.2014.surface.raster <- rasterize(xy.2014.surface,
                                     bsbd_raster,
                                     time.weight.2014.surface.prop,
                                     fun = sum)


# Check this looks ok
plot(time.weight.2014.surface.raster,
     xlim = c(17,18), ylim = c(56.8,57.7))






# 2009 + surface points
xy.2009.surface <- cbind(points.2009.surface$long,
                         points.2009.surface$lat)

time.weight.2009.surface <- points.2009.surface$time_interval

# Mean of time_intervals for points with <35 minute time interval
t.mean <- mean(time.weight.2009.surface[time.weight.2009.surface <
                                          35*60])

# Replace long time intervals with the mean time interval to prevent
# excessive weights on these points
time.weight.2009.surface[time.weight.2009.surface >
                           35*60] <- t.mean
time.weight.2009.surface.prop <- (100*time.weight.2009.surface             
                                  /  sum(time.weight.2009.surface))


# 2009 surface points raster layer
# rasterize(xy_coords, raster_parent, weight_vector, function_for_weight_vector)
time.weight.2009.surface.raster <- rasterize(xy.2009.surface,
                                             bsbd_raster,
                                             time.weight.2009.surface.prop,
                                             fun = sum)


# Check this looks ok
plot(time.weight.2009.surface.raster,
     xlim = c(17,18), ylim = c(56.8,57.7))






# Plot some maps -------

library(RColorBrewer)
library(maps)


# Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

col.obs.transp <- addalpha((brewer.pal(9,"OrRd")), alpha = 0.60)

# Batymetry colours
bath.break.points <- c(0, -20, seq(-30,-70,-10),-90,-110,-250)
bath.col <- rev(brewer.pal(9,"PuBu"))

# pdf("test.bath.pdf")
# # Plot bathymetric map
# plot(bsbd_raster, xlim = c(16.9, 18.3), ylim = c(56.5, 58), colNA = "green", col = bath.col, breaks = bath.break.points)
# 
# # Overlay the 2014 foraging data
# plot(time.weight.2014.surface.raster, add = T,
#      col = col.obs.transp,
#      horizontal = TRUE)
# map.scale(x= 17, y = 56.9, ratio = FALSE)
# 
# dev.off()

# Swedish coast-line data
load("SWE_adm0.RData")


# Bathymetry map -----
pdf("bathymetry_bsbd-0.9.3.pdf")
# win.metafile("bathymetry_bsbd-0.9.3.wmf")
png("bathymetry_bsbd-0.9.3.png", width = 1500, height = 1500, res = 200)

# Plot base map
break.points <- c(seq(0,4,0.5),5)
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 5))
plot(gadm, col=NA, bg = NA,xlim = c(16.9, 18.1), ylim = c(56.8, 57.65))
plot(bath_raster, colNA = "green", col = bath.col, add = TRUE ,
     breaks = bath.break.points, xlim = c(16.8, 18.3),
     ylim = c(56.8, 57.8))
title(main = "Bathymetry map", line = 3)
map.scale(x= 17.1, y = 56.9, ratio = FALSE)
plot(gadm, col="grey", bg = NA, add = T)
box(,col="grey50",lwd=2)
axis(side=(2), las=1, col="grey50", col.axis="grey50")
axis(side=(3), las=1, col="grey50", col.axis="grey50")
mtext("Depth (m)",
      side = 4,
      line = 1,
      las = 2,
      at = 57.5)
dev.off()



# Map for 2014 data -----
pdf("guillemots_2014_bsbd-0.9.3.pdf")
# win.metafile("guillemots_2014_bsbd-0.9.3.wmf")
png("guillemots_2014_bsbd-0.9.3.png", width = 1500, height = 1500, res = 200)
# Plot base map
break.points <- c(seq(0,2,0.25),2.5)
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 5))
plot(gadm, col=NA, bg = NA,xlim = c(16.9, 18.1),
     ylim = c(56.8, 57.65))
plot(bath_raster, colNA = "green", col = bath.col,
     add = TRUE , breaks = bath.break.points,
     xlim = c(16.8, 18.3), ylim = c(56.8, 57.8))

dist <- distanceFromPoints(time.weight.2014.surface.raster, xy = c(17.973, 57.286))
maxdist <- 100000 # 100 km
time.weight.2014.surface.raster[dist > maxdist] <- NA

plot(time.weight.2014.surface.raster, add = T,
     col = col.obs.transp,
     breaks = break.points,
     horizontal = TRUE)
title(main = "2014 GPS points - surface only", line = 3)
map.scale(x= 17.1, y = 56.9, ratio = FALSE)
plot(gadm, col="grey", bg = NA, add = T)
box(,col="grey50",lwd=2)
axis(side=(2), las=1, col="grey50", col.axis="grey50")
axis(side=(3), las=1, col="grey50", col.axis="grey50")
title(xlab = "Time in grid cell (%)", line = 0)
mtext("Depth (m)",
      side = 4,
      line = 1,
      las = 2,
      at = 57.5)
dev.off()




# Map for 2009 data ----
# range(time.weight.2009.surface.raster)
pdf("guillemots_2009_bsbd-0.9.3.pdf")
# win.metafile("guillemots_2009_bsbd-0.9.3.wmf")
png("guillemots_2009_bsbd-0.9.3.png", width = 1500, height = 1500, res = 200)


# Plot base map
break.points <- c(seq(0,4,0.5),5)
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 5))
plot(gadm, col=NA, bg = NA,xlim = c(16.9, 18.1),
     ylim = c(56.8, 57.65))
plot(bath_raster, colNA = "green", col = bath.col,
     add = TRUE , breaks = bath.break.points,
     xlim = c(16.8, 18.3), ylim = c(56.8, 57.8))
plot(time.weight.2009.surface.raster, add = T,
     col = col.obs.transp,
     breaks = break.points,
     horizontal = TRUE)
title(main = "2009 GPS points - surface only", line = 3)
map.scale(x= 17.1, y = 56.9, ratio = FALSE)
plot(gadm, col="grey", bg = NA, add = T)
box(,col="grey50",lwd=2)
axis(side=(2), las=1, col="grey50", col.axis="grey50")
axis(side=(3), las=1, col="grey50", col.axis="grey50")
title(xlab = "Time in grid cell (%)", line = 0)
mtext("Depth (m)",
      side = 4,
      line = 1,
      las = 2,
      at = 57.5)
dev.off()




# Map for difference between years ----

rast.2014 <- time.weight.2014.surface.raster
rast.2014[is.na(time.weight.2014.surface.raster)] <- 0

rast.2009 <- time.weight.2009.surface.raster
rast.2009[is.na(time.weight.2009.surface.raster)] <- 0
dif_raster <- rast.2014 - rast.2009


dif_raster[(dif_raster) == 0] <- NA

col.dif <- c(rev(brewer.pal(7,"Reds"))[3:7], (brewer.pal(9,"Greens"))[c(2:4,8:9)])

col.dif.transp <- addalpha(col.dif, alpha = 0.65)
# range(dif_raster)
break.points <- c(-5:0,0.5,1,1.5,2)

pdf("guillemots_year_dif_bsbd-0.9.3.pdf")
# png("guillemots_year_dif_bsbd-0.9.3.png")
# win.metafile("guillemots_year_dif_bsbd-0.9.3.wmf")
png("guillemots_year_dif_bsbd-0.9.3.png", width = 1500, height = 1500, res = 200)

# ?pdf
# ?png
# ?windows.metafile
# Plot base map
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 5))
plot(gadm, col=NA, bg = NA,xlim = c(16.9, 18.1),
     ylim = c(56.8, 57.65))
plot(bath_raster, colNA = "green", col = bath.col,
     add = TRUE , breaks = bath.break.points,
     xlim = c(16.8, 18.3), ylim = c(56.8, 57.8))
plot(dif_raster, add = T,
     breaks = break.points,
     col = col.dif.transp,
     horizontal = TRUE)
title(main = "Difference 2014-2009 - surface only", line = 3)
map.scale(x= 17.1, y = 56.9, ratio = FALSE)
plot(gadm, col="grey", bg = NA, add = T)
box(,col="grey50",lwd=2)
axis(side=(2), las=1, col="grey50", col.axis="grey50")
axis(side=(3), las=1, col="grey50", col.axis="grey50")
title(xlab = "Difference (2014 vs. 2009): -ve (less)  +ve (more)", line = 0)
mtext("Depth (m)",
      side = 4,
      line = 1,
      las = 2,
      at = 57.5)
dev.off()
