# For 2009 and 2014 guillemot GPS data produce
# raster layers for area use weighted by time in
# raster tile.
# Then calculate difference between the two rasters
# to show how area use differs between the two years.
# Plot all this and save to a PDF file.



# Load bathymetric data
load('bsbd_raster.RData')

# Load 2014 data
# link to database

# all 2014 data from R object
load("guillemot_trip_classification_data.RData")

points.2014.all <- points_all
save(points.2014.all, file = "points.2014.all.RData")
# ?save
# Use points_all table

# Filter out bad points and diving points etc.
levels(points_all$type)

f <- (points_all$type != "bad_location") &
      (points_all$type != "dive") &
      (points_all$type != "no_location")
f[is.na(f)] <- TRUE

points.2014 <- points_all[f,]
# i <- 5
time_interval <- NULL
time_interval[1] <- 0

# str(points_all)

new_device <- NULL
  t <- 1
# Calculate time between points
for(i in 2:(length(points.2014$type)-1)){
  if(points.2014$ring_number[i] !=
       points.2014$ring_number[i-1]){
         time_interval[i] <- 0
         new_device[t] <- i
         t <- t + 1
       } else if(points.2014$ring_number[i] !=
                   points.2014$ring_number[i+1]){
         time_interval[i] <- 0} else{
           time_interval[i] <- as.numeric(
              difftime(
                points.2014$date_time[i-1],
                points.2014$date_time[i],
                 units = "secs")
           )
         }
}
time_interval[length(points.2014$type)] <- 0
# ?difftime
new_device
time_interval <- time_interval * -1
range(time_interval)
hist(time_interval, xlim = c(0,2000), breaks = 1000)
# str(points_all$date_time)
sort(time_interval, decreasing = TRUE)[1:100]
sort(time_interval, decreasing = FALSE)[1:100]
summary(time_interval > 60*30)
# 58554/60/60

# ?sort
# time_interval[new_device[1]+1]

# Load 2009 data (maybe make new 'temp' table
# in DB first)

# Filter GPS data by quality criteria + distance
# from colony

# For 2014 data
# Exclude points that will have very high weight, where time
# interval is greater than 35 minutes
f2 <- time_interval < 60*35
# str(points.2014)
# Exlude points at or near the colony (withing 500 m buffer)
f3 <- (points.2014$coldist) > 500

f4 <- points.2014$device_info_serial != "5113"
summary(f2 & f3 & f4)

points.2014 <- cbind(points.2014, time_interval)

points.2014.f <- points.2014[(f2 & f3),]

# Weight criterion
tot_time <- sum(points.2014.f$time_interval)
point_weight <- points.2014.f$time_interval/tot_time
hist(point_weight)


# Map the bathymetry data -----
# Load required packages
library(RColorBrewer)
library(raster)


# Example figure
# Colour palette to diplay bathymetric data
col.bath <- c(rev(brewer.pal(9,"Reds"))[1:5], rev(brewer.pal(9,"Blues")), "black")

col.obs <- brewer.pal(9,"Greys")


# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

col.obs.transp <- addalpha(col.obs, alpha = 0.85)


karls_x <- 17.958252
karls_y <- 57.289848



plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath only")
points(karls_x, karls_y, pch = 4, col = "yellow", cex = 2)

# If you would like to add a scale you can use function 'map.scale'
# from the 'maps' package
library(maps)
map.scale(x= 17, y = 56.9, ratio = FALSE)



# Produce raster layers for both years using
# points in raster thing

# For 2014
xy <- cbind(points.2014.f$long, 
            points.2014.f$lat)

# ?rasterize
# test <- rasterize(xy,x, fun=function(x,...)length(x))


test <- rasterize(xy,bsbd_raster, point_weight*100, fun = sum)
plot(test, col = col.obs,  add = T,
     horizontal = TRUE)


# Map of this
# points(xy,
#        cex = 0.5, col = "black", alpha = 0.5)
pdf("2014_time_weighted_4.pdf")
plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "2014 - weighted by time")
plot(test, col = col.obs.transp,  add = T,
     horizontal = TRUE)
points(karls_x, karls_y, pch = 4, col = "red", cex = 2)
map.scale(x= 17, y = 56.9, ratio = FALSE)
dev.off()

# range(test)



plot(bsbd_raster, col = col.bath, xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath only")

test <- rasterize(xy,bsbd_raster, point_weight, fun = sum)
plot(test, col = col.obs,  add = T)


# Make a difference raster, by subtracting one
# from the other


# Plot all of the above on a map