
# Required files -----
# You should have the following files in the folder/ directory
# from which you will work.
# bsbd_raster.RData
# points.2014.all.RData

# Set working-directory -----
# Set working-directory to where above files are located
setwd("...")

# Load in required files ----
# Bathymetry
load('bsbd_raster.RData')

# 2014 GPS data
load('points.2014.all.RData')


# Caculate time intevals for 2014 GPS locations -----

# First filter GPS locations, so we only include
# higher quality (not 'bad_location') GPS locations
# and exclude those with no location ('dive' and 'no_location')
f <- (points.2014.all$type != "bad_location") &
  (points.2014.all$type != "dive") &
  (points.2014.all$type != "no_location")
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

# See what the time intervals look like
# Time interval is in seconds
range(time_interval)
# In seconds
sort(time_interval, decreasing = TRUE)[1:100]
# In hourse
sort(time_interval/(60*60), decreasing = TRUE)[1:100]
# There are some larger time intervals ('gaps') in the
# GPS data.





karls_x <- 17.958252
karls_y <- 57.289848
plot(bsbd_raster,  xlim = c(16.9, 18.5), ylim = c(56.7, 57.8), colNA = "black",
     main = "BSBD bath only")
points(karls_x, karls_y, pch = 4, col = "red", cex = 2)

long_int <- time_interval > 60*60*.5
points(points.2014$long[long_int],
       points.2014$lat[long_int])
median(time_interval)
mean(time_interval)

summary(points.2014$ring_number[long_int])
