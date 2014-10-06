
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
# In hours
sort(time_interval/(60*60), decreasing = TRUE)[1:100]
# There are some larger time intervals ('gaps') in the
# GPS data.
# Looking into these it turned out that the longest intervals
# are nearly all of points either at the colony or the gap
# between the last position at the colony, and the first point
# away from the colony.
# This is most likely owning to poor GPS reception at the
# colony, with the high cliff, and the murre-lab above both
# obscurring the view of the sky - leading to more failed
# fixes.

# When weighting data by time (total time in cell), suggest
# that we use mean time after removing time intervals greater
# than some threshold for points with longer time - so as
# not to give greater weight to these points




