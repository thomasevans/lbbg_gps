# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to look at the 'missing' GPS fixes from the
# data from RAW text files produced
# by igotu2gpx.



# Source functions -----
source("parse_igotu2gpx_txt.R")


# Get some example data -----
points <- parse.file(file = "D:/Dropbox/guillemot_2014_data/igotu2gpx_files/g01.txt")

# General idea ----
# Add column to indicate GPS fix type
# 1. fix
# 2. no fix - timed out but had some satellites?
# 3. no fix - timed out quickly, probably no satellites, perhaps underwater?
# 4. no fix - somthing else!

# Check the data to see what we have and what they correspond to
# Where do different 'non-fixes' occur? If at sea, likely diving,
# if at colony some sort of blocking, preening etc

# First look at types of 'missing' fixes -----
# When data is downloaded via @trip only vallid GPS fixes are
# retained. How exactly it decides what are vallid is unclear.
# However one type of invallid GPS fix is where no position
# estimate is gained, then latitude and longitude are both
# recorded as zero.
# Other types of invallid fix could include those where the
# EHPE (precision estimate) is high.



# Base map to plot different types of points on -----
# function to create a simple map, high-lighting flight and non-flight only
map.trip <- function(points = points){
  
  library(maps)
  
  p <- points[points$long != 0,]
  
  # plot base map
  # Set map limits
  c.xlim <- range(p$long)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.12
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(p$lat)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.1
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
  
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="grey70", bg = "gray90",
       #      xlab = "Latitude"
  )
  
  n <- length(p$lat)
  
  # Plot lines between points
  segments(p$long[-1], p$lat[-1],
           p$long[1:n-1], p$lat[1:n-1],
           col = "black", lty = 1, lwd = 1.5)
  
  # Plot points  
  points(p$lat~p$long,
         col = "blue", cex = 0.7)
  
  points(p$lat[p$speed > 5]~
           p$long[p$speed > 5],
         col = "red", cex = 0.7)
  
  
  # Scale bar and axis
  x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
  y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
  map.scale(x,y,ratio = FALSE, col="grey50",col.lab="grey50")
  # ?map.scale
  #   ?map.scale
  box(,col="grey50",lwd=2)
  axis(side=(1), las=1, col="grey50", col.axis="grey50")
  axis(side=(2), las=1, col="grey50", col.axis="grey50")
    
}

map.trip(points)


# Plot points missing location ----
# We can't actually plot their location, but we can hightlight the point before/ after one of these points - here we highlight the point prior
# subset data where latitude is zero
no_pos <- points$lat == 0
points.no_pos <- points[no_pos,]

# See the 'missing' fixes
View(points.no_pos)

# Plot 
n <- length(points$lat)
# Make an index
ind <- c(1:n)
# index of points without vallid GPS
no_pos_ind <- ind[no_pos]
summary(no_pos)

# Just get points before failed fixes which are vallid fixes themselves
# i.e. somtimes invallid points come in sequence, so need to exclude
pre.point <- no_pos_ind - 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

# Plot all 'non-fix' points
points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "orange", pch = 8)




# Plot points by 'timeout' thing ------
# see timeout values for missing fixes
summary(as.factor(as.character(points$timeout[no_pos_ind])))
# There are many at 12 s and 250 s
# Theorise that 12 s corresponds to when no satellite contact
# is gained - i.e. likely underwater.
# While other values are likely when some satellite contact made, but not sufficient.

# Replot map and show '12 s' point locations ----
map.trip(points)

no_pos_12 <- (points$lat == 0 ) & (points$timeout == 12)
no_pos_ind <- ind[no_pos_12]

pre.point <- no_pos_ind - 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "orange", pch = 8, cex = 1.2)
# Mainly likely to be diving locations

# Replot map and show '250 s' point locations ----
map.trip(points)

no_pos_250 <- (points$lat == 0 ) & (points$timeout == 250)
no_pos_ind <- ind[no_pos_250]

pre.point <- no_pos_ind - 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "orange", pch = 8, cex = 1.2)
# Mainly likely apparently colony locations - probably with poor view
# of sky when on ledge



# Replot map and show points neither 12 s nor 250 s point locations ----
map.trip(points)

no_pos_other <- (points$lat == 0 ) & (points$timeout != 250) &
  (points$timeout != 12) 

no_pos_ind <- ind[no_pos_other]

pre.point <- no_pos_ind - 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "orange", pch = 8, cex = 1.2)
# Apparently exclusively colony locations


# Look again at 250 s timeout locations ----
# Plot point after rather than before
map.trip(points)

no_pos_250 <- (points$lat == 0 ) & (points$timeout == 250)
no_pos_ind <- ind[no_pos_250]

pre.point <- no_pos_ind + 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "orange", pch = 8, cex = 1.2)
# Probably the points not at the colony are where the bird is
# returning/ going from colony - where GPS reception lost
# during aquisition - i.e. when the bird is on ledge.



# Filter for timeout thing -----
# Likely diving
# *  long == 0 and timeout == 12 s
# Likely at colony (just arrived at)
# *   long == 0 and timeout != 12
# Other 'real' GPS location
# *   long != 0



# some possible pattern in 'EHPE' too
summary(as.factor(as.character(points.no_pos$ehpe)))


points.no_pos$timeout[points.no_pos$ehpe == 344.64]


f01 <- (points$timeout == 12) & (points$lat == 0)
summary(f01)


f02 <- points$lat == 0
summary(f02)
hist(points$timeout[f02])
summary(points$timeout[f02])
summary(as.factor(as.character(points$timeout[f02])))
f03 <- (points$timeout == 250) & (points$lat == 0)

f04 <- points$lat != 0


summary(as.factor(as.character(points$MSVs_QCN[f03])))
str(points)



ind <- c(1:length(points$timeout))
sum(ind[f03])
pre.point <- ind[f03] - 1
pre.point.real <- points$long[pre.point] != 0
pre.point.new <- pre.point[pre.point.real]

# install.packages("maps")
library(maps)
c.xlim <- range(points$long[f04])
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.15
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(points$lat[f04])
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.15
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Plot base map
load("SWE_adm0.RData")


win.metafile("map.test.1.wmf",width = 7, height = 7)


par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))

plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="black", bg = "grey")


points.sub <- points[points$lat != 0,]
n <- length(points.sub$lat)

points.sub2 <- points[points$lat == 0,]


points(points$lat[pre.point.new]~points$long[pre.point.new],
       col = "red", pch = 16)
segments(points.sub$long[-1], points.sub$lat[-1],
         points.sub$long[1:n-1], points.sub$lat[1:n-1],
         col = "black", lty = 1, lwd = 1)




points(points$lat[points$lat != 0]~points$long[points$lat != 0],
       col = "blue")
points(points$lat[pre.point.new]~points$long[pre.point.new],
       col = "red")

points(points$lat[pre.point.new & points$speed > 5]~
         points$long[pre.point.new & points$speed > 5],
       col = "orange")



hist(points$speed[points$speed > 1], breaks = 40)







# ?jitter
# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE)
#   ?map.scale
box(,col="white",lwd=2)
axis(side=(1),las=1,col="white",col.axis="white")
axis(side=(2),las=1,col="white",col.axis="white")
dev.off()

