# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to look at the 'missing' GPS fixes from the
# data from RAW text files produced
# by igotu2gpx.



# Source functions -----
source("parse_igotu2gpx_txt.R")


# Get some example data -----
points <- parse.file(file = "D:/Dropbox/guillemot_2014_data/igotu2gpx_files/g01.txt")

# Index thing
n <- length(points$lat)
# Make an index
ind <- c(1:n)


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
map.trip <- function(points = points, xlim = NULL, ylim = NULL){
  
  library(maps)
  
  p <- points[points$long != 0,]
  
  # plot base map
  # Set map limits
  c.xlim <- range(p$long)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.12
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
#   xlim = NULL
  
  if(!is.null(xlim)){
    c.xlim <- xlim
  }
  
  c.ylim <- range(p$lat)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.1
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
if(!is.null(ylim)){
  c.ylim <- ylim
}

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




# Aquisition time - vallid GPS -----
# How about aquisition time for when fixes are gained? Is it possible to recognise when the bird was coming up from a dive - thus took longer to gain GPS data, but once at surface did?
str(points)

# Filter for only 'vallid' GPS points
pos <- (points$lat != 0 )
pos_ind <- ind[pos]

# look at this
hist(points$timeout[pos])
hist(points$timeout[pos], xlim = c(0,100), breaks = 40)

summary(as.factor(as.character(points$timeout[pos])))

hist(points$timeout[pos & points$timeout < 100],
     breaks = 40, freq = FALSE)

hist(points$timeout[pos],
     breaks = 40, freq = FALSE)


# ?hist
# Some suggestion of a bimodal distribution with
# aquisition times >20 s being a sepperate distribution
# than those <20 s.

# Map those points that are > 20s
map.trip(points = points)
f <- (points$lat != 0 ) & (points$timeout > 100)
f <- (points$lat != 0 ) & (points$timeout > 20)
f <- (points$lat != 0 ) & (points$timeout > 50)


points(points$lat[f]~points$long[f],
       col = "magenta", pch = 8, cex = 1.2)
# Mainly points at the colony where the single is
# likely to be bad - probably not a useful filtering
# criterion

# Possibly 'dodgy' points could be filtered out by
# timeout, with high values of timeout, say >100 being
# excluded
plot(points$timeout[points$lat != 0] ~ points$lat[points$lat != 0])

map.trip(points = points)
f <- (points$lat != 0 ) & (points$timeout > 100)
points(points$lat[f]~points$long[f],
       col = "green", pch = 8, cex = 1.2)
# This does indeed seem to include both 'dodgy' points
# likely at the colony and those 'dodgy' points some
# way away from the colony.
# We should check EHPE first though - as it's a more
# conventional way to filter points


# EHPE -----
# some possible pattern in 'EHPE' too
# For vallid fixes
summary(as.factor(as.character(points$ehpe[pos])))
hist(points$ehpe[pos])
hist(points$ehpe[pos & points$ehpe < 100], breaks = 40)

hist(points$ehpe[pos & points$ehpe > 100], breaks = 40)
# Nothing obvious or likely useful here. Though few
# point beyond ca. 250, how about filtering these out?

map.trip(points = points[points$long > 17.2,])
map.trip(points = points)

f <- (points$lat != 0 ) & (points$ehpe > 50)
points(points$lat[f]~points$long[f],
       col = "green", pch = 8, cex = 1.2)
f <- (points$lat != 0 ) & ((points$ehpe > 100) | (points$timeout > 100))
points(points$lat[f]~points$long[f],
       col = "orange", pch = 8, cex = 1.2)

summary(f)


# Number of satellites -----
summary(as.factor(as.character(points$sat_n[pos])))
hist(points$sat_n[pos])

map.trip(points = points, xlim = c(17.8,18.0),
         ylim = c(57.4,57.5))

f <- (points$lat != 0 ) & (points$sat_n <= 4)
points(points$lat[f]~points$long[f],
       col = "green", pch = 8, cex = 1.2)
f <- (points$lat != 0 ) & (points$sat_n <= 3)
points(points$lat[f]~points$long[f],
       col = "orange", pch = 8, cex = 1.2)
f <- (points$lat != 0 ) & (points$sat_n <= 2)
points(points$lat[f]~points$long[f],
       col = "magenta", pch = 8, cex = 1.2)
f <- (points$lat != 0 ) & (points$sat_n <= 0)
points(points$lat[f]~points$long[f],
       col = "black", pch = 8, cex = 1.2)



# Good GPS filter summary -----
f0 <- (points$lat != 0 ) & (points$timeout <= 100)
f1 <- (points$lat != 0 ) & (points$ehpe <= 100)
f2 <- f0 & f1
f3 <- f0 | f1

summary(f0)
summary(f1)
summary(f2)
summary(f3)


map.trip(points = points, xlim = c(17.9,18.0),
         ylim = c(57.25,57.3))
map.trip(points = points)

points(points$lat[!f2]~points$long[!f2],
       col = "magenta", pch = 8, cex = 1.2)
points(points$lat[!f3]~points$long[!f3],
       col = "green", pch = 8, cex = 1.2)

col <- (points$long < 17.98) &
  (points$long > 17.94) &
  (points$lat < 57.30) &
  (points$lat > 57.28)

fc0 <- col
fc1 <- col & f2
fc2 <- col & f3

summary(fc0)
summary(fc1)
summary(fc2)


# Unifiltered
map.trip(points = points)

# Colony area
f <- (points$lat != 0 ) & ((points$ehpe > 100) | (points$timeout > 150))
map.trip(points = points[!f,],xlim = c(17.8,18.0),
         ylim = c(57.28,57.35))
f2 <- (points$lat != 0 ) & (points$ehpe > 50) & !f
points(points$lat[f2 ]~points$long[f2 ],
       col = "green", pch = 8, cex = 1.2)


# Whole trip
f <- (points$lat != 0 ) & ((points$ehpe > 100) | (points$timeout > 150))
map.trip(points = points[!f,])
f2 <- (points$lat != 0 ) & (points$ehpe > 50) & !f
points(points$lat[f2 ]~points$long[f2 ],
       col = "green", pch = 8, cex = 1.2)



length(f)
length(f2)

# How many points do we lose by filtering EHPE quite
# drastically, does it mainly only affect those points
# at the colony, which we don't 'need' anyway?
points$ehpe[pos]

# For invallid fixes
no_pos <- points$lat == 0

summary(as.factor(as.character(points$ehpe[no_pos])))
hist(points$ehpe[no_pos])
hist(points$ehpe[no_pos & points$ehpe > 100], breaks = 40)

# Somthing at EHPE = 344.64
# Map these points

map.trip(points)

no_pos_ehpe <- (points$lat == 0 ) & (points$ehpe == 344.64)
no_pos_ind <- ind[no_pos_ehpe]

pre.point <- no_pos_ind + 1
pre.point.f <- pre.point[points$long[pre.point] != 0]

points(points$lat[pre.point.f]~points$long[pre.point.f],
       col = "green", pch = 8, cex = 1.2)
# All at colony, probably in some way related to
# bad reception - not useful.

# Does depend on device/ bird though. Seems that using
# a course filter on EHPE may help - suggest 100 m in
# addition to the timeout filter.

str(points)
# 












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

