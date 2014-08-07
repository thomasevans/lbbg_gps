# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to look at the 'missing' GPS fixes from the
# data from RAW text files produced
# by igotu2gpx.




source("parse_igotu2gpx_txt.R")





points <- parse.file(file = "D:/Dropbox/guillemot_2014_data/igotu2gpx_files/g01.txt")







# Add column to indicate GPS fix type
# 1. fix
# 2. no fix - timed out but had some satellites?
# 3. no fix - timed out quickly, probably no satellites, perhaps underwater?
# 4. no fix - somthing else!

# Check the data to see what we have and what they correspond to
# Where do different 'non-fixes' occur? If at sea, likely diving,
# if at colony some sort of blocking, preening etc


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

