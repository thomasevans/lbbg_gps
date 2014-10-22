


# Import required data ------

# From csv
# HG data
hg.points <- read.csv("fagelsundet_hg_data.csv",
                      header = TRUE)


# Required library ----
# install.packages("plotKML")
# install.packages("raster") 

library(plotKML)
library(raster)
# install.packages("rJava")
# str(hg.points)
# remove.packages("raster") 

# Sys.getenv("JAVA_HOME")

obs.xy <- cbind(hg.points$longitude, hg.points$latitude)

# extent(xy_loc)
# plot(xy_loc)

# Base raster layer -----
# Encompases most of range of GPS points
# Aprox. a 500m grid
base_raster <- raster(nrows = 338, ncols = 612,
                      xmn = 15, xmx = 20.5,
                      ymn = 59.5, ymx = 61.0,
                      )


base_raster_2 <- raster(nrows = (473), ncols = (857),
                      xmn = 15, xmx = 20.5,
                      ymn = 59.5, ymx = 61.0,
)
# res(base_raster)
# res(base_raster_2)
# Rasterize - all location -----
point_raster <- rasterize(obs.xy,
                          base_raster,
                           1,
                           fun = sum)

plot(point_raster)


# Calculate distances -----
# distance from colony

# colony location
col_loc <- cbind(17.929104, 60.630572)

# Previously saved function
source("deg.dist.R")

# Run for all points
col.dist <- 1000*deg.dist(col_loc[1],col_loc[2], obs.xy[,1], obs.xy[,2])

hist(col.dist[col.dist < 5000], breaks = 100)

# View speeds ------
hist(as.numeric(as.character(hg.points$speed)),
     xlim = c(0,50), breaks = 1000, ylim = c(0,50000))
abline(v = 4, lwd = 2, lty = 2, col = "red")
str(hg.points)


# Filter data -----
# Non-flight and not-colony
speed <- as.numeric(as.character(hg.points$speed))
summary(speed)
f <- (speed < 4) & (col.dist > 1000)
f[is.na(f)] <- FALSE

obs.xy.for <- obs.xy[f,]

# Make rasters -----
# All 'foraging' locations
point_raster_forage <- rasterize(obs.xy.for,
                          base_raster,
                          1,
                          fun = sum)
range(point_raster_forage)
plot(point_raster_forage)
KML(point_raster_forage, file = "raster_kml_test.kml",
    blur = 5,  col=rev(terrain.colors(255)))



point_raster_forage_2 <- rasterize(obs.xy.for,
                                 base_raster_2,
                                 1,
                                 fun = sum)
# range(point_raster_forage)
# plot(point_raster_forage)
# ncol(point_raster_forage_2)
KML(point_raster_forage_2, file = "raster_kml_test_2.kml",
    blur = 20,  col=rev(terrain.colors(255)))


?kml

install.packages("plotKML")
library(plotKML)

val.vec <- point_raster_forage_2@data@values
str(val.vec)

val.vec[is.na(val.vec)] <- 0

# plot(point_raster_forage_2)
# str(point_raster_forage_2)
x <- "red"
kml(point_raster_forage_2, file = "raster_kml_test_2_plotkml.kml",
    colour = val.vec)

point_raster_forage_2@data
?plotKML()



point_raster_forage_high <- point_raster_forage
point_raster_forage_high[point_raster_forage_high < 5] <- NA
KML(point_raster_forage_high, file = "raster_kml_test_high.kml",
    blur = 10,  col=rev(terrain.colors(255)))

point.val <- (point_raster_forage_high@data@values)
hist(point.val, breaks = 40)
range((point.val), na.rm = TRUE)
# # ?range
hist(log(point.val))
# 
# point_raster_forage_high@data@values <- log(point.val)
# summary(point_raster_forage_high@data@values)
# length(point_raster_forage_high@data@values)

# ?sum
# layer with log-values -----
log.sum <- function(x){
  sum.x <- sum(x)
  return(log(sum.x))
}
# log.sum(100000)

point_raster_forage_log <- rasterize(obs.xy.for,
                                 base_raster,
                                 1,
                                 fun = log.sum)
# ?rasterize
KML(point_raster_forage_log, file = "point_raster_forage_log.kml",
    blur = 10,  col=rev(terrain.colors(255)))



# Individual kml rasters ------
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






# ?rasterize

# ?rasterize
KML(point_raster_forage_6123, file = "point_raster_forage_6123.kml",
    blur = 20,  col=rev(terrain.colors(255)))

?plotKML

?SpatialPoints

points.6123 <- SpatialPoints(obs.xy.for.6123,
                             proj4string=CRS("+proj=longlat +datum=WGS84"))


proj(points.6123)
points.6123.df <- SpatialPointsDataFrame(points.6123, data = hg.points[f2,])
# CRS(points.6123.df)
plotKML(points.6123.df, file = "points.6123.kml")

point_raster_forage_log <- point_raster_forage
values(point_raster_forage_log) <- log(values(point_raster_forage_log))
plotKML(point_raster_forage_log, file = "test.plotkml.log.3.kml",
        min.png.width = (10*612))
# ?plotKML
# 338, ncols = 612

# install.packages("animation")
