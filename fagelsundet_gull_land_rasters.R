# Script to produce raster layers from gull GPS data
# After producing raster layer, layer is output to kml
# for vieweing in Google Earth


# Load/ install require packages -------
# install.packages(c("plotKML","raster","rJava","animation"))

# Load packages
library("plotKML")
library("raster")
library("rJava")
library("animation")

# Load GPS data -----

# From csv
# HG data
hg.points <- read.csv("fagelsundet_hg_data.csv",
                      header = TRUE)

# Fix date-time
hg.points$date_time <-  as.POSIXct(strptime(hg.points$date_time,
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"))


# Sort data-frame by device_info_serial, then by date_time
hg.points <- hg.points[order(hg.points$device_info_serial,
                                    hg.points$date_time), ]


# Calculate time intervals ------
# Calculat the time intervals between GPS points

# First time differences
n         <- length(hg.points$date_time)
time_pre  <- hg.points$date_time[1:(n-1)]
time_next <- hg.points$date_time[2:(n)]

t_diff <- as.numeric(difftime(time_next, time_pre,
                              units = "secs"))

# Give value to first point
t_diff <- c(NA,t_diff)


id_pre <- hg.points$device_info_serial[1:(n-1)]
id_next <- hg.points$device_info_serial[2:(n)]

id.fun <- function(id1, id2){
  if(id1 == id2) return(1) else return(0)
}

id_test <- mapply(id.fun, id1 = id_pre, id2 = id_next)
# summary(as.factor(id_test))
id_test <- c(0,id_test)

t_diff[id_test == 0] <- NA


hist(t_diff)
range(t_diff, na.rm = TRUE)

# median(t_diff, na.rm = TRUE)


# sort(t_diff, decreasing = FALSE)[1:100]

# There are some longer time intervals (not many)
# Here I replace time intervals longer than 1850 s (just over 30 minutes)
# with NA values - i.e. giving these points zero weight
sort(t_diff, decreasing = TRUE)[1:100]
time_interval <- t_diff
time_interval[t_diff > 1850] <- NA

hist(time_interval)


time_interval_narm <- time_interval

# Replace NAs with 0 (i.e. zero weight)
time_interval_narm[is.na(time_interval)] <- 0



# Calculate distances -----
# distance from colony

# colony location
col_loc <- cbind(17.929104, 60.630572)

# Previously saved function
source("deg.dist.R")

# Run for all points
col.dist <- 1000*deg.dist(col_loc[1],col_loc[2],
                          hg.points$longitude,
                          hg.points$latitude)


# Set-up base-raster layer -----
# Encompases most of range of GPS points
# Aprox. a 500m grid
base_raster <- raster(nrows = 305, ncols = 563,
                      xmn = 15, xmx = 20.5,
                      ymn = 59.5, ymx = 61.0,
)


# Extract XY location data
obs.xy <- cbind(hg.points$longitude, hg.points$latitude)


# Filter data -----

# Make speed into numeric vector
speed <- as.numeric(as.character(hg.points$speed))


# Non-flight and not-colony
summary(speed)
f <- (speed < 4) & (col.dist > 1000)
f[is.na(f)] <- FALSE

obs.xy.for <- obs.xy[f,]


time.weight.hg.all.for <- 100*((time_interval_narm[f]) /
  (sum(time_interval_narm[f])))

head(time.weight.hg.all.for)
hist(time.weight.hg.all.for)

# Make raster layers -----

# All 'foraging' locations
point_raster_forage <- rasterize(obs.xy.for,
                                 base_raster,
                                 time.weight.hg.all.for,
                                 fun = sum)

range(values(point_raster_forage), na.rm = TRUE)
hist(values(point_raster_forage), breaks = 100)
hist(values(point_raster_forage), breaks = 100,
     ylim = c(0,50))



plotKML(point_raster_forage, file = "point_raster_forage_percent.kml",
        min.png.width = (10*612))
# sort(time.weight.hg.all.for, decreasing = TRUE)[1:100]


# Top 100
val.100 <- sort(values(point_raster_forage), decreasing = TRUE)[100]
point_raster_forage_100 <- point_raster_forage
point_raster_forage_100[values(point_raster_forage) < val.100] <- NA

source("color_legend_png_fun.R")

values(point_raster_forage_100) <- log10(values(point_raster_forage_100))
data(SAGA_pal)

plotKML(point_raster_forage_100, file = "point_raster_forage_percent_100.kml",
        min.png.width = (10*612), colour_scale = SAGA_pal[[1]])

10^(range(values(point_raster_forage_100),na.rm = TRUE))
# Improve colour scale
color_legend_png(legend.file = "layer_legend.png",
                 ras.val = values(point_raster_forage_100),
                 zval = c(0.2,0.3,0.5,1,2,5,10,15
                          ))

hist(values(10^point_raster_forage_100), breaks = 40)
sum(10^values(point_raster_forage_100), na.rm = TRUE)


# 6120 HG example -----
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


# Output raster layers to kml files -----





# Get points ----------
coordinates(raster_layer)
values(raster_layer)

# Could summarise by number of birds, species present, individual IDs
# etc. making new raster layers for each to extract info - then 
# combining to a data.frame



# Put multiple rasters into a kml single file ----
stacked.ras <- stack(list(point_raster_forage, point_raster_forage_100))

# Doesn't work at the moment
kml_layer.Raster(stacked.ras, file = "test_stack.kml",
        min.png.width = (10*612),
        colour= "black")

# plot(stacked.ras)
