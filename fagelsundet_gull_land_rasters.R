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
# hist(time.weight.hg.all.for*100)

# Make raster layers -----
data(SAGA_pal)
source("color_legend_png_fun.R")

# All 'foraging' locations
point_raster_forage <- rasterize(obs.xy.for,
                                 base_raster,
                                 time.weight.hg.all.for,
                                 fun = sum)

range(values(point_raster_forage), na.rm = TRUE)
hist(values(point_raster_forage), breaks = 100,
     col = "dark grey")
hist(values(point_raster_forage), breaks = 100,
     ylim = c(0,50), col = "dark grey")

values(point_raster_forage) <- log10(values(point_raster_forage))

plotKML(point_raster_forage, file = "raster_hg_all.kml",
        min.png.width = (20*612),
        colour_scale = SAGA_pal[[1]])
# sort(time.weight.hg.all.for, decreasing = TRUE)[1:100]

# Improve colour scale
10^(range(values(point_raster_forage),na.rm = TRUE))

color_legend_png(legend.file = "layer_legend.png",
                 ras.val = values(point_raster_forage),
                 zval = c(0.001,0.01,0.1,1,5,15
                 ), dig = 3)

# Top 100
val.100 <- sort(values(point_raster_forage), decreasing = TRUE)[100]
point_raster_forage_100 <- point_raster_forage
point_raster_forage_100[values(point_raster_forage) < val.100] <- NA




plotKML(point_raster_forage_100, file = "point_raster_forage_percent_100.kml",
        min.png.width = (20*612), colour_scale = SAGA_pal[[1]])

10^(range(values(point_raster_forage_100),na.rm = TRUE))
# Improve colour scale
color_legend_png(legend.file = "layer_legend.png",
                 ras.val = values(point_raster_forage_100),
                 zval = c(0.2,0.3,0.5,1,2,5,10,15
                          ), dig = 1)

hist(values(10^point_raster_forage_100), breaks = 40)
sum(10^values(point_raster_forage_100), na.rm = TRUE)


# 6120 HG -----
f2 <- (speed < 4) & (col.dist > 1000) & hg.points$device_info_serial == "6120"
f2[is.na(f2)] <- FALSE
# summary(f2)
obs.xy.for.6120 <- obs.xy[f2,]

time.weight <- 100*((time_interval_narm[f2]) /
                                 (sum(time_interval_narm[f2])))

raster_6120 <- rasterize(obs.xy.for.6120,
                                 base_raster,
                                 time.weight,
                                 fun = sum)


values(raster_6120) <- log10(values(raster_6120))

plotKML(raster_6120, file = "raster_hg_6120_all.kml",
        min.png.width = (20*612),
        colour_scale = SAGA_pal[[1]])
# sort(time.weight.hg.all.for, decreasing = TRUE)[1:100]

# Improve colour scale
10^(range(values(raster_6120),na.rm = TRUE))

color_legend_png(legend.file = "layer_legend.png",
                 ras.val = values(raster_6120),
                 zval = c(0.001,0.01,0.1,1,5,12
                 ), dig = 3)

hist(values(10^raster_6120), breaks = 40, ylim = c(0,50),
     col = "dark grey")



# function wrap ------
make.raster <- function(device_id){
    f2 <- (speed < 4) & (col.dist > 1000) & hg.points$device_info_serial == device_id
    f2[is.na(f2)] <- FALSE
    # summary(f2)
    obs.xy.for.x <- obs.xy[f2,]
    
    time.weight <- 100*((time_interval_narm[f2]) /
                          (sum(time_interval_narm[f2])))
    
    raster_x <- rasterize(obs.xy.for.x,
                             base_raster,
                             time.weight,
                             fun = sum)
    
    
    values(raster_x) <- log10(values(raster_x))
    ?plotKML
    plotKML(raster_x, file = paste("raster_hg_", device_id, "_all.kml", sep = ""),
            min.png.width = (20*612),
            colour_scale = SAGA_pal[[1]])
    # sort(time.weight.hg.all.for, decreasing = TRUE)[1:100]
    
    # Improve colour scale
#     10^(range(values(raster_x),na.rm = TRUE))
    top.val <- floor(10^max(values(raster_x),na.rm = TRUE))
    
    color_legend_png(legend.file = "layer_legend.png",
                     ras.val = values(raster_x),
                     zval = c(0.001,0.01,0.1,1,5,top.val
                     ), dig = 3)
    
    png(paste("raster_hg_", device_id, "_all.png", sep = ""))
    hist(values(10^raster_x), breaks = 40, ylim = c(0,50),
         col = "dark grey", main = paste("Bird ID: ",
                                         device_id, sep = "")
    )
         dev.off()
}    

# Do for each HG -----
make.raster(6123)
make.raster(6128)
make.raster(6149)
make.raster(6150)
make.raster(6151)
make.raster(6156)


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
