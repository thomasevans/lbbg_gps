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

# CG data
hg.points <- read.csv("fagelsundet_cg_data.csv",
                      header = TRUE)

# GBBG data
hg.points <- read.csv("fagelsundet_gbbg_data.csv",
                      header = TRUE)

# Fix date-time
hg.points$date_time <-  as.POSIXct(strptime(hg.points$date_time,
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"))



# For GBBG data - correct data format
hg.points$latitude <- as.numeric(as.character(hg.points$latitude))
hg.points$longitude <- as.numeric(as.character(hg.points$longitude))
hg.points$speed     <- as.numeric(as.character(hg.points$speed))

str(hg.points)

# Sort data-frame by device_info_serial, then by date_time
hg.points <- hg.points[order(hg.points$device_info_serial,
                                    hg.points$date_time), ]

# For GBBG data specifically, retain only 2014 data
date.min <- as.POSIXct("2014-05-15 00:00:00", tz = "UTC")
hg.points <- hg.points[!is.na(hg.points$speed),]
hg.points <- hg.points[hg.points$date_time > date.min, ]


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
summary(time_interval)
length(time_interval)
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

# source("col.dist.fun.R")

# If want to use actual nest location
# Get nest locations
device_id <- unique(hg.points$device_info_serial)



library("RODBC")
# vignette("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Nest location (actually start location)
g <- data.frame(device_info_serial=numeric(),
                start_latitude = numeric(),
                start_longitude = numeric())
for(i in 1:length(device_id)){
g[i,] <- sqlQuery(gps.db,
              query = paste("SELECT gps_ee_track_session_limited_local.device_info_serial, gps_ee_track_session_limited_local.start_latitude, gps_ee_track_session_limited_local.start_longitude
          FROM gps_ee_track_session_limited_local
          WHERE (((gps_ee_track_session_limited_local.device_info_serial)= ",
                            device_id[i], "));", sep = ""),
              as.is = TRUE)
}

close(gps.db)

# g
# s_long <- as.numeric(g$start_longitude[1])
# s_lat  <- as.numeric(g$start_latitude[1])

bird.dist <- function(device_id,lat,long, z, ...){
  source("deg.dist.R")
  x <- as.data.frame(z[1])
  lat.1 <- as.numeric(x[x$device_info_serial == device_id,2])
  long.1 <- as.numeric(x[x$device_info_serial == device_id,3])
  dist <- deg.dist(long.1,lat.1,
                   long, lat, km = FALSE) 
  return(unlist(dist)[1])
}
# g

# x <- list(g)
# str(unlist(x))
# str(as.data.frame(x[1]))
col.dist <- NULL
# str(col.dist)
# col.dist <- mapply(bird.dist, lat = hg.points$latitude,
#                    long = hg.points$longitude,
#                    device_id = hg.points$device_info_serial,
#                    MoreArgs = (list(z = list(g)))
#                    )
# head(col.dist)
# str(col.dist)
# bird.dist(lat = as.numeric(hg.points$latitude[1]),
#           long = as.numeric(hg.points$longitude[1]),
#           device_id = hg.points$device_info_serial[1],
#           z = list(g))

# [1:100]

col.dist <- mapply(bird.dist,
                   lat = as.numeric(hg.points$latitude),
                   long = as.numeric(hg.points$longitude),
                   device_id = hg.points$device_info_serial,
                   MoreArgs = (list(z = list(g))))


# Run for all points
col.dist <- 1000*deg.dist(col_loc[1],col_loc[2],
                          hg.points$longitude,
                          hg.points$latitude)


col.dist <- col.dist*1000
hist(col.dist)

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
# summary(speed)
f <- (speed < 4) & (col.dist > 1000)
# summary(speed < 4)
# summary(col.dist > 1000)
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

# summary(is.na(values(point_raster_forage)))

x <- values(point_raster_forage)[!is.na(values(point_raster_forage))]
head(x)

range(values(point_raster_forage), na.rm = TRUE)







hist(values(point_raster_forage), breaks = 100,
     col = "dark grey")
hist(values(point_raster_forage), breaks = 100,
     ylim = c(0,50), col = "dark grey")

values(point_raster_forage) <- log10(values(point_raster_forage))

plotKML(point_raster_forage, file = "raster_cg_all.kml",
        min.png.width = (20*612),
        colour_scale = SAGA_pal[[1]])
# sort(time.weight.hg.all.for, decreasing = TRUE)[1:100]

# Improve colour scale
10^(range(values(point_raster_forage),na.rm = TRUE))


color_legend_png(legend.file = "layer_legend.png",
                 ras.val = values(point_raster_forage),
                  dig = 3)
# hist(values(point_raster_forage))


z.min <- 10^(min(values(point_raster_forage), na.rm = TRUE) - 0.1)
z.max <- 10^(max(values(point_raster_forage), na.rm = TRUE) + 0.1)

color_legend_png(legend.file = "layer_legend.png",
                 ras.val = (values(point_raster_forage)),
                 zval = c(0.001,0.01,0.1,1,5,10,17
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
                 zval = c(0.05,0.1,0.5,1,2,5,10,17
                          ), dig = 2)

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
#     ?plotKML
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

# For each CG -----
make.raster(2090)
make.raster(2082)
make.raster(2095)

# For each GBBG -----
make.raster(6121)
make.raster(821)
make.raster(6155)




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




# Comulative values
x <- c(0:100)
plot(x)
x.ran <- sample(x,100)
plot(x.ran)

x.sort <- sort(x.ran)
plot(x.sort)

?cumsum
x.cumsum <- cumsum(x.sort)

plot(x.cumsum)

f <- x.cumsum < 100
f
# Number of true values
sum(f)

# Then can filter vector by:
x < x.sort[sum(f)]


# Plot of cumulative % for all sites
plot(x.cumsum)
# Colour points that are less than 75%
points(x.cumsum[x.cumsum > 100], col = "red")
abline(h = 100, lwd = 2, lty = 2)


# Only 75% top sites
plot(x.cumsum[x.cumsum < 100])





# Making plots of cumulative percentage -------
# Cumulative thing
val.sort <- sort(values(point_raster_forage), decreasing = TRUE)
head(val.sort)
plot(val.sort)
val.cumsum <- c(0,cumsum(val.sort))

ob.num <- c(0:(length(val.cumsum)-1))
# Cumulative percentage plot
plot(val.cumsum ~ ob.num, ylim = c(0,100),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1)
# length(ob.num)
# length(val.cumsum)
# Colour points that are less than 75%
points(val.cumsum[val.cumsum > 75]~ ob.num[val.cumsum > 75], col = "red")
abline(h = 75, lwd = 2, lty = 2)
n_loc <- length(val.cumsum[val.cumsum < 75])
mtext(paste(n_loc, " locations represent 75 % of foraging time", sep = ""),
      side = 3, line = 2)

plot(val.cumsum[val.cumsum < 75]~ ob.num[val.cumsum < 75], ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "s")
lines(val.cumsum[val.cumsum > 75]~ ob.num[val.cumsum > 75],
      type = "s", lty = 3)
abline(h = 75, lwd = 1.5, lty = 2)


plot(val.cumsum[val.cumsum < 75]~ ob.num[val.cumsum < 75], ylim = c(0,100),
     xlim = c(0,length(val.cumsum)),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "s")
lines(val.cumsum[val.cumsum > 75]~ ob.num[val.cumsum > 75],
      type = "s", lty = 3)
abline(h = 75, lwd = 1.5, lty = 2)


# ?mtext
# val.cumsum

# Cumsum thing -------


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





# Extract XY location data
obs.xy <- cbind(hg.points$longitude, hg.points$latitude)

# Make speed into numeric vector
speed <- as.numeric(as.character(hg.points$speed))

# xmn = 15, xmx = 20.5,
# ymn = 59.5, ymx = 61.0
# Non-flight and not-colony
f <- ((speed < 4) & (col.dist > 1000) & ((hg.points$longitude > 15) &
                                          (hg.points$longitude < 20.5)) & 
                                  ((hg.points$latitude > 59.5) &
                                  (hg.points$longitude < 61.0)))
f[is.na(f)] <- FALSE


# Make rasters for each bird:
birds <- unique(hg.points$device_info_serial)

# Raster for all first
obs.xy.for <- obs.xy[f,]

time.weight.hg.all.for <- 100*((time_interval_narm[f]) /
                                 (sum(time_interval_narm[f])))

point_raster_forage <- rasterize(obs.xy.for,
                                 base_raster,
                                 time.weight.hg.all.for,
                                 fun = sum)
ras_birds <- list()

# Raster for each bird
# i <- 8
# 
# sum.na <- function(x){
#   x.new <- x[!is.na(x)]
#   sum(x.new)
# }

for(i in 1:length(birds)){
  bird_id <- birds[i]
  f2 <- f & (hg.points$device_info_serial == bird_id)
  obs.xy.for <- obs.xy[f2,]
  time.weight <- 100*((time_interval_narm[f2]) /
                                   (sum(time_interval_narm[f2])))
  sum(time.weight)
  ras_birds[i] <- rasterize(obs.xy.for,
                                   base_raster,
                                   time.weight,
                                   fun = sum)
}
# str(ras_birds)
# ?rasterize
# length(time.weight)

# sum(values(ras_birds[[i]]), na.rm = TRUE)
# ?sum
values_birds <- list()

for(i in 1:length(birds)){
  values_birds[i] <- list(values(ras_birds[[i]]))
}

values_birds_order <- list()
for(i in 1:length(birds)){
  values_birds_order[i] <- list(sort(values_birds[[i]], decreasing = TRUE))
}

# values_birds_order[[1]][1:100]

values_birds_order_cumsum <- list()
for(i in 1:length(birds)){
  values_birds_order_cumsum[i] <- list(cumsum(values_birds_order[[i]]))
}
# values_birds_order_cumsum[[1]][1:100]



val.sort <- sort(values(point_raster_forage), decreasing = TRUE)
val.cumsum <- cumsum(val.sort)


# Plots
# pdf("GBBG_cumsum.pdf")
png("GBBG_cumsum.png",  width = 2000, height = 1200, res = 200)
par(mfrow = c(1,2))

ob.num <- c(1:length(val.cumsum))

plot(val.cumsum~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n")

# ?rainbow
cols <- rainbow(length(birds))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(birds)){
  ob.num <- c(1:length(values_birds_order_cumsum[[i]]))
  x <- c(0,ob.num)
  y <- c(0,values_birds_order_cumsum[[i]])
  colx <- cols[i]
  lines( y ~ x ,
        type = "l",
        col = col.obs.transp[i],
        lwd = 2)
}

ob.num <- c(1:length(val.cumsum))
lines(c(0,val.cumsum) ~ c(0,ob.num),
      type = "l", lty = 1, lwd = 2,
      col = addalpha("black", alpha = 0.60))
# ??color
abline(h = 75, lwd = 2, lty = 2)


# 2nd plot
ob.num <- c(1:length(val.cumsum))

plot(val.cumsum~ ob.num, ylim = c(0,100),
     xlim = c(0,1000),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n")

# ?rainbow
cols <- rainbow(length(birds))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(birds)){
  ob.num <- c(1:length(values_birds_order_cumsum[[i]]))
  x <- c(0,ob.num)
  y <- c(0,values_birds_order_cumsum[[i]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
         lwd = 2)
}

ob.num <- c(1:length(val.cumsum))
lines(c(0,val.cumsum) ~ c(0,ob.num),
      type = "l", lty = 1, lwd = 2,
      col = addalpha("black", alpha = 0.60))
# ??color
abline(h = 75, lwd = 2, lty = 2)

legend(x = 500, y = 73,
       legend = c("All",(as.character(birds))),
#        cex = 0.5,
       lty = 1,
       col = c("black",col.obs.transp),
       lwd = 3)

dev.off()





plot(val.cumsum[val.cumsum < 75]~ ob.num[val.cumsum < 75], ylim = c(0,100),
     xlim = c(0,length(val.cumsum)),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n")
lines(val.cumsum ~ ob.num,
      type = "s", lty = 1)
abline(h = 75, lwd = 1.5, lty = 2)


