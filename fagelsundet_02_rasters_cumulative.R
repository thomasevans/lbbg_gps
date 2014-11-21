# Make rasters for different individual gulls, and species
# Ouput to KML files
# Make histogrammes to show distribution of extensive
# vs. spatially restricted foraging
# Calculate cumulative % things as before, then plot
# Calculate cumulative % thing by time periods, then plot.




# Required packages ------
library("RODBC")
library("sp")
# library("rgdal")
# library("rgeos")
library("raster")
# library("compiler")

# Read in data from DB -----
# Make connection to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# GPS data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.*, d.*
                    FROM fagelsundet_gulls_2014_gps_data AS g, fagelsundet_gulls_2014_gps_data_info as d
                    WHERE g.device_info_serial = d.device_info_serial AND
                    g.date_time = d.date_time
                    ORDER BY g.device_info_serial ASC, g.date_time ASC;"
                       ,as.is = TRUE)

# Check data structure
str(gps.points)


# Fix date_time
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))

# Fix logical (boolean) variables to TRUE/ FALSE from character types
gps.points$on_trip <- as.logical(gps.points$on_trip)
gps.points$flight_point <- as.logical(gps.points$flight_point)
gps.points$on_land <- as.logical(gps.points$on_land)


# Include study period data only ------
# Start date
s.date <- as.POSIXct(strptime("2014-05-15 00:00:00",
                              format = "%Y-%m-%d %H:%M:%S"),
                     tz = "UTC")
# End date
e.date <- as.POSIXct(strptime("2014-07-15 00:00:00",
                              format = "%Y-%m-%d %H:%M:%S"),
                     tz = "UTC")
# Filter data by these criteria
f <- (gps.points$date_time > s.date) & (gps.points$date_time < e.date)

summary(f)

# New data frame of just these points
gps.2014 <- gps.points[f,]


# Base-raster layer and extent -----

# Distance function
source("deg.dist.R")

# Distances (to calculate number of rows and columns)
x1 <- 14
x2 <- 20.5
y1 <- 58
y2 <- 62

# Length of sides
left.side <- deg.dist(x1,y1,x1,y2, km = FALSE)
right.side <- deg.dist(x2,y1,x2,y2, km = FALSE)
top.side <- deg.dist(x1,y2,x2,y2, km = FALSE)
bottom.side <- deg.dist(x1,y1,x2,y1, km = FALSE)

# Number of rows and columns to get aproximate 500 m cell sizes
num.rows <- round(((left.side + right.side)/2)/500)
num.cols <- round(((top.side + bottom.side)/2)/500)

# Make empty raster as base
base_raster <- raster(nrows = num.rows, ncols = num.cols,
                      xmn = 14, xmx = 20.5,
                      ymn = 58, ymx = 62
)
names(gps.2014)


# Filters for individuals + species ----
# First make dataframe of devices and species present in the data set
devices_unique_df <- unique(data.frame(gps.2014$device_info_serial, gps.2014$sp_lat))
row.names(devices_unique_df) <- NULL
names(devices_unique_df) <- c("device_info_serial", "sp_lat")


# Individual filters ------
filter_devices <- list()
n <- length(unique(devices_unique_df$device_info_serial))
for(i in 1:n){
  filter_devices[[i]] <- (gps.2014$device_info_serial == devices_unique_df$device_info_serial[i]) & (gps.2014$on_trip == TRUE) & (gps.2014$flight_point == FALSE)  
}

str(filter_devices[[c(1,5)]])
test <- filter_devices[[1]] | filter_devices[[3]]
summary(test)




# Species filters ------

idx <- c(1:length(devices_unique_df$device_info_serial))

# HG - all
hgs <- idx[devices_unique_df$sp_lat == "Larus argentatus"]

hg.filter <- filter_devices[[hgs[1]]]
for(i in 2:length(hgs)){
  hg.filter <- hg.filter | filter_devices[[hgs[i]]]
}
summary(hg.filter)

# CG - all
cgs <- idx[devices_unique_df$sp_lat == "Larus canus"]

cg.filter <- filter_devices[[cgs[1]]]
for(i in 2:length(cgs)){
  cg.filter <- cg.filter | filter_devices[[cgs[i]]]
}
summary(cg.filter)

# GBBG - all
ggs <- idx[devices_unique_df$sp_lat == "Larus marinus"]

gg.filter <- filter_devices[[ggs[1]]]
for(i in 2:length(ggs)){
  gg.filter <- gg.filter | filter_devices[[ggs[i]]]
}
summary(gg.filter)



# Period filters ------
# Can combine with any of the above
# Dates
period.date <- as.POSIXct(strptime(c("2014-05-15 00:00:00",
                                     "2014-06-01 00:00:00",
                                     "2014-06-15 00:00:00",
                                     "2014-07-01 00:00:00",
                                     "2014-07-15 00:00:00"),
                              format = "%Y-%m-%d %H:%M:%S"),
                     tz = "UTC")
# Filter data by these criteria
period.filters <- list()
for(i in 1:4){
  period.filters[[i]] <- (gps.2014$date_time > period.date[i]) & (
    gps.2014$date_time < period.date[i+1])
}
period.names <- c("May 2", "June 1", "June 2", "July 1")

names(gps.2014)


# Produce rasters ------
make.raster <- function(fx, gps.data = gps.2014, base_raster = base_raster){
 
  require("raster")
  
  gps.f <- gps.data[fx,]
  
  obs.xy <- cbind(gps.f$longitude, gps.f$latitude)
  
  
  # Replace long time intervals with NA values
  time_interval_narm <- gps.f$t_diff
  time_interval_narm[gps.f$t_diff > 1850] <- NA
  
  # Replace NAs with 0 (i.e. zero weight)
  time_interval_narm[is.na(time_interval_narm)] <- 0
  
  

  
  time.weight <- 100*(time_interval_narm /
                        (sum(time_interval_narm)))
  
  raster_x <- rasterize(obs.xy,
                        base_raster,
                        time.weight,
                        fun = sum)
  return(raster_x)
}   


# For each bird for each period

n <- length(devices_unique_df$device_info_serial)

# All periods
ras_birds <- list()
for(i in 1:n){
  f1 <- filter_devices[[i]]
  ras_birds[i] <- make.raster(fx = f1,
                              gps.data = gps.2014,
                              base_raster = base_raster)
}

# Period # 1
ras_birds_per_01 <- list()
for(i in 1:n){
  f1 <- (filter_devices[[i]] & period.filters[[1]])
  if(sum(f1, na.rm = TRUE) < 1){ras_birds_per_01[i] <- NA} else {
    ras_birds_per_01[i] <- make.raster(fx = f1,
                                       gps.data = gps.2014,
                                       base_raster = base_raster)
  }  
}

# Period # 2
ras_birds_per_02 <- list()
for(i in 1:n){
  f1 <- (filter_devices[[i]] & period.filters[[2]])
  if(sum(f1, na.rm = TRUE) < 1){ras_birds_per_02[i] <- NA} else {
    ras_birds_per_02[i] <- make.raster(fx = f1,
                                       gps.data = gps.2014,
                                       base_raster = base_raster)
  }  
}

# Period # 3
ras_birds_per_03 <- list()
for(i in 1:n){
  f1 <- (filter_devices[[i]] & period.filters[[3]])
  if(sum(f1, na.rm = TRUE) < 1){ras_birds_per_03[i] <- NA} else {
    ras_birds_per_03[i] <- make.raster(fx = f1,
                                       gps.data = gps.2014,
                                       base_raster = base_raster)
  }  
}

# Period # 4
ras_birds_per_04 <- list()
for(i in 1:n){
  f1 <- (filter_devices[[i]] & period.filters[[4]])
  if(sum(f1, na.rm = TRUE) < 1){ras_birds_per_04[i] <- NA} else {
    ras_birds_per_04[i] <- make.raster(fx = f1,
                                       gps.data = gps.2014,
                                       base_raster = base_raster)
  }  
}



# Output rasters to KML files ------
# Take previously produced raster files and output to KML files
# for viewing in Google Earth.


# Cumulative % by period and individual -------

# Function to get cumulative percentages
# Take in rasters
# Output cumulative values
raster.cumsum.fun <- function(x){
  if(length(x) != 642580){return(NA)}else{
    val.sort <- sort(values(x), decreasing = TRUE)
    return(cumsum(val.sort))
  }
}


# Loop through all individuals
# For each individual, and each period within individual run
# the cumulative % thing and output to a list

cumsum_period_all <- list()

for(i in 1:n){
  cumsum_period_all[i] <- list(raster.cumsum.fun(ras_birds[[i]]))
}

cumsum_period_01 <- list()

for(i in 1:n){
  cumsum_period_01[i] <- list(raster.cumsum.fun(ras_birds_per_01[[i]]))
}


cumsum_period_02 <- list()

for(i in 1:n){
  cumsum_period_02[i] <- list(raster.cumsum.fun(ras_birds_per_02[[i]]))
}


cumsum_period_03 <- list()

for(i in 1:n){
  cumsum_period_03[i] <- list(raster.cumsum.fun(ras_birds_per_03[[i]]))
}

cumsum_period_04 <- list()

for(i in 1:n){
  cumsum_period_04[i] <- list(raster.cumsum.fun(ras_birds_per_04[[i]]))
}


# Plots for cumulative % thing ------

# Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha = 1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# GBBG figure -----
png("GBBG_cumsum.png",  width = 2000, height = 1200, res = 200)
par(mfrow = c(3,2))

ob.num <- c(1:length(cumsum_period_all[[ggs[1]]]))

plot(cumsum_period_all[[ggs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     main = "May 15 - July 15")

# ?rainbow
cols <- rainbow(length(ggs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(ggs)){
  ob.num <- c(1:length(cumsum_period_all[[ggs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_all[[ggs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}



ob.num <- c(1:length(cumsum_period_01[[ggs[1]]]))

plot(cumsum_period_01[[ggs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "May - 2")

# ?rainbow
cols <- rainbow(length(ggs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(ggs)){
  ob.num <- c(1:length(cumsum_period_01[[ggs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_01[[ggs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

ob.num <- c(1:length(cumsum_period_02[[ggs[1]]]))

plot(cumsum_period_02[[ggs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 1")

# ?rainbow
cols <- rainbow(length(ggs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(ggs)){
  ob.num <- c(1:length(cumsum_period_02[[ggs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_02[[ggs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}


ob.num <- c(1:length(cumsum_period_03[[ggs[1]]]))

plot(cumsum_period_03[[ggs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 2")

# ?rainbow
cols <- rainbow(length(ggs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(ggs)){
  ob.num <- c(1:length(cumsum_period_03[[ggs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_03[[ggs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}




ob.num <- c(1:length(cumsum_period_04[[ggs[1]]]))

plot(cumsum_period_04[[ggs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "July - 1")

# ?rainbow
cols <- rainbow(length(ggs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(ggs)){
  ob.num <- c(1:length(cumsum_period_04[[ggs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_04[[ggs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

# plot(type = "n")

# legend(x = 30, y = 73,
#        legend = as.character(devices_unique_df$device_info_serial[ggs]),
#        #        cex = 0.5,
#        lty = 1,
#        col = col.obs.transp,
#        lwd = 3)

plot(x = c(0:10), y = c(0:10), type="n", axes=F, xlab="", ylab="")
legend(x = 0, y = 10,
       legend = as.character(devices_unique_df$device_info_serial[ggs]),
       #        cex = 0.5,
       lty = 1,
       col = col.obs.transp,
       lwd = 3)

dev.off()




# HG figure -----
png("HG_cumsum.png",  width = 2000, height = 1200, res = 200)
par(mfrow = c(3,2))

ob.num <- c(1:length(cumsum_period_all[[hgs[1]]]))

plot(cumsum_period_all[[hgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     main = "May 15 - July 15")

# ?rainbow
cols <- rainbow(length(hgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(hgs)){
  ob.num <- c(1:length(cumsum_period_all[[hgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_all[[hgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}



ob.num <- c(1:length(cumsum_period_01[[hgs[1]]]))

plot(cumsum_period_01[[hgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "May - 2")

# ?rainbow
cols <- rainbow(length(hgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(hgs)){
  ob.num <- c(1:length(cumsum_period_01[[hgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_01[[hgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

ob.num <- c(1:length(cumsum_period_02[[hgs[1]]]))

plot(cumsum_period_02[[hgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 1")

# ?rainbow
cols <- rainbow(length(hgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(hgs)){
  ob.num <- c(1:length(cumsum_period_02[[hgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_02[[hgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}


ob.num <- c(1:length(cumsum_period_03[[hgs[1]]]))

plot(cumsum_period_03[[hgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 2")

# ?rainbow
cols <- rainbow(length(hgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(hgs)){
  ob.num <- c(1:length(cumsum_period_03[[hgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_03[[hgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}




ob.num <- c(1:length(cumsum_period_04[[hgs[1]]]))

plot(cumsum_period_04[[hgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "July - 1")

# ?rainbow
cols <- rainbow(length(hgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(hgs)){
  ob.num <- c(1:length(cumsum_period_04[[hgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_04[[hgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

# plot(type = "n")

# legend(x = 30, y = 73,
#        legend = as.character(devices_unique_df$device_info_serial[hgs]),
#        #        cex = 0.5,
#        lty = 1,
#        col = col.obs.transp,
#        lwd = 3)


plot(x = c(0:10), y = c(0:10), type="n", axes=F, xlab="", ylab="")
legend(x = 0, y = 10,
       legend = as.character(devices_unique_df$device_info_serial[hgs]),
       #        cex = 0.5,
       lty = 1,
       col = col.obs.transp,
       lwd = 3)

dev.off()




# Figures for CG -----
png("CG_cumsum.png",  width = 2000, height = 1200, res = 200)
par(mfrow = c(3,2))

ob.num <- c(1:length(cumsum_period_all[[cgs[1]]]))

plot(cumsum_period_all[[cgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     main = "May 15 - July 15")

# ?rainbow
cols <- rainbow(length(cgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(cgs)){
  ob.num <- c(1:length(cumsum_period_all[[cgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_all[[cgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}



ob.num <- c(1:length(cumsum_period_01[[cgs[1]]]))

plot(cumsum_period_01[[cgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "May - 2")

# ?rainbow
cols <- rainbow(length(cgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(cgs)){
  ob.num <- c(1:length(cumsum_period_01[[cgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_01[[cgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

ob.num <- c(1:length(cumsum_period_02[[cgs[1]]]))

plot(cumsum_period_02[[cgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 1")

# ?rainbow
cols <- rainbow(length(cgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(cgs)){
  ob.num <- c(1:length(cumsum_period_02[[cgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_02[[cgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}


ob.num <- c(1:length(cumsum_period_03[[cgs[1]]]))

plot(cumsum_period_03[[cgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "June - 2")

# ?rainbow
cols <- rainbow(length(cgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(cgs)){
  ob.num <- c(1:length(cumsum_period_03[[cgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_03[[cgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}




ob.num <- c(1:length(cumsum_period_04[[cgs[1]]]))

plot(cumsum_period_04[[cgs[1]]]~ ob.num, ylim = c(0,100),
     xlim = c(0,50),
     ylab = "Foraging time (%)",
     xlab = "Number of grid squares",
     las = 1,
     type = "n",
     add = FALSE,
     main = "July - 1")

# ?rainbow
cols <- rainbow(length(cgs))
col.obs.transp <- addalpha(cols, alpha = 0.50)

for(i in 1:length(cgs)){
  ob.num <- c(1:length(cumsum_period_04[[cgs[i]]]))
  x <- c(0,ob.num)
  y <- c(0,cumsum_period_04[[cgs[i]]])
  colx <- cols[i]
  lines( y ~ x ,
         type = "l",
         col = col.obs.transp[i],
lwd = 2, abline(h = 75, lwd = 2, lty = 2)  ) 
}

# plot(type = "n")
plot(x = c(0:10), y = c(0:10), type="n", axes=F, xlab="", ylab="")
legend(x = 0, y = 10,
       legend = as.character(devices_unique_df$device_info_serial[cgs]),
       #        cex = 0.5,
       lty = 1,
       col = col.obs.transp,
       lwd = 3)
dev.off()