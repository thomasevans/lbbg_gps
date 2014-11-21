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


# Need to de-bug!!!!
#
make.raster <- function(filter, file.name, gps.data = gps.2014, base_raster = base_raster){
  require("plotKML")
  data(SAGA_pal)
  source("color_legend_png_fun.R")
  gps.f <- gps.data[filter,]
  
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
  
  
  values(raster_x) <- log10(values(raster_x))

  plotKML(raster_x, file = paste(file.name, ".kml", sep = ""),
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
  
  png(paste(file.name, "_hist", ".png", sep = ""))
  hist(values(10^raster_x), breaks = 40, ylim = c(0,50),
       col = "dark grey", main = file.name
  )
  dev.off()
}   


make.raster(filter = gg.filter, file.name = "GBBG_all")


# Cumulative % by period and individual -------

# Function to get cumulative percentages
# Take in lat-long data and time intervals
# Output ordered vector of cell values


# Loop through all individuals
# For each individual, and each period within individual run
# the cumulative % thing and output to a list


# Plots for cumulative % thing ------





