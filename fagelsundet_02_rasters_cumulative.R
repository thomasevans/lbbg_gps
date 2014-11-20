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

# Filters for individuals + species ----
device_ids <- unique(gps.2014$device_info_serial)


