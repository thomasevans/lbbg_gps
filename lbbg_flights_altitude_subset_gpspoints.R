
# Set working directory -----
# Set working directory to data location
setwd("D:/Dropbox/LBBG_flight_height/R_files")

# Link to database -------
#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
# sqlTables(gps.db)


# Get flight data -------
flights <- read.csv("flight_subset_altitude.csv", header = TRUE)

# Fix date-time data
flights$start_time <- as.POSIXct(as.character(flights$start_time), tz="UTC",
                            format = "%Y-%m-%d %H:%M:%S")

flights$end_time <- as.POSIXct(as.character(flights$end_time), tz="UTC",
                                 format = "%Y-%m-%d %H:%M:%S")

# For testing
# idx <- c(1:nrow(flights))
# i <- idx[flights$flight_id == 13571]

# For each flight get GPS points data +/- 20% time (min 5 minutes)
# Make DB querry to perform this.
# Add flight id as column too, to allow for simple subsetting later on.


# i <- 1

points_all <- NULL


for(i in 1:nrow(flights)){
#   for(i in 1:5){
    
    device_id <- flights$device_info_serial[i]
    datetime.start <- flights$start_time[i]
    datetime.end   <- flights$end_time[i]
    
    # Time difference to add/ subtract from flight start/ end time
    # Done to show some points prior and following flight
    # 20% of flight duration
    t.dif <- 0.2 * flights$duration[i]
    # If this is less than 5 minutes, change to 5 minutes
    if(t.dif < 300) t.dif <- 300
    
    t.dif.difftime <- as.difftime(t.dif, units = "secs")
    
    datetime.start.new <- datetime.start - t.dif.difftime
    datetime.end.new <- datetime.end + t.dif.difftime
    
    # Get GPS location fulfilling above extracted criteria
    points <- sqlQuery(gps.db, as.is = TRUE, query=
                         paste("SELECT DISTINCT g.*, m.cloud_cover_low_altitude,m.cloud_cover_total, m.significant_wave_height, m.sun_shine_duration_day, m.surface_roughness, m.temperature_2m, m.wind_u_10m, m.wind_v_10m, m.thermal_uplift, m.sea_level_pressure
                                 FROM gps_uva_tracking_speed_3d_limited AS g, move_bank_variables_all AS m
                                 WHERE g.device_info_serial = ",
                               device_id,
                               "
                                 AND g.date_time > #",
                               datetime.start.new,
                               "#
                                 AND g.date_time < #",
                               datetime.end.new,
                               "#
                                AND g.date_time = m.date_time
                                AND g.device_info_serial = m.device_info_serial
                                 ORDER BY g.date_time ASC;", sep = ""))
    
    # add flight id to gps points
    points <- cbind(points,flights$flight_id[i])
    
    points_all <- rbind(points_all,points)
}

# Fix name for flight_id column
names.points_all <- names(points_all)
names.points_all[length(names.points_all)] <- "flight_id"
names(points_all)  <- names.points_all

# Fix structure
str(points_all)
points_all$date_time <- as.POSIXct(points_all$date_time, tz="UTC",
                               format = "%Y-%m-%d %H:%M:%S")


# Export to csv file
write.csv(points_all, file = "flight_subset_altitude_points.csv",
          row.names = FALSE)

# Save as RData file, so that field types are preserved.
save(points_all, file = "flight_subset_altitude_points.RData")
