# Analysis of dive-bouts from 2009 guillemot GPS + TDR study



# Import required data ------

# From DB:
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Dive bouts
dive.bouts <- sqlQuery(gps.db,
                        query = "SELECT DISTINCT d.*
          FROM guillemots_dive_bouts_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time_start ASC;",
                        as.is = TRUE)

# str(dive.bouts)

# GPS data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT d.*
          FROM guillemots_gps_points_igu_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time ASC;",
                       as.is = TRUE)

# fix date_times
gps.points$date_time <- as.POSIXct(gps.points$date_time, tz = "UTC")
dive.bouts$date_time_start <- as.POSIXct(dive.bouts$date_time_start, tz = "UTC")
dive.bouts$date_time_end <- as.POSIXct(dive.bouts$date_time_end, tz = "UTC")


# Bathymetry
load('bsbd_raster.RData')

# str(gps.points)
# Combine two data - for loop to go through all bouts -----

# Start with an example bout
i <- 5
n_bouts <- length(dive.bouts$ring_number)

for(i in 1:n_bouts){
  
  # Re-set vairables
  t_sub_10 <- t_plus_10 <- t_sub_5 <- t_plus_5 <- NULL
  bird_id <- bout.gps.10 <- bout.gps.5 <- NULL
  
    t_sub_10 <-  dive.bouts$date_time_start[i] - 600
    t_plus_10 <-  dive.bouts$date_time_end[i]  + 600
    
    t_sub_5 <-  dive.bouts$date_time_start[i] - 300
    t_plus_5 <-  dive.bouts$date_time_end[i]  + 300
    
    bird_id <- dive.bouts$ring_number[i]
    bout.gps.10 <- NULL
    bout.gps.10 <- subset(gps.points, (ring_number == bird_id) &
                         (date_time > t_sub_10) &
                         (date_time < t_plus_10)
                       )
    bout.gps.5 <- NULL
    bout.gps.5 <- subset(gps.points, (ring_number == bird_id) &
                           (date_time > t_sub_5) &
                           (date_time < t_plus_5)
      )
    
    
    # For distance calculations
    source("deg.dist.R")
    
    if(length(bout.gps.10$ring_number)==0){
      
      # skip rest...
      # return NAs
      # GPS_data  <- FALSE
      
    } else {
      
      # IF GPS data continue...
      
      gps.data <- NULL
      
      # Use 5 minute if possible, but if too few points (<2)
      # use the 10 minute data
      if(length(bout.gps.5$ring_number) <2){
        gps.data <- bout.gps.10
      } else gps.data <- bout.gps.5
      
      
      # Re-set vairables
      xy.gps <- gps.bath <- bath.mean <- n_gps <-  bath.min <- bath.max <- NULL
      
      # XY GPS location data
      xy.gps <- cbind(gps.data$longitude, gps.data$latitude)
      
      # Extract bathymetry data for these positions
      gps.bath <- extract(bsbd_raster,xy.gps)
      
      bath.mean <- mean(gps.bath)
      bath.min <- min(gps.bath)
      bath.max <- max(gps.bath)
      
      n_gps <- length(gps.data$ring_number)
      
      
      # Re-set more variables
      points.next.lat <- points.next.long <- points.prev.lat <- points.prev.long <- dist.p2p <- dist.straight <- long.start <- NA
      lat.start <- long.end <- lat.end <- long.mid <- lat.mid <- NA
      
      # GPS point calculations
      # p2p distance
      if(n_gps > 1){
        points.next.lat <- xy.gps[-1,2]
        points.next.long <- xy.gps[-1,1]
        
        points.prev.lat <- xy.gps[-n_gps,2]
        points.prev.long <- xy.gps[-n_gps,1]
        
        # distances
        dist.p2p <- 1000*sum(deg.dist(points.prev.long,
                                 points.prev.lat,
                                 points.next.long,
                                 points.next.lat))
        
        dist.straight <- 1000*deg.dist(xy.gps[1,1],
                                       xy.gps[1,2],
                                       xy.gps[n_gps,1],
                                       xy.gps[n_gps,2])
        
        # locations
        long.start <- points.prev.long[1]
        lat.start <- points.prev.lat[1]
        long.end <- xy.gps[n_gps,1]
        lat.end <- xy.gps[n_gps,2]
        long.mid <- (long.end + long.start)/2
        lat.mid <- (lat.end + lat.start)/2
        
        
      } else {
        dist.p2p <- NA
        dist.straight <- NA
        long.start <- lat.start <- long.end <- lat.end <- NA
        long.mid <- gps.data$longitude
        lat.mid <- gps.data$latitude
      }
      
      
      # re-set values
      bath_max_dive_max <- bath_mean_dive_max <- bath_max_dive_max_mean <- bath_mean_dive_max_mean <- prop_bath_mean_dive_max <- prop_bath_mean_dive_max_mean <- NULL
      
      # Relate to dive bout depths
      bath_max_dive_max  <- -bath.max - dive.bouts$depth_max[i]
      bath_mean_dive_max <- -bath.mean - dive.bouts$depth_max[i]
      bath_max_dive_max_mean <- -bath.max - dive.bouts$depth_max_mean[i]
      bath_mean_dive_max_mean <- -bath.mean - dive.bouts$depth_max_mean[i]
      
      prop_bath_mean_dive_max  <-  bath_mean_dive_max/(-bath.mean)
      prop_bath_mean_dive_max_mean  <- bath_mean_dive_max_mean/(-bath.mean)
      
    }
} #End of for loop
