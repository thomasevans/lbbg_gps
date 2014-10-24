# Function to return distance from colony for GPS location
# device_info_serial <- 621
col.dist.fun <- function(lat,long,device_info_serial){
  # Get collony location
  require("RODBC")
# vignette("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
  
  # Nest location (actually start location)
  g <- sqlQuery(gps.db,
                         query = paste("SELECT gps_ee_track_session_limited_local.device_info_serial, gps_ee_track_session_limited_local.start_latitude, gps_ee_track_session_limited_local.start_longitude
          FROM gps_ee_track_session_limited_local
          WHERE (((gps_ee_track_session_limited_local.device_info_serial)= ",
device_info_serial, "));", sep = ""),
                         as.is = TRUE)

close(gps.db)

  s_long <- as.numeric(g$start_longitude[1])
  s_lat  <- as.numeric(g$start_latitude[1])

  # Get function to calculate distances
  source("deg.dist.R")
  
  if(lat == 0 | long == 0) {x <- NA} else {
    x <- deg.dist(s_long,s_lat,
                  long, lat, km = FALSE) 
  }
  return(x)
}