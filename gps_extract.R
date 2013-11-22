# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Script containing single function which take device_info_serial, and a start and end date time then extracts all GPS points in that range, returning a dataframe with this data. Takes data from both tables:
# gps_uva_tracking_speed_3d_limited
# lund_gps_parameters 


#Extract GPS points
gps.extract <- function(i, start.t, end.t){
  
  # i - device info serial
  # start.t - first point date-time
  # end.t - final point date_time
  #Function to extract required GPS data
  
  require(RODBC)
  
  gps.db2 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  
  

  q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
  g.latitude, g.x_speed, g.y_speed, g.z_speed, g.positiondop,
  g.speed_accuracy, c.bearing_next, c.bearing_prev, c.nest_gc_dist,
  c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
  c.turning_angle, c.flight_class,  c.flight_id, g.altitude
  FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS c
  WHERE g.device_info_serial = c.device_info_serial
  AND g.date_time = c.date_time
  AND "

  q1b <-  " ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
  

  
  q1c <- paste(" g.device_info_serial = ", i, " AND ",
              "g.date_time >= #", start.t, 
               "# AND g.date_time <= #", end.t, "# ", sep = "")
  
  
  gps.sub <- sqlQuery(gps.db2, query= gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                      ,as.is=TRUE)
  
  return(gps.sub)
}
# 