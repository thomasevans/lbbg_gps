# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Script containing single function which take device_info_serial, and a start and end date time then extracts all GPS points in that range, returning a dataframe with this data. Takes data from both tables:
# gps_uva_tracking_speed_3d_limited
# lund_gps_parameters 


#Extract GPS points
gps.extract <- function(i, start.t, end.t, weather = FALSE, ECMWF = FALSE, simple = FALSE, DB = FALSE){

  gc()
  # i - device info serial
  # start.t - first point date-time
  # end.t - final point date_time
  # simple - if TRUE only return device_info_serial, lat, long, date_time
  #Function to extract required GPS data
  
  require(RODBC)
  
  if(DB == FALSE){
  
  gps.db2 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
#   class(gps.db2)
   if(!inherits(gps.db2,"RODBC")){
     for(i in 1:4){
       gps.db2 <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
       if(inherits(gps.db2,"RODBC")) break
     }
   }

  } else gps.db2 <- DB
  
    if(simple == FALSE){
      if(weather == FALSE) {q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
      g.latitude, g.positiondop, g.speed_accuracy, g.vnorth,
      g.veast, g.vdown, g.speed_3d, g.h_accuracy, g.v_accuracy, c.bearing_next,
      c.bearing_prev, c.nest_gc_dist, c.calculated_speed,
      c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
      c.turning_angle, c.flight_class,  c.flight_id, g.altitude
      FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS c
      WHERE g.device_info_serial = c.device_info_serial
      AND g.date_time = c.date_time
      AND "} else {
        if(ECMWF == FALSE){
            q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
          g.latitude, g.positiondop, g.speed_accuracy, g.vnorth,
          g.veast, g.vdown, g.speed_3d, g.h_accuracy, g.v_accuracy, c.bearing_next,
          c.bearing_prev, c.nest_gc_dist, c.calculated_speed,
          c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
          c.turning_angle, c.flight_class,  c.flight_id, g.altitude, lw.uwnd_10m,
          lw.vwnd_10m
          FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS c, lund_points_weather as lw
          WHERE g.device_info_serial = c.device_info_serial
          AND g.date_time = c.date_time
          AND g.device_info_serial = lw.device_info_serial
          AND g.date_time = lw.date_time
          AND "
        }else {
          q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
          g.latitude, g.positiondop, g.speed_accuracy, g.vnorth,
          g.veast, g.vdown, g.speed_3d, g.h_accuracy, g.v_accuracy, c.bearing_next,
          c.bearing_prev, c.nest_gc_dist, c.calculated_speed,
          c.nest_bear, c.inst_ground_speed, c.p2p_dist,  c.time_interval_s,
          c.turning_angle, c.flight_class,  c.flight_id, g.altitude, lw.uwnd_10m,
          lw.vwnd_10m, w.wind_u_10m_ecmwf, w.wind_v_10m_ecmwf,
          w.wind_u_10m_flt_ht_ecmwf, w.wind_v_10m_flt_ht_ecmwf,
          w.surface_roughness_ecmwf, w.wind_speed_flt_ht_ecmwf,
          w.wind_dir_ecmwf, w.wind_speed_10m_ecmwf,
          m.cloud_cover_low_altitude, m.cloud_cover_total,
          m.significant_wave_height, m.sun_shine_duration_day,
          m.surface_roughness, m.temperature_10m
          FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS c, lund_points_weather as lw, lund_points_wind_ECMWF as w, move_bank_variables_all as m
          WHERE g.device_info_serial = c.device_info_serial
          AND g.date_time = c.date_time
          AND g.device_info_serial = lw.device_info_serial
          AND g.date_time = lw.date_time
          AND g.device_info_serial = m.device_info_serial
          AND g.date_time = m.date_time
          AND g.device_info_serial = w.device_info_serial
          AND g.date_time = w.date_time
          AND "
        }
      }
    } else {
      q1a <- "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude,
      g.latitude
      FROM gps_uva_tracking_speed_3d_limited AS g
      WHERE "      
    }

  q1b <-  " ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
  

  
  q1c <- paste(" g.device_info_serial = ", i, " AND ",
              "g.date_time >= #", start.t, 
               "# AND g.date_time <= #", end.t, "# ", sep = "")
  
  
  out <- tryCatch(
{
  gps.sub <- sqlQuery(gps.db2,
                      query = gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                      ,as.is=TRUE)
  return(gps.sub)
},
  error = function(cond){  gps.sub <- sqlQuery(gps.db2,
                                               query = gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                                               ,as.is=TRUE)
                           return(gps.sub)}
)

  if(DB == FALSE){
  odbcClose(gps.db2)
}
  
  return(out)
}
# 