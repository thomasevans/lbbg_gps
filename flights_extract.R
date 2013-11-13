# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Script containing single function which take device_info_serial, and a start and end date time for a flight then extracts all GPS points in that range, returning a dataframe with this data. Takes data from both tables:
# gps_uva_tracking_speed_3d_limited
# lund_gps_parameters 






#Extract flight points
flights.extract <- function(i, start.t, end.t){
  #Function to extract required flights data
  
  #Parts of query
  q1a <- "SELECT DISTINCT lf.*, lfc.*, lfw.*
  FROM lund_flights AS lf, lund_flight_paramaters AS lfc,
  lund_flights_weather AS lfw
  WHERE lf.flight_id = lfc.flight_id AND  lf.flight_id = lfw.flight_id
  AND "
  
  q1b <-  " ORDER BY lf.device_info_serial ASC, lf.flight_id ASC ;"
  
  
  #Accepts the aguments given to the function, to extract the
  #values requested.
  q1c <- paste(" lf.device_info_serial = ", i, " AND ",
               "(lf.end_time >= #", start.t, 
               "# AND lf.start_time <= #", end.t, "#) ", sep = "")
  
  #Get flight information
  flight.sub <- sqlQuery(gps.db, query= gsub("\n", " ", paste(q1a, q1c, q1b, sep=""))
                         ,as.is=TRUE)  
  drops <- c("flight_id.1","start_time.1", "device_info_serial.1", "flight_id.2", "start_time.2")
  flight.sub <- flight.sub[,!(names(flight.sub) %in% drops)]
  
  return(flight.sub)
}
