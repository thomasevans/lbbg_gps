# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to reprocess the wind data extracted using MoveBank
# for each GPS point the scalar wind speed is calculated at 10 m 
# (reference height) and at the flight height. To do this the surface
# roughness measure from MoveBank is used for the wind-shear calculations.
# All is then ouput to a new table in the database for later use.

# Get data from database

#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


# Get a copy of the flights DB table. If 'as.is' option is not True then
# appears to lose data time format
wind_data <- sqlQuery(gps.db, as.is = TRUE,
                  query = 
                        "SELECT DISTINCT w.*, f.altitude
                    FROM move_bank_variables_all AS w, gps_uva_tracking_speed_3d_limited AS f
                    WHERE w.device_info_serial = f.device_info_serial
                    AND w.date_time = f.date_time
                    ORDER BY w.device_info_serial ASC, w.date_time ASC;")




# ?sqlQuery
str(wind_data)

# Get date_time to correct format
wind_data$date_time <- as.POSIXct(wind_data$date_time, tz = "UTC")

str(wind_data)



# Function to make calculations on wind data
# Shear

# First adjust altitude (if < 1) new altitude is 1 m. -----
if.neg <- function(x){
  if(x < 1) return(1)
  else return(x)
}

alt_new <- sapply(wind_data$altitude,if.neg)
hist(alt_new)

# Then wind shear ----
wind.shear <- function(wind10, alt, roughness){
  a <- log(alt/roughness)
  b <- log(10/roughness)
  c <- a/b
  wind10*c  
}

# min(wind_data$alt)
wind.calculated.u <- wind.shear(wind_data$wind_u_10m, alt_new,
                                wind_data$surface_roughness)

wind.calculated.v <- wind.shear(wind_data$wind_v_10m, alt_new,
                                wind_data$surface_roughness)



# Direction and scalar speed ----
source("wind_dir_speed.R")
wind.10 <- t(mapply(wind.dir.speed,
                    wind_data$wind_u_10m, wind_data$wind_v_10m))
wind.flight <- t(mapply(wind.dir.speed,
                        wind.calculated.u, wind.calculated.v))

# Assemble to a single table to output to database ----
out.table <- cbind(wind_data$device_info_serial,
                   wind_data$date_time,
                   wind_data$wind_u_10m,
                   wind_data$wind_v_10m,
                   wind.calculated.u,
                   wind.calculated.v,
                   wind_data$surface_roughness,
                   wind.flight[,1],
                   wind.flight[,2],
                   wind.10[,1])

out.table <- as.data.frame(out.table)
names(out.table) <- c("device_info_serial", "date_time",
                      "wind_u_10m", "wind_v_10m",
                      "wind_u_10m_flt_ht", "wind_v_10m_flt_ht",
                      "surface_roughness",
                      "wind_speed_flt_ht",
                      "wind_dir",
                      "wind_speed_10m")
out.table$date_time <- wind_data$date_time

str(out.table)
# Output to new table in the database. ----
sqlSave(gps.db, out.table, tablename = "lund_points_wind_ECMWF",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime"))

# 
# sqlSave(gps.db, weather.data, tablename = "lund_flights_com_points_weather",
#         append = FALSE, rownames = FALSE, colnames = FALSE,
#         verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
#         test = FALSE, nastring = NULL,
#         varTypes =  c(device_info_serial = "integer",
#                       date_time = "datetime"))
