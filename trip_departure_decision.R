# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to compile and collate various variables
# related to foraging trip type descisions, whether to forage
# at sea or on Gotland. It will include summary information
# about the foraging trip and possible factors that may
# explain/ contribute to foraging trip destination decisions
# such as weather components.
# All is then ouput to a new table in the database for later use.



# Get data from database

#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


# Load in DB tables:
# trip details

trip_details <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                     FROM lund_trips as t
                     ORDER BY trip_id ASC;")

# hist(trip_details$gotland_time_prop)

# str(trip_details)


# gps_uva_track_session_limited
track_session <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
                     FROM gps_uva_track_session_limited as g
                     ORDER BY device_info_serial ASC;")


weather <- sqlQuery(gps.db,
         query =
  "SELECT DISTINCT move_bank_variables_all.*, lund_points_wind_ECMWF.*, lund_trips.trip_id
FROM (lund_trips INNER JOIN move_bank_variables_all ON (lund_trips.device_info_serial = move_bank_variables_all.device_info_serial) AND (lund_trips.start_time = move_bank_variables_all.date_time)) INNER JOIN lund_points_wind_ECMWF ON (move_bank_variables_all.date_time = lund_points_wind_ECMWF.date_time) AND (move_bank_variables_all.device_info_serial = lund_points_wind_ECMWF.device_info_serial)
ORDER BY lund_trips.trip_id;", as.is = TRUE)

names(weather)

# some are missing weather data, only a small fraction though, so will just exclude these
summary(trip_details$trip_id %in% weather$trip_id)


# Match up device_info_serial with bird_id (ring number)


# Date_time_local
# Convert date time from UTC to local (solar) time
# add 1h 10 min to get aprox solar time

# Extract date components
x <- as.yearmon(trip_details$start_time)
# Year
year <- format(x,"%Y")
# Month
month <- format(x,"%m")
