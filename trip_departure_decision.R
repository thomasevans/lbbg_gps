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


# Weather details, using start time of trips to make
# join with wind data etc.
# Querry built in Access
weather <- sqlQuery(gps.db,
         query =
           "SELECT DISTINCT move_bank_variables_all.date_time, move_bank_variables_all.device_info_serial, move_bank_variables_all.cloud_cover_low_altitude, move_bank_variables_all.cloud_cover_total, move_bank_variables_all.significant_wave_height, move_bank_variables_all.sun_shine_duration_day, move_bank_variables_all.surface_roughness, move_bank_variables_all.temperature_2m, move_bank_variables_all.wind_u_10m, move_bank_variables_all.wind_v_10m, move_bank_variables_all.thermal_uplift, move_bank_variables_all.sea_level_pressure, move_bank_ppt_dew_ECMWF.ecwf_ppt, move_bank_ppt_dew_ECMWF.ecwf_dew_point, lund_points_sun.time_of_day, lund_points_sun.sunrise_date_time, lund_points_sun.sunset_date_time, lund_points_sun.sunrise_dif_s, lund_points_sun.sunset_dif_s, lund_trips.trip_id
FROM lund_trips INNER JOIN (move_bank_variables_all INNER JOIN (move_bank_ppt_dew_ECMWF INNER JOIN lund_points_sun ON (move_bank_ppt_dew_ECMWF.date_time = lund_points_sun.date_time) AND (move_bank_ppt_dew_ECMWF.device_info_serial = lund_points_sun.device_info_serial)) ON (move_bank_variables_all.device_info_serial = move_bank_ppt_dew_ECMWF.device_info_serial) AND (move_bank_variables_all.date_time = move_bank_ppt_dew_ECMWF.date_time)) ON (lund_trips.start_time = move_bank_variables_all.date_time) AND (lund_trips.device_info_serial = move_bank_variables_all.device_info_serial)
ORDER BY trip_id ASC;
")



# track_session
# weather

names(weather)

# some are missing weather data, only a small fraction though, so will just exclude these
# False - trips missing weather data for first point
summary(trip_details$trip_id %in% weather$trip_id)



# Date_time_local
# Convert date time from UTC to local (solar) time
# add 1h 10 min to get aprox solar time
t <- 60*70
time_local <- trip_details$start_time + t

# Inspect this to check that it looks sensible
head(time_local)
head(trip_details$start_time)

# Extract date components
# Year
year <- format(trip_details$start_time,"%Y")
# Month
month <- format(trip_details$start_time,"%m")




# Combine weather and trip_details into a single dataframe
# Order trip_details by trip_id (probably already, but just to be certain!)
trip_details <- trip_details[order(trip_details$trip_id),]

# Order weather data via trip_id (as above for trip_details)
weather <- weather[order(weather$trip_id),]


# Combine two tables
# First filter so only have trips where we have both trip info
# and weather details
filter_trips <- trip_details$trip_id %in% weather$trip_id
summary(filter_trips)
all.equal(trip_details$trip_id[filter_trips],weather$trip_id)

trip_info <- cbind(trip_details,weather)

# Drops
# Drop some duplicate columns etc...
***********

# Replace device numbers with ring numbers


# Add sexes for each bird
  

# Add sunrise times, and calculate difference from these

# Output table (DB and csv)
