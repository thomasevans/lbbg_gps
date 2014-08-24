# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# Classification of guillemot GPS data into foraging trips
# Will base this largely on the script used to do this for the LBBG data,
# adapting slightly the various thresholds etc.
# Purpose of this script is simply to identify foraging trips,
# Then get start-time, end-time, and number each trip with unique ID
# Summary statistics for each foraging trip will be extracted in a
# second sepperate script.


# DB function to extract data for each device_ID/ ring number? for deployment
# period.

# Will need to do sepprately for the IGU and UvA devices
# Get IGU GPS data

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get deployments table
deployments <- sqlQuery(gps.db,
                   query = "SELECT DISTINCT d.*
          FROM guillemots_track_session AS d
          ORDER BY d.device_info_serial ASC;",
                   as.is = TRUE)



# Get UvA GPS data
# Get data for all devices attached to guillemots
# First get a list of device numbers from deployments table.
uva_dep_dev <- deployments$device_info_serial[
  deployments$device_type == "uva"]

igu_dep_dev <- deployments$device_info_serial[
  deployments$device_type == "igu"]

points_uva <- sqlQuery(gps.db,
                        query = paste("SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude
          FROM gps_ee_tracking_speed_limited AS g
          WHERE g.device_info_serial IN (", paste(unique(uva_dep_dev), collapse = ", "),
          ") 
          ORDER BY g.device_info_serial ASC, g.date_time ASC ;", sep = ""),
                        as.is = TRUE)


# Get IGU data
points_igu <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude
          FROM guillemots_gps_points_igu AS g 
          ORDER BY g.device_info_serial ASC, g.date_time ASC ;",
                       as.is = TRUE)


# Filter data to only include data when devices are deployed.




# Make some calculations on GPS data
# - distance from colony

# Classify into foraging trips (see previous
# analysis for the LBBG)

# Assemble table of foraging trips
# - trip_id
# - start_time
# - end_time
# - device_id


# Output details to DB