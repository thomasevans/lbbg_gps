# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to compile and collate various variables
# related to foraging trip type descisions, whether to forage
# at sea or on Gotland. It will include summary information
# about the foraging trip and possible factors that may
# explain/ contribute to foraging trip destination decisions
# such as weather components.
# All is then ouput to a new table in the database for later use.

# Slight hack to make sure we are using UTC time zone
Sys.setenv(TZ='UTC')


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

trip_info <- cbind(trip_details[filter_trips,],weather)

str(trip_info)


# Drops
# Drop some duplicate columns etc...
trip_info <- trip_info[,-38]

drops <- c("device_info_serial.1","date_time")
trip_info <- trip_info[,!(names(trip_info) %in% drops)]

str(trip_info)



# New time data columns ----
# Date_time_local
# Convert date time from UTC to local (solar) time
# add 1h 10 min to get aprox solar time
t <- 60*70
time_local <- trip_info$start_time + t

# Inspect this to check that it looks sensible
head(time_local)
head(trip_info$start_time)

# Extract date components
# Year
year <- format(trip_info$start_time,"%Y")
# Month
month <- format(trip_info$start_time,"%m")

# Time only
time_utc <- format(trip_info$start_time,"%H:%M:%S")
date_utc <- format(trip_info$start_time,"%Y-%m-%d")
head(time_utc)
head(date_utc)

# Add these new vairables to trip_info table
trip_info <- cbind(trip_info,time_local,year,month,time_utc,date_utc)

str(trip_info)


# dew ----
# Calculate difference between ambient tempreture (at 2m)
# dew point tempreture. If positive suggest dew, if negative,
# no dew. Rough indication at least
dew_dif <- trip_info$ecwf_dew_point -   trip_info$temperature_2m 

trip_info <- cbind(trip_info, dew_dif)


# Wind speed and direction ----
# Source file with wind direction and speed function
source("wind_dir_speed.R")

winds <- wind.dir.speed(trip_info$wind_u_10m,
                        trip_info$wind_v_10m)

wind_speed <- winds[,1]
wind_dir   <- winds[,2]
# hist(wind_speed)
# hist(wind_dir)
trip_info <- cbind(trip_info, wind_speed, wind_dir)



# Sex and bird ID -----
# Get Sex and bird ID table from DB
birds <- sqlQuery(gps.db, query="SELECT DISTINCT b.device_info_serial, b.ring_number, b.sex_tentative
                     FROM lund_birds_morph as b
                     ORDER BY device_info_serial ASC;")




# Combine with trip_info table
out.table <- merge(x = trip_info, y = birds,
                    by = "device_info_serial",
                    all = FALSE, sort = TRUE)

# Sort table by ring number then by date_time (start time)
sort.tab <- order(out.table$ring_number, out.table$start_time)
out.table <- out.table[sort.tab,]



# load("trip_info.RData")

# Output to DB ------

# sqlDrop(gps.db, "lund_trips_depart", errors = FALSE)

sqlSave(gps.db, out.table,
        tablename = "lund_trips_depart",
        append = TRUE, rownames = FALSE,
        colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE, fast = FALSE,
        test = FALSE, nastring = NULL,
# #         index = c("ring_number", "start_time"),
        varTypes =  c(start_time = "datetime",
                      end_time = "datetime",
                      sunrise_date_time = "datetime",
                      sunset_date_time = "datetime",
                      time_local = "datetime",
                      time_utc = "datetime",
                      date_utc = "datetime"
                      )
)


trips <- out.table
save(trips, file = "foraging_trip_info.RData")
# ?save

write.csv(trips, file = "foraging_trip_info.csv")
# ??save.table
