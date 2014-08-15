# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to produce a new database table to
# annotate the GPS fix data from the IGU loggers
# labelling points by whether they are vallid
# GPS fixes, plus weather they include diving etc.

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# Get GPS data
points <- sqlQuery(gps.db,
  query = "SELECT DISTINCT f.*
          FROM guillemots_gps_points_igu AS f
          ORDER BY f.device_info_serial ASC, f.date_time;",
  as.is = TRUE)


# Label by whether vallid GPS fix -----

# Has location?
gps_val <- (points$long != 0) & (points$lat != 0)
summary(gps_val)

# Is good quality GPS fix?
f0 <- gps_val & (points$timeout <= 100)
f1 <- gps_val & (points$ehpe <= 100)
# f2 <- gps_val & (points$timeout <= 150)
gps_ok <- f0 & f1
summary(gps_ok)
# gps_ok2 <- f0 & f2
# summary(gps_ok2)

# 
# map.trip(points = points)
# points(points$lat[!gps_ok]~points$long[!gps_ok],
#        col = "magenta", pch = 8, cex = 1.2)
# points(points$lat[!gps_ok2]~points$long[!gps_ok2],
#        col = "green", pch = 8, cex = 1.2)





# Label by 'behaviour' ------
#* 1. Flight
#* 2. Colony
#* 3. Diving (apparent)
#* 4. Water surface (swimming etc)
#* 5. Other (i.e. uncategorised)

# 1 . Flight ----
# label as flight if instantaneous speed exceeds
# 5 ms-1
flight <- points$speed_ms > 5
summary(flight)

# 2. Collony ----





# Some labelling of previous/ next points? -----
# Could be good for example to label if prio point was diving
# so as to plot points according to this.
# When plotting could get average/ intermediate position for diving
# events

# Output to new DB table ----
# Output annotation to new DB table include device_info_serial
# and date_time for primary key data

