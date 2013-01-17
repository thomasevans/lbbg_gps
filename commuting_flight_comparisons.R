#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Description#######
#In this script we produce various figures and summary statistics to compare outward and inward commuting flights.
#These were originally prepared for a meeting with Susanne and Anders on 2013-01-17


#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")

flights_weather <- ???????

#filters####
outward <- flights$trip_flight_type == "outward"
inward  <- flights$trip_flight_type == "inward"

# Speed comparision######
names(flights)

par(mfrow = c(1,2))

# Resultant speed
hist(flights$speed_a_b[outward & flights$speed_a_b < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30))
hist(flights$speed_a_b[inward & flights$speed_a_b < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30))
speed_inst_med

mean(flights$speed_a_b[outward & flights$speed_a_b < 25], na.rm = TRUE)
mean(flights$speed_a_b[inward & flights$speed_a_b < 25], na.rm = TRUE)

# Mean speed
hist(flights$speed_inst_mean[outward & flights$speed_a_b < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20))
hist(flights$speed_inst_mean[inward & flights$speed_a_b < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20))
speed_inst_med

mean(flights$speed_inst_mean[outward & flights$speed_inst_mean < 25], na.rm = TRUE)
mean(flights$speed_inst_mean[inward & flights$speed_inst_mean < 25], na.rm = TRUE)


# Median speed
hist(flights$speed_inst_med[outward & flights$speed_inst_med < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20))
hist(flights$speed_inst_med[inward & flights$speed_inst_med < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20))
speed_inst_med


mean(flights$speed_inst_med[outward & flights$speed_inst_med < 25], na.rm = TRUE)
mean(flights$speed_inst_med[inward & flights$speed_inst_med < 25], na.rm = TRUE)
