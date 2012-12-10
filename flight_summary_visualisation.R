#For flight data - description of flights, summary statistics and 

library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)




#Query the gull db to extract bird_id, nest_id, and nest locations
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")


#names(flights)

#flight duration
#'Distribution of flights of duration < 8 h
hist(flights$duration[flights$duration < 8*60*60]/60, main="Distribution of flights of duration < 8 h", xlab = "flight duration (minutes)", col="grey")
#'Distribution of flights of duration < 2 h
hist(flights$duration[flights$duration < 2*60*60]/60,main="Distribution of flights of duration < 2 h", xlab = "flight duration (minutes)", col="grey")


#'number of gps fixes per flight
hist(flights$points[flights$points < 200], main= "Number of gps fixes per flight", xlab="GPS fix number", col="grey")


#'Distance moved (A to B)
hist(flights$dist_a_b[flights$dist_a_b  <  50000]/1000, xlab="Distance, A to B (km)", main="Distance moved (A to B)", col="grey")

#'Distance moved (A to B) for first flight of trip
hist(flights$dist_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1]/1000, xlab="Distance, A to B (km)", main="Distance moved on first flight of trip", col="grey")

hist(flights$dist_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000]/1000, xlab="Distance, A to B (km)", main="Distance moved on first flight of trip ( >1 km)",breaks=15, col="grey")


#names(flights)
hist(1/flights$straigtness[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000], xlab="Straightness", main="Straightness of flights",breaks=15, col="grey")

plot(1/flights$straigtness[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000] ~ flights$speed_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000],main="Mean speed (ms-1) vs. straightness", ylab="Straightness",xlab="Mean Speed (ms-1)")
 

hist(flights$speed_inst_med[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000], main="Median instantaneous ground speed on outward flights",xlab="Speed (ms-1)", col="grey")



hist(flights$bearing_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000], main="Flight bearing",xlab="Flight bearing (degrees)", col="grey")


library(circular)
plot.circular((rad(flights$bearing_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000])+pi), stack=TRUE, bins=300, shrink=1.5,main= "Circular plot of above with radians", col="black", rotation="clockwise")

hist(flights$alt_max[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000  & flights$alt_max < 600 &  flights$alt_max > -20],breaks=40, main="Maximum altitude reached",xlab="Altitude (m)", col="grey")
