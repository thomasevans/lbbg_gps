#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#For flight data - description of flights, summary statistics and 

library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)




#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
FROM lund_trips AS t
ORDER BY t.trip_id ASC;")


flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")
i <- 5
trip_type <- 0
for(i in seq(along=flights$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id == flights$trip_id[i]][1]
}
#summary(as.factor(trip_type))


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

names(flights
#names(flights)
hist(1/(flights$straigtness[flights$dist_a_b  <  50000    &  flights$trip_flight_type == "inward"  & flights$dist_a_b  >  1000 & flights$points > 5 & flights$interval_min < 500]), xlab="Straightness", main="Straightness of flights",breaks=20, col="grey")
hist(flights$interval_min)
plot(1/flights$straigtness[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000] ~ flights$speed_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_n == 1  & flights$dist_a_b  >  1000],main="Mean speed (ms-1) vs. straightness", ylab="Straightness",xlab="Mean Speed (ms-1)")
 
    
      

hist(flights$speed_inst_med[flights$dist_a_b  <  50000    &  flights$trip_flight_type == "inward"   & flights$dist_a_b  >  1000], main="Median instantaneous ground speed on inward flights",xlab="Speed (ms-1)", col="grey")



hist(flights$bearing_a_b[flights$dist_a_b  <  50000    & flights$trip_flight_type == "inward"  & flights$dist_a_b  >  1000], main="Flight bearing",xlab="Flight bearing (degrees)", col="grey")


library(circular)

par(mfrow=c(1,2))

bearings.in <- as.circular(((flights$bearing_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_type == "inward"  & flights$dist_a_b  >  1000])),type ="directions",units = "degrees",template="geographics")

plot.circular(bearings.in, stack=TRUE, bins=90, shrink=1.7,main= "Heading direction of inward flights", col="black", rotation="clockwise",axes=TRUE,cex.lab=1.5)


bearings.out <- as.circular(((flights$bearing_a_b[flights$dist_a_b  <  50000    &  flights$trip_flight_type == "outward"  & flights$dist_a_b  >  1000])),type ="directions",units = "degrees",template="geographics")

plot.circular(bearings.out, stack=TRUE, bins=90, shrink=1.7,main= "Heading direction of outward flights", col="black", rotation="clockwise",axes=TRUE,cex.lab=1.5)


plot.circular(bearings.out, stack=TRUE, bins=90, shrink=1.7,main= "Heading direction of outward flights", col="black", rotation="clockwise",axes=TRUE,cex.lab=1.5)


par(mfrow=c(1,2))

hist(flights$alt_max[flights$dist_a_b  <  50000    &   flights$trip_flight_type == "outward" & flights$dist_a_b  >  1000  & flights$alt_max < 600 &  flights$alt_max > -20 & trip_type == 0],breaks=c(-20,-10,0,10,20,30,40,50,60,70,80,90,100,120,140,160,180,200,250,300,350,400,500,600,800,1000), main="Maximum altitude reached (outward)",xlab="Altitude (m)", col="grey",xlim=c(0,600),las=1,cex.lab=1.2,cex.axis=1.2)


hist(flights$alt_max[flights$dist_a_b  <  50000    &   flights$trip_flight_type == "inward"  & flights$dist_a_b  >  1000  & flights$alt_max < 600 &  flights$alt_max > -20 & trip_type == 0],breaks=c(-20,-10,0,10,20,30,40,50,60,70,80,90,100,120,140,160,180,200,250,300,350,400,500,600,800,1000),las=1, main="Maximum altitude reached (inward)",xlab="Altitude (m)", col="grey",xlim=c(0,600),cex.lab=1.2,cex.axis=1.2)