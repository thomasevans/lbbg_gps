#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Description ----
#In this script we produce various figures and summary statistics to look at drift during flights.
#These were originally prepared for a meeting with Susanne and Anders on 2014-03-05.


#Datbase functions ----
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB tables ------
# WHOLE FLIGHT
flights.whole <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")

# First half
flights.half.1 <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_half1 AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")

# Second half
flights.half.2 <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_half2 AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")




# Get weather data for flights  -----
# First build SQL query
q1 <- "SELECT DISTINCT f.*
FROM lund_flights_weather AS f
WHERE f.flight_id IN ("

fun <- function(x){
  x <- as.numeric(as.character(x))
  paste(" ", x, ",", sep = "")
}

q2 <- paste(sapply(flights.whole$flight_id,fun), collapse = "")                            
q3 <- ")
ORDER BY f.flight_id ASC;"

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query =
                              gsub("\n", " ",
                                   paste(q1, q2, q3, sep="")),
                            as.is = TRUE)
# str(flights.weather)
# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- NULL
tm <- as.POSIXlt(flights.weather$start_time)
#Check how this appears (i.e. time zone)
# tm[1:10]
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# tm[1:10]
flights.weather$start_time <- tm



# Trip info from DB -----
#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

#str(trips)
tm <- NULL
tm <- as.POSIXlt(trips$start_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$start_time <- tm

tm <- NULL
tm <- as.POSIXlt(trips$end_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$end_time <- tm


# Flight characteristics table ----
#Edited this to reference 'lund_flight_paramaters' rather than 'lund_flight_characteristics' as table name changed.
# Later to 'lund_flight_com_weather_par'
flights.characteristics <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                                    FROM lund_flight_com_weather_par AS f
                                    ORDER BY f.flight_id ASC;")



# Trip type and duration ----
trip_type <- 0
trip_duration <- 0
trip_gotland <- 0
trip_distmax <- 0

# Go through all flights.whole, and assign trip duration etc.
for(i in seq(along = flights.whole$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id ==
                                    flights.whole$trip_id[i]][1]
  trip_duration[i] <- trips$duration_s[trips$trip_id ==
                                         flights.whole$trip_id[i]][1]
  trip_gotland[i] <- trips$gotland[trips$trip_id ==
                                     flights.whole$trip_id[i]][1]
  trip_distmax[i] <- trips$dist_max[trips$trip_id ==
                                      flights.whole$trip_id[i]][1]
}

trip_gotland <- as.factor(trip_gotland)
summary(trip_gotland)


# Sepperate inward and outward flights  ----
outward <- (flights.whole$trip_flight_type == "outward") & trip_gotland == 0 & (flights.whole$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights.whole$points > 4 & flights.whole$dist_a_b > 2000 & flights.whole$points > 3
hist(flights.whole$dist_a_b[outward]/1000, breaks = 20, main = "out", xlab = "Distance (km)", xlim = c(0,120))

inward  <- (flights.whole$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights.whole$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights.whole$points > 4  & flights.whole$dist_a_b > 2000 & flights.whole$points > 3
hist(flights.whole$dist_a_b[inward]/1000, breaks = 40, main = "in", xlab = "Distance (km)", xlim = c(0,120))

# flights$n[1:10]
summary(inward)
summary(outward)



