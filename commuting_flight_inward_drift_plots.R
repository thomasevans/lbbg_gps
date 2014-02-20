# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Description -----
# This script makes plots to look at levels of wind drift during inward flights on foraging trips - a filter specifies only marine foraging trips.


# Database data downloand ----

# Connect to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)


# Get flight drift data + weather data (point level)
flight.points <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
                    FROM lund_flight_com_in_drift_points AS g
                    ORDER BY g.device_info_serial ASC, g.date_time ASC;")


# Get commuting flight summary data
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")


# Get trip info data
#Query the gull db to extract trip information
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")


# Filter flights ------
# Only include inward flights - use same filter as in stats analysis (for consistency etc)
# Filter flights table

# following from 'commuting_flight_comparisons_categorised_stats.R'
# Label trip type and duration -----
trip_type     <- 0
trip_duration <- 0
trip_gotland  <- 0
trip_distmax  <- 0

# Go through all trips, labelling by trip type duration etc.
# Later to be used for filtering criteria
for(i in seq(along = flights$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id ==
                                    flights$trip_id[i]][1]
  trip_duration[i] <- trips$duration_s[trips$trip_id ==
                                         flights$trip_id[i]][1]
  trip_gotland[i] <- trips$gotland[trips$trip_id ==
                                     flights$trip_id[i]][1]
  trip_distmax[i] <- trips$dist_max[trips$trip_id ==
                                      flights$trip_id[i]][1]
}

# Make to factors - not numeric
trip_type    <- as.factor(trip_type)
trip_gotland <- as.factor(trip_gotland)
# 
# summary(trip_type)
# summary(trip_gotland)


# Filter for inward flights meeting criteria for inclusion ------
inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000

summary(inward)





# Then use above filter %in% ... to filter the points table too.
f <- flight.points$flight_id %in% flights$flight_id[inward]
summary(f)
flight.points.f <- flight.points[f,]

# Plot some example flights for drift by distance etc, perhaps 10 in different colours (same for each graph...)
plot(flight.points.f$drift_prop, flight.points.f$dist_prop_to_goal,
     xlim = c(-1,1), ylim = c(0,1))


hist(flight.points.f$drift_prop, xlim = c(-10,10), breaks = 10000)

hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal < 0.2],
     breaks = 10000, xlim = c(-10,10))

hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal>0.4
                                       & flight.points.f$dist_prop_to_goal < 0.6],xlim = c(0,1.5), breaks = 100)
hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal>0.8],xlim = c(0,1.5), breaks = 100)

names(flight.points.f)

# Plot all data (maybe small points?)
# Add spline, and 95% CI? Loess etc
# See some of options found from a bit of Googling
