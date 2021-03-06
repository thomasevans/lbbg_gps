# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.


# Description ------
# Produce table for inward flights - flights to be used in the analysis


# Datbase functions ------
# Required library
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


# Get a copy of the flights DB table. -------
# WHOLE FLIGHT
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")


str(flights)  #check structure

# Get a copy of the weather DB table. -------

q1 <- "SELECT DISTINCT f.*
FROM lund_flights_weather AS f
WHERE f.flight_id IN ("

fun <- function(x){
  x <- as.numeric(as.character(x))
  paste(" ", x, ",", sep = "")
}

q2 <- paste(sapply(flights$flight_id,fun), collapse = "")                            
q3 <- ")
ORDER BY f.flight_id ASC;"

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query =
                              gsub("\n", " ",
                                   paste(q1, q2, q3, sep="")),
                            as.is = TRUE)

# Check structure of this
str(flights.weather)



# Fix date-time structure
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


# Get a copy of the trips DB table. -------

#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

str(trips)

# Fix date-time structure
tm <- NULL
tm <- as.POSIXlt(trips$start_time)
attr(tm,"tzone") <- "UTC"
trips$start_time <- tm

tm <- NULL
tm <- as.POSIXlt(trips$end_time)
attr(tm,"tzone") <- "UTC"
trips$end_time <- tm



# Get a copy of the flight movement paramaters DB table. -------
flights.characteristics <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                                    FROM lund_flight_com_weather_par AS f
                                    ORDER BY f.flight_id ASC;")




# Trip type and duration ------
# For each flight get info on type of trip, duration etc.
# Variables later used for filtering

trip_type     <- 0
trip_duration <- 0
trip_gotland  <- 0
trip_distmax  <- 0


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

# Change to factor
trip_gotland <- as.factor(trip_gotland)
summary(trip_gotland)



# filters -------
par(mfrow = c(1,1))
hist(trip_distmax[trip_distmax < 1000], breaks = 100)
hist(trip_distmax[trip_distmax < 100], breaks = 100,
     xlim = c(0,100))

# Includes migratory 'trips'
# range(trip_distmax)
names(flights)


summary(flights$trip_flight_type)
summary(trip_gotland)

outward <- ((flights$trip_flight_type == "outward") &
  trip_gotland == 0 & (flights$interval_mean < 800) &
  (trip_distmax > 4) & (trip_distmax < 400) &
  flights$points > 4 & flights$dist_a_b > 2000 &
  flights$points > 3)

summary(outward)

hist(flights$dist_a_b[outward]/1000, breaks = 20,
     main = "Out", xlab = "Distance (km)")
summary(outward)

inward  <- ((flights$trip_flight_type == "inward")  &
  (trip_gotland == 0) & (flights$interval_mean < 800) &
  (trip_distmax > 4) & (trip_distmax < 400) &
  flights$points > 4  & flights$dist_a_b > 2000 &
  flights$points > 3)

summary(inward)

hist(flights$dist_a_b[inward]/1000, breaks = 40, main = "in", xlab = "Distance (km)", xlim = c(0,120))

# flights$n[1:10]
summary(inward)
summary(outward)




# Calculate angle with respect to wind ------
dif.angle <- flights$bearing_a_b - flights.characteristics$winddir
dif.angle <- abs(dif.angle)

cor.ang <- function(x){
  if(x > 180){ y <- 180 - (x - 180)}
  else y <- x
  return(y)
}

dif.angle <- sapply(dif.angle, cor.ang)
hist(dif.angle)



# Inward flights only (not paired) -----
flights.inward <- cbind(flights[inward,],
                    flights.characteristics[inward,],
                    flights.weather[inward,],
                    dif.angle[inward])





# Output to DB table ------

out.table <- flights.inward
row.names(out.table) <- NULL

# Drop some columns
names(out.table)[109] <- "dif_angle"

names(out.table)

out.table.f <- out.table[,-c(35, 79, 80, 81, 88, 89, 90)]

x <- duplicated(names(out.table.f))
summary(x)

#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, out.table.f, tablename = "lund_flight_com_lbbg",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(start_time = "datetime", end_time = "datetime")
)

odbcCloseAll()  # close any database connections




# Paired data ------
#' Re-arrange the data for paired data comparison

flights.out <- cbind(flights[outward,],flights.characteristics[outward,],flights.weather[outward,],dif.angle[outward])
flights.in <- cbind(flights[inward,],flights.characteristics[inward,],flights.weather[inward,],dif.angle[inward])

# Re-order these by trip_id
flights.out  <- flights.out[order(flights.out$trip_id),]
flights.in   <- flights.in[order(flights.in$trip_id),]

# Find and index those flights for which there is a corresponding outward or inward flight for same trip.
x <- NA
for( i in seq(along = flights.out$trip_id)){
  if(any(flights.out$trip_id[i] == flights.in$trip_id)) x[i] = TRUE else{x[i] = FALSE}
}

y <- NA
for( i in seq(along = flights.in$trip_id)){
  if(any(flights.in$trip_id[i] == flights.out$trip_id)) y[i] = TRUE else{y[i] = FALSE}
}

# Check that this has worked
all.equal(flights.in$trip_id[y] , flights.out$trip_id[x])

flights.in <- flights.in[y,]
flights.out <- flights.out[x,]

length(flights.in$flight_id)
# ?t.test

length(unique(flights.in$device_info_serial))



#Rearrange data for comparision
flights.out$flight.type <- "out"
flights.in$flight.type <- "in"

flights.out$wind.type <- NA
flights.in$wind.type <- NA

flights.out$wind.type[flights.out$dif.angle < 60] <- "tail"
flights.out$wind.type[flights.out$dif.angle > 60 & flights.out$dif.angle < 120] <- "side"
flights.out$wind.type[flights.out$dif.angle > 120] <- "head"

flights.in$wind.type[flights.in$dif.angle < 60] <- "tail"
flights.in$wind.type[flights.in$dif.angle > 60 & flights.in$dif.angle < 120] <- "side"
flights.in$wind.type[flights.in$dif.angle > 90] <- "head"



names(flights.out)[length(flights.out)-2] <- "dif.angle"
names(flights.in)[length(flights.in)-2] <- "dif.angle"
names(flights.out) == names(flights.in)

flights.combined <- rbind(flights.out,flights.in)

flights.combined   <- flights.combined[order(flights.combined$trip_id),]




