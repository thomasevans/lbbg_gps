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
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf_half_1 AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")

# Second half
flights.half.2 <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf_half_2 AS w
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




# Re-arrange the data for paired data comparison ----

flights.out <- cbind(flights.whole[outward,],flights.characteristics[outward,],flights.weather[outward,])
flights.in <- cbind(flights.whole[inward,],flights.characteristics[inward,],flights.weather[inward,])

# Re-order these by trip_id
flights.out  <- flights.out[order(flights.out$trip_id),]
flights.in   <- flights.in[order(flights.in$trip_id),]

#Find and index those flights for which there is a corresponding outward or inward flight for same trip.
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

length(unique(flights.in$device_info_serial))

#Rearrange data for comparision
flights.out$flight.type <- "out"
flights.in$flight.type <- "in"


# For first half
# Re-arrange the data for paired data comparison

flights.half.1.out <- cbind(flights.half.1[outward,],flights.characteristics[outward,],flights.weather[outward,])
flights.half.1.in <- cbind(flights.half.1[inward,],flights.characteristics[inward,],flights.weather[inward,])

# Re-order these by trip_id
flights.half.1.out  <- flights.half.1.out[order(flights.half.1.out$trip_id),]
flights.half.1.in   <- flights.half.1.in[order(flights.half.1.in$trip_id),]

#Find and index those flights for which there is a corresponding outward or inward flight for same trip.
x <- NA
for( i in seq(along = flights.half.1.out$trip_id)){
  if(any(flights.half.1.out$trip_id[i] == flights.half.1.in$trip_id)) x[i] = TRUE else{x[i] = FALSE}
}

y <- NA
for( i in seq(along = flights.half.1.in$trip_id)){
  if(any(flights.half.1.in$trip_id[i] == flights.half.1.out$trip_id)) y[i] = TRUE else{y[i] = FALSE}
}

# Check that this has worked
all.equal(flights.half.1.in$trip_id[y] , flights.half.1.out$trip_id[x])

flights.half.1.in <- flights.half.1.in[y,]
flights.half.1.out <- flights.half.1.out[x,]




# For second half
# Re-arrange the data for paired data comparison ----

flights.half.2.out <- cbind(flights.half.2[outward,],flights.characteristics[outward,],flights.weather[outward,])
flights.half.2.in <- cbind(flights.half.2[inward,],flights.characteristics[inward,],flights.weather[inward,])

# Re-order these by trip_id
flights.half.2.out  <- flights.half.2.out[order(flights.half.2.out$trip_id),]
flights.half.2.in   <- flights.half.2.in[order(flights.half.2.in$trip_id),]

#Find and index those flights for which there is a corresponding outward or inward flight for same trip.
x <- NA
for( i in seq(along = flights.half.2.out$trip_id)){
  if(any(flights.half.2.out$trip_id[i] == flights.half.2.in$trip_id)) x[i] = TRUE else{x[i] = FALSE}
}

y <- NA
for( i in seq(along = flights.half.2.in$trip_id)){
  if(any(flights.half.2.in$trip_id[i] == flights.half.2.out$trip_id)) y[i] = TRUE else{y[i] = FALSE}
}

# Check that this has worked
all.equal(flights.half.2.in$trip_id[y] , flights.half.2.out$trip_id[x])

flights.half.2.in <- flights.half.2.in[y,]
flights.half.2.out <- flights.half.2.out[x,]




# Combining flight data tables
flights.combined <- rbind(flights.out,flights.in)
flights.combined   <- flights.combined[order(flights.combined$trip_id),]



# sample sizes ---------
# install.packages("reshape2")
library(reshape2)

aggregate(speed_inst_mean ~ device_info_serial,
          data = flights.combined,
          FUN = length)



# Alpha values -----
range(flights.combined$alpha_mean)
mean(flights.combined$alpha_mean)
hist(flights.combined$alpha_mean)


# Recalculate alpha according to track - heading
track_head <- flights.combined$ground_dir_mean - flights.combined$head_dir_mean
# Note when either heading or track close to 0/360 can get 'wrong' values
hist(track_head)
range(track_head)

# See uncorrected values
plot(flights.combined$ground_dir_mean ~ track_head,
     xlab = "Track - Heading", ylab = "Track")
abline(lm(flights.combined$ground_dir_mean ~ track_head),
       lwd = 2, lty = 3, col = "red")
lm(flights.combined$ground_dir_mean ~ track_head)


# Function to correct values when near 360
track_head_fun <- function(ground, head){
  x <- ground - head
  if(x < -180){
    x <- ((ground - 180) %% 360) - ((head - 180) %% 360)
  } else if(x > 180){
    x <- ((ground + 180) %% 360) - ((head + 180) %% 360)
  }
  return(x)
}



# Function to correct normalized ground (track) direction when !<-180 or !<180
ground_fun <- function(ground, ground.mean){
  x <- ground - ground.mean
  if(x < -180){
    x <- x + 360
  } else if(x > 180){
    x <- 360 - x
  }
  x <- x*-1
  return(x)
}


# Calculate alpha (T - H)  ----
track_head_all   <-  mapply(track_head_fun,
                            flights.combined$ground_dir_mean,
                            flights.combined$head_dir_mean)
# hist(track_head_all)
track_head_all_out   <-  mapply(track_head_fun,
                                flights.out$ground_dir_mean,
                                flights.out$head_dir_mean)
track_head_all_in   <-  mapply(track_head_fun,
                               flights.in$ground_dir_mean,
                               flights.in$head_dir_mean)
track_head_1_out   <-  mapply(track_head_fun,
                              flights.half.1.out$ground_dir_mean,
                              flights.half.1.out$head_dir_mean)
# hist(track_head_1_out)
track_head_1_in   <-  mapply(track_head_fun,
                             flights.half.1.in$ground_dir_mean,
                             flights.half.1.in$head_dir_mean)
track_head_2_out   <-  mapply(track_head_fun,
                              flights.half.2.out$ground_dir_mean,
                              flights.half.2.out$head_dir_mean)
track_head_2_in   <-  mapply(track_head_fun,
                             flights.half.2.in$ground_dir_mean,
                             flights.half.2.in$head_dir_mean)


# Inspect some of these
hist(track_head_1_in)
hist(track_head_2_in)
range(track_head_2_in)
range(track_head_1_out)
# Still some large values, but none now >180 or <-180
# Probably real values, though do seem like quite extreme outliers
# Would only really be possible where wind speeds similar to airspeeds and the bird is flying nearly directly against the wind.
sort(track_head_1_out, decreasing = TRUE)[1:50]
sort(track_head_1_out)[1:50]



# Calculate normalised track ----

library("circular")
# ?mean.circular


circ.mean   <- function(x){
  as.numeric(deg(mean.circular(rad(x))))
}

# str(mean.circular(rad(
#   flights.combined$ground_dir_mean))[1])

track_ground_normalized_all   <-  mapply(ground_fun,
                                    circ.mean(                                  
                                      flights.combined$ground_dir_mean),
                                    flights.combined$ground_dir_mean)
# hist(track_ground_normalized_all)
track_ground_normalized_all_out   <-  mapply(ground_fun,
                                             circ.mean(flights.out$ground_dir_mean),
                                             flights.out$ground_dir_mean)
track_ground_normalized_all_in   <-  mapply(ground_fun,
                                            circ.mean(flights.in$ground_dir_mean),
                                            flights.in$ground_dir_mean)
# hist(track_ground_normalized_all_out)
# hist(track_ground_normalized_all_in)

track_ground_normalized_1_out   <-  mapply(ground_fun,
                                           circ.mean(flights.half.1.out$ground_dir_mean),
                                           flights.half.1.out$ground_dir_mean)
track_ground_normalized_1_in   <-  mapply(ground_fun,
                                          circ.mean(flights.half.1.in$ground_dir_mean),
                                          flights.half.1.in$ground_dir_mean)
track_ground_normalized_2_out   <-  mapply(ground_fun,
                                           circ.mean(flights.half.2.out$ground_dir_mean),
                                           flights.half.2.out$ground_dir_mean)
track_ground_normalized_2_in   <-  mapply(ground_fun,
                                          circ.mean(flights.half.2.in$ground_dir_mean),
                                          flights.half.2.in$ground_dir_mean)


hist(track_ground_normalized_1_out)
hist(track_ground_normalized_2_out)



# Calculate normalised heading -----
heading_normalized_all   <-  mapply(ground_fun,
                                         circ.mean(                                  
                                           flights.combined$head_dir_mean),
                                         flights.combined$head_dir_mean)
# hist(heading_normalized_all)
heading_normalized_all_out   <-  mapply(ground_fun,
                                             circ.mean(flights.out$head_dir_mean),
                                             flights.out$head_dir_mean)
heading_normalized_all_in   <-  mapply(ground_fun,
                                            circ.mean(flights.in$head_dir_mean),
                                            flights.in$head_dir_mean)
# hist(heading_normalized_all_out)
# hist(heading_normalized_all_in)

heading_normalized_1_out   <-  mapply(ground_fun,
                                           circ.mean(flights.half.1.out$head_dir_mean),
                                           flights.half.1.out$head_dir_mean)
heading_normalized_1_in   <-  mapply(ground_fun,
                                          circ.mean(flights.half.1.in$head_dir_mean),
                                          flights.half.1.in$head_dir_mean)
heading_normalized_2_out   <-  mapply(ground_fun,
                                           circ.mean(flights.half.2.out$head_dir_mean),
                                           flights.half.2.out$head_dir_mean)
heading_normalized_2_in   <-  mapply(ground_fun,
                                          circ.mean(flights.half.2.in$head_dir_mean),
                                          flights.half.2.in$head_dir_mean)







# Plots of T or H vs. T - H     -------


# Plot of track vs. track - heading
# all data
plot(flights.combined$ground_dir_mean ~ track_head_all)
plot(track_ground_normalized_all ~ track_head_all)
plot(track_ground_normalized_all ~ flights.combined$ground_dir_mean)


# all out
plot(flights.out$ground_dir_mean ~ track_head_all_out)
plot(track_ground_normalized_all_out ~ flights.out$ground_dir_mean)
plot(track_ground_normalized_all_out ~ track_head_all_out)
abline(lm(track_ground_normalized_all_out ~ track_head_all_out))
plot(track_ground_normalized_all_out ~ track_head_all_out,
     xlim = c(-70,70), ylim = c(-70,70))

# all in
plot(flights.in$ground_dir_mean ~ track_head_all_in)
plot(track_ground_normalized_all_in ~ flights.in$ground_dir_mean)
plot(track_ground_normalized_all_in ~ track_head_all_in)
abline(lm(track_ground_normalized_all_in ~ track_head_all_in))

# all out - first half
plot(flights.half.1.out$ground_dir_mean ~ track_head_1_out)
plot(track_ground_normalized_1_out ~ flights.half.1.out$ground_dir_mean)
plot(track_ground_normalized_1_out ~ track_head_1_out)
abline(lm(track_ground_normalized_1_out ~ track_head_1_out))

# Indicating wind speeds, high (dark red) to low (light blue)
maxColorValue <- 200
palette <- colorRampPalette(c("light blue","dark red"))(maxColorValue)
plot(track_ground_normalized_1_out ~ track_head_1_out,
     col = palette[cut(flights.half.1.out$windspeed, maxColorValue)])




# all out - second half
plot(flights.half.2.out$ground_dir_mean ~ track_head_2_out)
plot(track_ground_normalized_2_out ~ flights.half.2.out$ground_dir_mean)
plot(track_ground_normalized_2_out ~ track_head_2_out)
abline(lm(track_ground_normalized_2_out ~ track_head_2_out))

plot(track_ground_normalized_2_out ~ track_head_2_out, xlim = c(-90,90),
     ylim = c(-90,90))
abline(a = 0, b = 1)
points(track_ground_normalized_1_out ~ track_head_1_out, col = "red")

maxColorValue <- 200
palette <- colorRampPalette(c("light blue","dark red"))(maxColorValue)
plot(track_ground_normalized_1_in ~ track_head_1_in,
     col = palette[cut(flights.half.1.in$windspeed, maxColorValue)])
abline(lm(track_ground_normalized_1_in ~ track_head_1_in))
f <- track_ground_normalized_1_in > -100 & 
  track_ground_normalized_1_in < 100 &
  track_head_1_in > -60 &
  track_head_1_in < 60
abline(lm(track_ground_normalized_1_in[f] ~ track_head_1_in[f]),
       lwd = 2, lty = 2)



maxColorValue <- 200
palette <- colorRampPalette(c("light blue","dark red"))(maxColorValue)
plot(track_ground_normalized_2_in ~ track_head_2_in,
     col = palette[cut(flights.half.1.in$windspeed, maxColorValue)])
abline(lm(track_ground_normalized_2_in ~ track_head_2_in))
f <- track_ground_normalized_2_in > -100 & 
  track_ground_normalized_2_in < 100 &
  track_head_2_in > -60 &
  track_head_2_in < 60
abline(lm(track_ground_normalized_2_in[f] ~ track_head_2_in[f]),
       lwd = 2, lty = 2)


# In start vs. end
plot(track_ground_normalized_2_in ~ track_head_2_in, xlim = c(-90,90),
     ylim = c(-90,90))
abline(a = 0, b = 1)
abline(v = 0)
arrows(track_head_1_in,track_ground_normalized_1_in,
         track_head_2_in,track_ground_normalized_2_in,
         col = "light grey")
points(track_ground_normalized_1_in ~ track_head_1_in, col = "red")
lm(track_ground_normalized_1_in ~ track_head_1_in)
lm(track_ground_normalized_2_in ~ track_head_2_in)


# Out start vs. end
plot(track_ground_normalized_2_out ~ track_head_2_out, xlim = c(-90,90),
     ylim = c(-90,90))
abline(a = 0, b = 1)
abline(v = 0)
arrows(track_head_1_out,track_ground_normalized_1_out,
       track_head_2_out,track_ground_normalized_2_out,
       col = "light grey")
points(track_ground_normalized_1_out ~ track_head_1_out, col = "red")
lm(track_ground_normalized_1_out ~ track_head_1_out)
lm(track_ground_normalized_2_out ~ track_head_2_out)




plot(flights.in$ground_dir_mean ~ track_head_all_in,
  xlim = c(-100, 100), ylim = c(0,360),
  col = "dark blue")
points(flights.out$ground_dir_mean ~ track_head_all_out,
      col = "dark red")

plot(track_ground_normalized_all_in ~ track_head_all_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(track_ground_normalized_all_out ~ track_head_all_out,
       col = "red")
abline(lm(track_ground_normalized_all_in ~ track_head_all_in),
       col = "blue", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_all_out ~ track_head_all_out),
       col = "red", lwd = 2, lty = 2)




# plot(track_ground_normalized_all_in ~ flights.in$alpha_mean,
#      xlim = c(-100, 100), ylim = c(-180,180),
#      col = "blue")
# points(track_ground_normalized_all_out ~ flights.out$alpha_mean,
#        col = "red")
# abline(lm(track_ground_normalized_all_in ~ flights.in$alpha_mean),
#        col = "blue", lwd = 2, lty = 2)
# abline(lm(track_ground_normalized_all_out ~ flights.out$alpha_mean),
#        col = "red", lwd = 2, lty = 2)
# 
# 
# plot(flights.in$alpha_mean~track_head_all_in)



# Plots for inward vs outward flights
plot(track_ground_normalized_all_in ~ track_head_all_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(track_ground_normalized_all_out ~ track_head_all_out,
       col = "red")
abline(lm(track_ground_normalized_all_in ~ track_head_all_in),
       col = "blue", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_all_out ~ track_head_all_out),
       col = "red", lwd = 2, lty = 2)


# Plots for inward flights only for track and heading
plot(track_ground_normalized_all_in ~ track_head_all_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
abline(lm(track_ground_normalized_all_in ~ track_head_all_in),
       col = "blue", lwd = 2, lty = 2)
points(heading_normalized_all_in ~ track_head_all_in,
       col = "red")
abline(lm(heading_normalized_all_in ~ track_head_all_in),
       col = "red", lwd = 2, lty = 2)

# Plots for inward flights only track and heading
f <- track_ground_normalized_all_in > 0 &  track_head_all_in < 50
f2 <- track_ground_normalized_all_in < 0 &  track_head_all_in < 50
plot(track_ground_normalized_all_in ~ track_head_all_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(heading_normalized_all_in ~ track_head_all_in,
       col = "red")
abline(lm(track_ground_normalized_all_in[f] ~ track_head_all_in[f]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_all_in[f] ~ track_head_all_in[f]),
       col = "red", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_all_in[f2] ~ track_head_all_in[f2]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_all_in[f2] ~ track_head_all_in[f2]),
       col = "red", lwd = 2, lty = 2)



track_ground_normalized_2_in
# Plots for inward flights only - 2nd half track and heading
f <- track_ground_normalized_2_in > 0 &  track_head_2_in < 50
f2 <- track_ground_normalized_2_in < 0 &  track_head_2_in < 50
plot(track_ground_normalized_2_in ~ track_head_2_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(heading_normalized_2_in ~ track_head_2_in,
       col = "red")
abline(lm(track_ground_normalized_2_in[f] ~ track_head_2_in[f]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_2_in[f] ~ track_head_2_in[f]),
       col = "red", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_2_in[f2] ~ track_head_2_in[f2]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_2_in[f2] ~ track_head_2_in[f2]),
       col = "red", lwd = 2, lty = 2)


# Plots for inward flights only - 1st half track and heading
f <- track_ground_normalized_1_in > 0 &  track_head_1_in < 50
f2 <- track_ground_normalized_1_in < 0 &  track_head_1_in < 50
plot(track_ground_normalized_1_in ~ track_head_1_in,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(heading_normalized_1_in ~ track_head_1_in,
       col = "red")
abline(lm(track_ground_normalized_1_in[f] ~ track_head_1_in[f]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_1_in[f] ~ track_head_1_in[f]),
       col = "red", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_1_in[f2] ~ track_head_1_in[f2]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_1_in[f2] ~ track_head_1_in[f2]),
       col = "red", lwd = 2, lty = 2)


# Plots for outward flights track and heading
plot(track_ground_normalized_all_out ~ track_head_all_out,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(heading_normalized_all_out ~ track_head_all_out,
       col = "red")
abline(lm(track_ground_normalized_all_out ~ track_head_all_out),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_all_out ~ track_head_all_out),
       col = "red", lwd = 2, lty = 2)


# Plots for inward flights only track and heading
f <- track_ground_normalized_all_out > 0 &  track_head_all_out < 50
f2 <- track_ground_normalized_all_out < 0 &  track_head_all_out < 50
plot(track_ground_normalized_all_out ~ track_head_all_out,
     xlim = c(-100, 100), ylim = c(-180,180),
     col = "blue")
points(heading_normalized_all_out ~ track_head_all_out,
       col = "red")
abline(lm(track_ground_normalized_all_out[f] ~ track_head_all_out[f]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_all_out[f] ~ track_head_all_out[f]),
       col = "red", lwd = 2, lty = 2)
abline(lm(track_ground_normalized_all_out[f2] ~ track_head_all_out[f2]),
       col = "blue", lwd = 2, lty = 2)
abline(lm(heading_normalized_all_out[f2] ~ track_head_all_out[f2]),
       col = "red", lwd = 2, lty = 2)

