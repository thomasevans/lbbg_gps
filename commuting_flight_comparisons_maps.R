# 3.  Plot all the used foraging trips in the paired comparisons
# a.	Need to work out whether I can make these files smaller – seems like coastline is somehow rasterised rather than vectorised. 





#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r


#Get a copy of the flights DB table.
flights.new <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights_commuting_3 AS f
                    ORDER BY f.flight_id ASC;")


flights$start_time <- as.POSIXct(flights$start_time,
                 tz="GMT",
                 format="%Y-%m-%d %H:%M:%S")

flights$end_time <- as.POSIXct(flights$end_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

# str(flights.new)
flights.new$start_time <- as.POSIXct(flights.new$start_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")
# flights.new$start_time[1:10]
flights.new$end_time <- as.POSIXct(flights.new$end_time,
                               tz="GMT",
                               format="%Y-%m-%d %H:%M:%S")



#str(flights)  #check structure

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                            FROM lund_flights_weather AS f
                            ORDER BY f.flight_id ASC;")

# str(flights.weather)
flights.weather$start_time <- as.POSIXct(flights.weather$start_time,
                               tz="GMT",
                               format="%Y-%m-%d %H:%M:%S")





#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")



#Edited this to reference 'lund_flight_paramaters' rather than 'lund_flight_characteristics' as table name changed.
flights.characteristics <- sqlQuery(gps.db,as.is = TRUE, query=
                                    "SELECT DISTINCT f.*
                                    FROM lund_flight_paramaters AS f
                                    ORDER BY f.flight_id ASC;")

flights.characteristics$start_time <- 
  as.POSIXct(flights.characteristics$start_time,
             tz="GMT",
             format="%Y-%m-%d %H:%M:%S")

# str(flights.characteristics )
# flights.characteristics$start_time[1]

#Trip type and duration#######
trip_type <- rep(0, length(trips$trip_id))
trip_duration <- rep(0, length(trips$trip_id))
trip_gotland <- rep(0, length(trips$trip_id))
trip_distmax <- rep(0, length(trips$trip_id))

# names(trips)

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




# Add some more paramaters to filter
# Non full trips (i.e. where there is a gap of > 25 minutes?)
# Trips to Gotland
# 
# Might be better to create new columns and label trips by these info.

trip_gotland <- as.factor(trip_gotland)
# summary(trip_gotland)
#filters####
# hist(trip_distmax[outward])
# 
# par(mfrow = c(1,1))
# hist(trip_distmax[trip_distmax < 1000])
# names(flights)
# 
# par(mfrow = c(1,2))

outward <- (flights$trip_flight_type == "outward") & trip_gotland == 0 & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4 & flights$dist_a_b > 2000
# hist(flights$dist_a_b[outward]/1000, breaks = 20, main = "out", xlab = "Distance (km)", xlim = c(0,120))

inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000
# hist(flights$dist_a_b[inward]/1000, breaks = 40, main = "in", xlab = "Distance (km)", xlim = c(0,120))


# summary(inward)
# summary(outward)

# hist(flights$interval_mean[outward])
# length(flights$interval_mean[outward])



*********************
  # Paired data comparisons ####

# Angle with respect to wind
dif.angle <- flights$bearing_a_b - flights.characteristics$winddir
# hist(dif.angle)
dif.angle <- abs(dif.angle)
# hist(dif.angle)
# hist(dif.angle[outward] %% 180)
# hist(dif.angle[inward] %% 180)
# dif.angle <- dif.angle %% 180
cor.ang <- function(x){
  if(x > 180){ y <- 180 - (x - 180)}
  else y <- x
  return(y)
}
# cor.ang(181)
dif.angle <- sapply(dif.angle, cor.ang)
#  hist(dif.angle)


# Paired data   ####
#' Re-arrange the data for paired data comparison

flights.out <- cbind(flights[outward,],flights.characteristics[outward,],flights.weather[outward,],dif.angle[outward])
flights.in <- cbind(flights[inward,],flights.characteristics[inward,],flights.weather[inward,],dif.angle[inward])

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

# length(flights.in$flight_id)

# length(unique(flights.in$device_info_serial))



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



names(flights.out)[65] <- "dif.angle"
names(flights.in)[65] <- "dif.angle"
names(flights.out) == names(flights.in)

flights.combined <- rbind(flights.out,flights.in)

flights.combined   <- flights.combined[order(flights.combined$trip_id),]




# Get GPS points for flights ####
# str(flights.combined)

# setwd("D:/Dropbox/R_projects/lbbg_gps")

# Get gps_extract function
source("gps_extract.R")

points.old <- NULL
# str(flights.combined)
# Get points.old for all flights
for(i in 1:length(flights.combined$device_info_serial)){
#   for(i in 1:10){
  
x <- NA

x <- gps.extract(flights.combined$device_info_serial[i],
                 flights.combined$start_time[i],
                 flights.combined$end_time[i])

x <- cbind(x,i,flights.combined$flight.type[i],flights.combined$wind.type[i])
points.old <- rbind(points.old,x)
}

# str(points.old)
# summary(points.old$flight_type == "out")
# data frames of just outward or inward flights

x <- names(points.old)
# length(x)
x <- c(x[1:21],"flight.type","wind.type")
names(points.old) <- x


points.old.out <- points.old[points.old$flight.type == "out",]
points.old.in <- points.old[points.old$flight.type == "in",]




points.new <- NULL
# str(flights.combined)
# i <- 5
# Get points.new for all flights
for(i in 1:length(flights.combined$device_info_serial)){
  #   for(i in 1:10){
  
  x <- NA
  
#   flights.new
  
  flight_id <- flights.combined$flight_id[i]
  f <- flights.new$flight_id == flight_id
  x <- gps.extract(flights.new$device_info_serial[f],
                   flights.new$start_time[f],
                   flights.new$end_time[f])
#   str(flights)
  x <- cbind(x,i,flights$trip_flight_type[flights$flight_id == flight_id],flights.combined$wind.type[flights.combined$flight_id == flight_id])
  points.new <- rbind(points.new,x)
}

# str(points.new)
# summary(points.new$flight_type == "out")
# data frames of just outward or inward flights

x <- names(points.new)
# length(x)
x <- c(x[1:21],"flight.type","wind.type")
names(points.new) <- x

# str(points.new)
points.new.out <- points.new[points.new$flight.type == "outward",]
points.new.in <- points.new[points.new$flight.type == "inward",]







# length(unique(points.in$flight_id) )
# Mapping data #####

source("maps_flights_old_new.R")




# maps.flights(points.old.in, points.new.in, seed = 1, flight.num = 20, plot.title = "Inward flights")



pdf("inward_flights_new3.pdf")
# svg("inward_flights_02.svg")
maps.flights(points.old.in, points.new.in, seed = 35, all.flights = TRUE, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.old.in, points.new.in, seed = 1, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.old.in, points.new.in, seed = 2, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.old.in, points.new.in, seed = 3, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.old.in, points.new.in, seed = 4, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.old.in, points.new.in, seed = 5, flight.num = 20, plot.title = "Inward flights")
dev.off()


pdf("outward_flights_new3.pdf")
# svg("inward_flights_02.svg")
maps.flights(points.old.out, points.new.out, seed = 35, all.flights = TRUE, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.old.out, points.new.out, seed = 1, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.old.out, points.new.out, seed = 2, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.old.out, points.new.out, seed = 3, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.old.out, points.new.out, seed = 4, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.old.out, points.new.out, seed = 5, flight.num = 20, plot.title = "Outward flights")
dev.off()

# ?set.seed


#   names(trips.sample)



# 
# 
# # Colours #####
# library(RColorBrewer)
# display.brewer.all()
# 
# # General colours to be used for figure
# ?brewer.pal
# 
# # Generating more colours than are in the palette - includes intermediate values.
# col.line <- colorRampPalette(brewer.pal(12,"Paired"))(100)
# # Change alpha value, to make transparent - allow to see overplotting
# test <- adjustcolor(col.line, 0.4)
# plot(c(1:1000),col= test)

# alarm()

beep <- function(n = 9){
  x <- c(1,1,3,1,1,3,1,1,3,1,1)
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(x[i])
  }
}
beep()