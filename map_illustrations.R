
# flight.numbers <- f.s
# [1] 29534   428 16683 16341 35108 16014 32821 36693 37708  2918
flight.numbers <- c(29534,428,16683,16341,35108,16014,32821,36693,37708,2918)
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


flights.old <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")

flights.new <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights_commuting AS f
                    ORDER BY f.flight_id ASC;")
f.n <- flights.new$flight_id %in% flight.numbers

f <- flights.old$flight_id %in% flight.numbers

# summary(f)
trip_ids <- flights.old$trip_id[f]

# names(flights.new)

trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

t.f <- trips$trip_id %in% trip_ids



# Whole trip -----
win.metafile("map.ex.trip_mono.wmf",width = 7, height = 7)

source("map_start_source.R")

# names(gps.data)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= trips$start_time[t.f][i]) & date_time <= trips$end_time[t.f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = "black", lty = 1, lwd = 2)
}

source("map_end_source.R")
dev.off()


# Whole trip -----
win.metafile("map.ex.trip.wmf",width = 7, height = 7)

source("map_start_source.R")

# names(gps.data)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= trips$start_time[t.f][i]) & date_time <= trips$end_time[t.f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 2)
}

source("map_end_source.R")
dev.off()

# Flights ------
win.metafile("map.ex.flight_trip.wmf",width = 7, height = 7)

source("map_start_source.R")

# names(gps.data)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= trips$start_time[t.f][i]) & date_time <= trips$end_time[t.f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 1)
}

for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.old$start_time[f][i]) & date_time <= flights.old$end_time[f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 3)
}

source("map_end_source.R")
dev.off()

# Flights old ------
win.metafile("map.ex.flight.wmf",width = 7, height = 7)

source("map_start_source.R")

# names(gps.data)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.old$start_time[f][i]) & date_time <= flights.old$end_time[f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 2)
}

source("map_end_source.R")
dev.off()


# Flight new and old -------
win.metafile("map.ex.new_old.wmf",width = 7, height = 7)

source("map_start_source.R")

# names(gps.data)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.old$start_time[f][i]) & date_time <= flights.old$end_time[f][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = "black", lty = 1, lwd = 2)
}


for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[f.n][i]) & date_time <= flights.new$end_time[f.n][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 2)
}

source("map_end_source.R")
dev.off()

# Map new only ----
win.metafile("map.ex.new3.wmf",width = 7, height = 7)
svg("map.ex.new2.svg", width = 7, height = 7)
cairo_pdf("map.ex.new3.pdf", width = 7, height = 7)
pdf("map.ex.new4.pdf", width = 7, height = 7)

# Flight new and old -------
source("map_start_source.R")
source("map_start_source2.R")

for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[f.n][i]) & date_time <= flights.new$end_time[f.n][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 2)
}
source("map_end_source2.R")
source("map_end_source.R")
dev.off()
# ?win.metafile




# Map new - 1st only ----
win.metafile("map.ex.new.half.wmf",width = 7, height = 7)
# Flight new and old -------
source("map_start_source.R")
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[f.n][i]) & date_time <= flights.new$end_time[f.n][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 1)
}
# names(flights.new)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[f.n][i]) & date_time <= flights.new$mid_dist_time[f.n][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 3)
}

source("map_end_source.R")
dev.off()
# ?win.metafile





# Map new - 1st only ----
win.metafile("map.ex.half.wmf",width = 7, height = 7)
source("map_start_source.R")

# names(flights.new)
for(i in 1:length(trip_ids)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[f.n][i]) & date_time <= flights.new$mid_dist_time[f.n][i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 3)
}

source("map_end_source.R")
dev.off()







# Map all inward flights ----
win.metafile("map.ex.new_all.wmf",width = 7, height = 7)
svg("map.ex.new2_all.svg", width = 7, height = 7)
cairo_pdf("map.ex.new3_all.pdf", width = 7, height = 7)
pdf("map.ex.new4_all.pdf", width = 7, height = 7)

str(flights.new)


flights.n <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.flight_id
                        FROM lund_flights AS f
                        WHERE f.trip_flight_type = 'inward'
                        ORDER BY f.flight_id ASC;")

flight.numbers <- 




source("map_start_source.R")
source("map_start_source2.R")

# all.flights <- flights.new$flight_id 


for(i in 1:length(flights.new$trip_id)){
  
  x <- trip_ids[i]
  gps.sub <- subset(gps.data, (date_time >= flights.new$start_time[i]) & date_time <= flights.new$end_time[i],
                    select=c(longitude, latitude))
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec[i], lty = 1, lwd = 2)
}
source("map_end_source2.R")
source("map_end_source.R")
dev.off()
# ?win.metafile
