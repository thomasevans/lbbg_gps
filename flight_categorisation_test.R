# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script is to test different methods to categorise flight to sepperate out commuting (directed) flight from other types of flight (mainly expected to be searching).


#'  1. Get a subset of flights to use in testing - get 50 of each
#'   i.  For outward flights
#'   ii. For inwward flights

# Get data - database  ####

# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

#Get a copy of the flights DB table.
flights.all <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r

flights.all$start_time <- as.POSIXct(flights.all$start_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

flights.all$end_time <- as.POSIXct(flights.all$end_time,
                               tz="GMT",
                               format="%Y-%m-%d %H:%M:%S")

# Make flight type a factor rather than character
flights.all$trip_flight_type <- as.factor(flights.all$trip_flight_type)
# summary(flights.all$trip_flight_type)


# Subsetting flight data ####
flights.com <- subset(flights.all, trip_flight_type == "inward"  | trip_flight_type == "outward")

# Fixed seed
set.seed(25)
# Sample of commuting flights
fs <- sample(flights.com$flight_id,100)

# Get GPS points for these flights
source("gps_extract.R")

# Index vector for flights.com
flights.com.ind <- 1:length(flights.com$flight_id)
# i <- 5
# Initialise 'points', which will hold all point data
points <- NULL
for( i in 1:length(fs)){
  id  <- fs[i]
  ind <- flights.com.ind[flights.com$flight_id == id]
  points.temp <- gps.extract(flights.com$device_info_serial[ind],flights.com$start_time[ind],flights.com$end_time[ind])
  points <- rbind(points,points.temp)
}

points$date_time <- as.POSIXct(points$date_time,
                                   tz="GMT",
                                   format="%Y-%m-%d %H:%M:%S")

#'  2. Plot maps for non categorised outward and inward flights

# Map non-categorised flights ####
# Get mapping function
source("maps_flights.R")

# map specified
maps.flights(points, all.flights = FALSE, flight.id = fs[20:25])

maps.flights(points, all.flights = TRUE)


#'  3. Make categorisation alogrithm

# Categorisation testing ####
ft <- fs[c(27, 10, 36, 41)]
c(27, 10, 36, 41)
maps.flights(points, all.flights = FALSE, flight.id = ft)
maps.flights(points, all.flights = FALSE, flight.id = fs[10])

x <- c(1,2,3,NULL,NULL)
x <- NULL
mean(x)
str(points)

# 10

# Subset flight data
f.points <- subset(points,flight_id == fs[27])
n <- length(f.points$flight_id)
id <- fs[27]
str(flights.com)

# Get direction - outward or inward
dir <- 1
if(flights.com$trip_flight_type[flights.com$flight_id == id] == "inward") dir <- -1

# Calculate speed relative to displacement from island
d.dist <- f.points$nest_gc_dist[2:n] - f.points$nest_gc_dist[1:(n-1)]
d.dist <- d.dist *1000   # Get to metres
d.speed <- d.dist/ f.points$time_interval_s[2:n]
d.speed.cor <- d.speed*dir
d.speed.cor <- c(0,d.speed.cor) #add a value for first point

# Change in speed relative to island relative to last 3 points
# Reverse point list if inward flight (work from island out)
if(dir == -1) {
  ds <- rev(d.speed.cor)} else {
    ds <- d.speed.cor
  }

#Change in speed from previous points
d.dif <- function(i, ds = ds){
  ds[i]/mean(ds[(i-1):(i-3)])
}

#apply function
x <- sapply(c(4:length(ds)),d.dif,ds=ds) 
x <- c(1,1,1,x)   #include first 3 points

s <- TRUE
p.stop <- NULL
for(i in 1:length(x)){
  if(x[i] < 0.2 & s){
    s <- FALSE
    p.stop <- i
  }
}

# plot(rev(f.points$nest_gc_dist))
# points(rev(f.points$nest_gc_dist)[1:p.stop], col = "red")

p.ind <- rep(FALSE,length(f.points$nest_gc_dist))
p.ind[1:p.stop] <- TRUE
p.ind <- rev(p.ind)
plot(f.points$latitude, f.points$longitude)
points(f.points$latitude[p.ind], f.points$longitude[p.ind], col = "red")


plot(x, ylim = c(-2,2))
plot(rev(f.points$nest_gc_dist))

plot(d.speed.cor)
time_interval_s
nest_gc_dist




#'  
#'  4. Test alorithm on a few individual flights
#'  
#'  5. Test algorithm on full sample (50 of each flight type)
#'  
#'  6. When satisfied with algorithm apply to all 'commuting' flights, flights classified as outward or inward.
#'  
#'  7. Re-analyse all these flights to get summary statistics
#'  
#'  8. Re-run previous statistical analyses on newly analysed data.
