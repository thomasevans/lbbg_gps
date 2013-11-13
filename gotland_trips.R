# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Looking at trips performned to Gotland - proportion of trips by period
# Number of trips etc.



#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

names(trips)
summary(as.factor(trips$gotland))
str(trips)

#Keep only trips where the minimum interval between GPS fixes is 800s

trips  <-  subset(trips,interval_min < 800)
trips  <-  subset(trips,dist_max > 2)   # Only include trips where maximum distance is at least 2 km

trips.g  <-  subset(trips,gotland ==1)   # Only include trips where maximum distance is at least 2 km


summary(trips.g$duration_s < 12*60*60)
summary(trips$duration_s < 12*60*60)

hist(trips.g$duration_s[trips.g$duration_s < 72*60*60])
# hist(trips$duration_s[trips$duration_s < 72*60*60])
# 100000/60/24/60


plot(trips$start_time,trips$duration_s, col = "red")
points(trips.g$start_time)

#Get month numbers
months <- as.factor(as.numeric(format(trips$start_time,format='%m')))
summary(months)
years <- as.factor(as.numeric(format(trips$start_time,format='%Y')))
summary(years)

days <- as.factor(as.numeric(format(trips$start_time,format='%j')))


got.month <- summary(months[trips$gotland == 1])
all.month <- summary(months)

# Proportion of all.trips to Gotland by month
prop <- as.vector(got.month)/as.vector(all.month)
prop

round((prop*100),1)



got.day<- summary(days[trips$gotland == 1])
all.day <- summary(days)

min(as.numeric(as.character(days)))
max(as.numeric(as.character(days)))

daysn <- as.numeric(as.character(days))
x <- NULL
y <- NULL
t <- NULL
# i <- 1
for(i in 1:140){
  x[i] <- sum((daysn[trips$gotland == 1] > ((i*1) + 90)) &  (daysn[trips$gotland == 1] < ((i*1 )+ 100)))
  y[i] <- sum((daysn > ((i*1) + 90)) &  (daysn < ((i*1) + 100)))
  t[i] <- ((i*1) + 90)
}
pro.ten <- 100*(x/y)

# Plot graph of proportion of foraging trips to Gotland by day (ten day window)
plot(t,pro.ten, ylim = c(0,100), xaxt= "n", xlab = "", ylab="% of trips to Gotland", main = "Gotland foraging trips", type="l")
d <- as.Date(c("2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01"))
d <- as.numeric(format(d,format='%j'))
d.lab <- c("April","May","June","July","August")
axis(1,at =d, labels = d.lab)
# segments(t,pro.ten)

# Proportion of all.trips to Gotland by day
prop.day <- as.vector(got.day)/as.vector(all.day)
prop.day

day.prop <- round((prop.day*100),1)
plot(day.prop)



y.n <- as.numeric(as.character(years))
high.res <- trips.g[y.n == 2013,]
str(high.res)

high.res <- subset(high.res,interval_min < 100)


#Plot these:
source("gps_extract.R")


map.trip(2582, high.res)

id <- 2381
trips <- high.res



# Would be good to later have this as a stand alone function which can be 'sourced' by other scripts. 
#Dependent on two other functions which are sourced within function:
# source("gps_extract.R")
# source("flights_extract.R")
# For a 'final' version it would be nice to only have to provide trip id, then within the function the neccessary data for that trip can all be downloaded (database).

map.trip <- function(id, trips){
  #Function to make a map for foraging trip
  
  require(maps)
  require(RODBC)
  source("gps_extract.R")
  source("flights_extract.R")
  #Establish a connection to the database
  gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  
  
  #First subset the data that we require  
  i      <-  trips$device_info_serial[trips$trip_id == id]
  start.t  <-  trips$start_time[trips$trip_id == id]
  end.t    <-  trips$end_time[trips$trip_id == id]
  
  gps.sub <- gps.extract(i, start.t, end.t)
  flights.sub <- flights.extract(i, start.t, end.t)
  
  
  # Set map limits
  c.xlim <- range(gps.sub$longitude)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(gps.sub$latitude)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par( mar = c(5, 4, 4, 2))
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="grey", bg = "white")
  # ?par
  
  # names(flights.sub)
  # Add points
  
  #Flight points
  #
  i <- 1
  for( i in seq(along = flights.sub$trip_flight_type)){
    flight.type <- flights.sub$trip_flight_type[i]
    if(flight.type == "outward"){
      points(gps.sub$longitude[gps.sub$flight_id ==
                                 flights.sub$flight_id[i]],
             gps.sub$latitude[gps.sub$flight_id ==
                                flights.sub$flight_id[i]],
             col = "blue", pch = 19)} else 
               if(flight.type == "inward"){
                 points(gps.sub$longitude[gps.sub$flight_id ==
                                            flights.sub$flight_id[i]],
                        gps.sub$latitude[gps.sub$flight_id ==
                                           flights.sub$flight_id[i]],
                        col = "red", pch = 19)}  else 
                        {
                          points(gps.sub$longitude[gps.sub$flight_id ==
                                                     flights.sub$flight_id[i]],
                                 gps.sub$latitude[gps.sub$flight_id ==
                                                    flights.sub$flight_id[i]],
                                 col = "dark grey", pch = 19)}
  }
  
  # Other points
  points(gps.sub$longitude[gps.sub$flight_id == 0],
         gps.sub$latitude[gps.sub$flight_id == 0],
         col = "black")
  
  
  # Add lines
  
  #First grey for all
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = "grey")
  
  for( i in seq ( along = (unique(gps.sub$flight_id)))){
    
    y <- unique(gps.sub$flight_id)[i]
    if(y != 0){
      x <- subset(gps.sub, flight_id == y,
                  select=c(longitude, latitude))
      z <- length(x$longitude)
      n <- length(gps.sub$longitude)
      segments(x$longitude[-1], x$latitude[-1],
               x$longitude[1:z-1], x$latitude[1:z-1],
               col = "black")
    }
  }
  
  # Scale bar and axis
  map.scale(ratio = FALSE)
  box()
  axis(side=(1),las=1)
  axis(side=(2),las=1)
  #   ?text
  mtext(paste("Device: ", trips$device_info_serial[id],
              "    Trip: ", trips$trip_id[id])
        , side = 3, line = 2, cex = 1)
  mtext(paste("Departure time: ", min(gps.sub$date_time), " UTC")
        , side = 3, line = 1, cex = 1)
  
  dur <- as.difftime(trips$duration_s[id], units= "secs")
  dur <- as.numeric(dur, units="hours")
  mtext(paste("Trip duration: ",
              format(round(dur, 2), nsmall = 2) , " hours")
        , side = 3, line = 0, cex = 1)
  #   ?grconvertX
  #   legend("topleft", pch=c(19, 19, 19, 1),
  #          c("IN", "OUT", "OTHER", "NON-FLIGHT"),
  #          col = c("red", "blue", "dark grey", "black"),
  #          )
  #   ?legend
  #   ?legend
  
}

