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


flights$start_time <- as.POSIXct(flights$start_time,
                 tz="GMT",
                 format="%Y-%m-%d %H:%M:%S")

flights$end_time <- as.POSIXct(flights$end_time,
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
flights.characteristics <- sqlQuery(gps.db,as.is = TRUE, query="SELECT DISTINCT f.*
                                    FROM lund_flight_paramaters AS f
                                    ORDER BY f.flight_id ASC;")
flights.characteristics$start_time <- as.POSIXct(flights.characteristics$start_time,
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

par(mfrow = c(1,1))
hist(trip_distmax[trip_distmax < 1000])
names(flights)

par(mfrow = c(1,2))

outward <- (flights$trip_flight_type == "outward") & trip_gotland == 0 & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4 & flights$dist_a_b > 2000
hist(flights$dist_a_b[outward]/1000, breaks = 20, main = "out", xlab = "Distance (km)", xlim = c(0,120))

inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000
hist(flights$dist_a_b[inward]/1000, breaks = 40, main = "in", xlab = "Distance (km)", xlim = c(0,120))


summary(inward)
summary(outward)

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

points <- NULL
str(flights.combined)
# Get points for all flights
for(i in 1:length(flights.combined$device_info_serial)){
#   for(i in 1:10){
  
x <- NA

x <- gps.extract(flights.combined$device_info_serial[i],
                 flights.combined$start_time[i],
                 flights.combined$end_time[i])

x <- cbind(x,i,flights.combined$flight.type[i],flights.combined$wind.type[i])
points <- rbind(points,x)
}

# str(points)
# summary(points$flight_type == "out")
# data frames of just outward or inward flights

x <- names(points)
length(x)
x <- c(x[1:21],"flight.type","wind.type")
names(points) <- x


points.out <- points[points$flight.type == "out",]
points.in <- points[points$flight.type == "in",]

# length(unique(points.in$flight_id) )
# Mapping data #####

maps.flights <- function(points.data=NULL, seed = 2, plot.title = "", all.flights = FALSE, flight.num = 50, alpha = 0.5){
#   ?title
  #Function to map flights
  #  Provide dataframe with flights points
  # If you want all flights plot, enter 'TRUE', default 'FALSE'
  # Choose number of flights to plot, 50 is default (ignored if all.flights = TRUE)
  # Alpha - for transparency of lines - when saving to some image types transparency is not supported, then enter 1 for alpha (i.e. not transparent).
  library(maps)
  
  set.seed(seed)
  
  
  fl.n <- unique(points.data$flight_id)  
  
  
  if(all.flights){
    f.s <- fl.n  
    flight.num <- length(fl.n)
  }else  f.s <- sample(fl.n,flight.num)
  
#   points.data <- points.in
  
    # Set map limits
  c.xlim <- range(points.data$longitude[points.data$flight_id %in% f.s])
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(points.data$latitude[points.data$flight_id %in% f.s])
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
#   par(bg = 'white')
  
    plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="white", bg = "grey")
#   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
#          "black")
    title(plot.title)
#     seed <- 3
    mtext(paste("seed: ",seed))
  
  # colours for lines  
#     library(RColorBrewer)
    # Generating more colours than are in the palette - includes intermediate values.
#     col.line <- colorRampPalette(brewer.pal(11,"Spectral"))(length(fl.n))
#     # Change alpha value, to make transparent - allow to see overplotting
#     col.line <- adjustcolor(col.line, 0.6)
#     # Shuffle colours
#     col.line <- col.line[sample.int(length(col.line))]
    
#   ?rainbow
  # Get colours from rainbow scale
  

  
  
#   ?rainbow
  col.line <- rainbow(length(fl.n), alpha = alpha)
  # 
  col.line <- col.line[sample.int(length(col.line))]
  

  # Plot lines for each flight
     for(i in 1:flight.num){
      
      x <- f.s[i]
      gps.sub <- subset(points.data, flight_id == x,
                     select=c(longitude, latitude))
      n <- length(gps.sub$longitude)
      segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
               gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
               col = col.line[i], lwd = 2)
  }
  
  # Scale bar and axis
  x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
  y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
  map.scale(x,y,ratio = FALSE)
#   ?map.scale
  box()
  axis(side=(1),las=1)
  axis(side=(2),las=1)
}
  
# ??save.wmf
# ?win.metafile
# win.metafile("inward_flights_01.wmf")
# install.packages("devEMF")
# library(devEMF)

# (filename = "inward_flights_01.emf", type= "emf")



pdf("inward_flights.pdf")
# svg("inward_flights_02.svg")
maps.flights(points.in, seed = 35, all.flights = TRUE, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.in, seed = 1, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.in, seed = 2, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.in, seed = 3, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.in, seed = 4, flight.num = 20, plot.title = "Inward flights")
maps.flights(points.in, seed = 5, flight.num = 20, plot.title = "Inward flights")
dev.off()


pdf("outward_flights.pdf")
# svg("inward_flights_02.svg")
maps.flights(points.out, seed = 35, all.flights = TRUE, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.out, seed = 1, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.out, seed = 2, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.out, seed = 3, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.out, seed = 4, flight.num = 20, plot.title = "Outward flights")
maps.flights(points.out, seed = 5, flight.num = 20, plot.title = "Outward flights")
dev.off()

# ?set.seed


#   names(trips.sample)





# Colours #####
library(RColorBrewer)
display.brewer.all()

# General colours to be used for figure
?brewer.pal

# Generating more colours than are in the palette - includes intermediate values.
col.line <- colorRampPalette(brewer.pal(12,"Paired"))(100)
# Change alpha value, to make transparent - allow to see overplotting
test <- adjustcolor(col.line, 0.4)
plot(c(1:1000),col= test)