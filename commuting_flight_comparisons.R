#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Description#######
#In this script we produce various figures and summary statistics to compare outward and inward commuting flights.
#These were originally prepared for a meeting with Susanne and Anders on 2013-01-17


#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")

flights_weather <- ???????


#Trip type and duration#######
trip_type <- 0
trip_duration <- 0
for(i in seq(along=flights$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id == flights$trip_id[i]][1]
  trip_duration[i] <- trips$duration_s[trips$trip_id == flights$trip_id[i]][1]
}



#filters####
outward <- flights$trip_flight_type == "outward"
inward  <- flights$trip_flight_type == "inward"

# Speed comparision######
names(flights)

par(mfrow = c(1,2))
# Resultant speed
hist(flights$speed_a_b[outward & flights$speed_a_b < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30), main = "outward", xlab = "speed a to b in ms -1")
x1 <- mean(flights$speed_a_b[outward & flights$speed_a_b < 25], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_a_b[inward & flights$speed_a_b < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30), main = "inward", xlab = "speed a to b in ms -1")
speed_inst_med
x2 <- mean(flights$speed_a_b[inward & flights$speed_a_b < 25], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")

par(mfrow = c(1,2))
# Mean speed
hist(flights$speed_inst_mean[outward & flights$speed_inst_mean < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20), main = "Outward", xlab = "Speed - mean - ms-1")
x1 <- mean(flights$speed_inst_mean[outward & flights$speed_inst_mean < 25], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_inst_mean[inward & flights$speed_inst_mean < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20), main = "Inward", xlab = "Speed - mean - ms-1")
x2 <- mean(flights$speed_inst_mean[inward & flights$speed_inst_mean < 25], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")


# Median speed
hist(flights$speed_inst_med[outward & flights$speed_inst_med < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20), main = "Outward", xlab = "Speed - median - ms-1")
x1 <- mean(flights$speed_inst_med[outward & flights$speed_inst_med < 25], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")
hist(flights$speed_inst_med[inward & flights$speed_inst_med < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.20), main = "Inward", xlab = "Speed - median - ms-1")
speed_inst_med
x2 <- mean(flights$speed_inst_med[inward & flights$speed_inst_med < 25], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")



# Dispersion measures###############

# Straightness
names(flights)
hist(flights$straigtness[inward & flights$straigtness < 1.2], xlim = c(0,1.2), breaks = 20, freq = FALSE, ylim = c(0, 5), main = "Inward", xlab = "Straightness")
x1 <- mean(flights$straigtness[inward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$straigtness[outward], xlim = c(0,1.2), breaks = 20, freq = FALSE, ylim = c(0, 5), main = "Outward", xlab = "Straightness")
speed_inst_med
x2 <- mean(flights$straigtness[outward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")

x1
x2

summary(flights$straigtness[inward & flights$straigtness < 1.2] == 0)

plot(flights$straigtness[outward] ~ flights$speed_a_b[outward], xlim = c(0, 20))


# r-vector, or 'rho'
hist(flights$rho[inward & flights$straigtness < 1.2], xlim = c(0,1), breaks = 20, freq = FALSE, ylim = c(0, 3), main = "Inward", xlab = "r")
x1 <- mean(flights$rho[inward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$rho[outward], xlim = c(0,1), breaks = 20, freq = FALSE, ylim = c(0, 3), main = "Outward", xlab = "r")
speed_inst_med
x2 <- mean(flights$rho[outward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")

x1
x2



#Dispersion versus speed##########
plot(flights$rho[inward & flights$straigtness < 1.2] ~ flights$speed_inst_med[inward & flights$straigtness < 1.2], xlim = c(3,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Inward", xlab = "Speed - median - ms-1", ylab = "r")

plot(flights$rho[outward & flights$straigtness < 1.2] ~ flights$speed_inst_med[outward & flights$straigtness < 1.2], xlim = c(3,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Ourtward", xlab = "Speed - median - ms-1", ylab = "r")




plot(flights$rho[inward & flights$straigtness < 1.2] ~ flights$speed_a_b[inward & flights$straigtness < 1.2], xlim = c(0,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Inward", xlab = "Speed - median - ms-1", ylab = "r")

plot(flights$rho[outward & trip_duration > 60*30] ~ flights$speed_a_b[outward & flights$straigtness < 1.2], xlim = c(0,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Ourtward", xlab = "Speed - median - ms-1", ylab = "r")


# Flight duration###########
par(mfrow=c(1,2))
#Plotting flight duration
hist(flights$duration[trip_duration > 60*30 & inward & flights$duration < 2*60*60]/60,
     main = "Inward", xlab = "flight duration (minutes)", col = "grey")
hist(flights$duration[trip_duration > 60*30 & outward & flights$duration < 2*60*60]/60,
     main = "outward", xlab = "flight duration (minutes)", col = "grey")

# Flight altitude##############
par(mfrow=c(1,2))
#Plotting flight duration
hist(flights$alt_max[trip_duration > 60*30 & inward & trip_type == 0 & flights$alt_max < 500 & flights$alt_max > -50],
     main = "Inward", xlab = "Altitude (max) - m", col = "grey"
     , xlim = c(-50, 500), ylim = c(0, 600))
hist(flights$alt_max[trip_duration > 60*30 & outward & trip_type == 0 & flights$alt_max < 500 & flights$alt_max > -50],
     main = "outward", xlab = "Altitude (max) - m", col = "grey"
     , xlim = c(-50, 500), ylim = c(0, 600))



# Flight directions#############

#Flight directions
library(circular)
par(mfrow=c(1,1))


aspect.plot <- function(p, p.bins=90, main = "", p.bw=30, p.axis=seq(0, 270, by=90), plot.title=NULL, p.shrink = 2) {
  # plot a vector of aspect measurements, with units of degrees, measured via compass
  #This function is using code from: http://casoilresource.lawr.ucdavis.edu/drupal/node/1042
  #Developed by Dylan E. Beaudette: http://casoilresource.lawr.ucdavis.edu/drupal/node/905
  
  p <- na.omit(p)
  
  # make a circular class object: using native degrees, template sets zero and direction
  c.p <- circular(p, units='degrees', template="geographics")
  
  # compute the mean
  m.p <- mean(c.p)
  
  # compute mean resultant length
  rho.p <- rho.circular(c.p)
  
  # setup custom axis
  a.p <- circular(p.axis, units='degrees', template="geographics")
  
  # circular histogram
  plot(c.p, axes = FALSE, stack = TRUE, bins=p.bins, shrink = p.shrink,
       cex = 1, sep = 0.06, pch=21, col=1,lwd=3, bg="#0000ff22")
  
  # add circular density, bw is the smoothness parameter
  lines(density(c.p, bw=p.bw),lwd=3, col="red", lty=2)
  
  # add axes: note work-around for strange bug...
  axis.circular(at=(-a.p) + 90, labels=c("N","E","S","W"), cex=1,5)
  
  # annotate north
  #text(0, 1.125, 'North', cex=0.85, font=1)
  
  # annotate mean with an arrow
  arrows.circular(m.p, shrink=rho.p, length=0.10, lwd=3, col="black")
  
  # add title
   text(0, -.25, main, cex=0.85, font=2)
}

bearings.out <- flights$bearing_a_b[outward & trip_duration > 60*30 & trip_type == 0]
bearings.in <- flights$bearing_a_b[inward & trip_duration > 60*30 & trip_type == 0]


aspect.plot(bearings.in,p.bins=45, p.shrink = 5, main = "")
aspect.plot(bearings.out,p.bins=45, p.shrink = 5, main = "")



# Weather conditions###############

# Wind direction
names(flights_weather)

library(oce)
graph.object <- as.windrose(uwnd.10m, vwnd.10m, dtheta = 15, debug=getOption("oceDebug"))
plot(graph.object)

# Wind speed
hist(flights_weather$wind.speed)

#flight versus wind direction
par(mfrow = c(1,1))
plot(flights$bearing_a_b[outward & trip_duration > 60*30 & trip_type == 0], flights_weather$wind.dir[outward & trip_duration > 60*30 & trip_type == 0], xlab = "flight bearing", mar = c(10,10,4,2 ) + 0.1, ylab = "wind direction")

par(mfrow = c(1,2))


dif.angle <- flights$bearing_a_b[inward & trip_duration > 60*30 & trip_type == 0] - flights_weather$wind.dir[inward & trip_duration > 60*30 & trip_type == 0]

dif.angle <- abs(dif.angle) %% 180

hist(dif.angle, xlim= c(0,180), main = "Inward", xlab = "angle from wind", ylab = "frequency")

dif.angle <- flights$bearing_a_b[outward & trip_duration > 60*30 & trip_type == 0] - flights_weather$wind.dir[outward & trip_duration > 60*30 & trip_type == 0]

dif.angle <- abs(dif.angle) %% 180

hist(dif.angle, xlim= c(0,180), main = "Outward", xlab = "angle from wind", ylab = "frequency")




