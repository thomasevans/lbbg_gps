#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Figures for BES 2012 poster

setwd("F:/Documents/Work/Presentations/BES_2012_12_17")
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)




#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
FROM lund_trips AS t
ORDER BY t.trip_id ASC;")


flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")
#i <- 5
trip_type <- 0
trip_duration <- 0
for(i in seq(along=flights$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id == flights$trip_id[i]][1]
  trip_duration[i] <- trips$duration_s[trips$trip_id == flights$trip_id[i]][1]
}




par(mfrow=c(1,1))
#Plotting flight duration
hist(flights$duration[trip_duration > 60*30 & trip_type == 0 & flights$duration < 2*60*60]/60, main = "Flight duration (< 2 h)", 
     xlab = "flight duration (minutes)", col = "grey")


#Flight speed
hist(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "outward" & flights$speed_inst_med < 30], main = "Median instantaneous ground speed on outward flights",  xlab = "Speed (ms-1)", col = "grey", xlim=c(0,25),las=1)

mean(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "outward" & flights$speed_inst_med < 30])

hist(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$speed_inst_med < 30], main = "Median instantaneous ground speed on inward flights",  xlab = "Speed (ms-1)", col = "grey", xlim=c(0,25),las=1)
mean(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$speed_inst_med < 30])


hist(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "normal" & flights$speed_inst_med < 30], main = "Median instantaneous ground speed on within trip flights",  xlab = "Speed (ms-1)", col = "grey", xlim=c(0,25),las=1)
mean(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "normal" & flights$speed_inst_med < 30],na.rm=TRUE)


# main = "Median instantaneous ground speed of inward flights"
#speed figure:
postscript("inward_flight_speed.ps",width=4.6,height=4.6)  #for post script file (good for printed documents)
hist(flights$speed_inst_med[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$speed_inst_med < 30],  xlab =expression(bold(paste("Speed (ms",""^{-1}, ")"))), main="", col = "#4D897C",las=1,cex.lab=1.7,cex.axis=1.5,lwd=3,pin=c(2,2),font.lab=2, xlim=c(0,26))
box(lwd=3)
dev.off()

#altitude:
#
postscript("inward_flight_altitude.ps",width=4.6,height=4.6)  #for post script file (good for printed documents)
#?postscript
#50mm
#Maximum altitude reached during inward flight
hist(flights$alt_max[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$alt_max < 500 & flights$alt_max > -20],  breaks = 40, main = "", xlab = "Altitude (m)", col = "#4D897C",las=1,cex.lab=1.7,cex.axis=1.5,lwd=3,pin=c(2,2),font.lab=2)
box(lwd=3)
dev.off()
max(flights$alt_max[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$alt_max > -20])


#Flight directions
library(circular)

# plot a vector of aspect measurements, with units of degrees, measured via compass
#This function is using code from: http://casoilresource.lawr.ucdavis.edu/drupal/node/1042
#Developed by Dylan E. Beaudette: http://casoilresource.lawr.ucdavis.edu/drupal/node/905

aspect.plot <- function(p, p.bins=90, p.bw=30, p.axis=seq(0, 270, by=90), plot.title=NULL) {
  # remove NA
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
  plot(c.p, axes=FALSE, stack=TRUE, bins=p.bins, shrink=2.5, cex=1, sep=0.06, pch=21, col=1,lwd=3, bg="#4D897C")
  
  # add circular density, bw is the smoothness parameter
  lines(density(c.p, bw=p.bw),lwd=3, col="black", lty=2)
  
  # add axes: note work-around for strange bug...
  axis.circular(at=(-a.p) + 90, labels=c("N","E","S","W"), cex=2)
  
  # annotate north
  #text(0, 1.125, 'North', cex=0.85, font=1)
  
  # annotate mean with an arrow
  arrows.circular(m.p, shrink=rho.p, length=0.10, lwd=3, col="#4D897C")
  
  # add title
  # text(0, -0.25, plot.title, cex=0.85, font=2)
}
postscript("inward_flight_direction_2.ps",width=8,height=8)  #for post script file (good for printed documents)
#NB the poster figure was further edited using Corel Draw X6
aspect.plot(bearings.out,p.bins=45)
box(lwd=3)
dev.off()


postscript("inward_flight_duration.ps",width=4.6,height=4.6)  #for post script file (good for printed documents)
#Flight durations
hist(flights$duration[trip_duration > 60*30 & trip_type == 0 & flights$trip_flight_type == "inward" & flights$duration < 2 * 60 * 60]/60, main = "", xlab = "Flight duration (minutes)", col = "#4D897C",las=1,cex.lab=1.7,cex.axis=1.5,lwd=3,pin=c(2,2),font.lab=2)
box(lwd=3)
dev.off()