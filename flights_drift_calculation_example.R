#' Scratchpad for thinking about analysis of wind drift/ wind effect
#' 
#' Plan to include:
#' 1. Extract suitable GPS points for example flight
#'  Look at previous plots to pick a trip, and then
#'  get data for that trip, and then that flight
#' 2. Plot these
#'  i. 2D x-z  (NB, x axis corresponds to W-E, z to S-N, and y to
#'   low-high altitude)
#'  ii. 3D x-y-z
#' 3. Calculate various paramaters
#'  i. For each flight GPS point:
#'    -airspeed
#'    -heading direction
#'    -bearing to goal (final flight point)
#'    -angle of compensation (heading and bearing to goal)
#'  ii. For whole flight:
#'    -Assuming fixed airspeed, heading and neccessary angle of 
#'    compensation for whole flight

#' Example trip chosen:
# Device: 624 Trip: 1634
# Departure time: 2012−06−16 13:02:24 UTC
# Trip duration: 6.16 hours
# flight 21290
# Inward flights 21290, 15475, 20446, 5604, 13108, 21862, 10053, 19946, 18729,15873

# flights.sample <- c(21290, 15475, 20446, 5604, 13108, 21862, 10053, 19946, 18729,15873)

# Outward flight: 5576, 10049, 17033, 15309

flight <- 5576
# Switch between calculating with nest as destination  and final point
# as destination (aka. goal).
type <- "out"
# type <- "in"

# x <- c(1,2,3,4)
# for( i in x){
#   print(i)
#   write(x+i, file = paste0(i,"_test.txt"))
# }


# for(i in 1:length(flights.sample)){
#   print(flights.sample[i])
# }

#   i <- 5

# flight <- flights.sample[i]

# To link to database
library(RODBC)




#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

#Get flight info for specified flight number
flights <- sqlQuery(gps.db, query= paste0("SELECT DISTINCT lf.*, lfc.*
                                          FROM lund_flights AS lf, lund_flights_characteristics AS lfc
                                          WHERE lf.flight_id = lfc.flight_id
                                          AND  lf.flight_id =", flight,"  ORDER BY lf.flight_id ASC;")
                    ,as.is=TRUE)

#Then extract GPS points and paramaters for these for the flight
#Where time is in range and device-info-serial is correct
points <- sqlQuery(gps.db, query=
                     paste0("SELECT DISTINCT g.*, c.* FROM gps_uva_tracking_speed_3d_limited AS g, gps_uva_track_session_limited AS t, cal_mov_paramaters AS c  WHERE g.device_info_serial = t.device_info_serial AND g.device_info_serial = c.device_info_serial AND g.date_time = c.date_time   AND g.date_time >= t.start_date    AND g.latitude IS NOT NULL    AND t.start_latitude < 59    AND g.device_info_serial = ", flights$device_info_serial[1],   "   AND g.date_time BETWEEN #", flights$start_time[1],  "# AND #", flights$end_time[1], "#  ORDER BY g.device_info_serial ASC, g.date_time ASC ;")
                   ,as.is=TRUE)

# points <- NULL

# Calculate weather paramaters for each GPS point (previous
# calculations were for whole flights only.

#Required packages
library(RNCEP)

# vignette(RNCEP)

points.weather <- NULL


#Wind Speed in E-W direction 'uwnd.sig995' (ms^-1) 'near surface'
<<<<<<< HEAD
uwnd <- NCEP.interp(variable = "uwnd.sig995", level = "surface",
=======
uwnd. <- NCEP.interp(variable = "uwnd.sig995", level = "surface",
>>>>>>> 861417703a98291db4d741096ac889d48bc508bb
                     lat = points$latitude, lon = points$longitude,  
                     dt = points$date_time,
                     reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                     interp = 'linear')
# ?NCEP.interp

#Add values to points.weather table
uwnd.sig995 <- (as.numeric(uwnd))
uwnd.sig995.sd <- (attr(uwnd, which = "standard deviation"))
points.weather <- cbind(points.weather,uwnd.sig995,uwnd.sig995.sd)


#Wind Speed in N-S direction 'vwnd.sig995' (ms^-1) 'near surface'
vwnd <- NCEP.interp(variable = "vwnd.sig995", level = "surface",
                    lat = points$latitude, lon = points$longitude,  
                    dt = points$date_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to points.weather table
vwnd.sig995 <- (as.numeric(vwnd))
vwnd.sig995.sd <- (attr(vwnd, which = "standard deviation"))
points.weather <- cbind(points.weather,vwnd.sig995,vwnd.sig995.sd)



#Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
uwnd10 <- NCEP.interp(variable = "uwnd.10m", level = "gaussian",
                      lat = points$latitude, lon = points$longitude,  
                      dt = points$date_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')





#Add values to points.weather table
uwnd.10m <- (as.numeric(uwnd10))
uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
points.weather <- cbind(points.weather,uwnd.10m,uwnd.10m.sd)


#Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
vwnd10 <- NCEP.interp(variable = "vwnd.10m", level = "gaussian",
                      lat = points$latitude, lon = points$longitude,  
                      dt = points$date_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')

#Add values to points.weather table
vwnd.10m <- (as.numeric(vwnd10))
vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
points.weather <- cbind(points.weather,vwnd.10m,vwnd.10m.sd)

#make into a dataframe
points.weather <- as.data.frame(points.weather)



#Wind shear#############################################
#Making aproximate calculation of wind speed at median flight height
#using equation one from:
#Hsu SA, Meindl EA, Gilhousen DB (1994) Determining the power-law wind-profile exponent under near-neutral stability conditions at sea. Journal of Applied Meteorology 33:757–772.
#Formular:
# u2 = u1 (Z2 / Z1) ^ P
# They suggest using a value of 0.11 for P
# u1 and u2 are the reference and desired wind speed measure
# Z1 and Z2 are the reference and desired heights respectively.
#
#Here we will use the values at 10m (uwnd.10m and vwnd.10m)
#Calculating first uwnd.10m.flt.ht and vwn.10m.flt.ht
#Then with simple Pythagoras theorem, the wind velocity
#(square root of sum of the squared values for u and v)
# (3^0.11)*5.69
# 
# ((43.5/10)^0.11)
# ((17.5/10)^0.11)

wind.shear <- function(uwind10, vwind10, ht.med){
  
  # Remove negative altitude values, if x is 1 m or less, replace with 1 m
  # Leave NA values as is.
  rem.neg <- function(x){
    if(is.na(x) == FALSE){
      if(x < 1) x <- 1
    }
    return(x)
  }
  
  #   rem.neg(-1)
  
  
  #For median flight height, remove values less than 1, and replace with
  # 1. See function 'rem.neg' above.
  ht.med <- sapply(ht.med, rem.neg)
  
  #Calculate the new wind speeds for both u and v wind vectors
  uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
  vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
  
  
  #New wind speed, using Pythagoras theorem to calculate wind speed
  wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
  
  #Vairables to export
  vairables <- cbind(uwind.new, vwind.new, wind.new)
  
  return(vairables)
}        #end of wind.shear function

rep.neg.alt <- function(x){if(x < 0.1){return(0.1)} else { return(x)}}
# ?apply
# points.weather <- points_weather
# Remove negative or <0.1 m altitudes with 0.1 value
alt.cor <- sapply(points$altitude, rep.neg.alt)

# Calculate wind-speed at flight height
# Get u and v vector at flight height, and scalar wind speed
wind.calculated    <- wind.shear(points.weather$uwnd.10m,
                                 points.weather$vwnd.10m,
                                 alt.cor)
names(points.weather)
# Make into dataframe
wind.calculated    <- as.data.frame(wind.calculated)

# Give column names
names(wind.calculated) <- c("uwind.10m.flt.ht", "vwind.10m.flt.ht",
                            "wind.10m.flt.ht")
points.weather <- cbind(points.weather,wind.calculated)


#Wind direction and speed#############
# 

wind.dir.speed <- function(uwind10, vwind10){
  # This function calculates the wind speed and direction based the u
  # v wind vectors
  
  #Wind speed Pythagoras theorem
  wind.speed <- sqrt((uwind10 * uwind10) + (vwind10 * vwind10))
  
  # Calculate direction in radians (0 - 90 deg)
  dir <- atan(abs(uwind10/ vwind10))
  
  #   atan(1)
  #   atan(0.5)
  #   dir <- atan(0.5)
  #   ?atan
  # Direction in degrees (0 - 90)
  dir <- dir * 180 / pi
  
  # Make into bearing from North
  if(uwind10 > 0 && vwind10 < 0){
    wind.dir <- (180 - dir)
  }else if(uwind10 < 0 && vwind10 < 0){
    wind.dir <- (dir + 180)
  }else  if(uwind10 < 0 && vwind10 > 0){
    wind.dir <- (360 - dir)
  }else   wind.dir <- (dir)
  
  x <- cbind(wind.speed, wind.dir)
  return(x)
}

# wind.dir.speed(1,100)




# Calculate wind speed (at 10m) and direction (bearing from north)
# for all flights
# I recieved help with mapply from StackOverflow: http://stackoverflow.com/questions/14196696/sapply-with-custom-function-series-of-if-statements
wind.info <- t(mapply(wind.dir.speed, points.weather$uwnd.10m,
                      points.weather$vwnd.10m))
# names(points.weather)
# Make dataframe
wind.info <- as.data.frame(wind.info)

# Give names to columns
names(wind.info) <- c("wind.speed.10m", "wind.dir.10m")

# Add calculated wind info to points.weather 
points.weather <- cbind(points.weather, wind.info)

# Direction from which wind is coming from (origin)
wind.origin.10m <- ((points.weather$wind.dir.10m + 180) %% 360)


# Add wind origin direction to table.
points.weather <- cbind(points.weather, wind.origin.10m)



#' Calculating heading vector (we already have the wind vector
#' and the track vector).
#' In principle this is simple vector addition.
#' 

# Calculating x and z component of heading vector
# Subtract wind vector from ground vector, leaving
# heading vector
# names(points)
# names(points.weather)
head.x <- points$veast  - points.weather$uwind.10m.flt.ht
head.z <- points$vnorth - points.weather$vwind.10m.flt.ht

head.info <- t(mapply(wind.dir.speed, head.x,
                      head.z))
# names(points.weather)
# Make dataframe
head.info <- as.data.frame(head.info)

# Give names to columns
names(head.info) <- c("head.speed", "head.dir")

#' Angle of compensation
#' To calculate this, we require both the bearing from the current
#' location to the goal (in this example the nest), and the
#' heading vector (calculated above). 
#' 
#' 

#Discovered that I had the formular wrong (long and lat in wrong order) in gps_calculations.R



#' Rerun nest location thing for this example:
#Query the gull db to extract bird_id, nest_id, and nest locations
nest_loc <- sqlQuery(gps.db, query="SELECT DISTINCT n.ring_number,
                     n.nest_id, n.latitude, n.longitude, t.device_info_serial
                     FROM gps_uva_nest_limited AS n, gps_uva_track_session_limited AS t
                     WHERE n.ring_number = t.ring_number
                     ORDER BY n.ring_number ASC;")


#Nest location #############
#function to produce two vectors of latitude and longitude positions
lookup_nest <- function(device_info){
  x <- nest_loc$device_info_serial == device_info
  lat <- nest_loc$latitude[x]
  long <- nest_loc$longitude[x]
  return(c(lat,long))
}

#for each gps point lookup the nest location  
nest_pos <- sapply(flights$device_info_serial, lookup_nest)

#to transpose this generated matrix, so that dimentions are correct
nest_pos <- t(nest_pos)
nest_pos <- as.data.frame(nest_pos)
colnames(nest_pos) <- c("lat","long")



#bearing from nest, uses Haversine formula
n <- length(points$longitude)
if(type == "in") {nest_bear <- earth.bear(points$longitude,points$latitude,nest_pos$long,nest_pos$lat)} else {
  nest_bear <- earth.bear(points$longitude,points$latitude,points$longitude[n],points$latitude[n])
}


ground.speed <- t(mapply(wind.dir.speed, points$veast,
                         points$vnorth))
# Make dataframe
ground.speed <- as.data.frame(ground.speed)

# Give names to columns
names(ground.speed) <- c("ground.speed", "ground.dir")



#' Calculating copensation angle and drift angle
#' 
<<<<<<< HEAD

#names(points)
#nest_bear
#names(head.info)
#head.info$head.dir
=======
names(points)
nest_bear
names(head.info)
head.info$head.dir
>>>>>>> 861417703a98291db4d741096ac889d48bc508bb


angle.cor <- function(x) if(x >= 180) return(-360 + x) else if(x <= -180) return(360 + x) else return(x)

angle.comp <- abs(head.info$head.dir   -   nest_bear)
angle.comp <-sapply(angle.comp, angle.cor) *-1


angle.drift <- nest_bear - ground.speed$ground.dir
angle.drift <- sapply(angle.drift, angle.cor)








# Dataframe of vairables created ####
#' 
vec.all <- cbind(nest_bear , head.info$head.dir, points.weather$wind.dir.10m,ground.speed$ground.dir)
vec.all <- as.data.frame(vec.all)
names(vec.all) <- c("nest_bear" , "head.dir", "wind.dir.10m", "ground.dir")
# vec.all <- cbind(vec.all, ground.speed$ground.speed,head.info$head.speed,points.weather$vwind.10m.flt.ht,angle.comp,angle.drift)
# names(vec.all) <- c("nest_bear" , "head.dir", "wind.dir.10m", "ground.dir","ground.speed","head.speed","wind.speed","compensation_angle","drift_angle")
vec.all <- cbind(vec.all, ground.speed$ground.speed,head.info$head.speed,points.weather$wind.10m.flt.ht)
names(vec.all) <- c("nest_bear" , "head.dir", "wind.dir.10m", "ground.dir","ground.speed","head.speed","wind.speed")
<<<<<<< HEAD

# names(points.weather)

#Mapping flight ####

# pdf(paste0("flight_",flight,".pdf"))
#win.metafile(paste0("flight_",flight,".wmf"))

library(maps)

#First subset the data that we require  
#   i      <-  trips.sample$device_info_serial[id]
#   start.t  <-  trips.sample$start_time[id]
#   end.t    <-  trips.sample$end_time[id]

gps.sub <- points
#   flights.sub <- flights.extract(i, start.t, end.t)


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
     ylim = c.ylim, col="dark grey", bg = "white")
# ?par

# names(flights.sub)
# Add points

#Flight points

points(gps.sub$longitude,
       gps.sub$latitude,
       col = "black", cex = 0.5)



# Add lines
# gps.sub$longitude,
# gps.sub$latitude,
#First grey for all
n <- length(gps.sub$longitude)
segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
         gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
         col = "grey")


#' Arrows
#' Make arrows for:
#'  Wind vectors    (blue)
#'  Heading vectors (red)
#'  Ground vectors (black)


#Arrows for wind
arrows(points$longitude, points$latitude, x1 = (points$longitude + (points.weather$uwind.10m.flt.ht*.001)), y1 = (points$latitude + (points.weather$vwind.10m.flt.ht*.001))
       ,length = 0.03, col = "blue"
)

#Arrows for heading
arrows(points$longitude, points$latitude, x1 = (points$longitude + (head.x*.001)), y1 = (points$latitude + (head.z*.001))
       ,length = 0.03, col = "red"
)

#Arrows for ground
arrows(points$longitude, points$latitude, x1 = (points$longitude + (points$veast*.001)), y1 = (points$latitude + (points$vnorth*.001))
       ,length = 0.03, col = "black"
)




# names(vec.all)
# names(points.weather)
# names(head.info)
# names(ground.speed)


# Scale bar and axis
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)

mtext(paste("Flight ID: ", flight)
      , side = 3, line = 0, cex = 1)

#dev.off()

#End of figure

# is.data.frame(vec.all)
# 
# write.table(vec.all, "clipboard", sep="\t")
# write.table(vec.all, "clipboard", fileEncoding = "UTF-16LE")
# 





# Drift and compensation angle calculation ####

# 1. Produce data.frame with all neccessary tracks
# Wind, track, goal (nest), heading, with both bearing and speed
# Use 'vec.all' created above

# 2. Subtract goal direction from all (i.e. so goal is zero, and others
# are now deviations from goal direction.
names(vec.all)

ang.cor2 <- function(x) if(x < 0) return(360 + x) else return(x)

angle.comp <- abs(head.info$head.dir   -   nest_bear)
angle.comp <-sapply(angle.comp, angle.cor) *-1

v.wind  <- sapply(vec.all$wind.dir.10m - vec.all$nest_bear,ang.cor2)
v.head  <- sapply(vec.all$head.dir - vec.all$nest_bear,ang.cor2)
v.track <- sapply(vec.all$ground.dir - vec.all$nest_bear,ang.cor2)

# negative deviation if 180 - 360, and positive if 0 - 180
ang.cor3 <- function(x) if(x <360 & x > 180) return(-1*(180 -(x - 180))) else return(x)

v.wind2  <- sapply(v.wind,ang.cor3)
v.head2  <- sapply(v.head,ang.cor3)
v.track2 <- sapply(v.track,ang.cor3)

ang.cor.drift <- function(x,y) if(x < 0) return(y*-1) else return(y)
ang.cor.comp  <- function(x,y) if(x < 0) return(y) else return(y*-1)
=======

# names(points.weather)
>>>>>>> 861417703a98291db4d741096ac889d48bc508bb

#Mapping flight ####

<<<<<<< HEAD
a.drift <- mapply(v.wind2, FUN = ang.cor.drift, y = v.track2)
a.comp  <- mapply(v.wind2, FUN = ang.cor.comp, y = v.head2)

names(vec.all)
#Need to remove the first and final points
v.wind2[-length(v.wind2)] [-1]

a.comp.full <- asin((vec.all$wind.speed[-length(v.wind2)] [-1]/vec.all$head.speed[-length(v.wind2)] [-1])*sin(abs(v.wind2[-length(v.wind2)] [-1])*pi/180))
a.comp.full <- a.comp.full/pi *180
a.comp.full <- c(NA,a.comp.full,NA)
# v.wind2
# asin(1.1)
comp.dif <- a.comp.full - a.comp

# Add to dataframe
vec.all.names <- names(vec.all)
vec.all <- cbind(vec.all,a.drift,a.comp,a.comp.full,comp.dif)
names(vec.all) <- c(vec.all.names,"drift_angle","compensation_angle","full_compensation_angle","angle_dif")

# ?write.table
#' Output HTML table.
library(xtable)
#print.xtable(xtable(vec.all), type="html", file=paste0("flight_",flight,".html"))
vec.all
#useing anser from: http://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
# rm(list=setdiff(ls(), c("i","flights.sample")))
# }
# rm(list=ls())
# warnings()

=======
# pdf(paste0("flight_",flight,".pdf"))
win.metafile(paste0("flight_",flight,".wmf"))

library(maps)

#First subset the data that we require  
#   i      <-  trips.sample$device_info_serial[id]
#   start.t  <-  trips.sample$start_time[id]
#   end.t    <-  trips.sample$end_time[id]

gps.sub <- points
#   flights.sub <- flights.extract(i, start.t, end.t)


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
     ylim = c.ylim, col="dark grey", bg = "white")
# ?par

# names(flights.sub)
# Add points

#Flight points

points(gps.sub$longitude,
       gps.sub$latitude,
       col = "black", cex = 0.5)



# Add lines
# gps.sub$longitude,
# gps.sub$latitude,
#First grey for all
n <- length(gps.sub$longitude)
segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
         gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
         col = "grey")


#' Arrows
#' Make arrows for:
#'  Wind vectors    (blue)
#'  Heading vectors (red)
#'  Ground vectors (black)


#Arrows for wind
arrows(points$longitude, points$latitude, x1 = (points$longitude + (points.weather$uwind.10m.flt.ht*.001)), y1 = (points$latitude + (points.weather$vwind.10m.flt.ht*.001))
       ,length = 0.03, col = "blue"
)

#Arrows for heading
arrows(points$longitude, points$latitude, x1 = (points$longitude + (head.x*.001)), y1 = (points$latitude + (head.z*.001))
       ,length = 0.03, col = "red"
)

#Arrows for ground
arrows(points$longitude, points$latitude, x1 = (points$longitude + (points$veast*.001)), y1 = (points$latitude + (points$vnorth*.001))
       ,length = 0.03, col = "black"
)




# names(vec.all)
# names(points.weather)
# names(head.info)
# names(ground.speed)


# Scale bar and axis
map.scale(ratio = FALSE)
box()
axis(side=(1),las=1)
axis(side=(2),las=1)
#   ?text
mtext(paste("Flight: ", )
      , side = 3, line = 1, cex = 1)

#   dur <- as.difftime(trips.sample$duration_s[id], units= "secs")
#   dur <- as.numeric(dur, units="hours")
mtext(paste("Flight ID: ", flight)
      , side = 3, line = 0, cex = 1)

dev.off()

#End of figure

# is.data.frame(vec.all)
# 
# write.table(vec.all, "clipboard", sep="\t")
# write.table(vec.all, "clipboard", fileEncoding = "UTF-16LE")
# 





# Drift and compensation angle calculation ####

# 1. Produce data.frame with all neccessary tracks
# Wind, track, goal (nest), heading, with both bearing and speed
# Use 'vec.all' created above

# 2. Subtract goal direction from all (i.e. so goal is zero, and others
# are now deviations from goal direction.
names(vec.all)

ang.cor2 <- function(x) if(x < 0) return(360 + x) else return(x)

angle.comp <- abs(head.info$head.dir   -   nest_bear)
angle.comp <-sapply(angle.comp, angle.cor) *-1

v.wind  <- sapply(vec.all$wind.dir.10m - vec.all$nest_bear,ang.cor2)
v.head  <- sapply(vec.all$head.dir - vec.all$nest_bear,ang.cor2)
v.track <- sapply(vec.all$ground.dir - vec.all$nest_bear,ang.cor2)

# negative deviation if 180 - 360, and positive if 0 - 180
ang.cor3 <- function(x) if(x <360 & x > 180) return(-1*(180 -(x - 180))) else return(x)

v.wind2  <- sapply(v.wind,ang.cor3)
v.head2  <- sapply(v.head,ang.cor3)
v.track2 <- sapply(v.track,ang.cor3)

ang.cor.drift <- function(x,y) if(x < 0) return(y*-1) else return(y)
ang.cor.comp  <- function(x,y) if(x < 0) return(y) else return(y*-1)


a.drift <- mapply(v.wind2, FUN = ang.cor.drift, y = v.track2)
a.comp  <- mapply(v.wind2, FUN = ang.cor.comp, y = v.head2)

names(vec.all)
#Need to remove the first and final points
v.wind2[-length(v.wind2)] [-1]

a.comp.full <- asin((vec.all$wind.speed[-length(v.wind2)] [-1]/vec.all$head.speed[-length(v.wind2)] [-1])*sin(abs(v.wind2[-length(v.wind2)] [-1])*pi/180))
a.comp.full <- a.comp.full/pi *180
a.comp.full <- c(NA,a.comp.full,NA)
# v.wind2
# asin(1.1)
comp.dif <- a.comp.full - a.comp

# Add to dataframe
vec.all.names <- names(vec.all)
vec.all <- cbind(vec.all,a.drift,a.comp,a.comp.full,comp.dif)
names(vec.all) <- c(vec.all.names,"drift_angle","compensation_angle","full_compensation_angle","angle_dif")

# ?write.table
#' Output HTML table.
library(xtable)
print.xtable(xtable(vec.all), type="html", file=paste0("flight_",flight,".html"))

#useing anser from: http://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
# rm(list=setdiff(ls(), c("i","flights.sample")))
# }
# rm(list=ls())
# warnings()
>>>>>>> 861417703a98291db4d741096ac889d48bc508bb
