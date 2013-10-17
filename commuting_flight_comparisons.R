#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#Description#######
#In this script we produce various figures and summary statistics to compare outward and inward commuting flights.
#These were originally prepared for a meeting with Susanne and Anders on 2013-01-17


#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")


# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r

tm <- as.POSIXlt(flights$start_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
flights$start_time <- tm

tm <- as.POSIXlt(flights$end_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
flights$end_time <- tm


# flights$start_time[1:10]

#str(flights)  #check structure

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights_weather AS f
ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- as.POSIXlt(flights.weather$start_time)
#Check how this appears (i.e. time zone)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# tm[1:10]
flights.weather$start_time <- tm


#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
FROM lund_trips AS t
ORDER BY t.trip_id ASC;")

#str(trips)

tm <- as.POSIXlt(trips$start_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$start_time <- tm

tm <- as.POSIXlt(trips$end_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$end_time <- tm


#Edited this to reference 'lund_flight_paramaters' rather than 'lund_flight_characteristics' as table name changed.
flights.characteristics <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                    FROM lund_flight_paramaters AS f
                    ORDER BY f.flight_id ASC;")




#Trip type and duration#######
trip_type <- 0
trip_duration <- 0
trip_gotland <- 0
trip_distmax <- 0

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

flights.out$wind.type[flights.out$dif.angle < 90] <- "tail"
flights.out$wind.type[flights.out$dif.angle > 90] <- "head"
flights.in$wind.type[flights.in$dif.angle < 90] <- "tail"
flights.in$wind.type[flights.in$dif.angle > 90] <- "head"



names(flights.out)[65] <- "dif.angle"
names(flights.in)[65] <- "dif.angle"
names(flights.out) == names(flights.in)

flights.combined <- rbind(flights.out,flights.in)




# install.packages("reshape2")
library(reshape2)

aggregate(speed_inst_mean ~ device_info_serial, data =flights.in, FUN=length)




# Speed comparision######

# pdf("out_vs_in_inst_speed.pdf")
par(mfrow = c(1,2))
hist(flights$speed_inst_mean[outward ],ylim = c(0,140), xlim = c(0,20),breaks="Scott", xlab = "Speed ms-1",las=1, cex.axis = 1, cex.lab = 1, col = "grey", main = "out")

x1 <- mean(flights$speed_inst_mean[outward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_inst_mean[inward],ylim = c(0,140), xlim = c(0,20),breaks="Scott", xlab = "Speed ms-1",las=1, cex.axis = 1, cex.lab = 1, col = "grey", main = "in")

x2 <- mean(flights$speed_inst_mean[inward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red", main = "in")

# dev.off()

x1
x2

sd(flights$speed_inst_mean[outward], na.rm = TRUE)
sd(flights$speed_inst_mean[inward], na.rm = TRUE)



par(mfrow = c(1,1))
hist(flights.in$speed_inst_mean-flights.out$speed_inst_mean, main = "Speed difference between outward and inward flights",xlab = "Speed ms-1", col = "grey", cex.main = 0.8)
x3 <- mean(flights.in$speed_inst_mean-flights.out$speed_inst_mean)
abline(v = x3, lwd = 2, lty = 2, col = "red", main = "in")
sd3 <- sd(flights.in$speed_inst_mean-flights.out$speed_inst_mean)

# paired t-test to compare inward and outward flight speeds.
t.test(flights.in$speed_inst_mean,flights.out$speed_inst_mean, paired = TRUE)




# Rho comparison ####
win.metafile("Out_in_straightness_comparison.wmf")
par(mfrow = c(1,2))
hist(flights$rho[outward],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,220), xlim = c(0,1), main = "Outward")
hist(flights$rho[inward] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,220), xlim = c(0,1), main = "Inward")
dev.off()


sd(flights$rho[inward])
sd(flights$rho[outward])
t.test(flights$rho[outward], flights$rho[inward])
wilcox.test(flights$rho[outward], flights$rho[inward])
t.test(flights.in$rho,flights.out$rho, paired = TRUE)

#Rearrange data for comparison:




# Altitude ####

names(flights)
par(mfrow = c(1,2))
hist(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],xlab = "Altitude (m)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",  main = "Outward", xlim = c(-20,150), ylim = c(0,150),breaks="Scott")
hist(flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ] ,xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "Inward", xlim = c(-20,150), ylim = c(0,150),breaks="Scott")


summary(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ] - flights$alt_mean[outward & (flights$alt_med > -50) & flights$alt_med < 500 ])
all.equal((flights$alt_med - flights$alt_mean) , rep(0,length(flights$alt_mean)))

flights$alt_med[1:20]
flights$alt_mean[1:20]

par(mfrow = c(1,2))
hist(flights$alt_mean[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],xlab = "Altitude (m)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",  main = "Outward", xlim = c(-20,150), ylim = c(0,150),breaks="Scott")
hist(flights$alt_mean[inward& (flights$alt_med > -50) & flights$alt_med < 500 ] ,xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "Inward", xlim = c(-20,150), ylim = c(0,150),breaks="Scott")


mean(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ])
mean(flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])

sd(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ])
sd(flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])

median(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ], na.rm = TRUE)
median(flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ], na.rm = TRUE)

names(flights)


hist(flights$alt_max[outward & (flights$alt_max > -50) & flights$alt_max < 500 ],xlab = "Altitude (m)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",  main = "Outward",breaks=20)
hist(flights$alt_max[inward & (flights$alt_max > -50) & flights$alt_max < 500 ] ,xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "Inward",breaks=20)
mean(flights$alt_max[outward & (flights$alt_max > -50) & flights$alt_max < 500 ])
mean(flights$alt_max[inward & (flights$alt_max > -50) & flights$alt_max < 500 ])

t.test(flights$alt_max[outward & (flights$alt_max > -50) & flights$alt_max < 500 ],flights$alt_max[inward & (flights$alt_max > -50) & flights$alt_max < 500 ])
# ?median
wilcox.test(flights$alt_max[outward & (flights$alt_max > -50) & flights$alt_max < 500 ],flights$alt_max[inward & (flights$alt_max > -50) & flights$alt_max < 500 ])



t.test(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])
wilcox.test(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])

*********************************


  
# Rho - wind condition ####
par(mfrow = c(2,2))
hist(flights.out$rho[flights.out$dif.angle < 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,100), xlim = c(0,1), main = "Out - tail")
hist(flights.out$rho[flights.out$dif.angle > 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,100), xlim = c(0,1), main = "Out - head")
hist(flights.in$rho[flights.in$dif.angle < 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,100), xlim = c(0,1), main = "In - tail")
hist(flights.in$rho[flights.in$dif.angle > 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,100), xlim = c(0,1), main = "In - head")


mean(flights.out$rho[flights.out$dif.angle < 90])
mean(flights.out$rho[flights.out$dif.angle > 90])
mean(flights.in$rho[flights.in$dif.angle < 90])
mean(flights.in$rho[flights.in$dif.angle > 90])



mod1 <- aov(flights.combined$rho ~ flights.combined$flight.type * flights.combined$wind.type)
summary(mod1)

mod2 <- aov(flights.combined$rho ~ flights.combined$flight.type + flights.combined$wind.type)
summary(mod2)

# Get means and SD for different categories
aggregate(rho ~ flight.type + wind.type, data =flights.combined, FUN=mean)
aggregate(rho ~ flight.type + wind.type, data =flights.combined, FUN=sd)

aggregate(rho ~  wind.type, data =flights.combined, FUN=mean)
aggregate(rho ~  wind.type, data =flights.combined, FUN=sd)



# Altitude - wind condition ####
sort(flights.out$alt_med[flights.out$dif.angle < 90])
sort(flights.in$alt_med[flights.in$dif.angle < 90])


alt.out.tail <- sqrt(10 + flights.out$alt_med[flights.out$dif.angle < 90])
alt.out.head <- sqrt(10 + flights.out$alt_med[flights.out$dif.angle > 90])
alt.in.tail <- sqrt(10 + flights.in$alt_med[flights.in$dif.angle < 90])
alt.in.head <- sqrt(10 + flights.in$alt_med[flights.in$dif.angle > 90])

par(mfrow = c(2,2))
hist(alt.out.tail,xlab = "Altitude (transformed)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", main = "Out - tail", breaks = "scott")
hist(alt.out.head,xlab = "Altitude (transformed)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", main = "Out - head", breaks = "scott")
hist(alt.in.tail,xlab = "Altitude (transformed)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "In - tail", breaks = "scott")
hist(alt.in.head,xlab = "Altitude (transformed)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",main = "In - head", breaks = "scott")




mean(alt.out.tail, na.rm = TRUE)
mean(alt.out.head, na.rm = TRUE)
mean(alt.in.tail, na.rm = TRUE)
mean(alt.in.head, na.rm = TRUE)


alt <- sqrt(10 + flights.combined$alt_med)

mod1 <- aov(alt ~ flights.combined$flight.type * flights.combined$wind.type)
summary(mod1)

mod2 <- aov(alt ~ flights.combined$flight.type + flights.combined$wind.type)
summary(mod2)

aggregate(alt ~ flight.type + wind.type, data = flights.combined, FUN = mean )
aggregate(alt ~  wind.type, data = flights.combined, FUN = mean )
aggregate(alt ~  wind.type, data = flights.combined, FUN = sd )


# Get means and SD for different categories
aggregate(alt_mean ~ flight.type + wind.type, data =flights.combined, FUN=mean)
aggregate(alt_mean ~ flight.type + wind.type, data =flights.combined, FUN=sd)

aggregate(alt_mean ~  wind.type, data =flights.combined, FUN=mean)
aggregate(alt_mean ~  wind.type, data =flights.combined, FUN=sd)





# Straightness - r - wind condition ####
par(mfrow = c(2,2))
hist(flights.out$straigtness[flights.out$dif.angle < 90 & flights.out$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), breaks = "scott", main = "Out - tail")
hist(flights.out$straigtness[flights.out$dif.angle > 90 & flights.out$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", xlim = c(0,1), main = "Out - head")
hist(flights.in$straigtness[flights.in$dif.angle < 90 & flights.in$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - tail")
hist(flights.in$straigtness[flights.in$dif.angle > 90 & flights.in$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - head")

par(mfrow=c(1,1))
plot(flights.in$straigtness~flights.in$rho)

mod1 <- aov(flights.combined$straigtness ~ flights.combined$flight.type * flights.combined$wind.type)
summary(mod1)

mod2 <- aov(flights.combined$straigtness ~ flights.combined$flight.type + flights.combined$wind.type)
summary(mod2)

# Get means and SD for different categories
aggregate(straigtness ~ flight.type + wind.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~ flight.type + wind.type, data =flights.combined, FUN=sd)

aggregate(straigtness ~  wind.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~  wind.type, data =flights.combined, FUN=sd)

aggregate(straigtness ~  flight.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~  flight.type, data =flights.combined, FUN=sd)



###

# ArcSine  ####
arcsine <- function(x){
  z <- sqrt(x)
  z <- asin(z)
  return(z)
}




# Weather conditions###############

# Wind direction
# install.packages("oce")
library(oce)


graph.object <- as.windrose(flights.combined$uwnd10m, flights.combined$vwnd10m, dtheta = 15, debug=getOption("oceDebug"))
plot(graph.object)



# Wind speed
wind.speed <- sqrt(flights.combined$uwnd10m^2  +  flights.combined$vwnd10m^2)
hist(wind.speed, col = "dark grey", xlab = "Wind speed in ms-1, for wind calculated for 10 m altitude")


# Sky conditions ######

names(flights.weather)
#Total Cloud Cover %
par(mfrow=c(1,1))
hist(flights.combined$tcdceatm, xlab = "Cloud cover (%)", main = "", col = "grey")




# Straightness vs. sky conditions ####

cloud.high <- flights.combined$tcdceatm > 60
cloud.low <- flights.combined$tcdceatm < 40
flights.combined$cloud.type <- rep(NA, length(flights.combined$tcdceatm))
flights.combined$cloud.type[cloud.high] <- "high"
flights.combined$cloud.type[cloud.low] <- "low"
flights.combined$cloud.type <- as.factor(flights.combined$cloud.type)


# With r - straightness based on distance travelled
par(mfrow = c(2,2))
hist(flights.out$straigtness[flights.out$tcdceatm > 60 & flights.out$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), breaks = "scott", main = "Out - high")
hist(flights.out$straigtness[flights.out$tcdceatm < 40  & flights.out$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", xlim = c(0,1), main = "Out - low")
hist(flights.in$straigtness[flights.in$tcdceatm > 60 & flights.in$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - high")
hist(flights.in$straigtness[flights.in$tcdceatm < 40  & flights.in$straigtness < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - low")

par(mfrow=c(1,1))
mod1 <- aov(flights.combined$straigtness ~ flights.combined$flight.type * flights.combined$cloud.type)
summary(mod1)

mod2 <- aov(flights.combined$straigtness ~ flights.combined$flight.type + flights.combined$cloud.type)
summary(mod2)

# Get means and SD for different categories
aggregate(straigtness ~ flight.type + cloud.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~ flight.type + cloud.type, data =flights.combined, FUN=sd)

aggregate(straigtness ~  cloud.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~  cloud.type, data =flights.combined, FUN=sd)

aggregate(straigtness ~  flight.type, data =flights.combined, FUN=mean)
aggregate(straigtness ~  flight.type, data =flights.combined, FUN=sd)


#With rho - based on angles
par(mfrow = c(2,2))
hist(flights.out$rho[flights.out$tcdceatm > 60 & flights.out$rho < 1],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), breaks = 10, main = "Out - high")
hist(flights.out$rho[flights.out$tcdceatm < 40  & flights.out$rho < 1],xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), main = "Out - low")
hist(flights.in$rho[flights.in$tcdceatm > 60 & flights.in$rho < 1],xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - high")
hist(flights.in$rho[flights.in$tcdceatm < 40  & flights.in$rho < 1],xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "In - low")

par(mfrow=c(1,1))
mod1 <- aov(flights.combined$rho ~ flights.combined$flight.type * flights.combined$cloud.type)
summary(mod1)

mod2 <- aov(flights.combined$rho ~ flights.combined$flight.type + flights.combined$cloud.type)
summary(mod2)

# Get means and SD for different categories
aggregate(rho ~ flight.type + cloud.type, data =flights.combined, FUN=mean)
aggregate(rho ~ flight.type + cloud.type, data =flights.combined, FUN=sd)

aggregate(rho ~  cloud.type, data =flights.combined, FUN=mean)
aggregate(rho ~  cloud.type, data =flights.combined, FUN=sd)

aggregate(rho ~  flight.type, data =flights.combined, FUN=mean)
aggregate(rho ~  flight.type, data =flights.combined, FUN=sd)







# vvv OLD things  vvv #####








cloud.high <- flights.weather$tcdceatm > 60
cloud.low <- flights.weather$tcdceatm < 40




par(mfrow = c(2,2))
hist(flights$rho[outward & cloud.high],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), main = "Outward", breaks = 20)
abline(v = mean(flights$rho[outward & cloud.high]), lty = 2, lwd = 2)
median((flights$rho[outward & cloud.high]))
mean(flights$rho[outward & cloud.high])


hist(flights$rho[inward & cloud.high] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "Inward", breaks = 10)
abline(v = mean(flights$rho[inward & cloud.high]), lty = 2, lwd = 2)
mean(flights$rho[inward & cloud.high])
median(flights$rho[inward & cloud.high])


hist(flights$rho[outward & cloud.low],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), main = "Outward", breaks = 20)
abline(v = mean(flights$rho[outward & cloud.low]), lty = 2, lwd = 2)
mean(flights$rho[outward & cloud.low])
median(flights$rho[outward & cloud.low])

hist(flights$rho[inward & cloud.low] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "Inward", breaks = 20)
abline(v = mean(flights$rho[inward & cloud.low]), lty = 2, lwd = 2)
mean(flights$rho[inward & cloud.low])
median(flights$rho[inward & cloud.low])

length(outward)
# length(dif.angle)

mean(flights$rho[outward & cloud.high])
mean(flights$rho[outward & cloud.low])
mean(flights$rho[inward & cloud.high])
mean(flights$rho[inward & cloud.low])


#Straightness (not rho) ####

cloud.high <- flights.weather$tcdceatm > 60
cloud.low <- flights.weather$tcdceatm < 40


# hist(flights$speed_a_b)
x <- (flights$straigtness < 1.01)
x[is.na(x)] <- FALSE
summary(x)
f <- (flights$speed_a_b > 3) & (flights$speed_a_b < 30) & x

summary(f)

par(mfrow = c(2,2))
hist(flights$straigtness[f & outward & cloud.high],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0.7,1), main = "", breaks = 40)
z <- median((flights$straigtness[f & outward & cloud.high]))
mean(flights$straigtness[f & outward & cloud.high])
abline(v = z, lty = 2, lwd = 2)


hist(flights$straigtness[f & inward & cloud.high] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0.7,1), main = "", breaks = 20)
mean(flights$straigtness[f & inward & cloud.high])
z <-  median(flights$straigtness[f & inward & cloud.high])
abline(v = z, lty = 2, lwd = 2)

hist(flights$straigtness[f & outward & cloud.low],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0.7,1), main = "", breaks = 40)
mean(flights$straigtness[f & outward & cloud.low])
z <- median(flights$straigtness[f & outward & cloud.low])
abline(v = z, lty = 2, lwd = 2)

hist(flights$straigtness[f & inward & cloud.low] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0.7,1), main = "", breaks = 20)
mean(flights$straigtness[f & inward & cloud.low])
z <- median(flights$straigtness[f & inward & cloud.low])
abline(v = z, lty = 2, lwd = 2)



library(psych)
x1 <- geometric.mean(flights$straigtness[f & inward & cloud.low])


names(flights.weather)





# ArcSine
arcsine <- function(x){
  z <- x * 0.01
  z <- sqrt(z)
  z <- asin(z)
  return(z)
}

par(mfrow = c(2,2))
hist(arcsine(flights$straigtness[f & outward & cloud.high]),xlab = "straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", main = "", breaks = 40)
abline(v = mean(flights$straigtness[f & outward & cloud.high]), lty = 2, lwd = 2)
median((flights$straigtness[f & outward & cloud.high]))
mean(flights$straigtness[f & outward & cloud.high])


hist(arcsine(flights$straigtness[f & inward & cloud.high]) ,xlab = "straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", main = "", breaks = 20)
abline(v = mean(flights$straigtness[f & inward & cloud.high]), lty = 2, lwd = 2)
mean(flights$straigtness[f & inward & cloud.high])
median(flights$straigtness[f & inward & cloud.high])

hist(arcsine(flights$straigtness[f & outward & cloud.low]),xlab = "straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", main = "", breaks = 40)
abline(v = mean(flights$straigtness[f & outward & cloud.low]), lty = 2, lwd = 2)
mean(flights$straigtness[f & outward & cloud.low])
median(flights$straigtness[f & outward & cloud.low])

hist(arcsine(flights$straigtness[f & inward & cloud.low]) ,xlab = "straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", main = "", breaks = 20)
abline(v = mean(flights$straigtness[f & inward & cloud.low]), lty = 2, lwd = 2)
  










# Paired t-tests ####

t.test(flights.in$alt_mean,flights.out$alt_mean, paired = TRUE)




# Angle with respect to wind - appears that inward flights more often have head-winds and outward flight more tail-winds.
t.test(flights.in$dif.angle,flights.out$dif.angle, paired = TRUE)
hist(flights.in$dif.angle)
hist(flights.out$dif.angle)


