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

install.packages("reshape2")
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
names(flights.combined)


mod1 <- aov(flights.combined$rho ~ flights.combined$flight.type * flights.combined$wind.type)
summary(mod1)

mod2 <- aov(flights.combined$rho ~ flights.combined$flight.type + flights.combined$wind.type)
summary(mod2)


aggregate(rho ~ flight.type + wind.type, data =flights.combined, FUN=mean)
aggregate(rho ~ flight.type + wind.type, data =flights.combined, FUN=sd)

aggregate(rho ~  wind.type, data =flights.combined, FUN=mean)
aggregate(rho ~  wind.type, data =flights.combined, FUN=sd)


par(mfrow = c(2,2))
hist(flights$rho[outward & dif.angle < 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,110), xlim = c(0,1), main = "Outward")
hist(flights$rho[outward & dif.angle > 90],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,110), xlim = c(0,1), main = "Outward")
hist(flights$rho[inward & dif.angle < 90] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,110), xlim = c(0,1), main = "Inward")
hist(flights$rho[inward & dif.angle > 90] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,110), xlim = c(0,1), main = "Inward")

length(outward)
# length(dif.angle)

# mean(flights$rho[outward & dif.angle < 90])
# mean(flights$rho[outward & dif.angle > 90])
# mean(flights$rho[inward & dif.angle < 90])
# mean(flights$rho[inward & dif.angle > 90])


alt.f <- flights$alt_med > -50 & flights$alt_med < 200 

names(flights.characteristics)
hist(flights.characteristics$windspeed)
high.s <- flights.characteristics$windspeed < 5

mean(flights$alt_med[outward & (dif.angle < 60) & alt.f])
mean(flights$alt_med[outward & (dif.angle > 120) & alt.f])
mean(flights$alt_med[inward & (dif.angle < 60) & alt.f])
mean(flights$alt_med[inward & (dif.angle > 120) & alt.f])
mean(flights$alt_med[inward & (dif.angle > 60 & dif.angle < 120) & alt.f])
mean(flights$alt_med[outward & (dif.angle > 60 & dif.angle < 120) & alt.f])


par(mfrow = c(2,3))
hist(flights$alt_med[outward & (dif.angle < 60) & alt.f],main="out - tail", col = "blue", xlab = "Altitude (m)")
hist(flights$alt_med[outward & (dif.angle > 120) & alt.f],main="out - head", col = "blue", xlab = "Altitude (m)")
hist(flights$alt_med[outward & (dif.angle > 60 & dif.angle < 120) & alt.f],main="out - side", col = "blue", xlab = "Altitude (m)")
hist(flights$alt_med[inward & (dif.angle < 60) & alt.f],main="in - tail", col = "red", xlab = "Altitude (m)")
hist(flights$alt_med[inward & (dif.angle > 120) & alt.f],main="in - head", col = "red", xlab = "Altitude (m)")
hist(flights$alt_med[inward & (dif.angle > 60 & dif.angle < 120) & alt.f],main="in - side", col = "red", xlab = "Altitude (m)")

mean(flights$alt_med[outward & (dif.angle < 60) & alt.f])
mean(flights$alt_med[outward & (dif.angle > 120) & alt.f])
mean(flights$alt_med[outward & (dif.angle > 60 & dif.angle < 120) & alt.f])
mean(flights$alt_med[inward & (dif.angle < 60) & alt.f])
mean(flights$alt_med[inward & (dif.angle > 120) & alt.f])
mean(flights$alt_med[inward & (dif.angle > 60 & dif.angle < 120) & alt.f])

sd(flights$alt_med[outward & (dif.angle < 60) & alt.f])
sd(flights$alt_med[outward & (dif.angle > 120) & alt.f])
sd(flights$alt_med[outward & (dif.angle > 60 & dif.angle < 120) & alt.f])
sd(flights$alt_med[inward & (dif.angle < 60) & alt.f])
sd(flights$alt_med[inward & (dif.angle > 120) & alt.f])
sd(flights$alt_med[inward & (dif.angle > 60 & dif.angle < 120) & alt.f])


f <- !is.na(flight.type.dir)  &  !is.na(flight.type.wind)
summary(f)

alt.start <- flights$alt_med

flights$alt_med <- sqrt(flights$alt_mean + 0)
hist(flights$alt_med)
hist(flights$alt_mean, xlim = c(-20,100))


mean(flights$alt_med[f & (dif.angle < 60) ])
mean(flights$alt_med[f &  (dif.angle > 120) ])
mean(flights$alt_med[f & (dif.angle > 60 & dif.angle < 120) ])

sd(flights$alt_med[f & (dif.angle < 60) ])
sd(flights$alt_med[f &  (dif.angle > 120) ])
sd(flights$alt_med[f & (dif.angle > 60 & dif.angle < 120) ])

mean(flights$alt_med[f & outward])
mean(flights$alt_med[f & inward])

sd(flights$alt_med[f & outward])
sd(flights$alt_med[f & inward])

mean(flights$alt_med[f])

sd(flights$alt_med[f])


flight.type.wind <- rep(NA,length(flights$alt_med))
flight.type.wind[(dif.angle < 60) & alt.f] <- "tail"
flight.type.wind[(dif.angle > 120) & alt.f] <- "head"
flight.type.wind[(dif.angle > 60 & dif.angle < 120) & alt.f] <- "side"
flight.type.wind <- as.factor(flight.type.wind)
summary(flight.type.wind)

flight.type.dir <- rep(NA,length(flights$alt_med))
flight.type.dir[outward & alt.f] <- "out"
flight.type.dir[inward & alt.f] <- "in"
flight.type.dir <- as.factor(flight.type.dir)
summary(flight.type.dir)




mod1 <- aov(flights$alt_med[f] ~ flight.type.dir[f]  * flight.type.wind[f] )
summary(mod1)

mod2 <- aov(flights$alt_med[f] ~ flight.type.dir[f] + flight.type.wind[f])
summary(mod2)

mod3 <- aov(flights$alt_med[f] ~ flight.type.wind[f])
summary(mod3)

par(mfrow=c(1,1))
# plot(sqrt(flights$alt_med) ~ flight.type.wind)

par(mfrow=c(1,1))
plot(TukeyHSD(mod3))


plot(mod3)

par(mfrow = c(2,2))
hist(flights$alt_med[outward & (dif.angle < 60) & alt.f], ylim = c(0,55), xlim = c(-10, 150), breaks=20, main ="", xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue")
x <- mean(flights$alt_med[outward & (dif.angle < 60) & alt.f])
abline(v = x, lwd = 2, lty = 2, col = "black")


hist(flights$alt_med[inward & (dif.angle < 60) & alt.f], ylim = c(0,55),  xlim = c(-10, 150), breaks=20, main ="", xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red")
x <- mean(flights$alt_med[inward & (dif.angle < 60) & alt.f])
abline(v = x, lwd = 2, lty = 2, col = "black")

hist(flights$alt_med[outward & (dif.angle > 120) & alt.f], ylim = c(0,55),  xlim = c(-10, 150), breaks=20, main ="", xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue")
x <- mean(flights$alt_med[outward & (dif.angle > 120) & alt.f])
abline(v = x, lwd = 2, lty = 2, col = "black")

hist(flights$alt_med[inward & (dif.angle > 120) & alt.f], ylim = c(0,55), xlim = c(-10, 150), breaks=20, main ="", xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red")
x <- mean(flights$alt_med[inward & (dif.angle > 120) & alt.f])
abline(v = x, lwd = 2, lty = 2, col = "black")




t.test(flights$alt_med[inward & (dif.angle > 120) & alt.f],flights$alt_med[inward & (dif.angle < 60) & alt.f])



?abs
par(mfrow = c(1,2))
# Resultant speed
hist(flights$speed_a_b[outward & flights$speed_a_b < 25], xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30), main = "outward", xlab = "speed a to b in ms -1")
x1 <- mean(flights$speed_a_b[outward & flights$speed_a_b < 25], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_a_b[inward & flights$speed_a_b < 25],  xlim = c(0,25), breaks = 20, freq = FALSE, ylim = c(0, 0.30), main = "inward", xlab = "speed a to b in ms -1")

x2 <- mean(flights$speed_a_b[inward & flights$speed_a_b < 25], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")


summary(outward)
summary(inward)


par(mfrow = c(1,2))
# Mean speed
hist(flights$speed_inst_mean[outward & flights$speed_inst_mean < 25], xlim = c(0,21), breaks = 20, freq = FALSE,  main = "Outward", xlab = "Speed - mean - ms-1")
x1 <- mean(flights$speed_inst_mean[outward & flights$speed_inst_mean < 25], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_inst_mean[inward & flights$speed_inst_mean < 25],  xlim = c(0,21), breaks = 20, freq = FALSE, main = "Inward", xlab = "Speed - mean - ms-1")
x2 <- mean(flights$speed_inst_mean[inward & flights$speed_inst_mean < 25], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")
x1
x2




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
hist(flights$straigtness[inward & flights$straigtness < 1.2], xlim = c(0,1.2), breaks = 20, freq = FALSE, ylim = c(0, 10), main = "Inward", xlab = "Straightness")
x1 <- mean(flights$straigtness[inward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$straigtness[outward], xlim = c(0,1.2), breaks = 40, freq = FALSE, ylim = c(0, 10), main = "Outward", xlab = "Straightness")
speed_inst_med
x2 <- mean(flights$straigtness[outward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")

x1
x2
x1 <- sd(flights$straigtness[inward], na.rm = TRUE)

x2 <- sd(flights$straigtness[outward], na.rm = TRUE)


summary(flights$straigtness[inward & flights$straigtness < 1.2] == 0)


par(mfrow = c(1,2))
plot(flights$straigtness[outward] ~ flights$speed_a_b[outward], xlim = c(0, 20), ylim = c(0, 1.1) , xlab = "Speed A - B (ms-1)", ylab = "Straightness")
plot(flights$straigtness[inward] ~ flights$speed_a_b[inward], xlim = c(0, 20), ylim = c(0, 1.1), xlab = "Speed A - B (ms-1)", ylab = "Straightness")


# r-vector, or 'rho'
hist(1-flights$rho[inward & flights$straigtness < 1.2], xlim = c(0,1), breaks = 20, freq = FALSE, ylim = c(0, 3), main = "Inward", xlab = "r")
x1 <- mean(1-flights$rho[inward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(1-flights$rho[outward], xlim = c(0,1), breaks = 20, freq = FALSE, ylim = c(0, 3), main = "Outward", xlab = "r")
speed_inst_med
x2 <- mean(1-flights$rho[outward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red")

x1
x2


hist(1-flights$rho[inward])
hist(1-flights$rho[outward])



#Dispersion versus speed##########
plot(1-flights$rho[inward & flights$straigtness < 1.2] ~ flights$speed_inst_med[inward & flights$straigtness < 1.2], xlim = c(3,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Inward", xlab = "Speed - median - ms-1", ylab = "r")

plot(1-flights$rho[outward & flights$straigtness < 1.2] ~ flights$speed_inst_med[outward & flights$straigtness < 1.2], xlim = c(3,22), breaks = 20, freq = FALSE, ylim = c(0, 1), main = "Ourtward", xlab = "Speed - median - ms-1", ylab = "r")




plot(1-flights$rho[inward & flights$straigtness < 1.2] ~ flights$speed_a_b[inward & flights$straigtness < 1.2], xlim = c(0,22),  ylim = c(0, 1), main = "Inward", xlab = "Speed - median - ms-1", ylab = "r")

plot(1-flights$rho[outward & flights$straigtness < 1.2] ~ flights$speed_a_b[outward & flights$straigtness < 1.2], xlim = c(0,22), ylim = c(0, 1), main = "Outward", xlab = "Speed - median - ms-1", ylab = "r")


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

library(oce)
graph.object <- as.windrose(flights.weather$uwnd10m, flights.weather$vwnd10m, dtheta = 15, debug=getOption("oceDebug"))
plot(graph.object)

# Wind speed
hist(flights.characteristics$wind.10m.flt.ht, col = "dark grey", xlab = "Wind speed in ms-1, for wind calculated for 10 m altitude")
str(flights.characteristics)
#flight versus wind direction
par(mfrow = c(1,1))
plot(flights$bearing_a_b[outward & trip_duration > 60*30 & trip_type == 0], flights.characteristics$wind.10m.flt.ht[outward & trip_duration > 60*30 & trip_type == 0], xlab = "flight bearing", mar = c(10,10,4,2 ) + 0.1, ylab = "wind direction")



# Flight direction, wind difference ####
par(mfrow = c(1,2))

dif.angle <- flights$bearing_a_b[inward & trip_duration > 60*30 & trip_type == 0] - flights.characteristics$wind.dir[inward & trip_duration > 60*30 & trip_type == 0]

angle.dif <- function(x){
  if(x > 180) {return (360 - x)}
  else return (x)
}

dif.angle <- sapply(abs(dif.angle), angle.dif)

hist(dif.angle, xlim= c(0,180), main = "Inward", xlab = "angle from wind", ylab = "frequency", col = "dark grey", xaxt = "n")
axis(1, at = c(0, 45, 90, 135, 180), lab = c("0", "45", "90", "135", "180"))

dif.angle <- flights$bearing_a_b[outward & trip_duration > 60*30 & trip_type == 0] - flights.characteristics$wind.dir[outward & trip_duration > 60*30 & trip_type == 0]

dif.angle <- sapply(abs(dif.angle), angle.dif)

hist(dif.angle, xlim= c(0, 190), main = "Outward", xlab = "angle from wind", ylab = "frequency", col = "dark grey", xaxt = "n")
axis(1, at = c(0, 45, 90, 135, 180), lab = c("0", "45", "90", "135", "180"))



# Sky conditions ######

names(flights.weather)
#Total Cloud Cover %
hist(flights.weather$tcdceatm[outward | inward])

cloud.high <- flights.weather$tcdceatm > 60
cloud.low <- flights.weather$tcdceatm < 40

# cloud.high <- flights.weather$pratesfc == 0
# cloud.low <- flights.weather$pratesfc > 0


names(flights)
names(flights.weather)
hist(sqrt(flights.weather$pratesfc))
summary(flights.weather$pratesfc == 0)

par(mfrow = c(1,1))
# plot(1-flights$rho ~ flights.weather$tcdceatm)





par(mfrow = c(2,2))
hist(1-flights$rho[outward & cloud.high],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), main = "Outward", breaks = 20)
abline(v = mean(1-flights$rho[outward & cloud.high]), lty = 2, lwd = 2)
median((1-flights$rho[outward & cloud.high]))
mean(1-flights$rho[outward & cloud.high])


hist(1-flights$rho[inward & cloud.high] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "Inward", breaks = 10)
abline(v = mean(1-flights$rho[inward & cloud.high]), lty = 2, lwd = 2)
mean(1-flights$rho[inward & cloud.high])
median(1-flights$rho[inward & cloud.high])


hist(1-flights$rho[outward & cloud.low],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", xlim = c(0,1), main = "Outward", breaks = 20)
abline(v = mean(1-flights$rho[outward & cloud.low]), lty = 2, lwd = 2)
mean(1-flights$rho[outward & cloud.low])
median(1-flights$rho[outward & cloud.low])

hist(1-flights$rho[inward & cloud.low] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", xlim = c(0,1), main = "Inward", breaks = 20)
abline(v = mean(1-flights$rho[inward & cloud.low]), lty = 2, lwd = 2)
mean(1-flights$rho[inward & cloud.low])
median(1-flights$rho[inward & cloud.low])

length(outward)
# length(dif.angle)

mean(1-flights$rho[outward & cloud.high])
mean(1-flights$rho[outward & cloud.low])
mean(1-flights$rho[inward & cloud.high])
mean(1-flights$rho[inward & cloud.low])


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


