#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

# Alternative working directory for when also running another script from same directory.
# setwd("D:/Dropbox/R_projects/lbbg_gps/workspace_alternative")


# Description -----
# In this script we produce various figures and summary statistics to compare outward and inward commuting flights.
# These were originally prepared for a meeting with Susanne and Anders on 2013-01-17


# Datbase functions -----
# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
# WHOLE FLIGHT
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")

# First half
flights.half <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_half1 AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")

#  str(flights)

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
# 
# tm <- as.POSIXlt(flights$start_time)
# # tm[1:10]
# attr(tm,"tzone") <- "UTC"
# # tm[1:10]
# flights$start_time <- tm
# 
# tm <- as.POSIXlt(flights$end_time)
# # tm[1:10]
# attr(tm,"tzone") <- "UTC"
# # tm[1:10]
# flights$end_time <- tm
# 
# str(flights)
# 
# tm <- as.POSIXlt(flights$mid_dist_time)
# # tm[1:10]
# attr(tm,"tzone") <- "UTC"
# # tm[1:10]
# flights$mid_dist_time <- tm

# flights$start_time[1:10]

#str(flights)  #check structure

q1 <- "SELECT DISTINCT f.*
FROM lund_flights_weather AS f
WHERE f.flight_id IN ("

fun <- function(x){
  x <- as.numeric(as.character(x))
  paste(" ", x, ",", sep = "")
}
# ?paste
q2 <- paste(sapply(flights$flight_id,fun), collapse = "")                            
q3 <- ")
ORDER BY f.flight_id ASC;"

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query =
                              gsub("\n", " ",
                                   paste(q1, q2, q3, sep="")),
                            as.is = TRUE)
# str(flights.weather)
# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- NULL
tm <- as.POSIXlt(flights.weather$start_time)
#Check how this appears (i.e. time zone)
# tm[1:10]
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# tm[1:10]
flights.weather$start_time <- tm


#Query the gull db to extract bird_id, nest_id, and nest locations
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")

#str(trips)
tm <- NULL
tm <- as.POSIXlt(trips$start_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$start_time <- tm

tm <- NULL
tm <- as.POSIXlt(trips$end_time)
# tm[1:10]
attr(tm,"tzone") <- "UTC"
# tm[1:10]
trips$end_time <- tm


#Edited this to reference 'lund_flight_paramaters' rather than 'lund_flight_characteristics' as table name changed.
# Later to 'lund_flight_com_weather_par'
flights.characteristics <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                                    FROM lund_flight_com_weather_par AS f
                                    ORDER BY f.flight_id ASC;")




#Trip type and duration#######
trip_type <- 0
trip_duration <- 0
trip_gotland <- 0
trip_distmax <- 0

# names(trips)
# str(flights)
# i <- 5
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

# head(trip_distmax)


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

outward <- (flights$trip_flight_type == "outward") & trip_gotland == 0 & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4 & flights$dist_a_b > 2000 & flights$n_points > 3
hist(flights$dist_a_b[outward]/1000, breaks = 20, main = "out", xlab = "Distance (km)", xlim = c(0,120))

inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000 & flights$n_points > 3
hist(flights$dist_a_b[inward]/1000, breaks = 40, main = "in", xlab = "Distance (km)", xlim = c(0,120))

# flights$n[1:10]
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



names(flights.out)[length(flights.out)-2] <- "dif.angle"
names(flights.in)[length(flights.in)-2] <- "dif.angle"
names(flights.out) == names(flights.in)
# 
# flights.out[1:10,87]
# flights.in[1:10,87]
# names(flights.in[87])

flights.combined <- rbind(flights.out,flights.in)

flights.combined   <- flights.combined[order(flights.combined$trip_id),]




# summary(flights.combined)

# Some function definitions --------

# Logit transformation   ####
logit <- function(x){
  if(x <1.000000000000001 & x > -0.00000000000001){   #Return NAs for values outside of range 0 - 1, with small tollerance either way.
    x.new <- 0.99999999999*x    #Bring all values in slightly, so that values of 1.0 can be processed.
#     x.new[x.new == 0] <- 0.0000000000001
    x.new[x.new == 0] <- 0.001
    x.logit <- log(x.new/(1-x.new))
    return(x.logit)}
  else return(NA)
}


anti.logit <- function(x){
 ex <- exp(x)
 fx <- ex/(ex + 1)
 return(fx)
}

if.neg <- function(x){
  if(x < 0) return(0)
  else return(1)
}


# sample sizes ---------
# install.packages("reshape2")
library(reshape2)

aggregate(speed_inst_mean ~ device_info_serial,
          data = flights.in,
          FUN = length)




# Speed comparision######

# pdf("out_vs_in_inst_speed.pdf")
par(mfrow = c(1,2))
hist(flights$speed_inst_mean[outward ],
     ylim = c(0, 120), xlim = c(0,25), breaks = "Scott",
     xlab = "Speed ms-1",las=1, cex.axis = 1,
     cex.lab = 1, col = "grey", main = "out")

# names(flights)
x1 <- mean(flights$speed_inst_mean[outward], na.rm = TRUE)
abline(v = x1, lwd = 2, lty = 2, col = "red")

hist(flights$speed_inst_mean[inward],
     ylim = c(0, 120), xlim = c(0, 25),
     breaks = "Scott", xlab = "Speed ms-1",
     las = 1, cex.axis = 1, cex.lab = 1,
     col = "grey", main = "in")

x2 <- mean(flights$speed_inst_mean[inward], na.rm = TRUE)
abline(v = x2, lwd = 2, lty = 2, col = "red", main = "in")

# dev.off()

x1
x2

sd(flights$speed_inst_mean[outward], na.rm = TRUE)
sd(flights$speed_inst_mean[inward], na.rm = TRUE)



par(mfrow = c(1,1))
b.break <- seq(-20,20,2.5)
hist(flights.in$speed_inst_mean-flights.out$speed_inst_mean, main = "Speed difference between outward and inward flights",xlab = "Speed ms-1", col = "grey", cex.main = 0.8, breaks = b.break)
x3 <- mean(flights.in$speed_inst_mean-flights.out$speed_inst_mean)
abline(v = x3, lwd = 2, lty = 2, col = "red", main = "in")
sd3 <- sd(flights.in$speed_inst_mean-flights.out$speed_inst_mean)

# paired t-test to compare inward and outward flight speeds.
t.test(flights.in$speed_inst_mean,flights.out$speed_inst_mean, paired = TRUE)




# Rho comparison ####
win.metafile("Out_in_straightness_comparison.wmf")
par(mfrow = c(1,2))
b.break <- seq(0,1,0.05)
hist(flights$rho[outward],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,500), xlim = c(0,1), main = "Outward", breaks = b.break)
hist(flights$rho[inward] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,500), xlim = c(0,1), main = "Inward", breaks = b.break)
dev.off()
# str(flights)

par(mfrow = c(1,2))
b.bar <- seq(0,1,0.05)
hist(flights$straigtness[outward],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,550), xlim = c(0,1), main = "Outward", breaks = b.bar)
hist(flights$straigtness[inward] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,550), xlim = c(0,1), main = "Inward", breaks = b.bar)

names(flights)
# Newly calculated rho for calculated heading (taking wind into account -
# i.e. not ground/ track vector) (commuting_flight_comparisons_categorised_wind_par.R)
par(mfrow = c(1,2))
b.bar <- seq(0,1,0.05)
hist(flights$head_dir_rho[outward],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,300), xlim = c(0,1), main = "Outward", breaks = b.bar)
hist(flights$head_dir_rho[inward] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,300), xlim = c(0,1), main = "Inward", breaks = b.bar)




sd(flights$rho[inward])
sd(flights$rho[outward])
t.test(flights$rho[outward], flights$rho[inward])
wilcox.test(flights$rho[outward], flights$rho[inward])
t.test(flights.in$rho,flights.out$rho, paired = TRUE)




# Altitude ####

names(flights)
par(mfrow = c(1,2))
hist(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],xlab = "Altitude (m)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",  main = "Outward", xlim = c(-20,150), ylim = c(0,150), breaks = 40)
hist(flights$alt_med[inward & (flights$alt_med > -50) & flights$alt_med < 500 ] ,xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "Inward", xlim = c(-20,150), ylim = c(0,150), breaks="Scott")




summary(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ] - flights$alt_mean[outward & (flights$alt_med > -50) & flights$alt_med < 500 ])
all.equal((flights$alt_med - flights$alt_mean) , rep(0,length(flights$alt_mean)))

flights$alt_med[1:20]
flights$alt_mean[1:20]

par(mfrow = c(1,2))
b.break <- seq(-10000,10000,10)
hist(flights$alt_mean[outward & (flights$alt_med > -50) & flights$alt_med < 500],
     xlab = "Altitude (m)",las = 1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",
     main = "Outward", xlim = c(-20,150), ylim = c(0,120), breaks =  b.break)
hist(flights$alt_mean[inward &  (flights$alt_med > -50) & flights$alt_med < 500],
     xlab = "Altitude (m)", las = 1, cex.axis = 1.0, cex.lab = 1.1, col = "red",
     main = "Inward", xlim = c(-20,150), ylim = c(0,120), breaks =  b.break)

# min(flights$alt_med, na.rm = TRUE)

par(mfrow = c(1,2))
b.break <- seq(-100,1000,10)
hist(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],xlab = "Altitude (m)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",  main = "Outward", xlim = c(-20,150), ylim = c(0,120),breaks = b.break)
hist(flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ] ,xlab = "Altitude (m)", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",  main = "Inward", xlim = c(-20,150), ylim = c(0,120),breaks =  b.break)



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

wilcox.test(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],flights$alt_max[inward & (flights$alt_med > -50) & flights$alt_med < 500 ])



t.test(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])
wilcox.test(flights$alt_med[outward & (flights$alt_med > -50) & flights$alt_med < 500 ],flights$alt_med[inward& (flights$alt_med > -50) & flights$alt_med < 500 ])

*********************************
  
  
  
# Rho - wind condition -----
b.spec <- seq(0,1,0.05)
#Graphing non-transformned data
# , ylim = c(0,150)
par(mfrow = c(2,3))
hist(flights.out$rho[flights.out$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = b.spec, main = "Out - tail")
hist(flights.out$rho[flights.out$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = b.spec, main = "Out - head")
hist(flights.out$rho[flights.out$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = b.spec, main = "Out - side")
hist(flights.in$rho[flights.in$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = b.spec,main = "In - tail")
hist(flights.in$rho[flights.in$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = b.spec,main = "In - head")
hist(flights.in$rho[flights.in$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = b.spec,  main = "In - side")


#logit transform data first
flights.out.rho <- sapply(flights.out$rho, FUN = logit)
hist(flights.out.rho)
flights.in.rho <- sapply(flights.in$rho, FUN = logit)
hist(flights.in.rho)



flights.combined.rho <- sapply(flights.combined$rho, FUN = logit)


par(mfrow = c(2,3))
b.spec <- seq(-4,8,.5)

hist(flights.out.rho[flights.out$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = b.spec, main = "Out - tail")
hist(flights.out.rho[flights.out$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = b.spec, main = "Out - head")
hist(flights.out.rho[flights.out$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = b.spec, main = "Out - side")
hist(flights.in.rho[flights.in$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = b.spec,main = "In - tail")
hist(flights.in.rho[flights.in$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = b.spec,main = "In - head")
hist(flights.in.rho[flights.in$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = b.spec,  main = "In - side")

par(mfrow=c(1,1))
plot(flights.in.rho[flights.in$wind.type == "side"]~ 
     flights.in$tcdceatm[flights.in$wind.type == "side"])
abline(lm(flights.in.rho[flights.in$wind.type == "side"]~ 
            flights.in$tcdceatm[flights.in$wind.type == "side"]))

plot(flights.in.rho[flights.in$wind.type == "head"]~ 
       flights.in$tcdceatm[flights.in$wind.type == "head"])
abline(lm(flights.in.rho[flights.in$wind.type == "head"]~ 
            flights.in$tcdceatm[flights.in$wind.type == "head"]))
names(flights)
plot(flights.in.rho[flights.in$wind.type == "tail"]~ 
       flights.in$tcdceatm[flights.in$wind.type == "tail"])
abline(lm(flights.in.rho[flights.in$wind.type == "tail"]~ 
            flights.in$tcdceatm[flights.in$wind.type == "tail"]))

plot(flights.out$alt_med~ 
       flights.out$wind_head_tail_median)
abline(lm(flights.out$alt_med~ 
            flights.out$wind_head_tail_median))

plot(flights.in$alt_med~ 
       flights.in$wind_head_tail_median)
abline(lm(flights.in$alt_med~ 
            flights.in$wind_head_tail_median))

# Head

device_info_serial <- as.factor(flights.combined$device_info_serial)
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
trip_id <- as.factor(flights.combined$trip_id)
# install.packages("lme4")


# Fitting a linear mixed-effects model to determine influence of 

# library(lme4)
library(nlme)

flight.type <- as.factor(flights.combined$flight.type)
wind.type <- as.factor(flights.combined$wind.type)
cloud <- flights.combined$tcdceatm
trip <- as.factor(flights.combined$trip_id)

mod <- list()


# Full factorial model
mod[[1]] <- lme(flights.combined.rho ~ flight.type * wind.type * cloud, random = ~1|device_info_serial/trip)

# Removing nested random effect of foraging trip id
mod[[2]] <- lme(flights.combined.rho ~ flight.type * wind.type * cloud, random = ~1|device_info_serial)

# AIC reduced by removing trip_id, we continue without trip id
AIC(mod[[1]],mod[[2]])


# Temporal autocorrelation structure
# ?ACF
plot(ACF(mod[[2]], maxLag = 100), alpha = 0.05, new = TRUE)
# Refit model with different correlation lags
mod_cor <- list()
for(i in 1:7){
  mod_cor[[i]] <- update(mod[[2]],correlation = corARMA(q = i), method="ML")
}

mod_ML <- list()
# Refit base model by ML
mod_ML[[1]] <- update(mod[[2]], method="ML")

# Compare AIC of models
for(i in 1:7){
  print(AIC(mod_cor[[i]]))
}
AIC(mod_ML[[1]])

# AIC lowest for base-model, don't keep autocorrelation structure.


# Model simplification
mod_ML[[2]] <- lme(flights.combined.rho ~ flight.type * wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[3]] <- lme(flights.combined.rho ~ flight.type * wind.type + wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[4]] <- lme(flights.combined.rho ~ flight.type + wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[5]] <- lme(flights.combined.rho ~ flight.type * cloud +  wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[6]] <- lme(flights.combined.rho ~ flight.type * cloud +  wind.type , random = ~1|device_info_serial, method="ML")


mod_ML[[7]] <- lme(flights.combined.rho ~ flight.type + wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[8]] <- lme(flights.combined.rho ~ flight.type * wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[9]] <- lme(flights.combined.rho ~ flight.type + wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[10]] <- lme(flights.combined.rho ~ flight.type *  cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[11]] <- lme(flights.combined.rho ~ flight.type +  cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[12]] <- lme(flights.combined.rho ~  wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[13]] <- lme(flights.combined.rho ~  wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[14]] <- lme(flights.combined.rho ~  wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[15]] <- lme(flights.combined.rho ~  cloud , random = ~1|device_info_serial, method="ML")

mod_ML[[16]] <- lme(flights.combined.rho ~  flight.type , random = ~1|device_info_serial, method="ML")

mod_ML[[17]] <- lme(flights.combined.rho ~  1 , random = ~1|device_info_serial, method="ML")

mod_ML_AIC <- NULL
for(i in 1:17){
  mod_ML_AIC[i] <- (AIC(mod_ML[[i]]))
}
t(mod_ML_AIC)
min(mod_ML_AIC)


summary(mod_ML[[11]])
anova(mod_ML[[11]])

# anova(mod_ML[[11]],mod_ML[[9]])

# Model 7 has lowest AIC value

# Refit final model by REML
mod_final <- update(mod_ML[[11]], method="REML")
summary(mod_final)
anova(mod_final)

#Checking final model assumptions
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod_final,~resid(.)|device_info_serial)
plot(mod_final)
# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)


# Get p values for model terms
# cloud
anova(mod_ML[[11]], mod_ML[[16]])

# Wind
anova(mod_ML[[11]], mod_ML[[7]])

# Flight.type
anova(mod_ML[[11]], mod_ML[[15]])

# Final model (comparing to null model)
anova(mod_ML[[11]],mod_ML[[17]])


summary(mod_final)
inc <- 3.708102

# Reduction in straightness for outward vs. inward flights
-(anti.logit(inc) - anti.logit(inc-0.651881))
#[1] -0.04587149

# side vs head wind
-(anti.logit(inc) - anti.logit(inc+0.182591))
# [1] 0.005580172

# tail vs head wind
-(anti.logit(inc) - anti.logit(inc+0.013179))
# [1] 0.0004354429

# cloud
anti.logit(inc) - anti.logit(inc*-0.003872)

-0.004016

# grand mean
anti.logit(inc)

# Get R squared values
library(MuMIn)
r.squaredGLMM(mod_final)

# S. Nakagawa, H. Schielzeth. “A General and Simple Method for Obtaining R2 from Generalized Linear Mixed-Effects Models.” Methods in Ecology and Evolution 4 (2013): 133–142. doi:Doi 10.1111/J.2041-210x.2012.00261.X.




# Flight altitude -----


# Inspect lowest values.
sort(flights.combined$alt_med)[1:100]

# Inspect highest values.
par(mfrow=c(1,1))
hist(rev(sort(flights.combined$alt_med))[1:200],breaks = 80)
abline(v = 150)
hist((sort(flights.combined$alt_med))[1:200],breaks = 80)
abline(v = -10)
# Maybe sensible to exclude values > 100 and < -10 - outlying values potentially leading from GPS errors
# Make a filter to this effect
f <- flights.combined$alt_med < 150  &   flights.combined$alt_med > -20

# Transformation
# Strong right-hand skew
hist(flights.combined$alt_med[f])
# log - too strong, now left hand skew
hist(log(flights.combined$alt_med[f]+20))
hist(log10(flights.combined$alt_med[f]+20))
# ?log
# square-root - better
hist(sqrt(flights.combined$alt_med[f] + 20))
# cubed-root - best apparently, also close to Boxcox recomendation
hist((flights.combined$alt_med[f]+20)^(1/3))
hist((flights.combined$alt_med[f]+20)^(1/4))  # This may relate to the wind-shear exponent
hist((flights.combined$alt_med[f]+20)^(0.10))  # This may relate to the wind-shear exponent

# hist((flights.combined$alt_med[f]+10)^(0.1))


library(MASS)
# intercept only model for testing here - ideally do on final model though
mod <- lm((flights.combined$alt_med[f]+20) ~ 1)
summary(mod)
mod
boxcox(mod, lambda = seq(.1, 0.45, len = 20))
# 95% CI includes zero, therefor the log transform is selected.


# Transformed vairable for further analysis
alt_trans <- log10(flights.combined$alt_med[f]+20)


names(flights.combined)

range(alt_trans)

# Histograms to show data - transformed
par(mfrow = c(2,3))
b.fix <- seq(0,2.3,0.1)
hist((alt_trans[flights.combined$wind.type[f] == "tail" & flights.combined$flight.type[f] == "out"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, xlim = range(alt_trans),  main = "Out - tail")
hist((alt_trans[flights.combined$wind.type[f] == "side" & flights.combined$flight.type[f] == "out"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, xlim = range(alt_trans),  main = "Out - side")
hist((alt_trans[flights.combined$wind.type[f] == "head" & flights.combined$flight.type[f] == "out"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, xlim = range(alt_trans),  main = "Out - head")

hist((alt_trans[flights.combined$wind.type[f] == "tail" & flights.combined$flight.type[f] == "in"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, xlim = range(alt_trans),  main = "In - tail")
hist((alt_trans[flights.combined$wind.type[f] == "side" & flights.combined$flight.type[f] == "in"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, xlim =range(alt_trans),  main = "In - side")
hist((alt_trans[flights.combined$wind.type[f] == "head" & flights.combined$flight.type[f] == "in"]),xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, xlim = range(alt_trans),  main = "In - head")



#Histograms of non-transformed data

par(mfrow = c(2,3))
b.fix <- seq(-20,150,10)
hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, main = "Out - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, main = "Out - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = b.fix, main = "Out - head")

hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, main = "In - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, main = "In - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = b.fix, main = "In - head")


summary(f)
length(f)

# library(lme4)
library(nlme)

# Prepare vairables
device_info_serial <- as.factor(flights.combined$device_info_serial[f])
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
trip_id <- as.factor(flights.combined$trip_id[f])
flight.type <- as.factor(flights.combined$flight.type[f])
wind.type <- as.factor(flights.combined$wind.type[f])
cloud <- flights.combined$tcdceatm[f]

# Transformed vairable for further analysis
alt_trans <- log10(flights.combined$alt_med[f]+20)
# log10

# Full factorial model
mod01 <- lme(alt_trans ~ flight.type * wind.type * cloud, random = ~1|device_info_serial/trip_id)
mod02 <- lme(alt_trans ~ flight.type * wind.type*cloud, random = ~1|device_info_serial)
anova(mod01,mod02)

# Temporal autocorrelation structure
plot(ACF(mod01, maxLag = 50), alpha = 0.01)
# ?corARMA
mod_cor <- list()
i <- 1
for(i in 1:7){
 mod_cor[[i]] <- update(mod02, correlation = corARMA(q = i))
}
mod_1_cor <- list()
mod_1_cor[[1]] <- update(mod01, correlation = corARMA(q = 1))
mod_1_cor[[2]] <- update(mod01, correlation = corARMA(q = 2))


AIC(mod01,mod02,mod_cor[[1]],mod_cor[[2]],mod_cor[[3]],mod_1_cor[[1]], mod_1_cor[[2]])

# Choose model 01 (mod01), including nested trip effect, but no autocorrelation structure


# Model simplification
mods_ML <- list()
# Full factorial
mods_ML[[1]] <- lme(alt_trans ~ flight.type * wind.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[2]] <- lme(alt_trans ~ flight.type * wind.type + wind.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[3]] <- lme(alt_trans ~ flight.type * wind.type + cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[4]] <- lme(alt_trans ~ flight.type + wind.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[5]] <- lme(alt_trans ~ flight.type + wind.type + cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[6]] <- lme(alt_trans ~ flight.type* cloud + wind.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[7]] <- lme(alt_trans ~ flight.type* cloud + flight.type*wind.type, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[8]] <- lme(alt_trans ~ flight.type + wind.type + cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[9]] <- lme(alt_trans ~ flight.type * wind.type, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[10]] <- lme(alt_trans ~ flight.type + wind.type, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[11]] <- lme(alt_trans ~ flight.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[12]] <- lme(alt_trans ~ flight.type + cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[13]] <- lme(alt_trans ~ wind.type * cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[14]] <- lme(alt_trans ~ wind.type + cloud, random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[15]] <- lme(alt_trans ~ flight.type , random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[16]] <- lme(alt_trans ~ wind.type , random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[17]] <- lme(alt_trans ~ cloud , random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML[[18]] <- lme(alt_trans ~ 1 , random = ~1|device_info_serial/trip_id, method = "ML")

mods_ML_AIC <- NULL
for(i in 1:18){
  mods_ML_AIC[i] <- AIC(mods_ML[[i]])
}
mods_ML_AIC
min(mods_ML_AIC)

# Model 8 has lowest AIC, that including all main effects with no interaction


# Refit final model by REML
mod_final <- update(mods_ML[[13]], method="REML")
summary(mod_final)
anova(mod_final)


#Checking final model assumptions
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod_final,~resid(.)|device_info_serial)
plot(mod_final)

# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)


# Get p values for model terms
# cloud
anova(mods_ML[[13]], mods_ML[[17]])
# p = 0.0002

# Wind type * cloud interaction
anova(mods_ML[[13]], mods_ML[[14]])
# p <.0001

# Wind
anova(mods_ML[[13]], mods_ML[[17]])
# p <.0001

# Flight.type
anova(mods_ML[[13]], mods_ML[[4]])
# p = 0.113

# All main effects (comparing to null model)
anova(mods_ML[[13]],mods_ML[[18]])
# p = <.0001
# str(cloud)
summary(mod_final)

x <- 2

fx <- function(x){
  x <- 10 ^ x
  x <- x - 20
  return(x)
}


inc <- 1.6513969
cl.mean <- mean(cloud)

# side vs head vs tail under mean cloud level
# head:
fx(inc + ((0 + (-0.0028193*cl.mean))*1 ) )

# tail:
fx(inc + ((0.1052224 + (-0.0028193*cl.mean))*0.561 ) )

# side:
fx(inc + ((-0.0867704 + (-0.0028193*cl.mean))*0.0028478 ) )


intercept + (wind.type + cloud*cloud.level)*interaction


# cloud

# High cloud (2*mean) vs mean cloud
# head:
fx(inc + ((0 + (-0.0028193*cl.mean*2))*1 ) ) -
  fx(inc + ((0 + (-0.0028193*cl.mean))*1 ) )

# tail:
fx(inc + ((0.1052224 + (-0.0028193*cl.mean*2))*0.561 ) ) -
  fx(inc + ((0.1052224 + (-0.0028193*cl.mean*1))*0.561 ) )


# side:
fx(inc + ((-0.0867704 + (-0.0028193*cl.mean*2))*0.0028478 ) ) -
  fx(inc + ((-0.0867704 + (-0.0028193*cl.mean*1))*0.0028478 ) ) 


par(mfrow = c(1,1))
plot(alt_trans~cloud)
plot(fx(alt_trans)~cloud)
abline(lm(alt_trans~cloud))

# grand mean
fx(inc)

# Get R squared values
library(MuMIn)
r.squaredGLMM(mod_final)

# S. Nakagawa, H. Schielzeth. “A General and Simple Method for Obtaining R2 from Generalized Linear Mixed-Effects Models.” Methods in Ecology and Evolution 4 (2013): 133–142. doi:Doi 10.1111/J.2041-210x.2012.00261.X.






# Straightness - r - wind condition -----


# LMMs of straightness - r - windcondition ####
par(mfrow=c(1,1))
flights.combined.r <- sapply(flights.combined$straigtness, FUN = logit)
hist(flights.combined.r)

par(mfrow=c(1,1))
r_straight <- sapply(flights.combined$straigtness, FUN = logit)
r_rho <-  sapply(flights.combined$rho, FUN = logit)

plot(flights.combined$straigtness~flights.combined$rho)
abline(lm(flights.combined$straigtness~flights.combined$rho))

plot(flights.combined$straigtness~flights.combined$rho,
     xlim = c(0.7,1.0), ylim = c(0.8,1.0))
abline(lm(flights.combined$straigtness~flights.combined$rho))

plot(r_straight~r_rho)
abline(lm(r_straight~r_rho))
# ?lm


# Statistical model

device_info_serial <- as.factor(flights.combined$device_info_serial)
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
flight.type <- as.factor(flights.combined$flight.type)
wind.type <- as.factor(flights.combined$wind.type)
cloud <- flights.combined$tcdceatm
trip <- as.factor(flights.combined$trip_id)
device_id <- as.factor(as.character((device_info_serial)))

library(nlme)
# cloud.original <- cloud
# cloud <- cl.th
mod <- list()
# Full factorial model
mod[[1]] <- lme(r_straight ~ flight.type * wind.type* cloud, random = ~1|device_id/trip)


###
# mod.cl <- lme(r_straight ~ flight.type * wind.type* cloud, random = ~1|device_id/trip, method="ML")
# t.cloud <- sapply((cloud/100),logit)
# mod.t.cl <- lme(r_straight ~ flight.type * wind.type* t.cloud, random = ~1|device_id/trip, method="ML")
# thresh <- function(x){
#   if(x < 50) return(0) else return(1)
# }
# cl.th <- as.factor(sapply(cloud, thresh))
# mod.cl.th <- lme(r_straight ~ flight.type * wind.type* cl.th, random = ~1|device_id/trip, method="ML")
# 
# mod.cl.poly <- lme(r_straight ~ flight.type * wind.type* cloud* cloud, random = ~1|device_id/trip, method="ML")
# 
# AIC(mod.cl.th)
# 
# summary(cl.th)
# plot(r_straight ~ cl.th)
# plot(r_straight ~ t.cloud)
# plot(r_straight ~ cloud)
# 
# AIC(mod.cl,mod.t.cl,mod.cl.th,mod.cl.poly)
# hist(t.cloud)
# hist(cloud)
# range(t.cloud)
# range(cloud)
# # logit(0)
# logit(0.01)
# sort(cloud)[1:100]

# Removing nested random effect of foraging trip id
mod[[2]] <- lme(r_straight ~ flight.type * wind.type* cloud, random = ~1|device_id)

# AIC reduced by removing trip_id, we continue without trip id
AIC(mod[[1]],mod[[2]])


# Temporal autocorrelation structure
# ?ACF
plot(ACF(mod[[2]], maxLag = 100), alpha = 0.05, new = TRUE)
# Refit model with different correlation lags
mod_cor <- list()
for(i in 1:7){
  mod_cor[[i]] <- update(mod[[2]],correlation = corARMA(q = i))
}

mod_ML <- list()
# Refit base model by ML
mod_ML[[1]] <- update(mod[[2]], method="ML")


x <- NULL
# Compare AIC of models
for(i in 1:7){
  x[i] <- (AIC(mod_cor[[i]]))
}
min(x)
x
AIC(mod[[2]])

model.full <- mod[[2]]

mod_ML <- list()
# lag 4 has lowest AIC
mod_ML[[1]] <- update(mod[[2]], method="ML")
# Model simplification
mod_ML[[2]] <- lme(r_straight ~ flight.type * wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[3]] <- lme(r_straight ~ flight.type * wind.type + wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[4]] <- lme(r_straight ~ flight.type + wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[5]] <- lme(r_straight ~ flight.type * cloud +  wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[6]] <- lme(r_straight ~ flight.type * cloud +  wind.type , random = ~1|device_info_serial, method="ML")


mod_ML[[7]] <- lme(r_straight ~ flight.type + wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[8]] <- lme(r_straight ~ flight.type * wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[9]] <- lme(r_straight ~ flight.type + wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[10]] <- lme(r_straight ~ flight.type *  cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[11]] <- lme(r_straight ~ flight.type +  cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[12]] <- lme(r_straight ~  wind.type * cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[13]] <- lme(r_straight ~  wind.type + cloud, random = ~1|device_info_serial, method="ML")

mod_ML[[14]] <- lme(r_straight ~  wind.type , random = ~1|device_info_serial, method="ML")

mod_ML[[15]] <- lme(r_straight ~  cloud , random = ~1|device_info_serial, method="ML")

mod_ML[[16]] <- lme(r_straight ~  flight.type , random = ~1|device_info_serial, method="ML")

mod_ML[[17]] <- lme(r_straight ~  1 , random = ~1|device_info_serial, method="ML")

mod_ML_AIC <- NULL
for(i in 1:17){
  mod_ML_AIC[i] <- (AIC(mod_ML[[i]]))
}
t(mod_ML_AIC)
min(mod_ML_AIC)


summary(mod_ML[[6]])
anova(mod_ML[[6]])



# Refit final model by REML
mod_final <- update(mod_ML[[6]], method="REML")
summary(mod_final)
anova(mod_final)

#Checking final model assumptions
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod_final,~resid(.)|device_info_serial)
qqnorm(mod_final)
# ?qqnorm
plot(mod_final)

# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)


# Get p values for model terms
# cloud
anova(mod_ML[[6]], mod_ML[[9]])

# Wind
anova(mod_ML[[6]], mod_ML[[10]])

# Flight.type
anova(mod_ML[[6]], mod_ML[[13]])

# Interaction
anova(mod_ML[[6]], mod_ML[[7]])


# All main effects (comparing to null model)  ########
anova(mod_ML[[6]], mod_ML[[17]])


summary(mod_final)
inc <- 3.884219
mean.cloud <- mean(cloud)
mean.st <- mean(r_straight)
anti.logit(mean(r_straight))
# Reduction in straightness for outward vs. inward flights
# out mean cloud + tail wind
# constand + windtype + interaction*(flighttype + cloud*cloud.level)
calc <- inc + 0.345060 + (0.008207*(-0.927643 + (-0.006145 * mean.cloud)))
anti.logit(calc)
calc
calc/mean.st
x1 <- calc

# out ... + head wind
calc <- inc + 0 + (0.008207*(-0.927643 + (-0.006145 * mean.cloud)))
anti.logit(calc)
calc
calc/mean.st

# out + side wind
calc <- inc + 0.312355 + (0.008207*(-0.927643 + (-0.006145 * mean.cloud)))
anti.logit(calc)
calc
calc/mean.st




# out 2 * mean cloud + tail wind
# constand + windtype + interaction*(flighttype + cloud*cloud.level)
calc <- inc + 0.345060 + (0.008207*(-0.927643 + (-0.006145 * 2*mean.cloud)))
anti.logit(calc)
calc
calc/mean.st




# in mean cloud + tail wind
# constand + windtype + interaction*(flighttype + cloud*cloud.level)
calc <- inc + 0.345060 + (1*(0 + (-0.006145 * mean.cloud)))
anti.logit(calc)
calc
calc/mean.st
x2 <- calc

x1/x2

# in mean 2 * cloud + tail wind
# constand + windtype + interaction*(flighttype + cloud*cloud.level)
calc <- inc + 0.345060 + (1*(0 + (-0.006145 * 2* mean.cloud)))
anti.logit(calc)
calc
calc/mean.st


# in mean  cloud + head wind
# constand + windtype + interaction*(flighttype + cloud*cloud.level)
calc <- inc + 0 + (1*(0 + (-0.006145 *  mean.cloud)))
anti.logit(calc)
calc
calc/mean.st

###

anti.logit(inc + 0.008207*(-0.006145*mean.cloud - 0.927643))
# In mean cloud
anti.logit(inc + 1*(1*mean.cloud*-0.805 -0))

#[1] -0.02998581

# side vs head wind
-(anti.logit(inc) - anti.logit(inc+0.389111))
# [1] 0.009929213

# tail vs head wind
-(anti.logit(inc) - anti.logit(inc+0.251291))
# [1] 0.00682292



-0.004016

# grand mean
anti.logit(inc)

# Get R squared values
library(MuMIn)
r.squaredGLMM(mod_final)

# S. Nakagawa, H. Schielzeth. “A General and Simple Method for Obtaining R2 from Generalized Linear Mixed-Effects Models.” Methods in Ecology and Evolution 4 (2013): 133–142. doi:Doi 10.1111/J.2041-210x.2012.00261.X.

##
















# Temporal autocorrelation structure
?ACF
# Looks sig up to lag 5, and potentially up to lag 9
plot(ACF(mod01, maxLag = 100),alpha=0.01)
mod01_t2 <- update(mod01,correlation = corARMA(q=2))
mod01_t3 <- update(mod01,correlation = corARMA(q=3))
mod01_t4 <- update(mod01,correlation = corARMA(q=4))





flight.type <- as.factor(flights.combined$flight.type)
wind.type <- as.factor(flights.combined$wind.type)

f <- !is.na(flights.combined.r)
length(flights.combined.r[f])
length(flight.type[f])
length(wind.type[f])
length(device_info_serial[f])
summary(is.na(flights.combined.r[f]))

summary(f)

flights.combined.r <- flights.combined.r[f]
flight.type <- flight.type[f]
wind.type <- wind.type[f]
device_id <- as.factor(as.character((device_info_serial[f])))
cloud <- flights.combined$tcdceatm[f]

mod01 <- lme(flights.combined.r ~ flight.type * wind.type*cloud, random = ~1|device_id)

summary(mod01)
anova(mod01)


# Temporal autocorrelation structure
?ACF
# Looks sig up to lag 5, and potentially up to lag 9
plot(ACF(mod01, maxLag = 100),alpha=0.01)
mod01_t2 <- update(mod01,correlation = corARMA(q=2))
mod01_t3 <- update(mod01,correlation = corARMA(q=3))
mod01_t4 <- update(mod01,correlation = corARMA(q=4))
mod01_t5 <- update(mod01,correlation = corARMA(q=5))
mod01_t6 <- update(mod01,correlation = corARMA(q=6))
mod01_t7 <- update(mod01,correlation = corARMA(q=7))


# Comparing autocorrelation levels
anova(mod01_t2,mod01_t3,mod01_t4,mod01_t5,mod01_t6,mod01_t7)
# select lag 6 for correlation structure, lowest AIC




# Model simplification
mod01_t2   <- lme(flights.combined.r ~ flight.type * wind.type*cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod02_t2   <- lme(flights.combined.r ~ flight.type * wind.type+cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod03_t2   <- lme(flights.combined.r ~ flight.type + wind.type*cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod04_t2   <- lme(flights.combined.r ~ flight.type + wind.type + cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod05_t2   <- lme(flights.combined.r ~ flight.type + wind.type, random = ~1|device_id,correlation = corARMA(q=6))

mod06_t2   <- lme(flights.combined.r ~ flight.type +  cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod07_t2   <- lme(flights.combined.r ~  wind.type + cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod08_t2   <- lme(flights.combined.r ~ flight.type , random = ~1|device_id,correlation = corARMA(q=6))

mod09_t2   <- lme(flights.combined.r ~ wind.type , random = ~1|device_id,correlation = corARMA(q=6))

mod10_t2   <- lme(flights.combined.r ~  cloud, random = ~1|device_id,correlation = corARMA(q=6))

mod11_t2   <- lme(flights.combined.r ~  1, random = ~1|device_id,correlation = corARMA(q=6))

mod12_t2   <- lme(flights.combined.r ~  flight.type * wind.type, random = ~1|device_id,correlation = corARMA(q=6))

anova(mod01_t2,mod02_t2,mod03_t2,mod04_t2,mod05_t2,mod06_t2,mod07_t2,mod08_t2,mod09_t2,mod10_t2,mod11_t2)
mod05_t2_ML <- update(mod05_t2, method= "ML")
mod08_t2_ML <- update(mod08_t2, method= "ML")
mod12_t2_ML <- update(mod12_t2, method= "ML")

mod09_t2_ML <- update(mod09_t2, method= "ML")
mod08_t2_ML <- update(mod08_t2, method= "ML")
mod11_t2_ML <- update(mod11_t2, method= "ML")

mod04_t2_ML <- update(mod04_t2, method = "ML")

#Remove flight type
anova(mod05_t2_ML,mod09_t2_ML)

#Remove wind type
anova(mod05_t2_ML,mod08_t2_ML)

#Including cloud too
anova(mod05_t2_ML,mod04_t2_ML)

anova(mod05_t2_ML,mod08_t2_ML)
anova(mod05_t2_ML,mod12_t2_ML)

library(MuMIn)
r.squaredGLMM(mod05_t2_ML)
r.squaredGLMM(mod08_t2_ML)
r.squaredGLMM(mod12_t2_ML)

r.squaredGLMM(mod05_t2)


anova(mod05_t2_ML)
anova(mod08_t2_ML)

summary(mod05_t2)


plot(mod05_t2)
plot( ACF(mod05_t2, maxLag = 50, resType = "n"), alpha = 0.01)

plot(mod05_t2,resid(.,type="p")~fitted(.)|device_id)
qqnorm(mod05_t2,~resid(.)|device_id)


mod_int <- 2.6226552
flt_out <- -0.5174390
x <- mod_int + flt_out

anti.logit(mod_int) - anti.logit(x)
anti.logit(x)

side <- -0.383
tail <- 0.1996534
anti.logit(mod_int) - anti.logit(mod_int + side)
anti.logit(mod_int) - anti.logit(mod_int + tail)




####


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






# playing with transformations ####
x <- flights.out$rho[flights.out$tcdceatm > 60 & flights.out$rho < 1]
x.new <- 0.9999*x
x.new[x.new == 0] <- 0.00001
x.logit <- log(x.new/(1-x.new))


par(mfrow = c(2,2))
hist(x.logit,xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = 10, main = "Out - high")


hist((flights.out$rho[flights.out$tcdceatm < 40  & flights.out$rho < 1]),xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "blue", main = "Out - low")
hist((flights.in$rho[flights.in$tcdceatm > 60 & flights.in$rho < 1]),xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "red", main = "In - high")
hist((flights.in$rho[flights.in$tcdceatm < 40  & flights.in$rho < 1]),xlab = "Straightness",las=1, breaks = 10, cex.axis = 1.0, cex.lab = 1.1, col = "red", main = "In - low")





# Straightness - with wind components -----------

device_info_serial <- as.factor(flights.combined$device_info_serial)
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
trip_id <- as.factor(flights.combined$trip_id)
# install.packages("lme4")


# Fitting a linear mixed-effects model to determine influence of 

# library(lme4)
library(nlme)

flight.type <- as.factor(flights.combined$flight.type)
wind.type <- as.factor(flights.combined$wind.type)
cloud <- flights.combined$tcdceatm
trip <- as.factor(flights.combined$trip_id)



# names(flights.combined)
side.type <- as.factor(sapply(flights.combined$wind_side_mean, if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean, if.neg))

head_tail_abs <- abs(flights.combined$wind_head_tail_mean)
side_abs <- abs(flights.combined$wind_side_mean)




mod <- list()


# Full factorial model
mod[[1]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial/trip)



# Removing nested random effect of foraging trip id
mod[[2]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial)

# AIC reduced by removing trip_id, we continue without trip id
AIC(mod[[1]],mod[[2]])


# Temporal autocorrelation structure
# ?ACF
plot(ACF(mod[[2]], maxLag = 100), alpha = 0.05, new = TRUE)
# Refit model with different correlation lags
mod_cor <- list()
for(i in 1:7){
  mod_cor[[i]] <- update(mod[[2]], correlation = corARMA(q = i))
}


# Compare AIC of models
for(i in 1:7){
  print(AIC(mod_cor[[i]]))
}
AIC(mod[[2]])

# AIC lowest for base-model, don't keep autocorrelation structure.

mod_ML <- list()
# ?lme
str(flight.type)
str(cloud)
str(side.type)


# Model simplification
mod_ML[[1]] <- lme(
  flights.combined.rho ~  flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")
# 
# library(MuMIn)
# ?dredge
# 
# dd <- dredge(mod_ML[[1]])
# 2^6
mod_ML[[2]] <- lme(
  flights.combined.rho ~ flight.type + cloud * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[3]] <- lme(
  flights.combined.rho ~ flight.type * cloud + side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[4]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[5]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[6]] <- lme(
  flights.combined.rho ~  cloud * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")



mod_ML[[7]] <- lme(
  flights.combined.rho ~ flight.type  * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[8]] <- lme(
  flights.combined.rho ~ flight.type * cloud  * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[9]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type *  head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[10]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type * side_abs *   head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[11]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type * side_abs *  head_tail_abs  ,
  random = ~1|device_info_serial, method = "ML")


for(i in 1:11){
  print(AIC(mod_ML[[i]]))
}

# continue with model 5

mod_ML[[5]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[12]] <- lme(
  flights.combined.rho ~  cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[13]] <- lme(
  flights.combined.rho ~ flight.type *  side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[14]] <-lme(
  flights.combined.rho ~ flight.type * cloud *  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[15]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type  * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[16]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs *  head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[17]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs  ,
  random = ~1|device_info_serial, method = "ML")

for(i in c(5, 12:17)){
  print(AIC(mod_ML[[i]]))
}

mod_ML[[5]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[18]] <- lme(
  flights.combined.rho ~ flight.type + cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[19]] <- lme(
  flights.combined.rho ~ flight.type * cloud + side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[20]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[21]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


for(i in c(5, 18:21)){
  print(AIC(mod_ML[[i]]))
}
# AIC(mod_ML[[1]])

summary(mod_ML[[5]])
anova(mod_ML[[5]])


# Get R squared values
library(MuMIn)
r.squaredGLMM(mod_ML[[5]])

# S. Nakagawa, H. Schielzeth. “A General and Simple Method for Obtaining R2 from Generalized Linear Mixed-Effects Models.” Methods in Ecology and Evolution 4 (2013): 133–142. doi:Doi 10.1111/J.2041-210x.2012.00261.X.


mod_ML[[5]] <- lme(
  flights.combined.rho ~ flight.type * cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")


mod_ML[[22]] <- lme(
  flights.combined.rho ~ flight.type * cloud *  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[23]] <- lme(
  flights.combined.rho ~ flight.type * cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[24]] <- lme(
  flights.combined.rho ~ flight.type * cloud *  side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[25]] <- lme(
  flights.combined.rho ~ flight.type + cloud *  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")
for(i in c(5, 22:25)){
  print(AIC(mod_ML[[i]]))
}


mod_ML[[23]] <- lme(
  flights.combined.rho ~ flight.type * cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[26]] <- lme(
  flights.combined.rho ~ flight.type * cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[27]] <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[28]] <- lme(
  flights.combined.rho ~ flight.type * cloud +  side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[29]] <- lme(
  flights.combined.rho ~ flight.type * cloud +  side_abs * head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

for(i in c(23, 26:29)){
  print(AIC(mod_ML[[i]]))
}

mod_ML[[27]] <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[30]] <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[31]] <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[32]] <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs + head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_ML[[33]] <- lme(
  flights.combined.rho ~  cloud + flight.type* side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

for(i in c(1:33)){
  print(AIC(mod_ML[[i]]))
}


summary(mod_ML[[27]])
anova(mod_ML[[27]])
AIC(mod_ML[[27]])

# Get R squared values
library(MuMIn)
r.squaredGLMM(mod_ML[[27]])

# S. Nakagawa, H. Schielzeth. “A General and Simple Method for Obtaining R2 from Generalized Linear Mixed-Effects Models.” Methods in Ecology and Evolution 4 (2013): 133–142. doi:Doi 10.1111/J.2041-210x.2012.00261.X.


mod_final_ML <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

mod_final <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "REML")
r.squaredGLMM(mod_final)

summary(mod_final)
fit.head <- data.frame(flight.type = "out", cloud = 0.5,
                       head_tail.type = 0,
                       side_abs = rep(1:10, 20),
                       head_tail_abs=rep(1:10, each = 20),
                       device_info_serial = 519)
fit.tail <- data.frame(flight.type = "out", cloud = 0.5,
                       head_tail.type = 1,
                       side_abs = rep(1:10, 20),
                       head_tail_abs=rep(1:20, each = 20),
                       device_info_serial = 519)

win.metafile("3D_graph_rho_wind.wmf")
par(mfrow=c(1,2),mai=c(0,0.1,0.2,0)+.02)
fit.head$pred <- predict(mod_final, newdata = fit.head)
persp(x=1:10,y=1:10,z=matrix(anti.logit(fit.head$pred),nrow=10,ncol=10,byrow=TRUE),
      xlab="side_abs",ylab="head_tail_abs",zlab="rho - fit",
      main="Head", zlim=c(0.93,0.99),
      theta = 40, phi = 40,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6)

fit.tail$pred <- predict(mod_final, newdata = fit.tail)
persp(x=1:10,y=1:10,z=matrix(anti.logit(fit.tail$pred),nrow=10,ncol=10,byrow=TRUE),
      xlab="side_abs",ylab="head_tail_abs",zlab="rho - fit",
      main="Tail",zlim=c(0.93,0.99), theta = 40, phi = 40,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6)
dev.off()
# ?persp

mod_final_ML_flight.type <- lme(
  flights.combined.rho ~ cloud +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_flight.type)

mod_final_ML_cloud <- lme(
  flights.combined.rho ~ flight.type +  side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_cloud)

mod_final_ML_side_abs <- lme(
  flights.combined.rho ~ flight.type + cloud +   head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_side_abs)


mod_final_ML_head_tail_abs <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs *  head_tail.type ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_head_tail_abs)

mod_final_ML_head_tail_type <- lme(
  flights.combined.rho ~ flight.type + cloud +  side_abs * head_tail_abs  ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_head_tail_type)

mod_final_ML_inc <- lme(
  flights.combined.rho ~ 1  ,
  random = ~1|device_info_serial, method = "ML")

anova(mod_final_ML, mod_final_ML_inc)
# VarCorr(mod_final_ML)
# anova(mod_final_ML, mod_final_ML_inc)
# getVarCov(mod_final)
# VarCorr(mod_final)


summary(mod_final)




names(flights)
library(CircStats)
plot(abs(deg(cos(rad(flights$wind_dir_track_mean))))~flights$alpha_mean, xlim = c(0,90))
abline(lm(abs(deg(cos(rad(flights$wind_dir_track_mean))))~flights$alpha_mean), col = "red", lwd = 3, lty = 3)
lm(abs(deg(cos(rad(flights$wind_dir_track_mean))))~flights$alpha_mean)
a <- seq(0,50, 5)
head_dir_mean


plot(flights$wind_dir_track_mean~flights$alpha_mean)
plot(flights$wind_dir_track_mean~flights$alpha_mean)

wind/heading
names(flights)

wind.ratio <- flights$wind_side_mean/ flights$speed_inst_mean

plot(abs(wind.ratio)~flights$alpha_mean, ylim = c(0,2), xlim = c(0,60))


# Altitude - with wind components -------

# Make a filter to this effect
f <- flights.combined$alt_med < 150  &   flights.combined$alt_med > -20
# hist(flights.combined$alt_med)

# Prepare vairables
device_info_serial <- as.factor(flights.combined$device_info_serial[f])
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
trip <- as.factor(flights.combined$trip_id[f])
flight.type <- as.factor(flights.combined$flight.type[f])
wind.type <- as.factor(flights.combined$wind.type[f])
cloud <- flights.combined$tcdceatm[f]
temp <- flights.combined$air_2m_mean[f] -273.15  # Convert to deg C
# hist(temp)
  # Transformed vairable for further analysis
alt_trans <- log10(flights.combined$alt_med[f]+20)

side.type <- as.factor(sapply(flights.combined$wind_side_mean[f], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean[f], if.neg))

head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f])
side_abs <- abs(flights.combined$wind_side_mean_10[f])


head_tail_abs2 <- abs(flights.combined$wind_head_tail_mean[f])
side_abs2 <- abs(flights.combined$wind_side_mean[f])

distance <- flights.combined$dist_a_b[f]

library(nlme)

mod <- list()


# Full factorial model
mod[[1]] <- lme(
  alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*temp*distance ,
  random = ~1|device_info_serial/trip)
# ?lme


# Removing nested random effect of foraging trip id
mod[[2]] <- lme(
  alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*temp*distance  ,
  random = ~1|device_info_serial)


# AIC reduced by removing trip_id, we continue without trip id
AIC(mod[[1]],mod[[2]])


# Temporal autocorrelation structure
# ?ACF
plot(ACF(mod[[2]], maxLag = 100), alpha = 0.05, new = TRUE)
# Refit model with different correlation lags
mod_cor <- list()
for(i in 1:7){
  mod_cor[[i]] <- update(mod[[2]], correlation = corARMA(q = i))
}

aic.val <- NULL
# Compare AIC of models
for(i in 1:7){
  aic.val[i] <- (AIC(mod_cor[[i]]))
}
AIC(mod[[2]])
AIC(mod[[1]])
aic.val
min(aic.val)
# AIC lowest for base-model, don't keep autocorrelation structure.



AIC(mod_cor[[5]])

mod_ML <- list()



# intervals(mod_ML[[1]])

# Model simplification
mod_ML[[1]] <- lme(
  alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*temp*distance  ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[2]] <- lme(
  alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*distance+temp*distance  ,
  random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[1]])
AIC(mod_ML[[2]])

mod_ML[[3]] <- lme(
  alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*distance+temp*distance   + temp * flight.type * cloud * side.type * side_abs * head_tail_abs,
  random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[2]])
AIC(mod_ML[[3]])


mod_ML[[4]] <- lme(
  alt_trans ~  flight.type * side.type*temp*distance + cloud * side_abs*temp*distance + cloud *head_tail_abs * head_tail.type *temp*distance,  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[2]])
AIC(mod_ML[[4]])

mod_ML[[5]] <- lme(
  alt_trans ~  flight.type * side.type*temp+ flight.type * side.type*distance + cloud * side_abs*temp*distance + cloud *head_tail_abs * head_tail.type *temp*distance,  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[5]])
AIC(mod_ML[[4]])

summary(mod_ML[[4]])



# ?lme
# 
# mod_ML[[2]] <- lme(
#   alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type,
#   random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[1]])

library(MuMIn)

r.squaredGLMM(mod_ML[[4]])





# ?dredge

# dd <- dredge(mod_ML[[1]])

require(foreach)
require(doParallel)


#Make cluster of number of devices instances
cl <- makeCluster(8)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


# alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
# random = ~1|device_info_serial


# alt_trans ~ flight.type * cloud * side.type * side_abs * head_tail_abs * head_tail.type*temp*distance ,
# random = ~1|device_info_serial , method = "ML")

clusterExport(cl, c("alt_trans", "flight.type", "cloud", "side.type",
                    "side_abs", "head_tail_abs", "head_tail.type",
                    "device_info_serial", "temp","distance"))

clusterEvalQ(cl= cl, library("nlme"))

# ?clusterEvalQ
dd <- pdredge(mod_ML[[5]], cluster = cl)


# close cluster
stopCluster(cl)

dd
summary(dd)

top.models <- get.models(dd, cumsum(weight) <= .95)
str(top.models)
top.models[[1]]


#Checking final model assumptions
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod_final,~resid(.)|device_info_serial)
plot(mod_final)

# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)




















mod_ML[[2]] <- lme(
  alt_trans ~  flight.type * cloud + side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[3]] <- lme(
  alt_trans ~  flight.type * cloud * side.type + side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[4]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[5]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs * head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[6]] <- lme(
  alt_trans ~  cloud + flight.type * side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[7]] <- lme(
  alt_trans ~  cloud * flight.type + side.type * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

# Compare AIC of models
for(i in 1:7){
  print(AIC(mod_ML[[i]]))
}

mod_ML[[4]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[8]] <- lme(
  alt_trans ~  flight.type * cloud * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[9]] <- lme(
  alt_trans ~  flight.type * cloud + side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[10]] <- lme(
  alt_trans ~  flight.type + cloud * side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[11]] <- lme(
  alt_trans ~  flight.type * cloud * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[12]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + head_tail_abs + head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[13]] <- lme(
  alt_trans ~  flight.type * cloud * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[14]] <- lme(
  alt_trans ~  flight.type * cloud * side_abs * head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

# Compare AIC of models
for(i in 8:14){
  print(AIC(mod_ML[[i]]))
}
AIC(mod_ML[[4]])

summary(mod_ML[[4]])


mod_ML[[4]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[15]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + head_tail_abs * head_tail.type*distance,
  random = ~1|device_info_serial  , method = "ML")

mod_ML[[16]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs*temp + head_tail_abs * head_tail.type*distance*temp,
  random = ~1|device_info_serial  , method = "ML")

AIC(mod_ML[[1]])
AIC(mod_ML[[4]])
AIC(mod_ML[[15]])
AIC(mod_ML[[16]])

summary(mod_ML[[16]])

summary(mod_ML[[4]])

r.squaredGLMM(mod_ML[[4]])


mod_ML[[15]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[16]] <- lme(
  alt_trans ~  flight.type * cloud * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[16]])

mod_ML[[17]] <- lme(
  alt_trans ~ flight.type * cloud + cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[18]] <- lme(
  alt_trans ~  flight.type * cloud + flight.type *side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

summary(mod_ML[[15]])
x <- NULL
for(i in 1:18){
  x[i] <- (AIC(mod_ML[[i]]))
}
min(x)
x


mod_ML[[15]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

mod_ML[[19]] <- lme(
  alt_trans ~  flight.type * cloud *  side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[15]])

mod_ML[[15]] <- lme(
  alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")

anova(mod_ML[[15]])

mod_ML[[16]] <- lme(
  alt_trans ~  flight.type *side.type + cloud *flight.type*  side_abs + side.type *side_abs+ cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[16]])
AIC(mod_ML[[15]])


mod_ML[[17]] <- lme(
  alt_trans ~  flight.type *side.type + cloud *flight.type*  side_abs + cloud *head_tail_abs * head_tail.type ,
  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[16]])
AIC(mod_ML[[17]])


anova(mod_ML[[16]])

anova(mod_ML[[17]])

mod_ML[[18]] <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs + flight.type *  side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[17]])
AIC(mod_ML[[18]])

summary(mod_ML[[18]])
AIC(mod_ML[[18]])
anova(mod_ML[[18]])
mod_ML[[19]] <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial , method = "ML")

AIC(mod_ML[[18]])
AIC(mod_ML[[19]])
anova(mod_ML[[19]])

mod_ML[[20]] <- lme(
  alt_trans ~  flight.type + side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[19]])
AIC(mod_ML[[20]])

mod_ML[[21]] <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs + cloud *head_tail_abs + cloud * head_tail.type + head_tail.type * head_tail_abs + head_tail.type* cloud ,  random = ~1|device_info_serial , method = "ML")
AIC(mod_ML[[19]])
AIC(mod_ML[[21]])

r.squaredGLMM(mod_ML[[19]])
anova(mod_ML[[19]])
summary(mod_ML[[19]])
mod_final <- update(mod_ML[[19]], method = "REML")
r.squaredGLMM(mod_final)
anova(mod_final)
summary(mod_final)

# AIC(mod_ML[[1]])
# AIC(mod_ML[[15]])
# AIC(mod_ML[[16]])
# 
# 
# AIC(mod_ML[[19]])


library(MuMIn)
# ?dredge
r.squaredGLMM(mod_final)


dd <- dredge(mod_ML[[16]])

require(foreach)
require(doParallel)


#Make cluster of number of devices instances
cl <- makeCluster(8)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


# alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
# random = ~1|device_info_serial

clusterExport(cl, c("alt_trans", "flight.type", "cloud", "side.type",
                    "side_abs", "head_tail_abs", "head_tail.type",
                    "device_info_serial", "temp"))

clusterEvalQ(cl= cl, library("nlme"))

# ?clusterEvalQ
dd <- pdredge(mod_ML[[19]], cluster = cl)
# # ?pdredge
# 
# #close cluster
stopCluster(cl)

dd
summary(dd)

top.models <- get.models(dd, cumsum(weight) <= .95)
str(top.models)
top.models[[1]]


#Checking final model assumptions
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod_final,~resid(.)|device_info_serial)
plot(mod_final)

# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)



alt_trans ~  flight.type * side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial
# summary(is.na(alt_trans))
# summary(is.na(flight.type))
# summary(is.na(side.type))
# summary(is.na(cloud))
# summary(is.na(side_abs))
# summary(is.na(head_tail_abs))
# summary(is.na(head_tail.type))
# summary(is.na(device_info_serial))


# Model paramater significance
#flight type
mod_ML_flighttype <- lme(
  alt_trans ~  side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")
anova(mod_ML_flighttype, mod_ML[[19]])

mod_ML[[19]] <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")

#cloud
cloud_mod <- lme(
  alt_trans ~  flight.type * side.type +  side_abs + head_tail_abs * head_tail.type ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")
anova(cloud_mod,mod_ML[[19]] )

# side winds
side_mod<- lme(
  alt_trans ~  flight.type  + cloud  + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")
anova(side_mod,mod_ML[[19]] )
summary(mod_final)


# Head- tail winds
head_tail_mod <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")
anova(head_tail_mod,mod_ML[[19]] )



# Model predictions
mod_ML[[19]] <- lme(
  alt_trans ~  flight.type * side.type + cloud * side_abs + cloud *head_tail_abs * head_tail.type ,  random = ~1|device_info_serial, correlation = corARMA(q = 5), method = "ML")
summary(mod_ML[[19]])

fx <- function(x){
  x <- 10 ^ x
  x <- x - 20
  return(x)
}

fit.tail.1 <- data.frame(flight.type = c("out", "out", "in", "in"),
                       cloud = mean(cloud),
                       head_tail.type = c(1,0,1,0),
                       side_abs = mean(side_abs),
                       side.type = 1,
                       head_tail_abs= mean(head_tail_abs),
                       device_info_serial = 519)
pred <- NULL

pred <- predict(mod_final, newdata = fit.tail.1)



fit.tail.1 <- data.frame(flight.type ="out",
                         cloud = c(20,80),
                         head_tail.type = 1,
                         side_abs = mean(side_abs),
                         side.type = 1,
                         head_tail_abs= mean(head_tail_abs),
                         device_info_serial = 519)
pred <- NULL

pred <- predict(mod_final, newdata = fit.tail.1)

fx(pred)

fit.tail.1 <- data.frame(flight.type ="out",
                         cloud = mean(cloud),
                         head_tail.type = 0,
                         side_abs = mean(side_abs),
                         side.type = 1,
                         head_tail_abs= c(0.5*mean(head_tail_abs),mean(head_tail_abs), 2*mean(head_tail_abs)),
                         device_info_serial = 519)
pred <- NULL

pred <- predict(mod_final, newdata = fit.tail.1)

fx(pred)


fit.tail.1 <- data.frame(flight.type ="out",
                         cloud = mean(cloud),
                         head_tail.type = 1,
                         side_abs = c(0.5*mean(side_abs),mean(side_abs), 2*mean(side_abs)),
                         side.type = 1,
                         head_tail_abs= mean(head_tail_abs),
                         device_info_serial = 519)
pred <- NULL

pred <- predict(mod_final, newdata = fit.tail.1)

fx(pred)






# Air speed and wind components ------

# Make a filter to this effect
f <- flights.combined$alt_med < 150  &   flights.combined$alt_med > -20
# hist(flights.combined$alt_med)

# Prepare vairables
device_info_serial <- as.factor(flights.combined$device_info_serial[f])
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
trip <- as.factor(flights.combined$trip_id[f])
flight.type <- as.factor(flights.combined$flight.type[f])
wind.type <- as.factor(flights.combined$wind.type[f])
cloud <- flights.combined$tcdceatm[f]
temp <- flights.combined$air_2m_mean[f] -273.15  # Convert to deg C
# hist(temp)
# Transformed vairable for further analysis
alt_trans <- log10(flights.combined$alt_med[f]+20)

side.type <- as.factor(sapply(flights.combined$wind_side_mean[f], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean[f], if.neg))

head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f])
side_abs <- abs(flights.combined$wind_side_mean_10[f])


head_tail_abs2 <- abs(flights.combined$wind_head_tail_mean[f])
side_abs2 <- abs(flights.combined$wind_side_mean[f])

distance <- flights.combined$dist_a_b[f]
# str(flights.combined)
air_speed <- flights.combined$head_speed_mean[f]

library(nlme)

mod <- list()


# Full factorial model
mod[[1]] <- lme(
  air_speed ~ flight.type * side.type * side_abs * head_tail_abs * head_tail.type*distance ,
  random = ~1|device_info_serial/trip)
# ?lme


library(MuMIn)
# ?dredge
r.squaredGLMM(mod[[1]])


# Removing nested random effect of foraging trip id
mod[[2]] <- lme(
  air_speed ~ flight.type * side.type * side_abs * head_tail_abs * head_tail.type*distance ,
  random = ~1|device_info_serial)


# AIC reduced by removing trip_id, we continue without trip id
AIC(mod[[1]],mod[[2]])


# Temporal autocorrelation structure
# ?ACF
plot(ACF(mod[[2]], maxLag = 100), alpha = 0.05, new = TRUE)
# Refit model with different correlation lags
mod_cor <- list()
for(i in 1:10){
  mod_cor[[i]] <- update(mod[[2]], correlation = corARMA(q = i))
}

aic.val <- NULL
# Compare AIC of models
for(i in 1:10){
  aic.val[i] <- (AIC(mod_cor[[i]]))
}

aic.val
min(aic.val)
AIC(mod[[2]])
AIC(mod[[1]])
# AIC lowest for base-model, don't keep autocorrelation structure.



AIC(mod_cor[[2]])

mod_ML <- list()



# intervals(mod_ML[[1]])

# Model simplification
mod_ML[[1]] <- lme(
  air_speed ~ flight.type * side.type * side_abs * head_tail_abs * head_tail.type*distance  ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")

summary(mod_ML[[1]])


mod_ML[[2]] <- lme(
  air_speed ~ flight.type + side.type * side_abs + head_tail_abs * head_tail.type+distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[1]])
AIC(mod_ML[[2]])

summary(mod_ML[[2]])

r.squaredGLMM(mod[[2]])


mod_ML[[3]] <- lme(
  air_speed ~ flight.type + side.type * side_abs * head_tail_abs * head_tail.type+distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[2]])
AIC(mod_ML[[3]])

mod_ML[[4]] <- lme(
  air_speed ~ flight.type + side.type * side_abs*distance + head_tail_abs * head_tail.type*distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[2]])
AIC(mod_ML[[4]])

summary(mod_ML[[4]])

r.squaredGLMM(mod_ML[[4]])

mod_ML[[5]] <- lme(
  air_speed ~ flight.type + side.type * side_abs + head_tail_abs * head_tail.type*distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[5]])
AIC(mod_ML[[4]])


mod_ML[[6]] <- lme(
  air_speed ~ flight.type + side.type * side_abs*distance + head_tail_abs * head_tail.type    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[6]])
AIC(mod_ML[[4]])

summary(mod_ML[[6]])

r.squaredGLMM(mod_ML[[6]])

mod_ML[[7]] <- lme(
  air_speed ~ flight.type + side.type * side_abs + head_tail_abs * head_tail.type    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[7]])
AIC(mod_ML[[4]])

anova(mod_ML[[4]],mod_ML[[7]])


summary(mod_ML[[4]])


r.squaredGLMM(mod_ML[[4]])
r.squaredGLMM(mod_ML[[7]])



mod_ML[[8]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2*distance + head_tail_abs2 * head_tail.type*distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[4]])
AIC(mod_ML[[8]])

summary(mod_ML[[8]])

r.squaredGLMM(mod_ML[[4]])
r.squaredGLMM(mod_ML[[8]])


mod_ML[[9]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + head_tail_abs2 * head_tail.type+distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[4]])
AIC(mod_ML[[8]])
AIC(mod_ML[[9]])

mod_ML[[10]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + head_tail_abs2 * head_tail.type*distance    ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(mod_ML[[4]])
AIC(mod_ML[[8]])
AIC(mod_ML[[10]])

for(i in 1:10){
  print(AIC(mod_ML[[i]]))
}



require(foreach)
require(doParallel)


#Make cluster of number of devices instances
cl <- makeCluster(8)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


# alt_trans ~  flight.type * cloud * side.type * side_abs + cloud *head_tail_abs * head_tail.type ,
# random = ~1|device_info_serial

clusterExport(cl, c("air_speed", "flight.type",
                    "side.type",
                    "side_abs2", "head_tail_abs2",
                    "head_tail.type", "distance",
                    "device_info_serial"))

clusterEvalQ(cl= cl, library("nlme"))

# ?clusterEvalQ
dd <- pdredge(mod_ML[[8]], cluster = cl)
# # ?pdredge
# 
# #close cluster
stopCluster(cl)

summary(dd)

top.models <- get.models(dd, cumsum(weight) <= .95)
str(top.models)
top.models[[1]]
AIC(top.models[[1]])
AIC(mod_ML[[8]])


mod_ML[[11]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
    head_tail_abs2 * head_tail.type + side.type  *distance +
    head_tail_abs2*distance,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
mod_ML[[12]] <- lme(
  air_speed ~ flight.type + side.type * side_abs + 
    head_tail_abs * head_tail.type + side.type  *distance +
    head_tail_abs*distance,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(top.models[[1]])
AIC(mod_ML[[11]])
AIC(mod_ML[[12]])

mod_ML[[13]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
    head_tail_abs2 * head_tail.type + side.type  *log(distance) +
    head_tail_abs2*log(distance),
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
AIC(top.models[[1]])
AIC(mod_ML[[11]])
AIC(mod_ML[[13]])


mod_ML[[15]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
    head_tail_abs2 * head_tail.type + side.type  *log10(distance/1000) +
    head_tail_abs2*log10(distance/1000),
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")

mod_ML[[16]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
    head_tail_abs2 * head_tail.type +
    head_tail_abs2*log10(distance/1000),
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")

AIC(top.models[[1]])
AIC(mod_ML[[11]])
AIC(mod_ML[[13]])
AIC(mod_ML[[14]])
AIC(mod_ML[[15]])
AIC(mod_ML[[16]])


mod_ML[[17]] <- lme(
  air_speed ~ flight.type +  
    head_tail_abs2 * head_tail.type +
    head_tail_abs2*log10(distance/1000),
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
r.squaredGLMM(mod_ML[[15]])
r.squaredGLMM(mod_ML[[17]])

mod_ML[[18]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
     side.type  *log10(distance/1000) ,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
r.squaredGLMM(mod_ML[[15]])
r.squaredGLMM(mod_ML[[18]])

mod_ML[[19]] <- lme(
  air_speed ~ flight.type + side.type * side_abs2 + 
    head_tail_abs2 * head_tail.type + side.type,
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
r.squaredGLMM(mod_ML[[15]])
r.squaredGLMM(mod_ML[[19]])

hist(air_speed)
var(air_speed)
sd(air_speed)
mean(air_speed)
median(air_speed)

mod_ML[[20]] <- lme(
  air_speed ~ flight.type +  side_abs2 + 
    head_tail_abs2 * head_tail.type + 
    head_tail_abs2*log10(distance/1000),
  random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
# ?log
r.squaredGLMM(mod_ML[[15]])
r.squaredGLMM(mod_ML[[20]])


mod_final <- update(mod_ML[[11]], method = "REML")
mod_final <- update(mod_ML[[15]], method = "REML")
# distance
library(MuMIn)
library(nlme)
r.squaredGLMM(mod_final)
summary(mod_final)
VarCorr(mod_final)


# fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
plot(ranef(mod_final))
# ?ranef

# install.packages("car")
# library(car)
# outlierTest(mod_final)


#Checking final model assumptions
# Label outlier points
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial, id = .05)
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial, id = .01)
plot(mod_final,resid(.,type="p")~fitted(.)|device_info_serial, id = .001)
# ?plot

qqnorm(mod_final,~resid(.)|device_info_serial)
plot(mod_final)
plot(mod_final)
fit.val <- fitted(mod_final)
par(mfrow=c(1,1))
par(pty="s")
plot(fit.val,air_speed, xlim = c(6,20), ylim = c(6,20),
     xlab = "Airspeed (fitted values)",
     ylab = "Airspeed (measured)")
abline(a = 0, b = 1, lwd = 2, lty = 2, col= "red")
?abline

hist(air_speed)
# See how autocorrelation structure looks - ok
plot( ACF(mod_final, maxLag = 50), alpha = 0.01)
plot( ACF(mod_final, maxLag = 50, resType = "n"), alpha = 0.01)

$ panel.args$ y  

plot(air_speed~side_abs2)
plot(air_speed~side_abs)

fx <- function(x,y){
  if(y == 0){
  out <- x*-1} else out <- x
  out
}
head_tail_abs2 * (2*head_tail.type
tail <- mapply(fx,head_tail_abs2,head_tail.type)
plot(air_speed~tail)
plot(air_speed[head_tail.type == 1]~head_tail_abs2[head_tail.type == 1])
plot(air_speed[head_tail.type == 0]~head_tail_abs2[head_tail.type == 0])

plot(air_speed~log10(distance/1000))
                  
                  

model.fit <- cbind(flight.type, side.type, side_abs2, 
                     head_tail_abs2 , head_tail.type,  distance,
                   device_info_serial )                  
model.fit$pred <- predict(mod_final, newdata = model.fit)
                  

#                length(device_info_serial)   
   
                  
                  
                  mod_ML[[15]] <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type + side.type  *log10(distance/1000) +
                      head_tail_abs2*log10(distance/1000),
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  
                 
                  mod.type <- lme(
                    air_speed ~ side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type + side.type  *log10(distance/1000) +
                      head_tail_abs2*log10(distance/1000),
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")                  
                  anova(mod.type,mod_ML[[15]])
summary(mod_final)


                  mod_side <- lme(
                    air_speed ~ flight.type + + 
                      head_tail_abs2 * head_tail.type + 
                      head_tail_abs2*log10(distance/1000),
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  anova(mod_side,mod_ML[[15]])
                  
                  
                  mod.side_type <- lme(
                    air_speed ~  side_abs2 + 
                      head_tail_abs2 * head_tail.type + 
                      head_tail_abs2*log10(distance/1000),
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")                  
                  anova(mod.side_type,mod_ML[[15]])
                  summary(mod_final)
                  
                  mod.side.dist.int <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type + 
                      head_tail_abs2*log10(distance/1000),
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  anova(mod.side.dist.int,mod_ML[[15]])
                  
                  
                  
                  
                  
                  mod_head <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                     side.type  *log10(distance/1000)
                      ,
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")                  
                  anova(mod_head,mod_ML[[15]])
                  
                  
                  
                  mod_head_int <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type + side.type  *log10(distance/1000) 
                    ,
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  anova(mod_head_int,mod_ML[[15]])
                  
                  
                  
                  mod_dist <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type ,
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  anova(mod_dist,mod_ML[[15]])
                  
                  mod_dist_int <- lme(
                    air_speed ~ flight.type + side.type * side_abs2 + 
                      head_tail_abs2 * head_tail.type   +log10(distance/1000) ,
                    random = ~1|device_info_serial , correlation = corARMA(q = 2), method = "ML")
                  anova(mod_dist_int,mod_ML[[15]])
                  
                  
                  
# air_speed ~ flight.type + side.type * side_abs2 + 
#   head_tail_abs2 * head_tail.type + side.type  *distance +
#   head_tail_abs2*distance,
fit.head <- data.frame(flight.type = "out",
                       head_tail.type = 0,
                       side.type = 1,
                       side_abs2 = rep(1:5, 20),
                       head_tail_abs2 = rep(1:5, each = 20),
                       device_info_serial = 519,
                       distance = mean(distance))
fit.head.right <- data.frame(flight.type = "out",
                       head_tail.type = 1,
                       side.type = 1,
                       side_abs2 = rep(1:5, 20),
                       head_tail_abs2 = mean(head_tail_abs2),
                       device_info_serial = 519,
                       distance = rep(seq(10000,50000, 10000), each = 20))

fit.head.left <- data.frame(flight.type = "out",
                       head_tail.type = 1,
                       side.type = 0,
                       side_abs2 = rep(1:5, 20),
                       head_tail_abs2 = mean(head_tail_abs2),
                       device_info_serial = 519,
                       distance = rep(seq(10000,50000, 10000), each = 20))

fit.tail.left <- data.frame(flight.type = "out",
                            head_tail.type = 1,
                            side.type = 0,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 20),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 20))

fit.head.left <- data.frame(flight.type = "out",
                            head_tail.type = 0,
                            side.type = 0,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 5),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 5))



win.metafile("RAND_TEST.wmf")
par(mfrow=c(1,2),mai=c(0,0.1,0.2,0)+.02)
fit.head$pred <- predict(mod_final, newdata = fit.head)
# ?persp
persp(x = unique(fit.head$distance)/1000,
      y = unique(fit.head$side_abs2),
      z = fit.head$pred,
      xlab="a",ylab="b",zlab="Va - fit",
      main="Head", 
      theta = -50, phi = 40,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6)


fit.head.left$pred <- predict(mod_final, newdata = fit.head.left)
fit.head.right$pred <- predict(mod_final, newdata = fit.head.right)
fit.tail.left$pred <- predict(mod_final, newdata = fit.tail.left)
fit.head.left$pred <- predict(mod_final, newdata = fit.head.left)


par(mfrow=c(1,1), mai = c(1,1,1,1)+.02)
plot(fit.head.left$pred~fit.head.left$distance)
plot(fit.head.left$pred~fit.head.left$side_abs2)
# plot(fit.head$pred~fit.head$side_abs2)
plot(fit.head.right$pred~fit.head.right$distance)
plot(fit.head.right$pred~fit.head.right$side_abs2)
# plot(fit.head$pred~fit.head$side_abs2)

plot(fit.tail.left$pred~fit.tail.left$distance)
plot(fit.tail.left$pred~fit.tail.left$head_tail_abs2)


plot(fit.head.left$pred~fit.head.left$distance)
plot(fit.head.left$pred~fit.head.left$head_tail_abs2)





# Data for prediction
fit.head.left <- data.frame(flight.type = "out",
                            head_tail.type = 0,
                            side.type = 0,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 5),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.head.left$pred <- predict(mod_final, newdata = fit.head.left)

par(mfrow=c(1,2))                  
# Draw plot
persp(x = unique(fit.head.left$head_tail_abs2),
      y = unique(fit.head.left$distance)/1000,
      z = matrix(fit.head.left$pred, 5,5),
      xlab= "Head wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Head - left", 
      theta = -40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10.5, 15))
                  



# Data for prediction
fit.head.right <- data.frame(flight.type = "out",
                            head_tail.type = 0,
                            side.type = 1,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 5),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.head.right$pred <- predict(mod_final, newdata = fit.head.right)

# Draw plot
persp(x = unique(fit.head.right$head_tail_abs2),
      y = unique(fit.head.right$distance)/1000,
      z = matrix(fit.head.right$pred, 5,5),
      xlab= "Head wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Head - right", 
      theta = -40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10, 14))





# Data for prediction
fit.tail.left <- data.frame(flight.type = "out",
                            head_tail.type = 1,
                            side.type = 0,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 5),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.tail.left$pred <- predict(mod_final, newdata = fit.tail.left)

# Draw plot
persp(x = unique(fit.tail.left$head_tail_abs2),
      y = unique(fit.tail.left$distance)/1000,
      z = matrix(fit.tail.left$pred, 5,5),
      xlab= "Tail wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Tail - left", 
      theta = 40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10.5, 14))
#                   zlim = c(10.5, 14))


# Data for prediction
fit.tail.right <- data.frame(flight.type = "out",
                            head_tail.type = 1,
                            side.type = 1,
                            side_abs2 = mean(side_abs2),
                            head_tail_abs2 = rep(1:5, 5),
                            device_info_serial = 519,
                            distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.tail.right$pred <- predict(mod_final, newdata = fit.tail.right)

# Draw plot
persp(x = unique(fit.tail.right$head_tail_abs2),
      y = unique(fit.tail.right$distance)/1000,
      z = matrix(fit.tail.right$pred, 5,5),
      xlab= "Tail wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Tail - right", 
      theta = 40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10, 14))


# Data for prediction
fit.tail.right.side <- data.frame(flight.type = "out",
                             head_tail.type = 1,
                             side.type = 1,
                             side_abs2 = rep(1:5, 5),
                             head_tail_abs2 = mean(head_tail_abs2),
                             device_info_serial = 519,
                             distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.tail.right.side$pred <- predict(mod_final, newdata = fit.tail.right.side)

                  par(mfrow=c(1,2))
                  
# Draw plot
persp(x = unique(fit.tail.right.side$side_abs2),
      y = unique(fit.tail.right.side$distance)/1000,
      z = matrix(fit.tail.right.side$pred, 5,5),
      xlab= "Side wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Tail - right", 
      theta = 40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10, 12.5))


# Data for prediction
fit.tail.left.side <- data.frame(flight.type = "out",
                                  head_tail.type = 1,
                                  side.type = 0,
                                  side_abs2 = rep(1:5, 5),
                                  head_tail_abs2 = mean(head_tail_abs2),
                                  device_info_serial = 519,
                                  distance = rep(seq(10000,50000, 10000), each = 5))

# Get model predictions:
fit.tail.left.side$pred <- predict(mod_final, newdata = fit.tail.left.side)

# Draw plot
persp(x = unique(fit.tail.left.side$side_abs2),
      y = unique(fit.tail.left.side$distance)/1000,
      z = matrix(fit.tail.left.side$pred, 5,5),
      xlab= "Side wind speed",
      ylab= "distance (km)",
      zlab= "Va - fit",
      main= "Tail - left", 
      theta = -40, phi = 30,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6,
      cex.axis = 0.6,
      cex.lab = 0.8,
      zlim = c(10, 12.5))
                  





test <- matrix(fit.head.left$pred, 10,10)
length(test)
length(fit.head.left$pred)

fit.tail$pred <- predict(mod_final, newdata = fit.tail)
persp(x=1:5,y=1:5,z=matrix((fit.tail$pred),nrow=5,ncol=5,byrow=TRUE),
      xlab="side_abs",ylab="head_tail_abs",zlab="rho - fit",
      main="Head", 
      theta = -60, phi = 40,
      col = "grey", lwd = 1.5,
      shade = 0.4, axes = TRUE, r = 0.7,
      ticktype = "detailed", cex = 0.6)
dev.off()

summary(mod_final)

# hist(air_speed)






# Drift -------
names(flights.combined)

flight.type
alpha_mean
head_speed_mean
ground_speed_mean
head_dir_mean
ground_dir_mean

library(CircStats)
cos.alpha.inv <- 1/(cos(rad(flights.combined$alpha_mean)))
hist(cos.alpha.inv)

vg.sub.va <- flights.combined$ground_speed_mean - 
  flights.combined$head_speed_mean

flight.type <- as.factor(flights.combined$flight.type)


range(flights.combined$alpha_mean)
mean(flights.combined$alpha_mean)
hist(flights.combined$alpha_mean)

hist(vg.sub.va)
par(mfrow=c(1,1))
plot(flights.combined$ground_dir_mean %% 180 ~ flights.combined$alpha_mean)

track_head <- flights.combined$ground_speed_mean - flights.combined$head_dir_mean

plot(flights.combined$ground_dir_mean ~ track_head,
     xlab = "Track - Heading", ylab = "Track")
abline(lm(flights.combined$ground_dir_mean ~ track_head),
       lwd = 2, lty = 3, col = "red")
lm(flights.combined$ground_dir_mean ~ track_head)


plot(sin(rad(flights.combined$ground_dir_mean)) ~ sin(rad(track_head)),
     xlab = "Sine of Track - Heading", ylab = "Sine of Track")
abline(lm(sin(rad(flights.combined$ground_dir_mean)) ~ sin(rad(track_head))),
       lwd = 3, lty = 3, col = "red")
lm(sin(rad(flights.combined$ground_dir_mean)) ~ sin(rad(track_head)))

plot(sin(rad(flights.combined$ground_dir_mean[flight.type == "in"])) ~ sin(rad(track_head[flight.type == "in"])),
     xlab = "Sine of Track - Heading", ylab = "Sine of Track")
abline(lm(sin(rad(flights.combined$ground_dir_mean[flight.type == "in"])) ~ sin(rad(track_head[flight.type == "in"]))),
       lwd = 3, lty = 3, col = "black")
lm(sin(rad(flights.combined$ground_dir_mean[flight.type == "in"])) ~ sin(rad(track_head[flight.type == "in"])))

points(sin(rad(flights.combined$ground_dir_mean[flight.type == "out"])) ~ sin(rad(track_head[flight.type == "out"])), col = "red")
abline(lm(sin(rad(flights.combined$ground_dir_mean[flight.type == "out"])) ~ sin(rad(track_head[flight.type == "out"]))),
       lwd = 3, lty = 3, col = "red")
lm(sin(rad(flights.combined$ground_dir_mean[flight.type == "out"])) ~ sin(rad(track_head[flight.type == "out"])))


plot(cos(rad(flights.combined$ground_dir_mean)) ~ cos(rad(track_head)),
     xlab = "Track - Heading", ylab = "Track")

plot(flights.combined$ground_dir_mean ~ flights.combined$alpha_mean,
     xlab = "Alpha", ylab = "Track")
abline(lm(flights.combined$ground_dir_mean ~ flights.combined$alpha_mean),
       lwd = 2, lty = 3, col = "red")
lm(flights.combined$ground_dir_mean ~ flights.combined$alpha_mean)




mod.01 <- glm(flights.combined$head_speed_mean ~ vg.sub.va*cos.alpha.inv*flight.type)
summary(mod.01)
mod.01 <- glm(flights.combined$head_speed_mean ~ vg.sub.va*cos.alpha.inv+vg.sub.va*flight.type + cos.alpha.inv*flight.type)
summary(mod.01)

mod.02 <- glm(flights.combined$head_speed_mean ~ vg.sub.va*cos.alpha.inv+vg.sub.va*flight.type)
summary(mod.02)

mod.03 <- glm(flights.combined$head_speed_mean ~ vg.sub.va + cos.alpha.inv+flight.type)
summary(mod.03)
anova(mod.03)
?glm

cos.test <- seq(-60,60,1)
cos.test.a <- cos(rad(cos.test))
plot(cos.test.a~cos.test, type = "l")
plot((1/cos.test.a)~cos.test, type = "l")


plot(flights.combined$head_speed_mean ~ vg.sub.va,
     ylab = "Va", xlab = "Vg - Va",
     ylim = c(5,22))
abline(lm(flights.combined$head_speed_mean ~ vg.sub.va), lwd = 3, lty = 2)
abline(lm(flights.combined$head_speed_mean[vg.sub.va< -1] ~ vg.sub.va[vg.sub.va< -1]), col = "blue", lwd = 3, lty = 2)
abline(lm(flights.combined$head_speed_mean[vg.sub.va> +1] ~ vg.sub.va[vg.sub.va> +1]), col = "red", lwd = 3, lty = 2)

plot(flights.combined$head_speed_mean ~ cos.alpha.inv, xlim = c(1,1.4))


plot(flights.combined$ground_dir_mean ~ flights.combined$alpha_mean)
plot(sin(rad(flights.combined$ground_dir_mean)) ~ flights.combined$alpha_mean)
plot(cos(rad(flights.combined$ground_dir_mean)) ~ flights.combined$alpha_mean)

plot(sin(rad(flights.combined$ground_dir_mean)) ~ flights.combined$alpha_mean)
lm.mod <- lm(sin(rad(flights.combined$ground_dir_mean)) ~ flights.combined$alpha_mean)
lm.mod
abline(lm.mod, col = "red", lwd = 2)

hist(cos(rad(flights.combined$ground_dir_mean)))
hist(sin(rad(flights.combined$ground_dir_mean)))
hist(flights.combined$ground_dir_mean, xlim = c(0,360), breaks = seq(0,360,20))
# max(flights.combined$ground_dir_mean)

names(flights.combined)
plot(flights.combined$wind_dir_track_mean~flights.combined$alpha_mean)

maxColorValue <- 200
palette <- colorRampPalette(c("light blue","dark red"))(maxColorValue)
plot(flights.combined$wind_dir_track_mean~flights.combined$alpha_mean,
     col = palette[cut(flights.combined$windspeed, maxColorValue)],
     xlab = "alpha", ylab = "wind relative to track",
     cex = 1)
# warnings()
abline(lm(flights.combined$wind_dir_track_mean~flights.combined$alpha_mean), lwd = 3, lty = 3)
lm(flights.combined$wind_dir_track_mean~flights.combined$alpha_mean)
# ?cut


plot(flights.combined$wind_dir_track_mean~flights.combined$alpha_mean,
     col = as.numeric(flight.type),
     xlab = "alpha", ylab = "wind relative to track",
     cex = 1)
abline(lm(flights.combined$wind_dir_track_mean[flight.type == "out"]~flights.combined$alpha_mean[flight.type == "out"]), col = "red")
abline(lm(flights.combined$wind_dir_track_mean[flight.type == "in"]~flights.combined$alpha_mean[flight.type == "in"]), col = "black")




plot(cos(rad(flights.combined$wind_dir_track_mean))~flights.combined$alpha_mean,
     col = palette[cut(flights.combined$windspeed, maxColorValue)],
     xlab = "alpha", ylab = "wind relative to track",
     cex = 1)



plot(sin(rad(flights.combined$wind_dir_track_mean))~flights.combined$alpha_mean,
     col = palette[cut(flights.combined$windspeed, maxColorValue)],
     xlab = "alpha", ylab = "Sine of wind relative to track",
     cex = 1)
abline(lm(sin(rad(flights.combined$wind_dir_track_mean))~flights.combined$alpha_mean), lwd = 3, lty = 2)
f <- flights.combined$windspeed < 2
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[f]))~flights.combined$alpha_mean[f]), lwd = 3, lty = 2, col = "blue")
f <- flights.combined$windspeed > 6
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[f]))~flights.combined$alpha_mean[f]), lwd = 3, lty = 2, col = "red")
# hist(flights.combined$windspeed)


plot(sin(rad(flights.combined$wind_dir_track_mean))~sin(rad(flights.combined$alpha_mean)),
     col = palette[cut(flights.combined$windspeed, maxColorValue)],
     xlab = "Sine of alpha", ylab = "Sine of wind relative to track",
     cex = 1)
abline(lm(sin(rad(flights.combined$wind_dir_track_mean))~sin(rad(flights.combined$alpha_mean))), lwd = 3, lty = 2)
f <- flights.combined$windspeed < 2
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[f]))~sin(rad(flights.combined$alpha_mean[f]))), lwd = 3, lty = 2, col = "blue")
f <- flights.combined$windspeed > 6
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[f]))~sin(rad(flights.combined$alpha_mean[f]))), lwd = 3, lty = 2, col = "red")
# hist(flights.combined$windspeed)

lm(sin(rad(flights.combined$wind_dir_track_mean))~sin(rad(flights.combined$alpha_mean)))

angles <- seq(-360,360,10)
plot(angles, sin(rad(angles)))
sin(rad(45))



ft <- flight.type == "in"
plot(sin(rad(flights.combined$wind_dir_track_mean[ft]))~sin(rad(flights.combined$alpha_mean[ft])),
     col = palette[cut(flights.combined$windspeed[ft], maxColorValue)],
     xlab = "Sine of alpha", ylab = "Sine of wind relative to track",
     cex = 1)
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft]))~sin(rad(flights.combined$alpha_mean[ft]))), lwd = 3, lty = 2)
f <- flights.combined$windspeed[ft] < 2
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft & f]))~sin(rad(flights.combined$alpha_mean[ft & f]))), lwd = 3, lty = 2, col = "blue")
f <- flights.combined$windspeed > 6
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft & f]))~sin(rad(flights.combined$alpha_mean[ft & f]))), lwd = 3, lty = 2, col = "red")
# hist(flights.combined$windspeed)



ft <- flight.type == "out"
plot(sin(rad(flights.combined$wind_dir_track_mean[ft]))~sin(rad(flights.combined$alpha_mean[ft])),
     col = palette[cut(flights.combined$windspeed[ft], maxColorValue)],
     xlab = "Sine of alpha", ylab = "Sine of wind relative to track",
     cex = 1)
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft]))~sin(rad(flights.combined$alpha_mean[ft]))), lwd = 3, lty = 2)
f <- flights.combined$windspeed[ft] < 2
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft & f]))~sin(rad(flights.combined$alpha_mean[ft & f]))), lwd = 3, lty = 2, col = "blue")
f <- flights.combined$windspeed > 6
abline(lm(sin(rad(flights.combined$wind_dir_track_mean[ft & f]))~sin(rad(flights.combined$alpha_mean[ft & f]))), lwd = 3, lty = 2, col = "red")
# hist(flights.combined$windspeed)
