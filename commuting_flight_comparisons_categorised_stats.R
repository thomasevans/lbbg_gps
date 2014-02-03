#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

#Description ----
# In this script we analyse flights to look at flight altitude, airspeed, and straightness, and how these are effected by different conditions, e.g. winds. Further we analyse flights with respect to wind drift - do the gulls compensate for this?

# Alternative working directory for when also running another script from same directory.
# setwd("D:/Dropbox/R_projects/lbbg_gps/workspace_alternative")



#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
# WHOLE FLIGHT
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")


# Get flight weather data
# Assemble SQL request
q1 <- "SELECT DISTINCT f.*
FROM lund_flights_weather AS f
WHERE f.flight_id IN ("

fun <- function(x){
  x <- as.numeric(as.character(x))
  paste(" ", x, ",", sep = "")
}

q2 <- paste(sapply(flights$flight_id,fun), collapse = "")                            

q3 <- ")
ORDER BY f.flight_id ASC;"

#Get a copy of the flights_weather DB table.
flights.weather <- sqlQuery(gps.db, query =
                              gsub("\n", " ",
                                   paste(q1, q2, q3, sep="")),
                            as.is = TRUE)

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r
tm <- NULL
tm <- as.POSIXlt(flights.weather$start_time)
#Check how this appears (i.e. time zone)
# head(tm)
attr(tm, "tzone") <- "UTC"
#Check how appears after change of time-zone - i.e. is the absolute time
#value unchanged?
# head(tm)
flights.weather$start_time <- tm



#Query the gull db to extract trip information
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")
# str(trips)

# Get flight information
flights.characteristics <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                                    FROM lund_flight_com_weather_par AS f
                                    ORDER BY f.flight_id ASC;")

# str(flights.characteristics)



# Label trip type and duration -----
trip_type     <- 0
trip_duration <- 0
trip_gotland  <- 0
trip_distmax  <- 0

# Go through all trips, labelling by trip type duration etc.
# Later to be used for filtering criteria
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

# Make to factors - not numeric
trip_type    <- as.factor(trip_type)
trip_gotland <- as.factor(trip_gotland)
# 
# summary(trip_type)
# summary(trip_gotland)


# Sepparate outward and inward flights ------
outward <- (flights$trip_flight_type == "outward") & trip_gotland == 0 & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4 & flights$dist_a_b > 2000

inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000

summary(inward)
summary(outward)


# Angle with respect to wind
dif.angle <- flights$bearing_a_b - flights.characteristics$winddir
# hist(dif.angle)
dif.angle <- abs(dif.angle)

cor.ang <- function(x){
  if(x > 180){ y <- 180 - (x - 180)}
  else y <- x
  return(y)
}
# cor.ang(181)
dif.angle <- sapply(dif.angle, cor.ang)
#  hist(dif.angle)


# Re-arrange the data for paired data comparison  -----
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
# Check that names all correspond between the two tables.
all.equal(names(flights.out), names(flights.in))


# Combined table of outward and inward flights
flights.combined <- rbind(flights.out,flights.in)

# Order flights by date-time at start of flight
flights.combined   <- flights.combined[order(flights.combined$start_time),]


# Some function definitions --------

# Logit transformation
logit <- function(x){
  if(x <1.0001 & x > -0.0001){   #Return NAs for values outside of range 0 - 1, with small tollerance either way.
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

# Function to remove negative values - reallocate as zero
if.neg <- function(x){
  if(x < 0) return(0)
  else return(1)
}



# sample sizes ---------
# install.packages("reshape2")
library(reshape2)

aggregate(speed_inst_mean ~ device_info_serial,
          data = flights.combined,
          FUN = length)

# Statistical analyses ------

# Altitude ------

# Make a filter
f <- flights.combined$alt_med < 150  &   flights.combined$alt_med > -20 & !is.na(flights.combined$wind_side_mean)
# hist(flights.combined$alt_med)
summary(f)


# Retain only trips where both the outward and inward flight fulfill above conditions
f2 <- flights.combined$trip_id %in% flights.combined$trip_id[!f]
f3 <- !f2
summary(f3)


# Prepare variables
device_info_serial <- flights.combined$device_info_serial[f3]
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
summary(device_info_serial)

trip_id <- trip <- as.factor(flights.combined$trip_id[f3])

flight.type <- as.factor(flights.combined$flight.type[f3])

date.time <- flights.combined$start_time[f3]
# summary(flight.type)
# Temperature at 2m level
temp_2m <- flights.combined$temperature_2mmean[f3]  -273.15
# hist(temp_2m)

# Total atomospheric cloud cover
cloud_total <- flights.combined$cloud_cover_totalmean[f3]
hist(cloud_total)
hist(logit(cloud_total))
cloud_total_logit <- logit(cloud_total)

# Low level cloud (may reflect atmospheric visibility better)
cloud_low <- flights.combined$cloud_cover_low_altitudemean[f3]
hist(cloud_low)
cloud_low_logit <- sapply(cloud_low,logit)
hist(cloud_low_logit)

# Altitude responce variable - including transformation
altitude <- flights.combined$alt_med[f3]
hist(altitude)
alt_trans <- log10(flights.combined$alt_med[f3]+20)
hist(alt_trans)

# Wind data
side.type <- as.factor(sapply(flights.combined$wind_side_mean_10[f3], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean_10[f3], if.neg))
head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f3])
side_abs <- abs(flights.combined$wind_side_mean_10[f3])

head_tail_vec <- (flights.combined$wind_head_tail_mean_10[f3])
# hist(head_tail_vec)
side_vec <- (flights.combined$wind_side_mean_10[f3])
# hist(side_vec)

# Flight distance
distance <- flights.combined$dist_a_b[f3]/1000
hist(distance/1000)  # distances in km

sea_level_pressure <- flights.combined$sea_level_pressuremean[f3] - 99000
# min(sea_level_pressure)

# Compile variables into single dataframe
alt_data <- cbind(device_info_serial, trip_id, flight.type,
                  date.time, temp_2m, cloud_total,
                  cloud_total_logit, cloud_low,
                  cloud_low_logit, altitude, side_abs,
                  alt_trans, side.type, head_tail.type,
                  head_tail_abs, head_tail_vec, side_vec,
                  distance, sea_level_pressure)
alt_data <- as.data.frame(alt_data)

str(alt_data)
alt_data$device_info_serial <- as.factor(alt_data$device_info_serial)
alt_data$trip_id <- as.factor(alt_data$trip_id)
alt_data$flight.type  <- as.factor(alt_data$flight.type)
alt_data$side.type   <- as.factor(alt_data$side.type)
alt_data$head_tail.type   <- as.factor(alt_data$head_tail.type)

# Statistical analysis

library(nlme)

mod <- list()
mod_ml <- list()

# full model with all 2-way interactions included
mod[[1]]  <-  lme(alt_trans ~
                    (flight.type + temp_2m + cloud_total +
                    cloud_low_logit + head_tail_vec + side_abs +
                    distance + sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = alt_data
                  )
mod_ml[[1]] <- update(mod[[1]], method = "ML")
summary(mod[[1]])


mod[[2]]  <-  lme(alt_trans ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_abs + side_abs +
                       distance + sea_level_pressure +
                       head_tail.type)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = alt_data
)
mod_ml[[2]] <- update(mod[[2]], method = "ML")
summary(mod[[2]])

anova(mod_ML[[1]], mod_ML[[2]])

# Better to allow different slope for tail and head winds

mod[[3]] <-  update(mod[[2]], random = ~1|device_info_serial)
anova(mod[[2]], mod[[3]])

# Better to include trip id in random effects - i.e taking account of nestedness

# Now simplify - only retain significant terms

anova(mod_ML[[2]])

mod[[4]]    <-  update(mod[[2]], .~. -distance:sea_level_pressure - cloud_low_logit:distance -  cloud_total:sea_level_pressure - temp_2m:sea_level_pressure)

mod_ML[[4]]  <- update(mod[[4]], method = "ML")

anova(mod[[4]])

mod[[5]]    <-  update(mod[[4]], .~. -flight.type:distance -  temp_2m:cloud_total - temp_2m:cloud_low_logit - cloud_total:head_tail_abs - cloud_total:distance - cloud_low_logit:sea_level_pressure - head_tail_abs:distance  - head_tail_abs:sea_level_pressure - distance:head_tail.type)

mod_ML[[5]] <- update(mod[[5]], method = "ML")

anova(mod_ML[[4]], mod_ML[[5]])

anova(mod[[5]])

mod[[6]]    <-  update(mod[[5]], .~. -sea_level_pressure:head_tail.type - head_tail_abs:side_abs - side_abs:sea_level_pressure)

mod_ML[[6]] <- update(mod[[6]], method = "ML")

anova(mod_ML[[5]], mod_ML[[6]])

anova(mod[[6]])



mod[[7]]    <-  update(mod[[6]], .~. -temp_2m:distance - cloud_low_logit:head_tail_abs)

mod_ML[[7]] <- update(mod[[7]], method = "ML")

anova(mod_ML[[6]], mod_ML[[7]])

anova(mod[[7]])

mod[[8]]    <-  update(mod[[7]], .~. -cloud_low_logit:head_tail.type - flight.type:sea_level_pressure)

mod_ML[[8]] <- update(mod[[8]], method = "ML")

anova(mod_ML[[7]], mod_ML[[8]])

anova(mod[[8]])


mod[[9]]    <-  update(mod[[8]], .~. -flight.type:head_tail.type)

mod_ML[[9]] <- update(mod[[9]], method = "ML")

anova(mod_ML[[8]], mod_ML[[9]])

anova(mod[[9]])


mod[[10]]    <-  update(mod[[9]], .~. -side_abs:distance)

mod_ML[[10]] <- update(mod[[10]], method = "ML")

anova(mod_ML[[9]], mod_ML[[10]])

anova(mod[[10]])



mod[[11]]    <-  update(mod[[10]], .~. -cloud_total:cloud_low_logit)

mod_ML[[11]] <- update(mod[[11]], method = "ML")

anova(mod_ML[[10]], mod_ML[[11]])

anova(mod[[11]])

# Stick with mod[[11]] for now, not all significant, but mostly, and AIC now increases if terms are removed.

# Look at model

intervals(mod[[11]])
anova(mod[[11]])
# assumptions
plot(mod[[11]], flight.type ~ resid(.), abline = 0)

plot(mod[[11]], resid(., type = "p") ~ fitted(.) | device_info_serial, id = 0.05, adj = -0.3)

plot(mod[[11]], alt_trans ~ fitted(.), id = 0.05, adj = -0.3)

qqnorm(mod[[11]], ~resid(.)| flight.type)
qqnorm(mod[[11]], ~resid(.)| device_info_serial)


plot(mod[[11]])
qqnorm(mod[[11]])


library(MuMIn)
r.squaredGLMM(mod[[11]])

# Explains 48% of variation in altitude, with 39% down to main effects, other 9% random effects




# Va -------

f <-  !is.na(flights.combined$wind_side_mean)
summary(f)
# names(flights.combined)
# hist(flights.combined$head_speed_mean)

f2 <- flights.combined$trip_id %in% flights.combined$trip_id[!f]
f3 <- !f2
summary(f3)



# Prepare variables
device_info_serial <- flights.combined$device_info_serial[f3]
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
summary(device_info_serial)

trip_id <- trip <- as.factor(flights.combined$trip_id[f3])

flight.type <- as.factor(flights.combined$flight.type[f3])

date.time <- flights.combined$start_time[f3]

# Temperature at 2m level
temp_2m <- flights.combined$temperature_2mmean[f3]  -273.15
# hist(temp_2m)

# Total atomospheric cloud cover
cloud_total <- flights.combined$cloud_cover_totalmean[f3]
hist(cloud_total)
hist(logit(cloud_total))
cloud_total_logit <- logit(cloud_total)

# Low level cloud (may reflect atmospheric visibility better)
cloud_low <- flights.combined$cloud_cover_low_altitudemean[f3]
hist(cloud_low)
cloud_low_logit <- sapply(cloud_low,logit)
hist(cloud_low_logit)

# Wind data
side.type <- as.factor(sapply(flights.combined$wind_side_mean_10[f3], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean_10[f3], if.neg))
head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f3])
side_abs <- abs(flights.combined$wind_side_mean_10[f3])

head_tail_vec <- (flights.combined$wind_head_tail_mean_10[f3])
# hist(head_tail_vec)
side_vec <- (flights.combined$wind_side_mean_10[f3])
# hist(side_vec)

# Flight distance
distance <- flights.combined$dist_a_b[f3]/1000
hist(distance/1000)  # distances in km

sea_level_pressure <- flights.combined$sea_level_pressuremean[f3] - 99000
# min(sea_level_pressure)

# Va
va <-  flights.combined$head_speed_mean[f3]

# Compile variables into single dataframe
va_data <- cbind(device_info_serial, trip_id, flight.type,
                  date.time, temp_2m,va , cloud_total,
                  cloud_total_logit, cloud_low,
                  cloud_low_logit, side_abs,
                   side.type, head_tail.type,
                  head_tail_abs, head_tail_vec, side_vec,
                  distance, sea_level_pressure)
va_data <- as.data.frame(va_data)

str(va_data)
va_data$device_info_serial <- as.factor(va_data$device_info_serial)
va_data$trip_id <- as.factor(va_data$trip_id)
va_data$flight.type  <- as.factor(va_data$flight.type)
va_data$side.type   <- as.factor(va_data$side.type)
va_data$head_tail.type   <- as.factor(va_data$head_tail.type)


# Statistics
library(nlme)

mod <- list()
mod_ml <- list()

# full model with all 2-way interactions included
mod[[1]]  <-  lme(va ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_vec + side_abs +
                       distance + sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = va_data
)
mod_ml[[1]] <- update(mod[[1]], method = "ML")
summary(mod[[1]])


mod[[2]]  <-  lme(va ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_abs + side_abs +
                       distance + sea_level_pressure +
                       head_tail.type)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = va_data
)
mod_ml[[2]] <- update(mod[[2]], method = "ML")
summary(mod[[2]])

anova(mod_ML[[1]], mod_ML[[2]])

# Better to keep Va as vector, not sepperate head/ tail

mod[[3]] <-  update(mod[[1]], random = ~1|device_info_serial)
anova(mod[[1]], mod[[3]])

# Better not to include trip id in random effects

# May be worth looking at correlation structure later
plot(ACF(mod[[3]], maxLag = 10), alpha = 0.01)

# Now simplify - only retain significant terms

anova(mod_ML[[3]])

mod[[4]]    <-  update(mod[[3]], .~. -cloud_low_logit:distance - cloud_total:sea_level_pressure - temp_2m:sea_level_pressure - temp_2m:cloud_total)

mod_ML[[4]]  <- update(mod[[4]], method = "ML")

anova(mod[[4]])

mod[[5]]    <-  update(mod[[4]], .~. -distance:sea_level_pressure - head_tail_vec:sea_level_pressure - cloud_low_logit:head_tail_vec - temp_2m:distance - temp_2m:side_abs)

mod_ML[[5]]  <- update(mod[[5]], method = "ML")

anova(mod_ML[[5]], mod_ML[[4]])
anova(mod[[5]])



mod[[6]]    <-  update(mod[[5]], .~. -flight.type:temp_2m - flight.type:cloud_low_logit - flight.type:distance)

mod_ML[[6]]  <- update(mod[[6]], method = "ML")

anova(mod_ML[[6]], mod_ML[[5]])
anova(mod[[6]])


mod[[7]]    <-  update(mod[[6]], .~. -temp_2m:cloud_low_logit - cloud_total:cloud_low_logit)

mod_ML[[7]]  <- update(mod[[7]], method = "ML")

anova(mod_ML[[7]], mod_ML[[6]])
anova(mod[[7]])


mod[[8]]    <-  update(mod[[7]], .~. -flight.type:sea_level_pressure - side_abs:sea_level_pressure)

mod_ML[[8]]  <- update(mod[[8]], method = "ML")

anova(mod_ML[[8]], mod_ML[[7]])
anova(mod[[8]])


mod[[9]]    <-  update(mod[[8]], .~. -cloud_low_logit:side_abs)

mod_ML[[9]]  <- update(mod[[9]], method = "ML")

anova(mod_ML[[9]], mod_ML[[8]])
anova(mod[[9]])


mod[[10]]    <-  update(mod[[9]], .~. -cloud_total:side_abs)

mod_ML[[10]]  <- update(mod[[10]], method = "ML")

anova(mod_ML[[10]], mod_ML[[9]])
anova(mod[[10]])


mod[[11]]    <-  update(mod[[10]], .~. -cloud_low_logit:sea_level_pressure - cloud_total:distance - cloud_total:head_tail_vec - flight.type:cloud_total)

mod_ML[[11]]  <- update(mod[[11]], method = "ML")

anova(mod_ML[[11]], mod_ML[[10]])
anova(mod[[11]])


mod[[12]]    <-  update(mod[[11]], .~. -cloud_low_logit - cloud_total)

mod_ML[[12]]  <- update(mod[[12]], method = "ML")

anova(mod_ML[[12]], mod_ML[[11]])
anova(mod[[12]])

anova(mod_ML[[9]], mod_ML[[12]])

# Stick with model 12, all significant



# Look at model

intervals(mod[[12]])
anova(mod[[12]])
VarCorr(mod[[12]])
summary(mod[[12]])

# assumptions
plot(mod[[12]], flight.type ~ resid(.), abline = 0)

plot(mod[[12]], resid(., type = "p") ~ fitted(.) | device_info_serial, id = 0.05, adj = -0.3)

plot(mod[[12]], va ~ fitted(.), id = 0.05, adj = -0.3)

qqnorm(mod[[12]], ~resid(.)| flight.type)
qqnorm(mod[[12]], ~resid(.)| device_info_serial)


plot(mod[[12]])
qqnorm(mod[[12]])


library(MuMIn)
r.squaredGLMM(mod[[12]])

# Explains 34% of variation in altitude, with 30% down to main effects, other 3% random effects



# Straightness -----
hist(flights.combined$straigtness)
hist(logit(flights.combined$straigtness))


f <-  !is.na(flights.combined$wind_side_mean)
summary(f)
# names(flights.combined)
# hist(flights.combined$head_speed_mean)

f2 <- flights.combined$trip_id %in% flights.combined$trip_id[!f]
f3 <- !f2
summary(f3)



# Prepare variables
device_info_serial <- flights.combined$device_info_serial[f3]
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
summary(device_info_serial)

trip_id <- trip <- as.factor(flights.combined$trip_id[f3])

flight.type <- as.factor(flights.combined$flight.type[f3])

date.time <- flights.combined$start_time[f3]

# Temperature at 2m level
temp_2m <- flights.combined$temperature_2mmean[f3]  -273.15
# hist(temp_2m)

# Total atomospheric cloud cover
cloud_total <- flights.combined$cloud_cover_totalmean[f3]
hist(cloud_total)
hist(logit(cloud_total))
cloud_total_logit <- logit(cloud_total)

# Low level cloud (may reflect atmospheric visibility better)
cloud_low <- flights.combined$cloud_cover_low_altitudemean[f3]
hist(cloud_low)
cloud_low_logit <- sapply(cloud_low,logit)
hist(cloud_low_logit)

# Wind data
side.type <- as.factor(sapply(flights.combined$wind_side_mean_10[f3], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean_10[f3], if.neg))
head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f3])
side_abs <- abs(flights.combined$wind_side_mean_10[f3])

head_tail_vec <- (flights.combined$wind_head_tail_mean_10[f3])
# hist(head_tail_vec)
side_vec <- (flights.combined$wind_side_mean_10[f3])
# hist(side_vec)

# Flight distance
distance <- flights.combined$dist_a_b[f3]/1000
hist(distance/1000)  # distances in km

sea_level_pressure <- flights.combined$sea_level_pressuremean[f3] - 99000
# min(sea_level_pressure)




straight <- (flights.combined$straigtness[f3])
straight_logit <-  (logit(flights.combined$straigtness[f3]))



# Compile variables into single dataframe
s_data <- cbind(device_info_serial, trip_id, flight.type,
                 date.time, temp_2m,
                 straight, straight_logit, cloud_total,
                 cloud_total_logit, cloud_low,
                 cloud_low_logit, side_abs,
                 side.type, head_tail.type,
                 head_tail_abs, head_tail_vec, side_vec,
                 distance, sea_level_pressure)
s_data <- as.data.frame(s_data)

str(s_data)
s_data$device_info_serial <- as.factor(s_data$device_info_serial)
s_data$trip_id <- as.factor(s_data$trip_id)
s_data$flight.type  <- as.factor(s_data$flight.type)
s_data$side.type   <- as.factor(s_data$side.type)
s_data$head_tail.type   <- as.factor(s_data$head_tail.type)



# Statistics
library(nlme)

mod <- list()
mod_ml <- list()

# full model with all 2-way interactions included
mod[[1]]  <-  lme(straight_logit ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_vec + side_abs +
                       distance + sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = s_data
)
mod_ml[[1]] <- update(mod[[1]], method = "ML")
summary(mod[[1]])


mod[[2]]  <-  lme(straight_logit ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_abs + side_abs +
                       distance + sea_level_pressure +
                       head_tail.type)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = s_data
)
mod_ml[[2]] <- update(mod[[2]], method = "ML")
summary(mod[[2]])

anova(mod_ML[[1]], mod_ML[[2]])

# Better to keep Va as vector, not sepperate head/ tail

mod[[3]] <-  update(mod[[1]], random = ~1|device_info_serial)
anova(mod[[1]], mod[[3]])

# Better not to include trip id in random effects

# May be worth looking at correlation structure later
plot(ACF(mod[[3]], maxLag = 10), alpha = 0.01)


# Now simplify - only retain significant terms

anova(mod_ML[[3]])

mod[[4]]    <-  update(mod[[3]], .~. -cloud_total:sea_level_pressure - cloud_low_logit:distance - temp_2m:sea_level_pressure)

mod_ML[[4]]  <- update(mod[[4]], method = "ML")

anova(mod[[4]])


mod[[5]]    <-  update(mod[[4]], .~. -side_abs:sea_level_pressure - head_tail_vec:distance - cloud_total:distance - cloud_total:head_tail_vec - temp_2m:cloud_total)

mod_ML[[5]]  <- update(mod[[5]], method = "ML")

anova(mod[[5]])


mod[[6]]    <-  update(mod[[5]], .~. -cloud_total:cloud_low_logit - temp_2m:head_tail_vec - flight.type:distance - flight.type:cloud_low_logit)

mod_ML[[6]]  <- update(mod[[6]], method = "ML")

anova(mod_ML[[5]], mod_ML[[6]])

anova(mod[[6]])


mod[[7]]    <-  update(mod[[6]], .~. -head_tail_vec:sea_level_pressure - cloud_total:side_abs - flight.type:side_abs - flight.type:temp_2m)

mod_ML[[7]]  <- update(mod[[7]], method = "ML")

anova(mod_ML[[7]], mod_ML[[6]])

anova(mod[[7]])



mod[[8]]    <-  update(mod[[7]], .~. -temp_2m:side_abs)

mod_ML[[8]]  <- update(mod[[8]], method = "ML")

anova(mod_ML[[8]], mod_ML[[7]])

anova(mod[[8]])

mod[[9]]    <-  update(mod[[8]], .~. -cloud_low_logit:head_tail_vec)

mod_ML[[9]]  <- update(mod[[9]], method = "ML")

anova(mod_ML[[9]], mod_ML[[8]])

anova(mod[[9]])


mod[[10]]    <-  update(mod[[9]], .~. -distance:sea_level_pressure)

mod_ML[[10]]  <- update(mod[[10]], method = "ML")

anova(mod_ML[[10]], mod_ML[[9]])

anova(mod[[10]])


mod[[11]]    <-  update(mod[[10]], .~. -temp_2m:distance - temp_2m:cloud_low_logit - temp_2m)

mod_ML[[11]]  <- update(mod[[11]], method = "ML")

anova(mod_ML[[11]], mod_ML[[9]])

anova(mod[[11]])


mod[[12]]    <-  update(mod[[11]], .~. -cloud_low_logit:sea_level_pressure)

mod_ML[[12]]  <- update(mod[[12]], method = "ML")

anova(mod_ML[[12]], mod_ML[[11]])

anova(mod[[12]])


mod[[13]]    <-  update(mod[[12]], .~. -cloud_total - flight.type:cloud_total)

mod_ML[[13]]  <- update(mod[[13]], method = "ML")

anova(mod_ML[[13]], mod_ML[[11]])

anova(mod[[13]])



mod[[14]]    <-  update(mod[[13]], .~. -flight.type:head_tail_vec)

mod_ML[[14]]  <- update(mod[[14]], method = "ML")

anova(mod_ML[[14]], mod_ML[[11]])

anova(mod[[14]])


mod[[15]]    <-  update(mod[[14]], .~. -flight.type:sea_level_pressure - sea_level_pressure)

mod_ML[[15]]  <- update(mod[[15]], method = "ML")

anova(mod_ML[[15]], mod_ML[[11]])

anova(mod[[15]])


mod[[16]]    <-  update(mod[[15]], .~. -head_tail_vec:side_abs)

mod_ML[[16]]  <- update(mod[[16]], method = "ML")

anova(mod_ML[[16]], mod_ML[[11]])

anova(mod[[16]])



# Look at model

intervals(mod[[16]])
anova(mod[[16]])
VarCorr(mod[[16]])
summary(mod[[16]])

# assumptions
plot(mod[[16]], flight.type ~ resid(.), abline = 0)

plot(mod[[16]], resid(., type = "p") ~ fitted(.) | device_info_serial, id = 0.05, adj = -0.3)

plot(mod[[16]], straight_logit ~ fitted(.), id = 0.05, adj = -0.3)

qqnorm(mod[[16]], ~resid(.)| flight.type)
qqnorm(mod[[16]], ~resid(.)| device_info_serial)


plot(mod[[16]])
qqnorm(mod[[16]])


library(MuMIn)
r.squaredGLMM(mod[[16]])

# Explains 18% of variation in altitude, with 17% down to main effects, other 1% random effects





# Straightness - other measure (angles thing) ------
hist(flights.combined$ground_dir_rho)
hist(logit(flights.combined$ground_dir_rho))
names(flights.combined)

f <-  !is.na(flights.combined$wind_side_mean) & (flights.combined$ground_dir_rho > 0.5) 
summary(f)
# names(flights.combined)
# hist(flights.combined$head_speed_mean)

f2 <- flights.combined$trip_id %in% flights.combined$trip_id[!f]
f3 <- !f2
f4 <- resids.17 < 10
summary(f3)



# Prepare variables
device_info_serial <- flights.combined$device_info_serial[f3]
device_info_serial[device_info_serial == 687] <- 596
device_info_serial <- as.factor(device_info_serial)
summary(device_info_serial)

trip_id <- trip <- as.factor(flights.combined$trip_id[f3])

flight.type <- as.factor(flights.combined$flight.type[f3])

date.time <- flights.combined$start_time[f3]

# Temperature at 2m level
temp_2m <- flights.combined$temperature_2mmean[f3]  -273.15
# hist(temp_2m)

# Total atomospheric cloud cover
cloud_total <- flights.combined$cloud_cover_totalmean[f3]
hist(cloud_total)
hist(logit(cloud_total))
cloud_total_logit <- logit(cloud_total)

# Low level cloud (may reflect atmospheric visibility better)
cloud_low <- flights.combined$cloud_cover_low_altitudemean[f3]
hist(cloud_low)
cloud_low_logit <- sapply(cloud_low,logit)
hist(cloud_low_logit)

# Wind data
side.type <- as.factor(sapply(flights.combined$wind_side_mean_10[f3], if.neg))
head_tail.type <- as.factor(sapply(flights.combined$wind_head_tail_mean_10[f3], if.neg))
head_tail_abs <- abs(flights.combined$wind_head_tail_mean_10[f3])
side_abs <- abs(flights.combined$wind_side_mean_10[f3])

head_tail_vec <- (flights.combined$wind_head_tail_mean_10[f3])
# hist(head_tail_vec)
side_vec <- (flights.combined$wind_side_mean_10[f3])
# hist(side_vec)

# Flight distance
distance <- flights.combined$dist_a_b[f3]/1000
hist(distance/1000)  # distances in km

sea_level_pressure <- flights.combined$sea_level_pressuremean[f3] - 99000
# min(sea_level_pressure)

hist(flights.combined$ground_dir_rho)
hist(logit(flights.combined$ground_dir_rho))


straight <- (flights.combined$ground_dir_rho[f3])
straight_logit <-  (logit(flights.combined$ground_dir_rho[f3]))



# Compile variables into single dataframe
s_data <- cbind(device_info_serial, trip_id, flight.type,
                date.time, temp_2m,
                straight, straight_logit, cloud_total,
                cloud_total_logit, cloud_low,
                cloud_low_logit, side_abs,
                side.type, head_tail.type,
                head_tail_abs, head_tail_vec, side_vec,
                distance, sea_level_pressure)
s_data <- as.data.frame(s_data)

str(s_data)
s_data$device_info_serial <- as.factor(s_data$device_info_serial)
s_data$trip_id <- as.factor(s_data$trip_id)
s_data$flight.type  <- as.factor(s_data$flight.type)
s_data$side.type   <- as.factor(s_data$side.type)
s_data$head_tail.type   <- as.factor(s_data$head_tail.type)

s_data <- s_data[f4,]

# Statistics
library(nlme)

mod <- list()
mod_ml <- list()

# full model with all 2-way interactions included
mod[[1]]  <-  lme(straight_logit ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_vec + side_abs +
                       distance + sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = s_data
)
mod_ml[[1]] <- update(mod[[1]], method = "ML")
summary(mod[[1]])


mod[[2]]  <-  lme(straight_logit ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_abs + side_abs +
                       distance + sea_level_pressure +
                       head_tail.type)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = s_data
)
mod_ml[[2]] <- update(mod[[2]], method = "ML")
summary(mod[[2]])

anova(mod_ML[[1]], mod_ML[[2]])

# Better to keep Va as vector, not sepperate head/ tail

mod[[3]] <-  update(mod[[1]], random = ~1|device_info_serial)
anova(mod[[1]], mod[[3]])

# Better not to include trip id in random effects

# May be worth looking at correlation structure later
plot(ACF(mod[[3]], maxLag = 10), alpha = 0.01)


# Now simplify - only retain significant terms

anova(mod_ML[[3]])

mod[[4]]    <-  update(mod[[3]], .~. -cloud_low_logit:distance - cloud_total:sea_level_pressure - flight.type:head_tail_vec)

mod_ML[[4]]  <- update(mod[[4]], method = "ML")

anova(mod[[4]])


mod[[5]]    <-  update(mod[[4]], .~. -cloud_total:distance - cloud_total:head_tail_vec -temp_2m:side_abs)

mod_ML[[5]]  <- update(mod[[5]], method = "ML")

anova(mod[[5]])



mod[[6]]    <-  update(mod[[5]], .~. -flight.type:cloud_low_logit - flight.type:distance - cloud_low_logit:head_tail_vec)

mod_ML[[6]]  <- update(mod[[6]], method = "ML")

anova(mod[[6]])


mod[[7]]    <-  update(mod[[6]], .~. -head_tail_vec:sea_level_pressure - flight.type:temp_2m)

mod_ML[[7]]  <- update(mod[[7]], method = "ML")

anova(mod[[7]])



mod[[8]]    <-  update(mod[[7]], .~. -head_tail_vec:side_abs - temp_2m:cloud_low_logit)

mod_ML[[8]]  <- update(mod[[8]], method = "ML")

anova(mod_ML[[7]], mod_ML[[8]])
anova(mod[[8]])


mod[[9]]    <-  update(mod[[8]], .~. -temp_2m:sea_level_pressure - distance:sea_level_pressure)

mod_ML[[9]]  <- update(mod[[9]], method = "ML")

anova(mod_ML[[8]], mod_ML[[9]])
anova(mod[[9]])



mod[[10]]    <-  update(mod[[9]], .~. -temp_2m:cloud_total - flight.type:sea_level_pressure)

mod_ML[[10]]  <- update(mod[[10]], method = "ML")

anova(mod_ML[[9]], mod_ML[[10]])
anova(mod[[10]])



mod[[11]]    <-  update(mod[[10]], .~. -cloud_low_logit:side_abs - cloud_total:side_abs - temp_2m:distance)

mod_ML[[11]]  <- update(mod[[11]], method = "ML")

anova(mod_ML[[10]], mod_ML[[11]])
anova(mod[[11]])



mod[[12]]    <-  update(mod[[11]], .~. -cloud_low_logit:sea_level_pressure)

mod_ML[[12]]  <- update(mod[[12]], method = "ML")

anova(mod_ML[[11]], mod_ML[[12]])
anova(mod[[12]])


mod[[13]]    <-  update(mod[[12]], .~. -cloud_total:cloud_low_logit - flight.type:cloud_total)

mod_ML[[13]]  <- update(mod[[13]], method = "ML")

anova(mod_ML[[12]], mod_ML[[13]])
anova(mod[[13]])


mod[[14]]    <-  update(mod[[13]], .~. -cloud_total)

mod_ML[[14]]  <- update(mod[[14]], method = "ML")

anova(mod_ML[[12]], mod_ML[[14]])
anova(mod[[14]])



mod[[15]]    <-  update(mod[[14]], .~. -temp_2m:head_tail_vec)

mod_ML[[15]]  <- update(mod[[15]], method = "ML")

anova(mod_ML[[14]], mod_ML[[15]])
anova(mod[[15]])


mod[[16]]    <-  update(mod[[15]], .~. -side_abs:distance)

mod_ML[[16]]  <- update(mod[[16]], method = "ML")

anova(mod_ML[[15]], mod_ML[[16]])
anova(mod[[16]])


mod[[17]]    <-  update(mod[[16]], .~. -side_abs:sea_level_pressure - sea_level_pressure)

mod_ML[[17]]  <- update(mod[[17]], method = "ML")

anova(mod_ML[[15]], mod_ML[[17]])
anova(mod[[17]])


mod[[18]]    <-  update(mod[[17]], .~. -flight.type:side_abs)

mod_ML[[18]]  <- update(mod[[18]], method = "ML")

anova(mod_ML[[15]], mod_ML[[18]])
anova(mod[[18]])


# Look at model

intervals(mod[[17]])
anova(mod[[17]])
VarCorr(mod[[17]])
summary(mod[[17]])

# assumptions
plot(mod[[17]], flight.type ~ resid(.), abline = 0)

plot(mod[[17]], resid(., type = "p") ~ fitted(.) | device_info_serial, id = 0.05, adj = -0.3)

plot(mod[[17]], straight_logit ~ fitted(.), id = 0.05, adj = -0.3)

qqnorm(mod[[17]], ~resid(.)| flight.type)
qqnorm(mod[[17]], ~resid(.)| device_info_serial)


# resids.17 <- resid(mod[[17]])
# hist(resid(mod[[17]]))
plot(mod[[17]])
qqnorm(mod[[17]])


library(MuMIn)
r.squaredGLMM(mod[[17]])

# Explains 18% of variation in altitude, with 17% down to main effects, other 1% random effects
