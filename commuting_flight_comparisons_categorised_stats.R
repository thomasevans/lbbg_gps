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

# Make a filter to this effect
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
distance <- flights.combined$dist_a_b[f3]
hist(distance/1000)  # distances in km

sea_level_pressure <- flights.combined$sea_level_pressuremean[f3]
hist(sea_level_pressure)

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

# full model with all 2-way interactions included
mod[[1]]  <-  lme(alt_trans ~
                    (flight.type + temp_2m + cloud_total +
                    cloud_low_logit + head_tail_vec + side_abs +
                    distance + sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = alt_data
                  )
summary(mod[[1]])


modb  <-  lme(alt_trans ~
                    (flight.type + temp_2m + cloud_total +
                       cloud_low_logit + head_tail_abs + side_abs +
                       head_tail.type + distance +
                       sea_level_pressure)^2,
                  random = ~1|device_info_serial/trip_id,
                  data = alt_data
)

modb_ML <- update(modb, method = "ML")

anova(mod_ML[[1]], modb_ML)

# Compare different random effect structure
mod[[2]]  <-  update(mod[[1]], random = ~1|device_info_serial)

anova(mod[[1]], mod[[2]])

# Spatio-temporal correlation
plot(ACF(mod[[2]], maxLag = 10), alpha = 0.01)
# No signs of strong autocorrelation, probably owing to model overfitting

anova(mod[[2]])

mod_ML <- list()


mod_ML[[2]] <- update(mod[[2]], method = "ML")
mod_ML[[1]] <- update(mod[[1]], method = "ML")

# str(alt_data)

# Removing interaction terms

# Remove distance:sea_level_pressure
mod[[3]] <- update(mod[[1]], .~. -distance:sea_level_pressure)
mod_ML[[3]] <- update(mod[[3]], method = "ML")

anova(mod_ML[[2]], mod_ML[[3]])

anova(mod[[3]])

# Remove cloud_total:sea_level_pressure
mod[[4]] <- update(mod[[3]], .~. -cloud_total:sea_level_pressure)
mod_ML[[4]] <- update(mod[[4]], method = "ML")

anova(mod_ML[[3]], mod_ML[[4]])

anova(mod[[4]])

# Remove flight.type:head_tail_vec
mod[[5]] <- update(mod[[4]], .~. -flight.type:head_tail_vec)
mod_ML[[5]] <- update(mod[[5]], method = "ML")

anova(mod_ML[[4]], mod_ML[[5]])

anova(mod[[5]])


# Remove cloud_low_logit:distance
mod[[6]] <- update(mod[[5]], .~. -cloud_low_logit:distance)
mod_ML[[6]] <- update(mod[[6]], method = "ML")

anova(mod_ML[[6]], mod_ML[[5]])

anova(mod[[6]])


# Remove temp_2m:cloud_total
mod[[7]] <- update(mod[[6]], .~. -temp_2m:cloud_total)
mod_ML[[7]] <- update(mod[[7]], method = "ML")

anova(mod_ML[[6]], mod_ML[[7]])

anova(mod[[7]])


# Remove temp_2m:sea_level_pressure
mod[[8]] <- update(mod[[7]], .~. -temp_2m:sea_level_pressure)
mod_ML[[8]] <- update(mod[[8]], method = "ML")

anova(mod_ML[[7]], mod_ML[[8]])

anova(mod[[8]])


# Remove flight.type:distance  
mod[[9]] <- update(mod[[8]], .~. -flight.type:distance  )
mod_ML[[9]] <- update(mod[[9]], method = "ML")

anova(mod_ML[[8]], mod_ML[[9]])

anova(mod[[9]])


# Remove temp_2m:distance   
mod[[10]] <- update(mod[[9]], .~. -temp_2m:distance)
mod_ML[[10]] <- update(mod[[10]], method = "ML")

anova(mod_ML[[10]], mod_ML[[9]])

anova(mod[[10]])


# Remove side_abs:distance  
mod[[11]] <- update(mod[[10]], .~. -side_abs:distance)
mod_ML[[11]] <- update(mod[[11]], method = "ML")

anova(mod_ML[[11]], mod_ML[[10]])

anova(mod[[11]])


# Remove flight.type:temp_2m   
mod[[12]] <- update(mod[[11]], .~. -flight.type:temp_2m )
mod_ML[[12]] <- update(mod[[12]], method = "ML")

anova(mod_ML[[12]], mod_ML[[11]])

anova(mod[[12]])


# Remove flight.type:sea_level_pressure    
mod[[13]] <- update(mod[[12]], .~. -flight.type:sea_level_pressure  )
mod_ML[[13]] <- update(mod[[13]], method = "ML")

anova(mod_ML[[13]], mod_ML[[12]])

anova(mod[[13]])


# Remove temp_2m:cloud_low_logit     
mod[[14]] <- update(mod[[13]], .~. -temp_2m:cloud_low_logit   )
mod_ML[[14]] <- update(mod[[14]], method = "ML")

anova(mod_ML[[14]], mod_ML[[13]])

anova(mod[[14]])


# Remove cloud_total:distance     
mod[[15]] <- update(mod[[14]], .~. -cloud_total:distance)
mod_ML[[15]] <- update(mod[[15]], method = "ML")

anova(mod_ML[[15]], mod_ML[[14]])

anova(mod[[15]])


# Remove cloud_low_logit:sea_level_pressure    
mod[[16]] <- update(mod[[15]], .~. -cloud_low_logit:sea_level_pressure)
mod_ML[[16]] <- update(mod[[16]], method = "ML")

anova(mod_ML[[16]], mod_ML[[15]])

anova(mod[[16]])

# Remove side_abs:sea_level_pressure    
mod[[17]] <- update(mod[[16]], .~. -side_abs:sea_level_pressure)
mod_ML[[17]] <- update(mod[[17]], method = "ML")

anova(mod_ML[[17]], mod_ML[[16]])

anova(mod[[17]])


# Remove cloud_low_logit:head_tail_vec   
mod[[18]] <- update(mod[[17]], .~. -cloud_low_logit:head_tail_vec)
mod_ML[[18]] <- update(mod[[18]], method = "ML")

anova(mod_ML[[18]], mod_ML[[17]])

anova(mod[[18]])

# Remove head_tail_vec:distance   
mod[[19]] <- update(mod[[18]], .~. -head_tail_vec:distance)
mod_ML[[19]] <- update(mod[[19]], method = "ML")

anova(mod_ML[[19]], mod_ML[[18]])

anova(mod[[19]])


# Remove temp_2m:head_tail_vec   
mod[[20]] <- update(mod[[19]], .~. -temp_2m:head_tail_vec)
mod_ML[[20]] <- update(mod[[20]], method = "ML")

anova(mod_ML[[20]], mod_ML[[19]])

anova(mod[[20]])


# Remove temp_2m:head_tail_vec   
mod[[21]] <- update(mod[[19]], .~. -temp_2m:head_tail_vec)
mod_ML[[21]] <- update(mod[[21]], method = "ML")

anova(mod_ML[[21]], mod_ML[[19]])


# Remove temp_2m:head_tail_vec   
mod[[22]] <- update(mod[[19]], .~. -head_tail_vec:sea_level_pressure - sea_level_pressure)
mod_ML[[22]] <- update(mod[[22]], method = "ML")

anova(mod_ML[[22]], mod_ML[[19]])

anova(mod[[22]])



# Remove temp_2m:head_tail_vec   
mod[[23]] <- update(mod[[22]], .~. -temp_2m:head_tail_vec)
mod_ML[[23]] <- update(mod[[23]], method = "ML")

anova(mod_ML[[23]], mod_ML[[22]])

anova(mod[[23]])


# Remove temp_2m:head_tail_vec   
mod[[24]] <- update(mod[[22]], .~. -cloud_total:cloud_low_logit)
mod_ML[[24]] <- update(mod[[24]], method = "ML")

anova(mod_ML[[24]], mod_ML[[22]])


# Remove temp_2m:head_tail_vec   
mod[[25]] <- update(mod[[22]], .~. -temp_2m:head_tail_vec)
mod_ML[[25]] <- update(mod[[25]], method = "ML")

anova(mod_ML[[25]], mod_ML[[22]])

anova(mod[[25]])



# Re-test random effects
mod[[26]] <- update(mod[[22]], random = ~1|device_info_serial)

anova(mod[[22]], mod[[26]])

summary(mod[[25]])
intervals(mod[[25]])





