#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

# Alternative working directory for when also running another script from same directory.
# setwd("D:/Dropbox/R_projects/lbbg_gps/workspace_alternative")


#Description#######
#In this script we produce various figures and summary statistics to compare outward and inward commuting flights.
#These were originally prepared for a meeting with Susanne and Anders on 2013-01-17


#Datbase functions#########
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

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





# Logit transformation   ####
logit <- function(x){
  if(x <1.000000000000001 & x > -0.00000000000001){   #Return NAs for values outside of range 0 - 1, with small tollerance either way.
    x.new <- 0.99999999999*x    #Bring all values in slightly, so that values of 1.0 can be processed.
    x.new[x.new == 0] <- 0.0000000000001
    x.logit <- log(x.new/(1-x.new))
    return(x.logit)}
  else return(NA)
}


anti.logit <- function(x){
 ex <- exp(x)
 fx <- ex/(ex + 1)
 return(fx)
}


# sample sizes ####
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
hist(flights$rho[outward],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", ylim = c(0,325), xlim = c(0,1), main = "Outward")
hist(flights$rho[inward] ,xlab = "Straightness", las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", ylim = c(0,325), xlim = c(0,1), main = "Inward")
dev.off()


sd(flights$rho[inward])
sd(flights$rho[outward])
t.test(flights$rho[outward], flights$rho[inward])
wilcox.test(flights$rho[outward], flights$rho[inward])
t.test(flights.in$rho,flights.out$rho, paired = TRUE)




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

#Graphing non-transformned data
par(mfrow = c(2,3))
hist(flights.out$rho[flights.out$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = 10, main = "Out - tail")
hist(flights.out$rho[flights.out$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = 10, main = "Out - head")
hist(flights.out$rho[flights.out$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(0,1), breaks = 10, main = "Out - side")
hist(flights.in$rho[flights.in$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = 10,main = "In - tail")
hist(flights.in$rho[flights.in$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = 10,main = "In - head")
hist(flights.in$rho[flights.in$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(0,1), breaks = 10,  main = "In - side")


#logit transform data first
flights.out.rho <- sapply(flights.out$rho, FUN = logit)
hist(flights.out.rho)
flights.in.rho <- sapply(flights.in$rho, FUN = logit)
hist(flights.in.rho)



flights.combined.rho <- sapply(flights.combined$rho, FUN = logit)


par(mfrow = c(2,3))
hist(flights.out.rho[flights.out$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = "scott", main = "Out - tail")
hist(flights.out.rho[flights.out$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = "scott", main = "Out - head")
hist(flights.out.rho[flights.out$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue",xlim = c(-4,8), breaks = "scott", main = "Out - side")
hist(flights.in.rho[flights.in$wind.type == "tail"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = "scott",main = "In - tail")
hist(flights.in.rho[flights.in$wind.type == "head"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = "scott",main = "In - head")
hist(flights.in.rho[flights.in$wind.type == "side"],xlab = "Straightness",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red",xlim = c(-4,8), breaks = "scott",  main = "In - side")

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


# Full factorial model
mod01 <- lme(flights.combined.rho ~ flight.type * wind.type, random = ~1|device_info_serial)
summary(mod01)
anova(mod01)

# Temporal autocorrelation structure
?ACF
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
mod02_t6   <- lme(flights.combined.rho ~ flight.type + wind.type, random = ~1|device_info_serial,correlation = corARMA(q=6))
summary(mod02_t6)

mod03_t6   <- lme(flights.combined.rho ~ flight.type , random = ~1|device_info_serial,correlation = corARMA(q=6))
summary(mod03_t6)

# Null model, no fixed effects, only random effects
mod04_t6   <- lme(flights.combined.rho ~ 1 , random = ~1|device_info_serial,correlation = corARMA(q=6))
summary(mod04_t6)


# AIC comparison REML models fitting
anova(mod01_t6,mod02_t6,mod03_t6,mod04_t6)
# Model 3 has lowest AIC value

# Refitting models to compare via maximum liklihood, allowing comparison when fixed effects differ.
mod01_t6_ML <- update(mod01_t6, method="ML")
mod02_t6_ML <- update(mod02_t6, method="ML")
mod03_t6_ML <- update(mod03_t6, method="ML")
mod04_t6_ML <- update(mod04_t6, method="ML")

anova(mod01_t6_ML,mod02_t6_ML,mod03_t6_ML,mod04_t6_ML)
anova(mod02_t6_ML,mod03_t6_ML)
anova(mod03_t6_ML,mod04_t6_ML)

# Final model fitted by REML
summary(mod03_t6)
anova(mod03_t6)


summary(mod03_t6_ML)
anova(mod03_t6_ML)

anti.logit(1.9237288) - anti.logit(1.9237288-0.3189052)



#Checking final model assumptions
plot(mod03_t6,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod03_t6,~resid(.)|device_info_serial)
plot(mod03_t6)

plot( ACF(mod03_t6, maxLag = 50), alpha = 0.01)
plot( ACF(mod03_t6, maxLag = 50, resType = "n"), alpha = 0.01)





cloud <- flights.combined$tcdceatm

mod04_cl_t2   <- lme(flights.combined.rho ~ flight.type*cloud, random = ~1|device_info_serial, correlation = corARMA(q=6))
mod04_cl_t2_ML <- update(mod04_cl_t2, method="ML")

summary(mod04_cl_t2)
anova(mod03_t6_ML,mod04_cl_t2_ML)
anova(mod03_t6,mod04_cl_t2)

mod04_cl_t2
plot(mod04_cl_t2)
plot( ACF(mod04_cl_t2, maxLag = 50, resType = "n"), alpha = 0.01)

plot(mod04_cl_t2,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod04_cl_t2,~resid(.)|device_info_serial)

rsquared.glmm(list(mod04_t2_ML,mod04_cl_t2))

library(MuMIn)
r.squaredGLMM(mod04_cl_t2)
r.squaredGLMM(mod03_t6)
















# Get means and SD for different categories
aggregate(flights.combined.rho ~ flight.type + wind.type, data =flights.combined, FUN=mean)
aggregate(flights.combined.rho ~ flight.type + wind.type, data =flights.combined, FUN=sd)


aggregate(flights.combined.rho ~  flight.type, data =flights.combined, FUN=mean)
aggregate(flights.combined.rho ~  flight.type, data =flights.combined, FUN=sd)







# str(flights.combined)
# hist(flights.combined$tcdceatm)
# Altitude - wind condition ####
# Just include outward and inward flights:
# flights.combined

# Inspect lowest values.
sort(flights.combined$alt_med)[1:100]

# Inspect highest values.
rev(sort(flights.combined$alt_med))[1:100]

# Maybe sensible to exclude values > 100 and < -10 - outlying values potentially leading from GPS errors
# Make a filter to this effect
f <- flights.combined$alt_med < 100  &   flights.combined$alt_med > -10

# Transformation
# Strong right-hand skew
hist(flights.combined$alt_med[f])
# log - too strong, now left hand skew
hist(log(flights.combined$alt_med[f]))
# square-root - better
hist(sqrt(flights.combined$alt_med[f] + 10))
# cubed-root - best apparently, also close to Boxcox recomendation
hist((flights.combined$alt_med[f]+10)^(1/3))


library(MASS)
# intercept only model for testing here - ideally do on final model though
mod <- lm((flights.combined$alt_med[f]+10) ~ 1)
summary(mod)
boxcox(mod, lambda = seq(.1, 0.45, len = 20))

# Transformed vairable for further analysis
alt_trans <- (flights.combined$alt_med[f]+10)^(1/3)


names(flights.combined)



# Histograms to show data - transformed
par(mfrow = c(2,3))
hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "out"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "out"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "out"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - head")

hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "in"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "in"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "in"]+10)^(1/3), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - head")


#Histograms of non-transformed data

par(mfrow = c(2,3))
hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "out"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "blue", breaks = "scott", main = "Out - head")

hist((flights.combined$alt_med[f & flights.combined$wind.type == "tail" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - tail")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "side" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - side")
hist((flights.combined$alt_med[f & flights.combined$wind.type == "head" & flights.combined$flight.type == "in"]), ,xlab = "Altitude (trans)",las=1, cex.axis = 1.0, cex.lab = 1.1, col = "red", breaks = "scott", main = "In - head")





# library(lme4)
library(nlme)

flight.type <- as.factor(flights.combined$flight.type[f])
wind.type <- as.factor(flights.combined$wind.type[f])
device_info_serial <- device_info_serial[f]
length(device_info_serial)
length(wind.type)

# Full factorial model
mod01 <- lme(alt_trans ~ flight.type * wind.type, random = ~1|device_info_serial)
summary(mod01)
anova(mod01)

# Temporal autocorrelation structure
plot(ACF(mod01, maxLag = 50),alpha=0.01)
mod01_t2 <- update(mod01,correlation = corARMA(q=2))
mod01_t3 <- update(mod01,correlation = corARMA(q=3))
mod01_t4 <- update(mod01,correlation = corARMA(q=4))
mod01_t4 <- update(mod01,correlation = corARMA(q=4))

# Comparing autocorrelation levels
anova(mod01,mod01_t2,mod01_t3,mod01_t4)
# select lag 2 for correlation structure, lowest AIC, lower than no autocorrelation function


# Model simplification
mod02_t2   <- lme(alt_trans ~ flight.type * wind.type, random = ~1|device_info_serial,correlation = corARMA(q=2))
summary(mod02_t2)

mod03_t2   <- lme(alt_trans ~ flight.type + wind.type, random = ~1|device_info_serial,correlation = corARMA(q=2))
summary(mod03_t2)


mod04_t2   <- lme(alt_trans ~  wind.type, random = ~1|device_info_serial,correlation = corARMA(q=2))
summary(mod04_t2)

mod05_t2   <- lme(alt_trans ~ flight.type , random = ~1|device_info_serial,correlation = corARMA(q=2))
summary(mod05_t2)

# Null model, no fixed effects, only random effects
mod06_t2   <- lme(alt_trans ~ 1 , random = ~1|device_info_serial,correlation = corARMA(q=2))
summary(mod06_t2)

# Compare models
anova(mod02_t2,mod03_t2,mod04_t2,mod05_t2,mod06_t2)

# Lowest AIC (and BIC) is given by mod04_t2, which is wind type only (not flight type).



# Refitting models to compare via maximum liklihood, allowing comparison when fixed effects differ.
mod02_t2_ML <- update(mod02_t2, method="ML")
mod03_t2_ML <- update(mod03_t2, method="ML")
mod04_t2_ML <- update(mod04_t2, method="ML")
mod05_t2_ML <- update(mod05_t2, method="ML")
mod06_t2_ML <- update(mod06_t2, method="ML")

# Flight type NS, p = 0.66
summary(anova(mod04_t2_ML,mod03_t2_ML))

# Wind type hight significant, p < 0.001 for null model vs model including wind type
anova(mod04_t2_ML,mod06_t2_ML)

#Final model fitted via REML
summary(mod04_t2)


#Checking final model assumptions
plot(mod04_t2_ML,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod04_t2_ML,~resid(.)|device_info_serial)
plot(mod04_t2_ML)

plot( ACF(mod04_t2_ML, maxLag = 50), alpha = 0.01)
plot( ACF(mod04_t2_ML, maxLag = 50, resType = "n"), alpha = 0.01)

#Backtransform coefficients
a <- (2.9159989 ^3) + 10
b <- ((2.9159989 + 0.4634817)  ^3) + 10

b - a
mod04_t2_ML





cloud <- flights.combined$tcdceatm[f]
mod04_cl_t2   <- lme(alt_trans ~  wind.type*cloud, random = ~1|device_info_serial,correlation = corARMA(q=2))
mod04_cl_t2_ML <- update(mod04_cl_t2, method="ML")

summary(mod04_cl_t2)
anova(mod04_t2_ML,mod04_cl_t2_ML)
anova(mod04_t2,mod04_cl_t2)

mod04_cl_t2
plot(mod04_cl_t2)
plot( ACF(mod04_cl_t2, maxLag = 50, resType = "n"), alpha = 0.01)

plot(mod04_cl_t2,resid(.,type="p")~fitted(.)|device_info_serial)
qqnorm(mod04_cl_t2,~resid(.)|device_info_serial)

rsquared.glmm(list(mod04_t2_ML,mod04_cl_t2))

library(MuMIn)
r.squaredGLMM(mod04_cl_t2)
r.squaredGLMM(mod04_t2)


# Straightness - r - wind condition ####


# LMMs of straightness - r - windcondition ####

flights.combined.r <- sapply(flights.combined$straigtness, FUN = logit)
hist(flights.combined.r)

par(mfrow=c(1,1))
r_straight <- sapply(flights.combined$straigtness, FUN = logit)
r_rho <-  sapply(flights.combined$rho, FUN = logit)
plot(flights.combined$straigtness~flights.combined$rho, ylim = c(0,1))
abline(lm(flights.combined$straigtness~flights.combined$rho))
plot(r_straight~r_rho)
abline(lm(r_straight~r_rho))

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


# vvv OLD things  vvv #####

# install.packages("psych")
library(psych)
x1 <- geometric.mean(flights.combined$straigtness[cloud.low])
geometric.mean(flights.combined$straigtness[cloud.low])
mean(flights.combined$straigtness[cloud.low])

names(flights.weather)


# Angle with respect to wind - appears that inward flights more often have head-winds and outward flight more tail-winds.
t.test(flights.in$dif.angle,flights.out$dif.angle, paired = TRUE)
hist(flights.in$dif.angle)
hist(flights.out$dif.angle)

