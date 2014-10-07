# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# For commuting fligths calculate airspeed, heading etc...


# Get data from database -----

# Connect to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

# Get Flight data
#' Get all 'lund_flights_commuting' and 'lund_flights_commuting_par'.
flights <- sqlQuery(
  gps.db, query =
      "SELECT DISTINCT lf.*, lfc.*
       FROM lund_flights_commuting AS lf, lund_flights_commuting_par AS lfc
       WHERE lf.flight_id = lfc.flight_id
       ORDER BY lf.flight_id ASC;"
  , as.is = TRUE)


# flights <- flights[1:10,]

# Get GPS point data in two steps
# First get GPS points and paramaters
source("gps_extract.R")
# ?file


# c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
# cat("abc\ndef\n", file = Tfile)
# readLines(Tfile)
# seek(Tfile, 0, rw = "r") # reset to beginning
# readLines(Tfile)
# cat("ghi\n", file = Tfile)
# readLines(Tfile)
# close(Tfile)

# flight_id <- flights$flight_id[10]
# Function to get GPS points for each flight and add a column for flight_id
gps.wrap <- function(flight_id, flights){
#   ?readLines
#   readLines(Tfile)
  cat(paste(flight_id, "\n"), file = Tfile)
  idx <- flights$flight_id == flight_id
#   summary(idx)
  if(any(is.na(flights$device_info_serial[idx]),
         is.na(flights$start_time[idx]),
         is.na(flights$end_time[idx]))){x = NULL} else{
    x <- gps.extract(flights$device_info_serial[idx],
                     flights$start_time[idx],
                     flights$end_time[idx],
                     weather = TRUE, ECMWF = TRUE)
  }
 
  if(length(x[,1]) == 0) return(cbind(flight_id,rep(NA,51)))
  else return(cbind(flight_id,x))
#   return(list(n))
}

# ?sink

# For testing purposes only analyse first 100 flights
#  flights <- flights[1:10,]

# source("gps_extract.R")

# gps.extract(flights$device_info_serial[1], flights$start_time[1], flights$end_time[1], weather = TRUE, ECMWF = TRUE)

# Testing
#  gps.wrap(flights$flight_id[10], flights)
# flights[flights$flight_id == 12635,]
# 
# idx <- flights$flight_id == 12635
# if(any(is.na(flights$device_info_serial[idx]), is.na(flights$start_time[idx]), is.na(flights$end_time[idx])))
# x <- gps.extract(flights$device_info_serial[idx], flights$start_time[idx], flights$end_time[idx], weather = TRUE, ECMWF = TRUE)
# if(length(x[,1]) == 0) return(cbind(flight_id,rep(NA,51)))
# else return(cbind(flight_id,x))


# sink()
# ?sink
# 5+2
# Get the data (took ca. 1 h for <5000 flights - after had
# 'pooled' driver setting)
# 
Tfile <- file("progress", "w+")
# readLines(Tfile)
gps.data.list <- list()
system.time({
  gps.data.list <- lapply(X = flights$flight_id, gps.wrap, flights = flights)
})
close(Tfile)
save(gps.data.list, file = "gps.data.list.weather.ecmwf2.RData")

# load(file = "gps.data.list.weather.ecmwf2.RData")

# Determine which flights failed to return data
names(flights)
fn <- length(flights$flight_id)

x <- NULL
for(i in 1:fn){
  x[i] <- length(gps.data.list[[i]])
}

# summary(as.factor(x))

idx <- c(1:fn)

# gps.data.list.ok[[1]]

# Might need to change this
idx.wrong <- idx[x == 102]
# gps.data.list[[idx.wrong[1]]]
# gps.wrap(flights$flight_id[idx.wrong[1]], flights)

# x <- gps.extract(flights$device_info_serial[idx.wrong[1]],
#                  flights$start_time[idx.wrong[1]],
#                  flights$end_time[idx.wrong[1]],
#                  weather = FALSE)

# Remove list items for flights that didn't return data
gps.data.list.ok <- gps.data.list[(!(idx %in% idx.wrong))]


# Merge to dataframe
gps.data <- do.call( rbind , gps.data.list.ok)
# str(gps.data)
flights.ok <- flights[(flights$flight_id %in% unique(gps.data$flight_id)),]


# Clearn workspace so we only have the data we want
flights <- flights.ok
# rm(list = setdiff(ls(), c("flights","gps.data")))

# Close connection
odbcCloseAll()


# Calculations ----

# Dataframe of calculated paramaters ------------
gps.data.par <- cbind(gps.data[,1:3])
# str(wind.data)

# Add wind paramaters to dataframe
gps.data.par <- cbind(gps.data.par,
                      gps.data$wind_u_10m_ecmwf,
                      gps.data$wind_v_10m_ecmwf,
                      gps.data$wind_u_10m_flt_ht_ecmwf,
                      gps.data$wind_v_10m_flt_ht_ecmwf,
                      gps.data$wind_speed_flt_ht_ecmwf,
                      gps.data$wind_dir_ecmwf,
                      gps.data$wind_speed_10m_ecmwf)


# Direction from which wind is coming from (origin)
wind_origin_ecmwf <- ((gps.data$wind_dir_ecmwf + 180) %% 360)
# hist(wind_origin)
gps.data.par <- cbind(gps.data.par,wind_origin_ecmwf)


# Flight vector components ------------------------

#' Calculating heading vector (we already have the wind vector
#' and the track vector).
#' In principle this is simple vector addition.
#' 

# Calculating u and v component of heading vector
# Subtract wind vector from ground vector, leaving
# heading vector
# names(points)
# names(points.weather)
head_u_ecmwf <- gps.data$veast  - gps.data$wind_u_10m_flt_ht_ecmwf
head_v_ecmwf <- gps.data$vnorth - gps.data$wind_v_10m_flt_ht_ecmwf

source("wind_dir_speed.R")


head.info <- t(mapply(wind.dir.speed, head_u_ecmwf,
                      head_v_ecmwf))
# names(points.weather)
# Make dataframe
head.info <- as.data.frame(cbind(head.info, head_u_ecmwf, head_v_ecmwf))

# Give names to columns
names(head.info) <- c("head_speed_ecmwf", "head_dir_ecmwf", "head_u_ecmwf", "head_v_ecmwf")

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, head.info)

# Ground speed + track heading
ground_speed <- t(mapply(wind.dir.speed, gps.data$veast,
                         gps.data$vnorth))
ground_heading <- ground_speed[,2]
ground_speed   <- ground_speed[,1]

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, ground_speed, ground_heading)


# median(ground_speed[ground_speed < 50], na.rm = TRUE)
# median(gps.data.par$head_speed[gps.data.par$head_speed < 50], na.rm = TRUE)

# Heading track vs. ground heading, 0 - 180 indicates 'drift' to right
# Values 180 - 360 indicate 'drift' to left.
# hist((gps.data.par$ground_heading - gps.data.par$head_dir) %% 360)

names(gps.data.par) <- c("flight_id", "device_info_serial",
                         "date_time", "wind_u_10m_ecmwf",
                         "wind_v_10m_ecmwf",
                         "wind_u_10m_flt_ht_ecmwf",
                         "wind_v_10m_flt_ht_ecmwf",
                         "wind_speed_flt_ht_ecmwf",
                         "wind_dir_ecmwf", "wind_speed_10m_ecmwf",
                         "wind_origin_ecmwf", "head_speed_ecmwf",
                         "head_dir_ecmwf", "head_u_ecmwf",
                         "head_v_ecmwf", "ground_speed",
                         "ground_heading")



# Flight vectors - relative to track ----------

bear_cor <- function(x, track){
  out <- x - track
  out <- out %% 360
#   if(out > 180) return(360 - out) else return(out)
}

head_dir <-  mapply(bear_cor,
                 x = gps.data.par$head_dir_ecmwf,
                 track = gps.data.par$ground_heading)

str(gps.data.par)
wind_dir <-   mapply(bear_cor,
                      x = gps.data.par$wind_dir_ecmwf,
                      track = gps.data.par$ground_heading)



par <- cbind(wind_dir, gps.data.par$wind_speed_flt_ht_ecmwf,
             head_dir, gps.data.par$head_speed,
             gps.data.par$ground_speed)

par <- as.data.frame(par)
names(par) <- c("wind_dir","wind_speed","head_dir","head_speed","track_speed")

head(par)


# Function to get alpha or beta, if angle is reflex then subtract it from 360
ang.cor <- function(x){
  if(is.na(x)) return(x) else{
    if(x > 180) return(360 - x)
    else return(x)}
}

# alpha <- NULL
alpha <- sapply(X = par$head_dir, FUN = ang.cor)

beta <- sapply(X = par$wind_dir, FUN = ang.cor)

par <- cbind(par, alpha, beta)

# side and head wind components
wind.comp <- function(beta, wind_speed, wind_dir){
  if(is.na(beta) | is.na(wind_speed) | is.na(wind_dir)){
      return(c(NA,NA))} else {
        # Package needed to convert degrees to radians
        require(CircStats)
        # If beta angle is more than 90 do these calculations
        if(beta > 90){
          beta <- 180 - beta
          beta.rad <- rad(beta)
          
          wind_head_tail <- (cos(beta.rad))*wind_speed
          # As beta >90 wind must be tails wind, make negative
          wind_head_tail <- wind_head_tail * -1
          
          wind_side <- (sin(beta.rad))*wind_speed
          # If wind comes from left make negative
          if(wind_dir < 180)  wind_side <- wind_side * -1
        } else {
          beta.rad <- rad(beta)
          wind_head_tail <- (cos(beta.rad))*wind_speed
          wind_side <- (sin(beta.rad))*wind_speed
          if(wind_dir < 180)  wind_side <- wind_side * -1
        }
        # For testing, check that calculated side wind and head wind components would add up to original wind vector (i.e. wind speed)
      #   test_var <- sqrt((wind_side*wind_side) + (wind_head_tail*wind_head_tail))
      #   return(c(wind_side, wind_head_tail, test_var))
        return(c(wind_side, wind_head_tail))
  }
}

wind.comp_calc <- mapply(wind.comp,
                         beta = par$beta,
                         wind_speed = par$wind_speed,
                         wind_dir = par$wind_dir) 

wind.comp_calc_10 <- mapply(wind.comp,
                         beta = par$beta,
                         wind_speed = gps.data.par$wind_speed_10m_ecmwf,
                         wind_dir = par$wind_dir) 



names(gps.data)
names(gps.data.par)
alpha.new <- gps.data.par$ground_heading - gps.data.par$head_dir


alpha.calc <- function(head, track){
  if(is.na(track) | is.na(head)){
    theta = NA
    }else{
      theta <- track - head
      if(abs(theta) > 180){
        theta <- 360 - abs(theta)
      }
      theta <- abs(theta)
      if(head > track){
        theta <- theta*-1
      }
    }
  return(theta)  
}

alpha.new <- mapply(alpha.calc, gps.data.par$head_dir, 
                    gps.data.par$ground_heading)

# hist(alpha.new)
# min(alpha.new, na.rm = TRUE)
# max(alpha.new, na.rm = TRUE)
# 
# beta.new

# str(wind.comp_calc)

# Merge to dataframe
# temp <- do.call( rbind , wind.comp_calc)


# Merge to dataframe
temp <- as.data.frame(t(wind.comp_calc))
names(temp) <- c("wind_side", "wind_head_tail")

# wind.comp_calc_10
temp2 <- as.data.frame(t(wind.comp_calc_10))
names(temp2) <- c("wind_side_10", "wind_head_tail_10")

par <- cbind(par, temp)
names(par)
new.par <- par[,c(1,3,6:9)]
new.par <- cbind(new.par,alpha.new)
names(new.par) <- c("wind_dir_track",
                    "head_dir_track", "alpha.old",
                    "beta.old", "wind_side",
                    "wind_head_tail", "alpha")
# head(new.par)
gps.data.par.out <- cbind(gps.data.par, new.par, temp2)
head(gps.data.par.out)
# length(unique(names(gps.data.par.out))) == length(names(gps.data.par.out))
names(gps.data.par.out)

# Save data to database -------
odbcCloseAll()

# library(RODBC)

gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
# names(gps.data.par)

#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps.data.par.out, tablename = "lund_flight_points_wind_par_ecmwf",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(device_info_serial = "integer",
                      date_time = "datetime"))

# length(unique(gps.data.par.out$flight_id))

odbcCloseAll()

beep <- function(n = 9){
  x <- c(1,1,3,1,1,3,1,1,3,1,1)
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(x[i])
  }
}
beep()


