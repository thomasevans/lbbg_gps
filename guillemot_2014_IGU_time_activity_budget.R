


# Load data from DB -----


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get deployments table
deployments <- sqlQuery(gps.db,
                        query = "SELECT DISTINCT d.*
                        FROM guillemots_track_session AS d
                        ORDER BY d.device_info_serial ASC;",
                        as.is = TRUE)

# Get data into correct format
str(deployments)
deployments$device_info_serial <- as.factor(deployments$device_info_serial)
deployments$ring_number <- as.factor(deployments$ring_number)
deployments$start_date <- as.POSIXct(deployments$start_date, tz = "UTC")
deployments$end_date <- as.POSIXct(deployments$end_date, tz = "UTC")
deployments$successful_deployment <- as.factor(deployments$successful_deployment)





# Get IGU data along with point classification
points_igu <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.latitude, g.longitude, c.coldist, c.diveprev, c.divenext, c.type, g.speed_ms
                       FROM guillemots_gps_points_igu AS g, guillemots_gps_points_igu_class AS C
                       WHERE g.device_info_serial = c.device_info_serial
                       AND g.date_time = c.date_time
                       ORDER BY g.device_info_serial ASC, g.date_time ASC ;",
                       as.is = TRUE)
# summary(points_igu$type)

# Fix data structure ----
str(points_igu)
points_igu$device_info_serial <- as.factor(points_igu$device_info_serial)
points_igu$date_time <- as.POSIXct(points_igu$date_time, tz = "UTC")
points_igu$type <- as.factor(points_igu$type)

names(points_igu)[9] <- "speed"

# Filter data by deployment status ------
# Filter data to only include data when devices are deployed.

hours_2 <- as.difftime(2, units = "hours")

# str(deployments)
# deployments$device_type <- as.factor(deployments$device_type)

# For IGU devices
dep_igu <- deployments[(deployments$device_type == "igu") &
                         deployments$successful_deployment == 1,]
n <- length(dep_igu[,1])

points_igu_f <- NULL
# i <- 4
for(i in 1:n){
  ring_number <- dep_igu$ring_number[i]
  t_start <- dep_igu$start_date[i] - hours_2
  t_end <- dep_igu$end_date[i] - hours_2
  device <- dep_igu$device_info_serial[i]
  
  f <- ((points_igu$device_info_serial == as.character(device)) &
          (points_igu$date_time > t_start) &
          (points_igu$date_time < t_end))
#   levels(points_igu$type)
  z <- (points_igu$type != "bad_location") &
    (points_igu$type != "no_location")
  summary(z)
#   z <- (points_igu$device_info_serial == as.character(device))
#   t <- (points_igu[z,2])> t_start  & (points_igu[z,2]) < t_end
#   summary(t)
  #   summary(points_igu$device_info_serial == as.character(device))
  
  
  points_s <- points_igu[f & z,]
  
#   str(points_s)
  
  points_s <- cbind(points_s, ring_number)
  points_igu_f <- rbind(points_igu_f, points_s)
}









# Calculate time intervals -----
# Calculate time between locations
time_interval <- NULL

# Set first to zero (no preceding points)
time_interval[1] <- 0


# Calculate time between points
# If final or first point from bird (ring_number)
# Set to zero
for(i in 2:(length(points_igu_f$type)-1)){
  if(points_igu_f$ring_number[i] !=
       points_igu_f$ring_number[i-1]){
    time_interval[i] <- 0
  } else if(points_igu_f$ring_number[i] !=
              points_igu_f$ring_number[i+1]){
    time_interval[i] <- 0} else{
      time_interval[i] <- as.numeric(
        difftime(
          points_igu_f$date_time[i-1],
          points_igu_f$date_time[i],
          units = "secs") * -1
      )
    }
}
# Set final point to zero otherwise will get
# error with above code where compares current
# and next point
time_interval[length(points_igu_f$type)] <- 0

hist(time_interval, breaks = 1000,
     xlim = c(0,6000))

time_interval_new <- time_interval
# Replace intervals > 700 with zero, to reduce their weight to zero
time_interval_new[time_interval_new > 2000] <- 0


summary(points_igu_f$type)

# ?aggregate
str(points_igu_f)
id <- as.factor(points_igu_f$ring_number)
# id <- list(id)
# str(id)

# Total time for each type
time_active <- aggregate(time_interval_new ~ 
            points_igu_f$ring_number +
            points_igu_f$type,
#           by = id,
          FUN = sum
          
)

# Total time per individual
time_bird <- aggregate(time_interval_new ~ 
            points_igu_f$ring_number,
          FUN = sum          
)

names(time_active) <- c("ring_number", "type", "time_tot")
names(time_bird) <- c("ring_number","time_tot")

n <- length(time_bird$ring_number)
nb <- length(time_active$ring_number)


p_active <- rep(NA,nb)
for(i in 1:n){
  f <- time_active$ring_number == time_bird$ring_number[i]
  p_active[f] <- time_active$time_tot[f]/time_bird$time_tot[i]
  
}

cbind(time_active,p_active)

