# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to extract sunrise and sunset times for
# all GPS locations. Then it makes some calculations based
# on these. Such as time sinse/ to sunrise, and whether it
# is night of day.
# All is then ouput to a new table in the database for later use.

# Connect to database and extract GPS data
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


gps.points <- sqlQuery(gps.db,
                    query = "SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude, g.latitude
FROM gps_uva_tracking_speed_3d_limited AS g
ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                    ,as.is=TRUE)


# Get date_time in correct format
gps.points$date_time <- 
  as.POSIXct(gps.points$date_time,
             tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

# Get GPS locations in correct format for sunrise/set function
pos <- matrix(cbind(gps.points$longitude, gps.points$latitude),ncol = 2)


# Filter out points with NA values
f.na <- is.na(gps.points$longitude) | is.na(gps.points$latitude) | is.na(gps.points$date_time)
# summary(f.na)

# Convert to spatial object
library("maptools")

pos.sp <- SpatialPoints(pos[!f.na,],
                        proj4string = 
                          CRS("+proj=longlat +datum=WGS84"))


# head(pos.sp)
# head(pos)

str(gps.points)


# Get sunrise times
sunrise_date_time <- sunriset(pos.sp,
                              gps.points$date_time[!f.na],
                              direction = "sunrise",
                              POSIXct.out = TRUE)

# Get sunset times
sunset_date_time <- sunriset(pos.sp,
                             gps.points$date_time[!f.na],
                             direction = "sunset",
                             POSIXct.out = TRUE)



# Calculate time since/ to sunrise
sunrise_dif_s <- difftime(gps.points$date_time[!f.na],
                        sunrise_date_time[,2],
                        units = "secs")

sunrise_dif_s <- as.numeric(sunrise_dif_s)

# Calculate time since/ to sunset
sunset_dif_s <- difftime(gps.points$date_time[!f.na],
                         sunset_date_time[,2],
                          units = "secs")

sunset_dif_s <- as.numeric(sunset_dif_s)
# hist(sunset_dif_s)


# Is it night or day?
s1 <- sign(sunrise_dif_s)
s2 <- sign(sunset_dif_s) + 3

s3 <- s1 + s2
hist(s3)
#

t.fun <- function(x){
  if(x == 2 | x == 3 |x == 4 ) return("DAY")
  else return("NIGHT")
}

time_of_day <- sapply(s3, t.fun)
summary(as.factor(time_of_day))




# Compile to data frame and ouput to DB
out.tab <- cbind(gps.points$device_info_serial[!f.na],
                 gps.points$date_time[!f.na],
                 sunrise_date_time[,2], sunrise_dif_s,
                 sunset_date_time[,2], sunset_dif_s,
                 time_of_day)

out.tab2 <- as.data.frame(out.tab)

names(out.tab2) <- c("device_info_serial",
                    "date_time",
                    "sunrise_date_time",
                    "sunrise_dif_s",
                    "sunset_date_time",
                    "sunset_dif_s",
                    "time_of_day")

head(out.tab2)
str(out.tab2)


# Fix data types
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Origin date
startdate <- "1970-01-01"
startdate <- as.Date(startdate)

as.date_time <- function(x){
  t <- as.numeric.factor(x)
  as.POSIXct(t, origin = startdate, tz = "UTC")
}

# Date times
out.tab2$date_time <- as.date_time(out.tab2$date_time)

out.tab2$sunrise_date_time <- as.date_time(
  out.tab2$sunrise_date_time)

out.tab2$sunset_date_time <- as.date_time(
  out.tab2$sunset_date_time)

# Numerics
out.tab2$sunrise_dif_s <- as.numeric.factor(
  out.tab2$sunrise_dif_s)
out.tab2$sunset_dif_s <- as.numeric.factor(
  out.tab2$sunset_dif_s)
out.tab2$device_info_serial <- as.numeric.factor(
  out.tab2$device_info_serial)

str(out.tab2)


# Now ouput to database
sqlSave(gps.db, out.tab2, tablename = "lund_points_sun",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date",
                     sunrise_date_time = "Date",
                     sunset_date_time = "Date"))