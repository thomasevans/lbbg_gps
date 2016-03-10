# Import of GPS tracking data 'dump' file from UvA-BiTs DB

# Read in data -----
data.df <- read.csv("D:/Dropbox/R_projects/gps_data_all_20160309.csv",
                    header = TRUE)


# Check structure of data df -----
str(data.df)

# Fix structure
# Test with subset first
# data.df.sub <- data.df[1:100,]
str(data.df)

data.df$device_info_serial <- as.character(data.df$device_info_serial)

# head(data.df$date_time)
data.df$date_time <- as.POSIXct(as.character(data.df$date_time), tz = "UTC")

data.df$latitude <- as.numeric(as.character(data.df$latitude))
data.df$longitude <- as.numeric(as.character(data.df$longitude))
data.df$altitude <- as.numeric(as.character(data.df$altitude))

# Remove pressure column
data.df$pressure <- NULL

data.df$temperature <- as.numeric(as.character(data.df$temperature))
data.df$satellites_used <- as.integer(as.character(data.df$satellites_used))
data.df$gps_fixtime <- as.integer(as.character(data.df$gps_fixtime))
data.df$positiondop <- as.numeric(as.character(data.df$positiondop))
data.df$h_accuracy <- as.numeric(as.character(data.df$h_accuracy))
data.df$v_accuracy <- as.numeric(as.character(data.df$v_accuracy))
data.df$x_speed <- as.numeric(as.character(data.df$x_speed))
data.df$y_speed <- as.numeric(as.character(data.df$y_speed))
data.df$z_speed <- as.numeric(as.character(data.df$z_speed))
data.df$speed_accuracy <- as.numeric(as.character(data.df$speed_accuracy))
data.df$location <- as.character(data.df$location)
data.df$speed_3d <- as.numeric(as.character(data.df$speed_3d))
data.df$speed_2d <- as.numeric(as.character(data.df$speed_2d))
data.df$direction <- as.numeric(as.character(data.df$direction))
data.df$altitude_agl <- as.numeric(as.character(data.df$altitude_agl))

save(data.df, file = "gps_data_all.RData")


# Output data to database ------

# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the databases
gull.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

out.data <- data.df[order(data.df$device_info_serial, data.df$date_time),] 


# Output to new table in the database.
sqlSave(gull.db, out.data, tablename = "gps_ee_tracking_speed_limited_20160309",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime"))

