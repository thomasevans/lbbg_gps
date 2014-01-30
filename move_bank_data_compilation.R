#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.

#' Data from MoveBank, extracted as sepperate files for each vairable
#' Here we combine all the extracted variables and output to a 
#' csv file, this will then be imported into a new Access DB table.


# ?read.csv

# ?scan

setwd("D:/Dropbox/LBBG_Movebank/Data_downloaded/MoveBank_data_all")
# 
# cloud_cover_low_altitude <- read.csv("cloud_cover_low_altitude.csv",
#                                      header = TRUE,
#                                      

n <- count.fields("cloud_cover_low_altitude.csv", sep = ",")
head(n)
col.num <- max(n)


mycols <- rep("NULL", col.num)
mycols[c(3, 9, col.num)] <- NA
data.com <- read.table("cloud_cover_low_altitude.csv", sep = ",", colClasses = mycols, skip = 1, row.names = NULL)
# ?read.table
names(data.com) <- c("datetime","device_info_serial","cloud_cover_low_altitude")

# Get datetime to correct format
data.com$datetime <- as.POSIXct(as.character(data.com$datetime), tz = "UTC")

# Get rest of vairables
mycols <- rep("NULL", col.num)
mycols[c(col.num)] <- NA

# Each to own vectors
cloud_cover_total <- read.table("cloud_cover_total.csv", sep = ",", colClasses = mycols, skip = 1)
significant_wave_height <- read.table("significant_wave_height.csv", sep = ",", colClasses = mycols, skip = 1)
sun_shine_duration_day <- read.table("sun_shine_duration_day.csv", sep = ",", colClasses = mycols, skip = 1)
surface_roughness <- read.table("surface_roughness.csv", sep = ",", colClasses = mycols, skip = 1)
temperature_10m <- read.table("temperature_10m.csv", sep = ",", colClasses = mycols, skip = 1)
wind_u_10m <- read.table("wind_u_10m.csv", sep = ",", colClasses = mycols, skip = 1)
wind_v_10m <- read.table("wind_v_10m.csv", sep = ",", colClasses = mycols, skip = 1)





data.com <- cbind(data.com, cloud_cover_total, significant_wave_height,
                  sun_shine_duration_day, surface_roughness,
                  temperature_10m, wind_u_10m, wind_v_10m)
names(data.com) <- c("datetime","device_info_serial",
                     "cloud_cover_low_altitude",
                     "cloud_cover_total", "significant_wave_height",
                     "sun_shine_duration_day", "surface_roughness",
                     "temperature_10m", "wind_u_10m", "wind_v_10m")

# Have a look at the data.

hist(data.com$cloud_cover_low_altitude)
hist(data.com$cloud_cover_total)
hist(data.com$significant_wave_height)
hist(data.com$sun_shine_duration_day/60/60)
hist(data.com$significant_wave_height)
hist(data.com$surface_roughness)
hist(data.com$surface_roughness, xlim = c(0, 0.002), breaks = 100000) 
  # Range mainly corresponding to water surface

# min(data.com$surface_roughness)
hist(data.com$temperature_10m - 273.15)
hist(data.com$wind_u_10m)
hist(data.com$wind_v_10m)

row.names(data.com) <- NULL

# ?write.table
write.table(data.com, "move_bank_vairables_all.csv",
            sep = ",", row.names = FALSE)







# Set up thermal uplift and sea level pressure
n <- count.fields("uplift_pressure_all.csv", sep = ",")
head(n)
col.num <- max(n)


mycols <- rep("NULL", col.num)
mycols[c(3, 9, (col.num -1), col.num)] <- NA
data.com <- read.table("uplift_pressure_all.csv",
                       sep = ",", colClasses = mycols,
                       skip = 1, row.names = NULL)

names(data.com) <- c("datetime","device_info_serial",
                     "thermal_uplift",
                     "sea_level_pressure")

# Get datetime to correct format
data.com$datetime <- as.POSIXct(as.character(data.com$datetime), tz = "UTC")



row.names(data.com) <- NULL

# ?write.table
write.table(data.com, "move_bank_vairables_uplift_pressure.csv",
            sep = ",", row.names = FALSE)



