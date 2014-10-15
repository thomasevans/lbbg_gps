# Analysis of dives from 2009 guillemot GPS + TDR study



# Import required data ------

# From DB:
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# GPS data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT d.*
                       FROM guillemots_gps_points_igu_2009 AS d
                       ORDER BY d.ring_number ASC, d.date_time ASC;",
                       as.is = TRUE)

# fix date_times
gps.points$date_time <- as.POSIXct(gps.points$date_time, tz = "UTC")

dives <- read.csv("all_dive_data.csv")
names(dives)
# Correct data-time
# Date_time
# combine date and time into a character vector
date_time <- paste(dives$start_date,
                   dives$start_time, sep = " ")
# Convert to date-time object
date_time_start <-  as.POSIXct(strptime(date_time,
                                        format = "%d/%m/%Y %H:%M:%S",
                                        tz = "UTC"))

date_time_end <- date_time_start + dives$duration_dive


head(date_time_start)

# Bathymetry
# load('bsbd_raster.RData')

# Alternative raster layer
library("raster")
bath_raster <- raster("bsbd-0.9.3.grd")

# str(gps.points)
# Combine two data - for loop to go through all bouts -----


# x <- c(0,4,5)
# y <- c(3,5,2)
# z <- cbind.data.frame(x,y)
# z[4,] <- NA
# z

# Start with an example bout
# i <- 5

# For distance calculations
source("deg.dist.R")


# Initiate variables called in For loop
dive_start_time <- NULL
dive_bird_id  <- NULL
dive_depth <- NULL
dive_bath  <- NULL
dive_long  <- NULL
dive_lat   <- NULL

n_bouts <- length(dive.bouts$bird.name)
# str(dives)
# i <- 100
for(i in 1:n_bouts){
  

  t_sub <-  date_time_start[i] - 180
  t_plus <-  date_time_end[i]  + 180
  
  bird_id <- dives$bird.name[i]
#   summary(gps.points$ring_number == as.character(bird_id))
#   summary(gps.points$ring_number)
#   summary(as.factor(gps.points$ring_number))
  bird_id <- toupper(bird_id)
#   str(gps.points$ring_number)
#   str(bird_id)
  
  gps.sub <- NULL
  gps.sub <- subset(gps.points, (ring_number == as.character(bird_id)) &
                          (date_time > t_sub) &
                          (date_time < t_plus)
  )
  
  
  if(length(gps.sub$ring_number)==0){
    
    # skip rest...
    # return NAs
    dive_start_time[i] <- date_time_start[i]
    dive_bird_id[i]  <- bird_id
    dive_depth[i] <- dives$depth_max[i]
    dive_bath[i]  <- NA
    dive_long[i]  <- NA
    dive_lat[i]   <- NA
#     str(dives)
    
    
  } else {
    
    # IF GPS data continue...
    
    # XY GPS location data
    xy.gps <- cbind(gps.sub$longitude, gps.sub$latitude)
    
    # Extract bathymetry data for these positions
    gps.bath <- extract(bath_raster,xy.gps)
    
    bath.mean <- mean(gps.bath, na.rm = TRUE)
     
    n_gps <- length(gps.sub$ring_number)
    
    long_mean <- mean(gps.sub$longitude)   
    lat_mean <- mean(gps.sub$latitude)
    
    dive_start_time[i] <- date_time_start[i]
    dive_bird_id[i]  <- bird_id
    dive_depth[i] <- dives$depth_max[i]
    dive_bath[i]  <- bath.mean
    dive_long[i]  <- long_mean
    dive_lat[i]   <- lat_mean
    
    
    
  }
} #End of for loop

hist(dive_bath)
summary(dive_bath)

# Histograms of dive info -----

pdf("guillemots_dive_ind_histograms.pdf")
# png("guillemots_year_dif_bsbd-0.9.3.png")
# win.metafile("guillemots_year_dif_bsbd-0.9.3.wmf")
png("guillemots_dive_ind_histograms.png", width = 1000, height = 1500, res = 200)

par(mfrow = c(3,1))
par( mar = c(4, 4, 2, 1))

hist(dive_depth,
     xlim = c(0,90), breaks = 20,
     ylim = c(0,700),
     las = 1,
     ylab = "n dives",
     xlab = "Dive depth (m)",
     main = "",
     col = "red",
     yaxs = "i",
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2
)
box()



dive_dif <- -dive_bath - dive_depth


# str(dive.bouts)
hist((dive_dif),
     xlim = c(-50,100), breaks = 20,
     ylim = c(0,150),
     las = 1,
     ylab = "n dives",
     xlab = "Distance above sea bottom (m)",
     main = "",
     col = "red",
     yaxs = "i",
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2
)
abline(v = 0, lty = 2, lwd = 2, col = "dark grey")
box()


# str(dive.bouts)

p_dive <- dive_depth/(-dive_bath)
# hist(p_dive)
p_dive <- p_dive*100
# str(dive.bouts)
hist(100 - p_dive,
     xlim = c(0,150), breaks = 100,
     ylim = c(0,100),
     las = 1,
     ylab = "n dive bouts",
     xlab = "% of sea depth",
     main = "",
     col = "red",
     yaxs = "i",
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2
)
abline(v = 100, lty = 2, lwd = 2, col = "dark grey")
box()
dev.off()



# Plot dive depth vs. bathymetry ------
pdf("guillemots_dive_ind_water_depth.pdf")
# png("guillemots_year_dif_bsbd-0.9.3.png")
# win.metafile("guillemots_year_dif_bsbd-0.9.3.wmf")
png("guillemots_dive_ind_water_depth.png", width = 1500, height = 1500, res = 200)
par(mfrow=c(1,1))
par(mar= c(5, 4, 4, 2) + 0.1)
plot(-dive_bath , dive_depth,
     ylim = c(90,-2), xlim = c(0,120),
     xaxs = "i",
     xlab = "Water depth (m)",
     ylab = "Dive depth (m)",
     las = 1,
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2,
     main = "Dive depths vs. sea depth"
)
polygon(x = c(0,130,130), y = c(0,130,0),
        col = "light blue",
        lty = 4)
polygon(x = c(0,0,95), y = c(95,0,95),
        col = "grey")

# ?abline
abline(a = 0, b = 0.5, col = "black", lty = 2, lwd = 2)
# abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
abline(a = 15, b = 1, col = "red", lty = 3, lwd = 1)
abline(a = -15, b = 1, col = "red", lty = 3, lwd = 1)
abline(a = -30, b = 1, col = "blue", lty = 3, lwd = 1)
abline(a = -45, b = 1, col = "blue", lty = 3, lwd = 1)

text(x = 40, y = 40, labels = "Bottom", col = "black")
text(x = 45, y = 30, labels = "+15 m", col = "red")
text(x = 35, y = 50, labels = "-15 m", col = "red")
text(x = 50, y = 20, labels = "+30 m", col = "blue")
text(x = 55, y = 10, labels = "+45 m", col = "blue")
text(x = 85, y = 45, labels = "50%", col = "black")
points(-dive_bath , dive_depth,
       cex = 0.8)
box()
dev.off()


str(bout.info)
str(dive.bouts)


# Export table to database table -----

export.tab <- bout.info
export.tab <- cbind(export.tab, dive.bouts[,1:2])

str(export.tab)


# Write to database
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.tab,
        tablename = "guillemots_dive_bouts_2009_bathymetry",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time_start = "datetime")
)




# Checking data ----
sort(-bout.info$bath.mean)

test.sub <- bout.info[((bout.info$bath.mean == bout.info$bath.mean[7]) & (!is.na(bout.info$bath.mean))),]

points(test.sub$long.mid, test.sub$lat.mid, col = "red")

# summary(((bout.info$bath.mean == -11.6470) & (!is.na(bout.info$bath.mean))))
