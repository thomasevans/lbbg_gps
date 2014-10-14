# Analysis of dive-bouts from 2009 guillemot GPS + TDR study



# Import required data ------

# From DB:
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Dive bouts
dive.bouts <- sqlQuery(gps.db,
                        query = "SELECT DISTINCT d.*
          FROM guillemots_dive_bouts_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time_start ASC;",
                        as.is = TRUE)

# str(dive.bouts)

# GPS data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT d.*
          FROM guillemots_gps_points_igu_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time ASC;",
                       as.is = TRUE)

# fix date_times
gps.points$date_time <- as.POSIXct(gps.points$date_time, tz = "UTC")
dive.bouts$date_time_start <- as.POSIXct(dive.bouts$date_time_start, tz = "UTC")
dive.bouts$date_time_end <- as.POSIXct(dive.bouts$date_time_end, tz = "UTC")


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

c.gps_info <- NULL
c.bath_max_dive_max <- NULL
c.bath_mean_dive_max <- NULL
c.bath_max_dive_max_mean <- NULL
c.bath_mean_dive_max_mean <- NULL
c.prop_bath_mean_dive_max <- NULL
c.prop_bath_mean_dive_max_mean <- NULL

c.long.start <- NULL
c.lat.start <- NULL
c.long.end <- NULL
c.lat.end <- NULL
c.long.mid <- NULL
c.lat.mid <- NULL

c.dist.p2p <- NULL
c.dist.straight <- NULL

c.bath.mean <- NULL
c.bath.min <- NULL
c.bath.max <- NULL
c.n_gps <- NULL

n_bouts <- length(dive.bouts$ring_number)


for(i in 1:n_bouts){
  
  # Re-set vairables
  t_sub_10 <- t_plus_10 <- t_sub_5 <- t_plus_5 <- NULL
  bird_id <- bout.gps.10 <- bout.gps.5 <- NULL
  
    t_sub_10 <-  dive.bouts$date_time_start[i] - 600
    t_plus_10 <-  dive.bouts$date_time_end[i]  + 600
    
    t_sub_5 <-  dive.bouts$date_time_start[i] - 300
    t_plus_5 <-  dive.bouts$date_time_end[i]  + 300
    
    bird_id <- dive.bouts$ring_number[i]
    bout.gps.10 <- NULL
    bout.gps.10 <- subset(gps.points, (ring_number == bird_id) &
                         (date_time > t_sub_10) &
                         (date_time < t_plus_10)
                       )
    bout.gps.5 <- NULL
    bout.gps.5 <- subset(gps.points, (ring_number == bird_id) &
                           (date_time > t_sub_5) &
                           (date_time < t_plus_5)
      )
    
    

    
    if(length(bout.gps.10$ring_number)==0){
      
      # skip rest...
      # return NAs
    
      
      # Put NA/ 0 for variables
      c.gps_info[i] <- FALSE
      c.bath_max_dive_max[i] <- NA
      c.bath_mean_dive_max[i] <- NA
      c.bath_max_dive_max_mean[i] <- NA
      c.bath_mean_dive_max_mean[i] <- NA
      c.prop_bath_mean_dive_max[i] <- NA
      c.prop_bath_mean_dive_max_mean[i] <- NA
      
      c.long.start[i] <- NA
      c.lat.start[i] <- NA
      c.long.end[i] <- NA
      c.lat.end[i] <- NA
      c.long.mid[i] <- NA
      c.lat.mid[i] <- NA
      
      c.dist.p2p[i] <- NA
      c.dist.straight[i] <- NA
      
      c.bath.mean[i] <- NA
      c.bath.min[i] <- NA
      c.bath.max[i] <- NA
      c.n_gps[i] <- 0
      
      
      
    } else {
      
      # IF GPS data continue...
      
      gps.data <- NULL
      
      # Use 5 minute if possible, but if too few points (<2)
      # use the 10 minute data
      if(length(bout.gps.5$ring_number) <2){
        gps.data <- bout.gps.10
      } else gps.data <- bout.gps.5
      
      
      # Re-set vairables
      xy.gps <- gps.bath <- bath.mean <- n_gps <-  bath.min <- bath.max <- NULL
      
      # XY GPS location data
      xy.gps <- cbind(gps.data$longitude, gps.data$latitude)
      
      # Extract bathymetry data for these positions
      gps.bath <- extract(bath_raster,xy.gps)
      
      bath.mean <- mean(gps.bath)
      # Because bathymetry are negative 
      bath.min <- max(gps.bath)
      bath.max <- min(gps.bath)
      
      n_gps <- length(gps.data$ring_number)
      
      
      # Re-set more variables
      points.next.lat <- points.next.long <- points.prev.lat <- points.prev.long <- dist.p2p <- dist.straight <- long.start <- NA
      lat.start <- long.end <- lat.end <- long.mid <- lat.mid <- NA
      
      # GPS point calculations
      # p2p distance
      if(n_gps > 1){
        points.next.lat <- xy.gps[-1,2]
        points.next.long <- xy.gps[-1,1]
        
        points.prev.lat <- xy.gps[-n_gps,2]
        points.prev.long <- xy.gps[-n_gps,1]
        
        # distances
        dist.p2p <- 1000*sum(deg.dist(points.prev.long,
                                 points.prev.lat,
                                 points.next.long,
                                 points.next.lat))
        
        dist.straight <- 1000*deg.dist(xy.gps[1,1],
                                       xy.gps[1,2],
                                       xy.gps[n_gps,1],
                                       xy.gps[n_gps,2])
        
        # locations
        long.start <- points.prev.long[1]
        lat.start <- points.prev.lat[1]
        long.end <- xy.gps[n_gps,1]
        lat.end <- xy.gps[n_gps,2]
        long.mid <- (long.end + long.start)/2
        lat.mid <- (lat.end + lat.start)/2
        
        
      } else {
        dist.p2p <- NA
        dist.straight <- NA
        long.start <- lat.start <- long.end <- lat.end <- NA
        long.mid <- gps.data$longitude
        lat.mid <- gps.data$latitude
      }

      # re-set values
      bath_max_dive_max <- bath_mean_dive_max <- bath_max_dive_max_mean <- bath_mean_dive_max_mean <- prop_bath_mean_dive_max <- prop_bath_mean_dive_max_mean <- NULL
      
      # Relate to dive bout depths
      bath_max_dive_max  <- -bath.max - dive.bouts$depth_max[i]
      bath_mean_dive_max <- -bath.mean - dive.bouts$depth_max[i]
      bath_max_dive_max_mean <- -bath.max - dive.bouts$depth_max_mean[i]
      bath_mean_dive_max_mean <- -bath.mean - dive.bouts$depth_max_mean[i]
      
      prop_bath_mean_dive_max  <-  bath_mean_dive_max/(-bath.mean)
      prop_bath_mean_dive_max_mean  <- bath_mean_dive_max_mean/(-bath.mean)
      
      
      # Put all above values together
      c.gps_info[i] <- TRUE
      c.bath_max_dive_max[i] <- bath_max_dive_max
      c.bath_mean_dive_max[i] <- bath_mean_dive_max
      c.bath_max_dive_max_mean[i] <- bath_max_dive_max_mean
      c.bath_mean_dive_max_mean[i] <- bath_mean_dive_max_mean
      c.prop_bath_mean_dive_max[i] <- prop_bath_mean_dive_max
      c.prop_bath_mean_dive_max_mean[i] <- prop_bath_mean_dive_max_mean
   

      c.long.start[i] <- long.start
      c.lat.start[i] <- lat.start
      c.long.end[i] <- long.end
      c.lat.end[i] <- lat.end
      c.long.mid[i] <- long.mid
      c.lat.mid[i] <- lat.mid
      
  
      
      c.dist.p2p[i] <- dist.p2p
      c.dist.straight[i] <- dist.straight
      
      c.bath.mean[i] <- bath.mean
      c.bath.min[i] <- bath.min
      c.bath.max[i] <- bath.max
      c.n_gps[i] <- n_gps
      
    
    }
} #End of for loop


bout.info <- cbind.data.frame(c.gps_info, c.bath_max_dive_max,
                      c.bath_mean_dive_max,
                      c.bath_max_dive_max_mean,
                      c.bath_mean_dive_max_mean,
                      c.prop_bath_mean_dive_max,
                      c.prop_bath_mean_dive_max_mean,
                      c.long.start, c.lat.start,
                      c.long.end, c.lat.end,
                      c.long.mid, c.lat.mid,
                      c.dist.p2p, c.dist.straight,
                      c.bath.mean, c.bath.min,
                      c.bath.max, c.n_gps                      
                      )


names(bout.info) <- c("gps_info", "bath_max_dive_max",
   "bath_mean_dive_max",
   "bath_max_dive_max_mean",
   "bath_mean_dive_max_mean",
   "prop_bath_mean_dive_max",
   "prop_bath_mean_dive_max_mean",
   "long.start", "lat.start",
   "long.end", "lat.end",
   "long.mid", "lat.mid",
   "dist.p2p", "dist.straight",
   "bath.mean", "bath.min",
   "bath.max", "n_gps")

names(dive.bouts)



# Histograms of dive info -----

pdf("guillemots_dive_histograms.pdf")
# png("guillemots_year_dif_bsbd-0.9.3.png")
# win.metafile("guillemots_year_dif_bsbd-0.9.3.wmf")
png("guillemots_dive_histograms.png", width = 1000, height = 1500, res = 200)

par(mfrow = c(3,1))
par( mar = c(4, 4, 2, 1))

hist((dive.bouts$depth_max_mean),
     xlim = c(0,90), breaks = 20,
     ylim = c(0,100),
     las = 1,
     ylab = "n dive bouts",
     xlab = "Dive depth (m)",
     main = "",
     col = "red",
     yaxs = "i",
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2
)
box()


# str(dive.bouts)
hist((bout.info$bath_mean_dive_max_mean),
     xlim = c(-20,100), breaks = 20,
     ylim = c(0,9),
     las = 1,
     ylab = "n dive bouts",
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

# str(dive.bouts)
hist(100 - (100 * bout.info$prop_bath_mean_dive_max_mean),
     xlim = c(0,150), breaks = 20,
     ylim = c(0,20),
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
pdf("guillemots_dive_water_depth.pdf")
# png("guillemots_year_dif_bsbd-0.9.3.png")
# win.metafile("guillemots_year_dif_bsbd-0.9.3.wmf")
png("guillemots_dive_water_depth.png", width = 1500, height = 1500, res = 200)
plot(-bout.info$bath.mean , dive.bouts$depth_max_mean,
     ylim = c(90,-2), xlim = c(0,120),
     xaxs = "i",
     xlab = "Water depth (m)",
     ylab = "Dive depth (m)",
     las = 1,
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.lab = 1.2,
     main = "Dive depths (bout mean) vs. sea depth"
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
points(-bout.info$bath.mean , dive.bouts$depth_max_mean,
       cex = 1.5)
box()
dev.off()

#-----

hist(dive.bouts$depth_max_mean)
hist(bout.info$bath.mean)

hist(bout.info$bath_mean_dive_max)
