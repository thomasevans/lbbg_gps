# Script to map individual flights, illustrating key features
# such as altitude and speed.


# Use as sourcable file for function 'map.flight.id'

# Wrap below into a single function
# Function accepts 'flight_id' as sole argument
# Outputs map

# For testing
flight.id <- 564

# 1. Get GPS data (including wind info)  ########

# Connect to DB
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


# Get start date-time and end date-time and device info serial
flight.info <- sqlQuery(gps.db, as.is = TRUE, query=
    paste("SELECT DISTINCT f.*
            FROM lund_flights AS f
            WHERE f.flight_id = ",
          flight.id,
          "
          ORDER BY f.flight_id ASC;", sep = ""))



device_id <- flight.info$device_info_serial[1]
datetime.start <- flight.info$start_time[1]
datetime.end   <- flight.info$end_time[1]

# Get GPS location fulfilling above extracted criteria
points <- sqlQuery(gps.db, as.is = TRUE, query=
                     paste("SELECT DISTINCT g.*
            FROM gps_uva_tracking_speed_3d_limited AS g
            WHERE g.device_info_serial = ",
                           device_id,
                           "
          AND g.date_time > #",
                           datetime.start,
                           "#
          AND g.date_time < #",
                           datetime.end,
                           "#
          ORDER BY g.date_time ASC;", sep = ""))


# Plot quickly to see how this looks
# plot(points$latitude~points$longitude)



# 2. Get base-map data #####
# Should be able to use the same one as previously used for guillemot
# data.
# Swedish coast-line data
load("SWE_adm0.RData")


# 3. Set-up graphic paramaters ######


# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# Axis limits (long/ lat)
x.lim <- range(points$longitude)
x.dif <- x.lim[2] - x.lim[1]
x.lim <- c(x.lim[1]-(0.25*x.dif), x.lim[2]+(0.25*x.dif))


y.lim <- range(points$latitude)
y.dif <- y.lim[2] - y.lim[1]
y.lim <- c(y.lim[1]-(0.25*y.dif), y.lim[2]+(0.25*y.dif))


# Sybol colour scale
library(RColorBrewer)
library(maps)

col.obs.transp <- addalpha((brewer.pal(9,"YlOrRd")), alpha = 0.60)


# Make vector of point colours based on speed
speed_2d <- sqrt((points$veast*points$veast) + (points$vnorth*points$vnorth))
# hist(speed_2d)

col.speed <- cut(speed_2d, breaks = c(0,seq(6,16,2),20,25,100),
                 labels = col.obs.transp)
# ?cut
# plot(speed_2d, bg = as.character(col.speed),pch = 21, type = "n")
# 
# # Sybol size vector (based on altitude)
# plot(points$altitude, bg = as.character(col.speed),pch = 21, type = "n")
# points(points$altitude, bg = as.character(col.speed),
#        pch = 21, cex = (1 + ((points$altitude)/20)))
# 
# 1 + ((points$altitude)/20)
# 
# plot(points$latitude~points$longitude, bg = as.character(col.speed),
#      pch = 21, cex = (0.6 + ((points$altitude)/40)))

point.size.speed <- (0.6 + ((points$altitude)/40))

# 4. Make map #######

# Plot base-map (using axis limits)
par(mfrow=c(1, 1))
par( mar = c(5, 4, 4, 5))
plot(gadm, col= "grey50", bg = NA, xlim = c(16.9, 18.1),
     ylim = c(56.8, 57.65))

# Plot as proper map to get sensible projection


# Add track lines
# Use 'segments', and plot in grey, lwd = 2?

# Add filled sybols (showing speed)
# pch = 21 (filled circle)
# col = "black" (symbol outline)
# bg = colour vector (fill - speed)

# Add scale bar
# perhaps include alpha channel?
# distance in km...
library(maps)
map.scale(x= 17, y = 56.9, ratio = FALSE)


# Add wind arrow
# Size arrow according to wind speed
# Place arrow in centre?
# Include alpha channel

# More tidying...
box(,col="grey50",lwd=2)
axis(side=(2), las=1, col="grey50", col.axis="grey50")
axis(side=(3), las=1, col="grey50", col.axis="grey50")
