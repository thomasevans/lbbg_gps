# Script to map individual flights, illustrating key features
# such as altitude and speed.


# Use as sourcable file for function 'map.flight.id'

# Wrap below into a single function
# Function accepts 'flight_id' as sole argument
# Outputs map

# For testing
flight.id <- 25476

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
            FROM lund_flight_com_lbbg AS f
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

pdf("test_25476.pdf")

# Plot base-map (using axis limits)
par(mfrow=c(1, 1))
par( mar = c(5, 4, 4, 5))
# Plot as proper map to get sensible projection
plot(gadm, col= "grey50", bg = "grey90", xlim = x.lim,
     ylim = y.lim
     )

# Add title indicating which flight it is
title(main = paste("Flight ID:", flight.id))

# Add track lines
# Use 'segments', and plot in grey, lwd = 2?
# ?segments
n <- length(points$longitude)
segments(points$longitude[1:n-1],
         points$latitude[1:n-1],
         points$longitude[2:n],
         points$latitude[2:n],
         lwd = 2,
#          col = addalpha("blue", alpha = 0.60))
         col = as.character(col.speed[-1]))


# Add filled sybols (showing speed)
# pch = 21 (filled circle)
# col = "black" (symbol outline)
# bg = colour vector (fill - speed)
points(points$longitude, points$latitude,
       bg = as.character(col.speed),
       pch = 21, cex = 0.8*point.size.speed)

# Add scale bar
# perhaps include alpha channel?
# distance in km...
# library(maps)
ax.lim <- par("usr")
x.len <- ax.lim[2] - ax.lim[1]
y.len <- ax.lim[4] - ax.lim[3]

map.scale(x = (ax.lim[1]+0.12*x.len),
          y = (ax.lim[3]+0.12*y.len), ratio = FALSE,
          col = "grey50")


# Add wind arrow
# Size arrow according to wind speed
# Place arrow in centre?
# Include alpha channel
# ?arrow
# ?rad
# install.packages("circular")
library("circular")
library("shape")
unit.length <- sqrt((x.dif*x.dif) + (y.dif*y.dif))/10

Arrows(x0 = (x.lim[1]+0.65*x.dif),
       y0 = (y.lim[1]+0.65*y.dif),
       x1 = ((x.lim[1]+0.65*x.dif) +
               unit.length*(flight.info$uwnd10m/5)),
       y1 = ((y.lim[1]+0.65*y.dif) +
               unit.length*(flight.info$vwnd10m/5)),
       lwd = 2,
       arr.type = "triangle",
       arr.length = 0.5,
       col = addalpha("blue", alpha = 0.8)
       
         )

wind.10m <- sqrt((flight.info$uwnd10m*flight.info$uwnd10m) +
  (flight.info$vwnd10m*flight.info$vwnd10m))

# ?round
wind.10m <- round(wind.10m, digits = 2)


text(x = (x.lim[1]+0.74*x.dif),
     y = (y.lim[1]+0.68*y.dif),
labels = bquote(.(wind.10m) ~ ms^-1))



     
# ?expression
# More tidying...
box(col = "grey50", lwd = 2)
axis(side = (2), las = 1, col = "grey50", col.axis = "grey50")
axis(side = (1), las = 1, col = "grey50", col.axis = "grey50")

dev.off()
