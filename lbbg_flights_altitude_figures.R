


# For testing
flight.id <- 16641

# Function to produce map and plots to illustrate
# flights by lesser black-backed gulls
map.flight.id <- function(flight.id = 25476){
  
  # 1. Get GPS data (including wind info)  ########
  
  # Connect to DB
  library(RODBC)
  
  # Establish a connection to the database
  gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')
  
  #See what tables are available
  #sqlTables(gps.db)
  
  
  # Get start date-time and end date-time and device info serial
  
  
  # Analysed section only
  #   flight.info <- sqlQuery(gps.db, as.is = TRUE, query=
  #                             paste("SELECT DISTINCT f.*
  # 31
  #                FROM lund_flight_com_lbbg AS f
  #      WHERE f.flight_id = ",
  #                                   flight.id,
  #                                   "
  #             AND f.flight_id = t.flight_id
  #             ORDER BY f.flight_id ASC;", sep = ""))
  
  # Whole flight
  flight.info <- sqlQuery(gps.db, as.is = TRUE, query=
                            paste("SELECT DISTINCT f.*, t.uwnd10m, t.vwnd10m
                                  FROM lund_flights AS f, lund_flight_com_lbbg as t
                                  WHERE f.flight_id = ",
                                  flight.id,
                                  "
                                  AND f.flight_id = t.flight_id
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
  
  
  #   points <- points[10:40,]
  
  
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
  
  
  # points$altitude
  
  # 4. Make map #######
  
  #    pdf("test_16641_3.pdf")
  
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
  
  
  if(flight.info$interval_mean < 20){
    n <- length(points$longitude)
    ind <- c(1:n)
    ind.5 <- ind%%5
    ind.t <- rep(FALSE,n)
    ind.t[ind.5 == 1] <- TRUE
    #     points(points$longitude, points$latitude,
    #            bg = as.character(col.speed),
    #            pch = 21, cex = 1*point.size.speed)
  } else ind.t <- TRUE
  points(points$longitude[ind.t], points$latitude[ind.t],
         bg = as.character(col.speed[ind.t]),
         pch = 21, cex = 1*point.size.speed[ind.t])
  
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
  #   unit.length <- sqrt((x.len*x.len) + (y.len*y.len))/10
  
  
  
  # map.scale(x = (ax.lim[1]+0.12*x.len),
  #           y = (ax.lim[3]+0.12*y.len), ratio = FALSE,
  #           col = "grey50")
  
  #   ?Arrows
  Arrows(x0 = (ax.lim[1]+0.25*x.len),
         y0 = (ax.lim[3]+0.65*y.len),
         x1 = ((ax.lim[1]+0.25*x.len) +
                 (x.len/10)*(flight.info$uwnd10m/5)),
         y1 = ((ax.lim[3]+0.65*y.len) +
                 (y.len/10)*(flight.info$vwnd10m/5)),
         lwd = 2,
         arr.type = "simple",
         arr.length = 0.5,
         col = addalpha("blue", alpha = 0.8)
         
  )
  
  wind.10m <- sqrt((flight.info$uwnd10m*flight.info$uwnd10m) +
                     (flight.info$vwnd10m*flight.info$vwnd10m))
  
  # ?round
  wind.10m <- round(wind.10m, digits = 2)
  
  
  text(x = (ax.lim[1]+0.27*x.len),
       y = (ax.lim[3]+0.60*y.len),
       labels = bquote(.(wind.10m) ~ ms^-1))
  
  
  
  
  # ?expression
  # More tidying...
  box(col = "grey50", lwd = 2)
  axis(side = (2), las = 1, col = "grey50", col.axis = "grey50")
  axis(side = (1), las = 1, col = "grey50", col.axis = "grey50")
  
  
  # Make some plots
  # names(points)
  
  date_time <- as.POSIXct(points$date_time, tz = "UTC")
  
  # 
  # y.min <- min(c((speed_2d),points$vdown*5))
  # y.max <- max(c((speed_2d),points$vdown*5))
  
  par(mfrow = c(4,1))
  
  par(mar=c(0,4,0,4))         
  
  # Speed over time
  plot(speed_2d~date_time,
       #        xlab = "Time",
       #        ylab = expression("Speed ms"^{-1}),
       ylab = "",
       ylim = c(0,max(speed_2d, na.rm = TRUE) + 1),
       axes = FALSE)
  
  #   ?segments
  # points$speed_accuracy
  segments(x0 = date_time,
           x1 = date_time,
           y0 = speed_2d - points$speed_accuracy,
           y1 = speed_2d + points$speed_accuracy,
           lwd = 1,
           col = "grey70")
  points(speed_2d ~ date_time, pch = 21, bg = "white")
  
  axis(4, las = 1)              # y-axis
  box()
  grid()
  #   title(main = paste("Flight ID:", flight.id))
  title(ylab = expression("Speed ms"^{-1}), line = 1)
  #   points(points$vdown*5 ~ date_time,
  #          col = "red")
  
  par(mar=c(0,4,0,4))         # no top spacing
  plot(points$altitude~date_time,
       #        xlab = "Time",
       #        ylab = "Height (m)",
       ylab = "",
       ylim = c(-20,max(points$altitude , na.rm = TRUE) +1),
       axes = FALSE)
  segments(x0 = date_time,
           x1 = date_time,
           y0 = points$altitude - points$v_accuracy,
           y1 = points$altitude + points$v_accuracy,
           lwd = 1,
           col = "grey70")
  points(points$altitude ~ date_time, pch = 21, bg = "white")
  axis(2,las = 1)                # y-axis
  title(ylab = "Height (m)", line = 2.5)
  grid()
  box()
  
  vh <- (speed_2d/points$altitude)
  
  par(mar=c(0,4,0,4))         # no top spacing
  plot(vh~date_time,
       #      xlab = "Time",
       #      ylab = "v/h"
       ylab = "",
       axes = FALSE)
  axis(4,las = 1)              # y-axis
  title(ylab = "v/h", line = 1)
  box()
  grid()
  par(mar=c(3,4,0,4))         
  
  plot(vh~date_time,
       #      xlab = "Time",
       ylab = "v/h (log scale)",
       #      ylim = c(-.5,1)
       log = "y",
       las = 1
  )
  grid()
  title(xlab = "Time", line = 2)
  
  # par(mfrow = c(1,1))
  # hist(vh[vh > -4 & vh < 4], breaks = 80)
  
  
  # library(ggplot2)
  # library(reshape2)
  # str(points)
  # 
  # points.sub <- cbind.data.frame(date_time,points$altitude,vh,speed_2d)
  # names(points.sub) <- c("date_time", "altitude", "vh", "speed_2d")
  # 
  # # Use melt to reshape data so values and variables are in separate columns
  # dt.df <- melt(points.sub, measure.vars = c("altitude", "vh", "speed_2d"))
  # 
  # ggplot(dt.df, aes(x = date_time, y = value)) +
  #   geom_point(aes(color = variable)) +
  #   facet_grid(variable ~ ., scales = "free_y")
  #   # Suppress the legend since color isn't actually providing any information
  # #   opts(legend.position = "none")
  
  
  
  
  
  # ?plot
  
  #   names(flight.info)
  
  #   abline(h = flight.info$alt_med,
  #          )
  
  
  
  
  
  
  #   dev.off()
}


flight.ids <- c(414,25476,3388,16014,20533,29451,
                33976, 34550, 35143, 16336)

flight.ids <- c(16570,16740, 32591, 38508, 16231, 29732,
                8571, 16570, 25010,
                12721, 33419, 35235, 19451, 5034,
                33629, 38459, 20442, 5332, 29405,
                37799, 22953, 19804, 16770, 37713)

# high resolution flights
flight.ids <- c(37471, 29604, 39428,
                28481, 19804, 39526,
                23129, 28499, 30870,
                3060, )
flight.ids <- c(34145, 31321, 22953, 5034,
                24234, 24611, 16641, 25568,
                16606, 38576, 38660, 39016)

pdf("flights_different_conditions_graphs_highres2_whole_vh3-log.pdf")
for(i in 1:length(flight.ids)){
  map.flight.id(flight.id = flight.ids[i])
}
dev.off()
