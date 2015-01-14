# Script for plotting figures to look at flight behaviour and activity
# for the lesser black-backed gulls.


# Load data ------
# Set working directory
# NB this should be changed to the location where you have
# saved the data files, and where you want to output any graphic
# files.
setwd("D:/Dropbox/LBBG_flight_height/R_files")

# Load flight information (meta-data)
flight.details <- read.csv("flight_subset_altitude.csv",
                        head = TRUE)


#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
flight.details$start_time <- as.POSIXct(flight.details$start_time, tz="UTC",
                            format = "%Y-%m-%d %H:%M:%S")
flight.details$end_time <- as.POSIXct(flight.details$end_time, tz="UTC",
                                     format = "%Y-%m-%d %H:%M:%S")


# If you wish to view the data run the following:
fix(flight.details)
# Or use 'View', but this might only work in RStudio
View(flight.details)


# Load GPS point data
load("flight_subset_altitude_points.RData")

summary((points_all$flight_id == flight.id))


sample(points_all$flight_id,100)

# For testing (if de-bugging/ modifying the function 'map.flight.id' below)
flight.id <- 34145
points <- points_all[(points_all$flight_id == flight.id),]
flight.details <- flight.info[(flight.info$flight_id == flight.id),]

# Function to produce map and plots to illustrate
# flights by lesser black-backed gulls
map.flight.id <- function(flight.id = NULL,
                          points = NULL,
                          flight.details = NULL){
  
  

  # Swedish coast-line data ######
  load("SWE_adm0.RData")
  
  
  # Set-up graphic paramaters ######
  
  # Function to add alpha channel to colours, allows use of
  # transparency in graphics.
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
  
  
  if(flight.details$interval_mean < 20){
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
                 (x.len/10)*(flight.details$uwnd10m/5)),
         y1 = ((ax.lim[3]+0.65*y.len) +
                 (y.len/10)*(flight.details$vwnd10m/5)),
         lwd = 2,
         arr.type = "simple",
         arr.length = 0.5,
         col = addalpha("blue", alpha = 0.8)
         
  )
  
  wind.10m <- sqrt((flight.details$uwnd10m*flight.details$uwnd10m) +
                     (flight.details$vwnd10m*flight.details$vwnd10m))
  
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
  
  #   names(flight.details)
  
  #   abline(h = flight.details$alt_med,
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
