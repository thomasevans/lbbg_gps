# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script contains a function originally from 'commuting_flight_comparisons_maps.R'
# The function maps flights based on an input of point data




maps.flights <- function(points.data = NULL, seed = NULL, plot.title = "", all.flights = FALSE, flight.num = 50, alpha = 0.5, flight.id = FALSE){
  #   ?title
  #Function to map flights
  #  Provide dataframe with flights points
  # If you want all flights plot, enter 'TRUE', default 'FALSE'
  # Choose number of flights to plot, 50 is default (ignored if all.flights = TRUE)
  # Alpha - for transparency of lines - when saving to some image types transparency is not supported, then enter 1 for alpha (i.e. not transparent).
  library(maps)
  
  seed.new <- seed
  if(is.null(seed)){seed.new <- 2}
  
  set.seed(seed.new)
  
  
  fl.n <- unique(points.data$flight_id)  

#   ?is.logical
  # Test if a list of flight.id is provided
  if(is.logical(flight.id)){
    f <- FALSE
  } else f <- TRUE
  
#   flight.id <- c(1,34,"d")
  
  if(all.flights){
    f.s <- fl.n  
    flight.num <- length(fl.n)
  }else
    {if(!f){
      f.s <- sample(fl.n,flight.num)
    } else {
      f.s <- flight.id
      flight.num <- length(fl.n)
    }
    }


  
  #   points.data <- points.in
  
  # Set map limits
  c.xlim <- range(points.data$longitude[points.data$flight_id %in% f.s])
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.15
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  c.ylim <- range(points.data$latitude[points.data$flight_id %in% f.s])
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.15
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
#   plot(gadm, xlim = c(17,17.5), ylim = c(57,58))
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="white", bg = "grey")
  #   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
  #          "black")
  title(plot.title)
  #     seed <- 3
  if(!is.null(seed)){
  mtext(paste("seed: ",seed))
  }
  # colours for lines  
  #     library(RColorBrewer)
  # Generating more colours than are in the palette - includes intermediate values.
  #     col.line <- colorRampPalette(brewer.pal(11,"Spectral"))(length(fl.n))
  #     # Change alpha value, to make transparent - allow to see overplotting
  #     col.line <- adjustcolor(col.line, 0.6)
  #     # Shuffle colours
  #     col.line <- col.line[sample.int(length(col.line))]
  
  #   ?rainbow
  # Get colours from rainbow scale
  
  
  
  
  #   ?rainbow
  col.line <- rainbow(length(fl.n), alpha = alpha)
  # 
  col.line <- col.line[sample.int(length(col.line))]
  
  
  # Plot lines for each flight
  for(i in 1:flight.num){
    
    x <- f.s[i]
    gps.sub <- subset(points.data, flight_id == x,
                      select=c(longitude, latitude))
    n <- length(gps.sub$longitude)
    segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
             gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
             col = col.line[i], lwd = 2)
  }
  
  # Scale bar and axis
  x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
  y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
  map.scale(x,y,ratio = FALSE)
  #   ?map.scale
  box()
  axis(side=(1),las=1)
  axis(side=(2),las=1)
}