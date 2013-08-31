#This script includes functions to analyse and visualise GPS tracks.


#Get test data
test10min <- read.table ("C:/Users/Tom/Dropbox/jose_shearwaters/test10min.txt", head = T, dec =",")


#Make vector with combined date and time. This will be character formatted
test_date_time_txt <- paste(test10min$Date, " ", test10min$Time, sep = "")
#View first 10 lines to check if it looks sensible.
# test_date_time_txt[1:10]

#Convert character formated date-time to POSIXct format (as requested by as.ltraj function. 
test_date_time_posix <- as.POSIXct(test_date_time_txt, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
#View first 10 lines
# test_date_time_posix[1:10]


#Testing (run these lines if testing things within the functions).
# long <- test10min$Longitude
# lat <- test10min$Latitude
# r1 <- 10000
# r2 <- 50000
# r3 <- 100000
# date_time <- test_date_time_posix


#Function to calculate tortoisity and 'Derived ground speed'.
votier_calc <- function(long, lat, date_time, r1 = 1000, r2 = 10000, r3 = 100000){
  #Long - a vector of longitude values
  #Lat - a vector of latitude values
  #r1, r2, r3  - the radii, in metres, for which values will be calculated, r3 > r2 > r1. Default values are provided (1, 10, 100 km).
  #date_time should be in POSIXct format
  
    #Function to calculate grand-circle distance. Copied from package 'fossil'. As this is the only function required from this package, it seems better to include it in the script.
  #Vavrek (2012) 'fossil' v0.3.7. Palaeoecological and Palaeogeographical Analysis Tools. http://matthewvavrek.com/programs-and-code/fossil/
  deg.dist <- function (long1, lat1, long2, lat2) 
  {
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 40041.47/(2 * pi)
    d <- R * c
    return(d)
  }
  
#   require(fossil)
  
  #Test that the longitude and latitude vectors are of equal length.
  if(length(long) != length(lat) | length(lat) != length(date_time)){warning("ERROR: Longitude, latitude, or date_time vector lengths differ, please provide vectors for longitude and latitude of equal length (i.e. x,y coordinates for each GPS fix), and date_time for each GPS fix")
                                  break
  }
  
  #Test that radii provided fulfill condition r3 > r2 > r1
  if( r3 <= r2 | r2 <= r1) {warning("ERROR: Values for radii must fulfill condition r3 > r2 > r1. Please provide values for radii where r3 is the largest, r2 intermediate, and r1 the least. Radii are in kilometres.")
  break
  }

  #Checking if date_time is provided in correct format.
  if(!inherits(date_time, "POSIXct")){
    warning("ERROR: date_time must be a POSIXct vector. Please supply date_time in POSIXct format")
    break
  }
  
  
  #Produce an index of GPS locations
  i <- c(1:length(long))
  
#   id <- 500
  
  idx <- c(1:length(long))
  
  
  #Calculate inter-point distances
  p2p_dist <- deg.dist(long[-length(long)], lat[-length(lat)], long[-1],lat[-1])*1000
  p2p_dist <- c(0,p2p_dist)  
  
#   length(p2p_dist)
  
  #index <- idx
  #idz <- 100
  #Define new function (internal to the function 'votier_calc'), where the calculations will be carried out for each GPS point. This function will then be applied for all GPS points using an apply function.
  gps_calc <- function(idz , indexz = idx, p2p_distz = p2p_dist, date_timez = date_time, longz = long, latz = lat, r1z = r1, r2z = r2, r3z = r3){
    #id is the index for the GPS location we want to make calculations for.
    #long, the vector of longitudes
    #lat, the vector of latitudes
    #r1, r2, r3, the previously defined radii 
    
    
    
    
#     idz <- 50
    
    #Package required for calculating geographical distances
#     require(fossil)
    #library(fossil)
    
    #for all trip locations, set distance to focal point to NA
    dist <- rep(NA, length(longz))
    
    id_max <- length(longz)
    
    #for prior points
    location_id <- idz
    
    #Set distance to focal location as zero
    dist[idz] <- 0
    
    #Set an initial value for d - which will keep track of the distance between the focal point and the GPS point for which the distance is being calculated.
    d <- r3z - (0.1 * r3z)
    while(d <= r3z & location_id >= 2){ 
      location_id <- location_id - 1
      dist[location_id] <- deg.dist(longz[idz],latz[idz],longz[location_id],latz[location_id])* 1000
        d <- dist[location_id]
    }
    first_point_r3 <- location_id
    
    #for post points
    location_id <- idz
    d <- r3z - (0.1 * r3z)
    while(d <= r3z & location_id < id_max){ 
      location_id <- location_id + 1
      dist[location_id] <- deg.dist(longz[idz],latz[idz],longz[location_id],latz[location_id]) * 1000
      d <- dist[location_id]    }
    
    final_point_r3 <- location_id
    

    #Make a filter criterion to only take points which fall within the r3 radius, i.e. those for which we have previously calculated distances from circle centre.
    subs1 <- (indexz >= first_point_r3 ) & (indexz <= final_point_r3)
#     summary(subs)
    
    #rx <- r3
    #This function calculates the tortoisity and the 'derived ground speed' for each GPS point
    param_calc <- function(longx = longz[subs1], latx = latz[subs1], indexx = indexz[subs1], p2p_distx = p2p_distz[subs1], date_timex = date_timez[subs1], rx, idxx = idz, distx = dist[subs1]){
      #Takes calculated values from above, then calculates paramaters for that GPS point (id), and the defined redius (r).
      
      #package required for distance calculations
#       require(fossil)
      
      #Get minimum index (i.e. entering circle)  
        d <- rx 
      location_id <- idxx
      while(d <= rx & location_id > 1){ 
        location_id <- location_id - 1
        d <- distx[indexx == location_id]
      }
      first_point <- location_id
      
        #Get maximum index (i.e. exiting circle)  
        location_id <- idxx
      d <- rx - 10
      while(d <= rx & location_id < id_max){ 
        location_id <- location_id + 1
        d <- distx[indexx == location_id]    }
        final_point <- location_id
        
        #Filter, to only get data which fall within distance r of circle centre (focal GPS point).
        subs <- (indexx >= first_point ) & (indexx <= final_point)
        
        #Tortoisity calculation
        #First calculate the straight-line distance, from first location to final location. This should be close to r, but not exactly, for GPS locations will not fall exactly on the circumferance of the circle.
        dist_ab <- deg.dist(longx[indexx == final_point],latx[indexx == final_point],longx[indexx == first_point],latx[indexx == first_point]) * 1000
        dist_total <- sum(p2p_distx[subs])
        tort <- dist_ab / dist_total
      
        #'Derived ground speed', here the straight line distance moved (close to circle radius * 2) over the time taken.
        #Time difference between final and first location
        time_dif <- date_timex[indexx == final_point] - date_timex[indexx == first_point]
        time_dif <- as.numeric(time_dif, units="secs")
        #Metres over seconds - i.e. metres per second
        speed <- dist_ab / time_dif
      
#       distx[237]
      
      #Test if the distance between the first point and the centre of the circle are close to the intended radius (here we have a tolerance of upto 20 % less than intended.
      #The idea of thise is that with the GPS recording interval, it is likely that the GPS point closest to the edge of the circle, may be some distance a way - here this is accepted up to a certain tolerance.
      if((distx[indexx == first_point] < (0.8 * rx)) | (distx[indexx == final_point] < (0.8 * rx)) ){
      tort <- NA
      speed <- NA
      }
      
      return(c(tort,speed))
      
    }
#end of function 'param_calc'
    
    
#     param_calc(rx = 50)
    
    
    #R1
    r1_param <- param_calc(rx = r1z)
    tort1 <- r1_param[1]
    speed1 <- r1_param[2]
    
    #R2
    r2_param <- param_calc(rx = r2z)
    tort2 <- r2_param[1]
    speed2 <- r2_param[2]
    
    #R3
    r3_param <- param_calc(rx = r3z)
    tort3 <- r3_param[1]
    speed3 <- r3_param[2]
    
    
    return(c(tort1, speed1, tort2, speed2, tort3, speed3))
    
    
  }
  #End of function 'gps_calc'
  
# gps_calc(idz = id)
  all.points <-  sapply(idx,gps_calc)
  all.points2 <- t(all.points)
  all.points3 <- as.data.frame(all.points2)
  names(all.points3) <- c("tort1","v1","tort2","v2","tort3","v3")
  
  
  return(all.points3)
}
#End of calculation 'votier_calc'






#Run for test data-set.
gps_vot_calc <- votier_calc(long = test10min$Longitude, lat = test10min$Latitude, date_time = test_date_time_posix, r1 = 5000, r2 = 10000, r3 = 20000)

#Look at final lines
gps_vot_calc[1000:1336,]

#Plot graphs to visualise results.
par(mfrow=c(1,1))
# names(gps_vot_calc)
hist(gps_vot_calc$r1, breaks = 20)
hist(gps_vot_calc$r2, breaks = 20)
hist(gps_vot_calc$r3, breaks = 20)
hist(gps_vot_calc$v1, breaks = 20)
hist(gps_vot_calc$v2, breaks = 20)
hist(gps_vot_calc$v3, breaks = 20)

plot(gps_vot_calc$v1 , gps_vot_calc$r1, main = "5 km")
plot(gps_vot_calc$v2 , gps_vot_calc$r2, main = "10 km")
plot(gps_vot_calc$v3 , gps_vot_calc$r3, main = "20 km")

mov.param <- gps_vot_calc$r2

#THIS FOLLOWING PART IS TO DRAW A MAP WITH THE POINTS.
map.track <- function(col.points = "PuBuGn", long, lat, mov.param, asc = TRUE){
#col.points, give the colour pallete for the colour to be used for GPS points. See 'function brewer.pal in package 'RColorBrewer' for details. Acceptable arguments are: Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
#long and lat, the longitude and latitude of points.
#mov.param, the movement paramater to plot. Must be numeric vector. 
#asc, colour with darker colour for high value or low values?                      

#Required packages for maps                     
require(graphics)
require(maps)
require(mapdata)
#Package to make colour palates
require(RColorBrewer)
                      
#Make a colour scale with 100 shades.
col.graph <-    colorRampPalette(brewer.pal(9,col.points))(100)

                      
#Determine range/ bounding for x and y axis. Here we choose the range of values for which the points represent plus some margin either side of this.
long.bound <-  range(long)
long.bound[1] <- long.bound[1] -  (0.35 * abs(long.bound[2]-long.bound[1]))
long.bound[2] <- long.bound[2] +  (0.25 * abs(long.bound[2]-long.bound[1]))

lat.bound <-  range(lat)
lat.bound[1] <- lat.bound[1] -  (0.25 * abs(lat.bound[2]-lat.bound[1]))
lat.bound[2] <- lat.bound[2] +  (0.25 * abs(lat.bound[2]-lat.bound[1]))

#Draw background map
map(database = "worldHires", regions = ".", fill=TRUE, col="grey",bg = "white",bty="7", xlim = long.bound, ylim=lat.bound)

                      #Add GPS points to map, coloured by previously determined colour scale.
if(asc == TRUE){                      
points(long,lat,col = col.graph[100-abs(floor(mov.param*100))], cex = 0.4)
} else points(long,lat,col = col.graph[abs(floor(mov.param*100))], cex = 0.4)
                      
#Addd a border to the map.
box()
                      
#Add axis and labels
axis(side=(1),las=1)
axis(side=(2),las=1)
                      
#Add a scale bar
map.scale(ratio=FALSE,relwidth = 0.4, cex = 0.6)
}
             
#Map the example data
map.track(col.points = "Greys", test10min$Longitude, test10min$Latitude, mov.param = gps_vot_calc$r1, asc = TRUE)






#Brownian-bridge thing
# Kranstauber B, Kays R, LaPoint SD, Wikelski M, Safi K (2012) A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology

#First load in required package
library("move")

#Make 'move' trajectory object from example file data.
gps.track <- move(x = test10min$Longitude, y = test10min$Latitude, time= test_date_time_posix, proj=CRS("+proj=longlat"), data = test10min, animal= 1, sensor = 1)

#Plot above created object to check that it looks sensible.
plot(gps.track)

#Summary info for this.
show(gps.track)
#More summary info - not sure what units these are in - values look a bit odd. It looks like some are in metres and others in km.
summary(gps.track)

# View the help document for the 'move' package.
vignette("move")

# Brownian Bridge Movement Model Utilization Distribution
# I couldn't get this to work!! I have emailed one of the package authors for help, and I couldn't even get the example code to work!/Tom

# 
# data(state)
# states <- data.frame(state.x77, state.center)
# states <- states[states$x > -121,]
# coordinates(states) <- c("x", "y")
# proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
# summary(states)
# state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
# 



data <- move(system.file("extdata","leroy.csv.gz", package="move"))[1:80,]
# ?spTransform
data <- gps.track
data2 <- spTransform(data, CRSobj="+proj=aeqd", center=TRUE)
dBMvar <- brownian.motion.variance.dyn(object=data2, location.error=rep(23.5,n.locs(data2)), margin=13, window.size=31)



# Brownian Bridge Movement Model 

# Calculating home range thing
# install.packages("adehabitatHR")
hrBootstrap(x = gps.track, rep = 100, unin = 'km', unout = 'km2')
# ?hrBootstrap


#FPT thing


