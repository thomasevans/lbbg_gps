
# long = c(0,1,2)
# lat = c(2,3,4)
# for(i in 1:10){
# r1 <- 10000
# r2 <- 50000
# r3 <- 100000


#Get test data
test10min <- read.table ("C:/Users/Tom/Dropbox/jose_shearwaters/test10min.txt", head = T, dec =",")

# names(test10min)
# long <- test10min$Longitude
# lat <- test10min$Latitude


#Make vector with combined date and time. This will be character formatted
test_date_time_txt <- paste(test10min$Date, " ", test10min$Time, sep = "")
#View first 10 lines to check if it looks sensible.
# test_date_time_txt[1:10]

#Convert character formated date-time to POSIXct format (as requested by as.ltraj function. 
test_date_time_posix <- as.POSIXct(test_date_time_txt, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
#View first 10 lines
# test_date_time_posix[1:10]
# # date_time <- "sljasd"
#  date_time2 <- test_date_time_posix
# 
# date_time <- date_time2



# length(long)
# length(lat)
# length(date_time)

#Function to calculate tortoisity and '
votier_calc <- function(long, lat, date_time, r1 = 1, r2 = 10, r3 = 100){
  #Long - a vector of longitude values
  #Lat - a vector of latitude values
  #r1, r2, r3  - the radii, in km, for which values will be calculated, r3 > r2 > r1. Default values are provided (1, 10, 100 km).
  #date_time should be in POSIXct format
  
  require(fossil)
  
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
    require(fossil)
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
    
    #rx <- r2z
    #This function calculates the tortoisity and the 'derived ground speed' for each GPS point
    param_calc <- function(longx = longz[subs1], latx = latz[subs1], indexx = indexz[subs1], p2p_distx = p2p_distz[subs1], date_timex = date_timez[subs1], rx, idxx = idz, distx = dist[subs1]){
      #Takes calculated values from above, then calculates paramaters for that GPS point (id), and the defined redius (r).
      
      #package required for distance calculations
      require(fossil)
      
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
  names(all.points3) <- c("r1","v1","r2","v2","r3","v3")
  
  
  return(all.points3)
}
#End of calculation 'votier_calc'

gps_vot_calc <- votier_calc(long = test10min$Longitude, lat = test10min$Latitude, date_time = test_date_time_posix, r1 = 1000, r2 = 10000, r3 = 100000)

names(gps_vot_calc)
hist(gps_vot_calc$r1, breaks = 20)
hist(gps_vot_calc$r2, breaks = 20)
hist(gps_vot_calc$r3, breaks = 20)
hist(gps_vot_calc$v1, breaks = 20)
hist(gps_vot_calc$v2, breaks = 20)
hist(gps_vot_calc$v3, breaks = 20)

plot(gps_vot_calc$v2 ~ gps_vot_calc$r2, main = "50 km")
plot(gps_vot_calc$v1 ~ gps_vot_calc$r1, main = "5 km")
plot(gps_vot_calc$v3 ~ gps_vot_calc$r3, main = "100 km")

#Package to make colour palates
library(RColorBrewer)
#Make a colour scale for blue - 100 shades of blue
col.graph <-    colorRampPalette(brewer.pal(9,"Blues"))(100)

#Plot map with points coloured by tortoisity (dark is low straightness - i.e. high tortoisity).
plot(test10min$Latitude ~ test10min$Longitude, col = col.graph[100-floor(gps_vot_calc$r1*100)])

#As above but for speed.
plot(test10min$Latitude ~ test10min$Longitude, col = col.graph[100-floor(gps_vot_calc$v1/max(gps_vot_calc$v1)*100)])


#Plot map with points coloured by tortoisity (dark is low straightness - i.e. high tortoisity).
plot(test10min$Latitude ~ test10min$Longitude, col = col.graph[100-floor(gps_vot_calc$r2*100)])

#As above but for speed.
plot(test10min$Latitude ~ test10min$Longitude, col = col.graph[100-floor(gps_vot_calc$v2/max(gps_vot_calc$v2)*100)])



#THIS FOLLOWING PART IS TO DRAW A MAP WITH THE POINTS.

library(graphics)
library(maps)
library(mapdata)
#Package to make colour palates
library(RColorBrewer)
#Make a colour scale for blue - 100 shades of blue
col.graph <-    colorRampPalette(brewer.pal(9,"Reds"))(100)




long.bound <-  range(test10min$Longitude)
long.bound[1] <- long.bound[1] -  (0.35 * abs(long.bound[2]-long.bound[1]))
long.bound[2] <- long.bound[2] +  (0.25 * abs(long.bound[2]-long.bound[1]))

lat.bound <-  range(test10min$Latitude)
lat.bound[1] <- lat.bound[1] -  (0.25 * abs(lat.bound[2]-lat.bound[1]))
lat.bound[2] <- lat.bound[2] +  (0.25 * abs(lat.bound[2]-lat.bound[1]))

map(database = "worldHires", regions = ".", fill=TRUE,col="grey",bg = "white",bty="7", xlim = long.bound, ylim=c(23.5,27))
points(test10min$Longitude,test10min$Latitude,col = col.graph[100-(floor(gps_vot_calc$r2*100))], cex = 0.4)

box()
axis(side=(1),las=1)
axis(side=(2),las=1)
map.scale(ratio=FALSE,relwidth = 0.4, cex = 0.6)
# ?map.scale
