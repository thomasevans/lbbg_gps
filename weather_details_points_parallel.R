# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# Required packages #####
#To link to database
library(RODBC)
require(foreach)
require(doParallel)


# Database functions - get data from the database ####
#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

setwd("D:/Dropbox/R_projects/lbbg_gps")

#for all of data_base, except pre-deployment and null records
#excluding individuals with start north of 59 (i.e. those birds
#from Fågelsundet)
# gps <- sqlQuery(gps.db, query="SELECT DISTINCT t.device_info_serial, t.date_time, g.latitude, g.longitude, g.altitude, g.vnorth, g.veast, t.trip_id 
#   FROM gps_uva_tracking_speed_3d_limited AS g, lund_gps_parameters AS t
#   WHERE g.device_info_serial = t.device_info_serial
#     AND g.date_time = t.date_time
#     AND t.trip_id > 0
#   ORDER BY t.device_info_serial ASC, t.date_time ASC ;"
#                 ,as.is=TRUE)

# save("gps", file = "gps_wind_drift_analysis.RData")
# To speed things up - cached copy of GPS table above.
 load("gps_wind_drift_analysis.RData")

# str(gps)
gps <- gps[,-c(6,7,8,9)]

# Correct date_time
# gps$date_time <- as.POSIXct(gps$date_time,
#                             tz="GMT",
#                             format="%Y-%m-%d %H:%M:%S")


#For testing take just 100 points
# x <- sample(1:length(gps$date_time), 1000)
# gps <- gps[x,]


#Writing a new version which will make 40 instances dividing the data evenly between each
n <- length(gps$date_time)
s <- floor(n/40)
v <- seq(1,40*s,s)
vt <- v-1
vt <- c(vt[-1],n)


# clusterExport(cl, c("gps"))


# Make a list of available devices
devices <- sort(unique(gps$device_info_serial))


#Make cluster of number of devices instances
cl <- makeCluster(20)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  
# ?registerDoParallel
#export the gps data and trip list

clusterExport(cl, c("gps","v","vt"))  


#make a list object to recieve the data
lst <- list()



#get weather data for each flight of each trip
#Use system.time to time how long this takes.
# On 2013-09-30 took 24875 s (<7 h)
system.time({lst <- foreach(i = 1:40 ) %dopar%{
  
  # Package to extract weather data from NOAA
  require(RNCEP)
  
  #   i <- 40
  # Get device ID
  #   d <- devices[i]
  
  # Make subset of GPS points for this device.
  #   sub01 <- subset(gps, gps$device_info_serial == d)
  sub01 <- gps[v[i]:vt[i],]
#   sub01[1,]
  
  #free-up some memory
  rm("gps")
  
  #   ?NCEP.interp
  #   Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
  uwnd10 <- NCEP.interp(
    variable = "uwnd.10m",
    level = "gaussian",
    lat = sub01$latitude,
    lon = sub01$longitude,
    dt = sub01$date_time,
    reanalysis2 = FALSE,
    keep.unpacking.info = TRUE,
    interp = 'linear'
  )

  
#   #Testing to locate error
#   x <- 1
#   test.x <- rep(NA, 23792)
#   for(x in 1:23792){
#     test.x[x] <- NCEP.interp(
#     variable = "uwnd.10m",
#     level = "gaussian",
#     lat = sub01$latitude[x],
#     lon = sub01$longitude[x],
#     dt = sub01$date_time[x],
#     reanalysis2 = FALSE,
#     keep.unpacking.info = TRUE,
#     interp = 'linear'
#   )
#   }
#   x
#    gps$date_time[951100:951133]
  
  #Add values to points.weather table
  uwnd.10m <- (as.numeric(uwnd10))
  uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
  #       points.weather <- cbind(uwnd.10m,uwnd.10m.sd)
  
  
  x <- cbind(sub01$device_info_serial,sub01$date_time, uwnd.10m,uwnd.10m.sd)
  
  #   x <- cbind(1,3,4)
  #   paste("data_uwnd10m_", d, ".Rdata", sep = "")
  x.df <- as.data.frame(x)
  #   ?save
  names(x.df) <- c("device_info_serial","date_time","uwnd.10m", "uwnd.10m.sd")
  save(x.df,
       file = paste("testdata_uwnd10m_", i, ".Rdata", sep = ""))
  
  #output data as list (this will be appended to the global list, lst.
  list(x)   
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)









#Make cluster of number of devices instances
cl <- makeCluster(20)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  
# ?registerDoParallel
#export the gps data and trip list

clusterExport(cl, c("gps","v","vt"))  


#make a list object to recieve the data
lst2 <- list()



#get weather data for each flight of each trip
#Use system.time to time how long this takes.
# On 2013-09-30 took 24875 s (<7 h)
system.time({lst2 <- foreach(i = 1:40 ) %dopar%{
  
  # Package to extract weather data from NOAA
  require(RNCEP)
  
  #   i <- 1
  # Get device ID
  #   d <- devices[i]
  
  # Make subset of GPS points for this device.
  #   sub01 <- subset(gps, gps$device_info_serial == d)
  sub01 <- gps[v[i]:vt[i],]
  
  #Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
  vwnd10 <- NCEP.interp(
    variable = "vwnd.10m",
    level = "gaussian",
    lat = sub01$latitude,
    lon = sub01$longitude,  
    dt = sub01$date_time,
    reanalysis2 = FALSE,
    keep.unpacking.info = TRUE,
    interp = 'linear'
  )
  
  #Add values to points.weather table
  vwnd.10m <- (as.numeric(vwnd10))
  vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
  
  #   vwnd.10m <- 3  #test
  #   vwnd.10m.sd  <- 4  #test
  
  
  x <- cbind(sub01$device_info_serial,sub01$date_time, vwnd.10m,vwnd.10m.sd)
  
  #   x <- cbind(1,3,4)
  #   paste("data_vwnd10m_", d, ".Rdata", sep = "")
  x.df <- as.data.frame(x)
  
  names(x.df) <- c("device_info_serial","date_time","vwnd.10m", "vwnd.10m.sd")
  
  #   ?save
  save(x.df,
       file = paste("testdata_vwnd10m_", i, ".Rdata", sep = ""))
  
  #output data as list (this will be appended to the global list, lst.
  list(x)   
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)







z <- 0
for (i in 1: length(lst)){
  y <- matrix(unlist(lst[i]), ncol = 4, byrow = F)
  z <- rbind(z,y)
}

dz <- dim(z)
z2 <- z[2:dz[1],]
weather.data <- as.data.frame(z2)
names(weather.data) <- c("device_info_serial","date_time","uwnd.10m","uwnd.10m.sd")
row.names(weather.data)<-NULL
weather.data  <- weather.data[order(weather.data$device_info_serial,weather.data$date_time),]




z <- 0
for (i in 1: length(lst2)){
  y <- matrix(unlist(lst2[i]), ncol = 4, byrow = F)
  z <- rbind(z,y)
}

dz <- dim(z)
z2 <- z[2:dz[1],]
weather.data2 <- as.data.frame(z2)
names(weather.data2) <- c("device_info_serial","date_time","vwnd.10m","vwnd.10m.sd")
row.names(weather.data2)<-NULL
weather.data2  <- weather.data2[order(weather.data2$device_info_serial,weather.data2$date_time),]


final.data <- cbind(weather.data,weather.data2$vwnd.10m,weather.data2$vwnd.10m.sd)

names(final.data) <- c("device_info_serial", "date_time", "uwnd.10m", "uwnd.10m.sd", "vwnd.10m", "vwnd.10m.sd")

# x <- as.POSIXct(final.data$date_time[1:10], tz = "UTC", origin = "1970-01-01")


final.data$date_time <- as.POSIXct(final.data$date_time, tz = "UTC", origin = "1970-01-01")

row.names(final.data)<-NULL

#make sure that the data is in order
final.data   <- final.data[order(final.data$device_info_serial,final.data$date_time),]
# ?order


#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, final.data, tablename = "lund_points_weather",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(date_time = "datetime"))


# Test that all combinations of device_info_serial and datetime are unqiue - i.e. should be as we wish to use these in combination to give a primary key for the database.
# x <- unique(as.data.frame(cbind(final.data$device_info_serial,final.data$date_time)))
# x of same lenghth as original data.frame - no non-unique records


# Rescue data from above, when list writing failed ####
# for (i in seq(along = devices )){
#   
## for (i in 2:4){
#     
#   
#   # Get device ID
#   d <- devices[2]
#   
#   load(paste("data_uwnd10m_", d, ".Rdata", sep = ""))
#   
#   y <- x.df
#   load(paste("data_vwnd10m_", d, ".Rdata", sep = ""))
#   x.df <- x.df[,-c(1:2)]
#   test <- cbind(y,x.df)
#   
#   if( i == 2){
#     z <- test
#   }else{
#     z <- rbind(z,test)
#   }
#     
#   
# }
# 


