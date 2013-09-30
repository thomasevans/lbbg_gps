# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


#This script gets weather data for each flight, using the NCEP/NCAR Reanalysis.
#For each flight, the position at the start and the date-time then is used.
#Values are extracted for various weather factors, mainly at ground level.
#Calculations are then made on these values. For example to calculate the
#aproximate wind speed at the bird flight height using wind shear equations.

#*** A new version of weather_details, now written to take advantage of parallel functions, so as to speed up execution




#Required packages
library(RODBC) 
library(RNCEP)

#Database queries #################################


#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
  FROM lund_flights AS f
  ORDER BY f.flight_id ASC ;"
                    ,as.is=TRUE)

# flights$start_time
flights$start_time  <- as.POSIXct(flights$start_time,
                                  tz="GMT",
                                  format="%Y-%m-%d %H:%M:%S")


flights$end_time  <- as.POSIXct(flights$end_time,
                                tz="GMT",
                                format="%Y-%m-%d %H:%M:%S")



#testing - take a subset of the data
# flights.original <- flights
# flights <- flights.original
# flights <- flights.original[c(1:10,20000:20010,30010:30025),]

# str(flights)

weather.info <- function(d,flights){
  
  require(RNCEP)
  
#    d <- 624
  
  sub01 <- subset(flights, flights$device_info_serial == d)
#   flights_weather <- NA
  
  #Total cloud cover 'tcdc.eatm' (total cloud cover %, guassian grid)
  cloud <- NCEP.interp(variable = "tcdc.eatm", level = "gaussian",
                       lat = sub01$start_lat, lon = sub01$start_long,  
                       dt = sub01$start_time,
                       reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                       interp = 'linear')
  
  #Add values to flights_weather table
  tcdc.eatm <- (as.numeric(cloud))
  tcdc.eatm.sd <- (attr(cloud, which = "standard deviation"))
  flights_weather <- cbind(sub01$flight_id, sub01$start_time,
                           sub01$device_info_serial, tcdc.eatm,
                           tcdc.eatm.sd)

  
  
  air.temp <- NCEP.interp(variable = "air.sig995", level = "surface",
                          lat = sub01$start_lat, lon = sub01$start_long,  
                          dt = sub01$start_time,
                          reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                          interp = 'linear')
  
  #Add values to flights_weather table
  air.sig995 <- (as.numeric(air.temp))
  air.sig995.sd <- (attr(air.temp, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,air.sig995,air.sig995.sd)
  
  
  
  #Precipitation rate at surface 'prate.sfc' (Precipitation Kg/m^2/s)
  ppt <- NCEP.interp(variable = "prate.sfc", level = "gaussian",
                     lat = sub01$start_lat, lon = sub01$start_long,  
                     dt = sub01$start_time,
                     reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                     interp = 'linear')
  
  #Add values to flights_weather table
  prate.sfc <- (as.numeric(ppt))
  prate.sfc.sd <- (attr(ppt, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,prate.sfc,prate.sfc.sd)
  
  #Pressure at surface 'pres.sfc' (Pa)
  sur.pres <- NCEP.interp(variable = "pres.sfc", level = "surface",
                          lat = sub01$start_lat, lon = sub01$start_long,  
                          dt = sub01$start_time,
                          reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                          interp = 'linear')
  
  #Add values to flights_weather table
  pres.sfc <- (as.numeric(sur.pres))
  pres.sfc.sd <- (attr(sur.pres, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,pres.sfc,pres.sfc.sd)
  
  
  
  #Pressure at sea level 'slp' (Pa)
  sea.pres <- NCEP.interp(variable = "slp", level = "surface",
                          lat = sub01$start_lat, lon = sub01$start_long,  
                          dt = sub01$start_time,
                          reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                          interp = 'linear')
  
  #Add values to flights_weather table
  slp <- (as.numeric(sea.pres))
  slp.sd <- (attr(sea.pres, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,slp,slp.sd)
  
  
  
  #Wind Speed in E-W direction 'uwnd.sig995' (ms^-1) 'near surface'
  uwnd <- NCEP.interp(variable = "uwnd.sig995", level = "surface",
                      lat = sub01$start_lat, lon = sub01$start_long,  
                      dt = sub01$start_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')
  
  #Add values to flights_weather table
  uwnd.sig995 <- (as.numeric(uwnd))
  uwnd.sig995.sd <- (attr(uwnd, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,uwnd.sig995,uwnd.sig995.sd)
  
  
  #Wind Speed in N-S direction 'vwnd.sig995' (ms^-1) 'near surface'
  vwnd <- NCEP.interp(variable = "vwnd.sig995", level = "surface",
                      lat = sub01$start_lat, lon = sub01$start_long,  
                      dt = sub01$start_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')
  
  #Add values to flights_weather table
  vwnd.sig995 <- (as.numeric(vwnd))
  vwnd.sig995.sd <- (attr(vwnd, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,vwnd.sig995,vwnd.sig995.sd)
  
  
  
  #Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
  uwnd10 <- NCEP.interp(variable = "uwnd.10m", level = "gaussian",
                        lat = sub01$start_lat, lon = sub01$start_long,  
                        dt = sub01$start_time,
                        reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                        interp = 'linear')
  
  #Add values to flights_weather table
  uwnd.10m <- (as.numeric(uwnd10))
  uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,uwnd.10m,uwnd.10m.sd)
  
  
  #Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
  vwnd10 <- NCEP.interp(variable = "vwnd.10m", level = "gaussian",
                        lat = sub01$start_lat, lon = sub01$start_long,  
                        dt = sub01$start_time,
                        reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                        interp = 'linear')
  
  #Add values to flights_weather table
  vwnd.10m <- (as.numeric(vwnd10))
  vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,vwnd.10m,vwnd.10m.sd)
    
    
  
  # Output a matrix for the bird of weather variables
  return(flights_weather)  
  
}

# str(flights)
# x <- NA
# x <- weather.info(624,flights)

#Run function 'weather.info' in parallel########
require(foreach)
require(doParallel)

#use x cores, general solution for any windows machine.
cl <- makeCluster(parallel::detectCores())     

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)   


# Make a list of available devices
devices <- sort(unique(flights$device_info_serial))

#this maybe neccessary so that the clustered instances or R have the
#required vairables/ functions in their scope, i.e. those functions
#and vairables which are referred to within the 'foreach' function.
clusterExport(cl, c("flights","devices"))   

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#make a list object to recieve the data
lst <- list()




#get weather data for each flight of each trip
#Use system.time to time how long this takes.
system.time({lst <- foreach(i = seq(along = devices )) %dopar%{
  
  #calculate the trip numbers for the device i. i.e. the function 
  #which we wish to run for each device.     
  x <- weather.info(devices[i],flights)
  x <- t(x)
  
  #output data as list (this will be appended to the global list, lst.
  list(x)   
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)

weather.data <-  data.frame(matrix(unlist(lst), nrow = length(flights$flight_id), byrow = T))

# 
# y <- data.frame(matrix(unlist(x), nrow = 6, byrow = F))
# names(y) <- c("flight_id", "start_time",
#               "device_info_serial","tcdc.eatm",
#               "tcdc.eatm.sd", "air.sig995",
#               "air.sig995.sd", "prate.sfc",
#               "prate.sfc.sd", "pres.sfc",
#               "pres.sfc.sd", "slp", "slp.sd",
#               "uwnd.sig995", "uwnd.sig995.sd",
#               "vwnd.sig995", "vwnd.sig995.sd",
#               "uwnd.10m", "uwnd.10m.sd",
#               "vwnd.10m", "vwnd.10m.sd")



# Need to add column headers
names(weather.data) <- c("flight_id", "start_time",
                         "device_info_serial","tcdc.eatm",
                         "tcdc.eatm.sd", "air.sig995",
                         "air.sig995.sd", "prate.sfc",
                         "prate.sfc.sd", "pres.sfc",
                         "pres.sfc.sd", "slp", "slp.sd",
                         "uwnd.sig995", "uwnd.sig995.sd",
                         "vwnd.sig995", "vwnd.sig995.sd",
                         "uwnd.10m", "uwnd.10m.sd",
                         "vwnd.10m", "vwnd.10m.sd")


weather.data$start_time <- as.POSIXct(weather.data$start_time, tz = "UTC", origin = "1970-01-01")


#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, weather.data, tablename = "lund_flights_weather",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(start_time = "datetime"))
