# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


#This script gets weather data for each flight, using the NCEP/NCAR Reanalysis.
#For each flight, the position at the start and the date-time then is used.
#Values are extracted for various weather factors, mainly at ground level.
#Calculations are then made on these values. For example to calculate the
#aproximate wind speed at the bird flight height using wind shear equations.



#* Should try to parellelize this program, such that it uses all available process threads. For each or somthing similar.



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

# 
# tm <- as.POSIXlt(flights$start_time)
# # tm[1:10]
# attr(tm,"tzone") <- "UTC"
# # tm[1:10]
# flights$start_time <- tm
# 
# tm <- as.POSIXlt(flights$end_time)
# # tm[1:10]
# attr(tm,"tzone") <- "UTC"
# # tm[1:10]
# flights$end_time <- tm


#Testing##########################
#make subset of the data
# flights_original <- flights
# # flights <- flights_original
# flights <- flights[1:10,]


#Get weather data #################################
#library(RNCEP)  #package to extract weather data

flights_weather <- NULL

  #Total cloud cover 'tcdc.eatm' (total cloud cover %, guassian grid)
  cloud <- NCEP.interp(variable = "tcdc.eatm", level = "gaussian",
              lat = flights$start_lat, lon = flights$start_long,  
              dt = flights$start_time,
              reanalysis2 = FALSE, keep.unpacking.info = TRUE,
              interp = 'linear')

  #Add values to flights_weather table
  tcdc.eatm <- (as.numeric(cloud))
  tcdc.eatm.sd <- (attr(cloud, which = "standard deviation"))
  flights_weather <- cbind(flights_weather, tcdc.eatm, tcdc.eatm.sd)

#system.time(x <- 400)
###Need to run everything below here:

  #Air tempreture at surface 'air.sig995' (Air tempreture at surface in deg K)
  system.time(air.temp <- NCEP.interp(variable = "air.sig995", level = "surface",
                       lat = flights$start_lat, lon = flights$start_long,  
                       dt = flights$start_time,
                       reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                       interp = 'linear'))

  #Add values to flights_weather table
  air.sig995 <- (as.numeric(air.temp))
  air.sig995.sd <- (attr(air.temp, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,air.sig995,air.sig995.sd)



#Precipitation rate at surface 'prate.sfc' (Precipitation Kg/m^2/s)
ppt <- NCEP.interp(variable = "prate.sfc", level = "gaussian",
                        lat = flights$start_lat, lon = flights$start_long,  
                        dt = flights$start_time,
                        reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                        interp = 'linear')

  #Add values to flights_weather table
  prate.sfc <- (as.numeric(ppt))
  prate.sfc.sd <- (attr(ppt, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,prate.sfc,prate.sfc.sd)

#Pressure at surface 'pres.sfc' (Pa)
sur.pres <- NCEP.interp(variable = "pres.sfc", level = "surface",
                   lat = flights$start_lat, lon = flights$start_long,  
                   dt = flights$start_time,
                   reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                   interp = 'linear')

  #Add values to flights_weather table
  pres.sfc <- (as.numeric(sur.pres))
  pres.sfc.sd <- (attr(sur.pres, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,pres.sfc,pres.sfc.sd)



#Pressure at sea level 'slp' (Pa)
sea.pres <- NCEP.interp(variable = "slp", level = "surface",
                      lat = flights$start_lat, lon = flights$start_long,  
                      dt = flights$start_time,
                      reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                      interp = 'linear')

#Add values to flights_weather table
slp <- (as.numeric(sea.pres))
slp.sd <- (attr(sea.pres, which = "standard deviation"))
flights_weather <- cbind(flights_weather,slp,slp.sd)



#Wind Speed in E-W direction 'uwnd.sig995' (ms^-1) 'near surface'
uwnd <- NCEP.interp(variable = "uwnd.sig995", level = "surface",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
uwnd.sig995 <- (as.numeric(uwnd))
uwnd.sig995.sd <- (attr(uwnd, which = "standard deviation"))
flights_weather <- cbind(flights_weather,uwnd.sig995,uwnd.sig995.sd)


#Wind Speed in N-S direction 'vwnd.sig995' (ms^-1) 'near surface'
vwnd <- NCEP.interp(variable = "vwnd.sig995", level = "surface",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
vwnd.sig995 <- (as.numeric(vwnd))
vwnd.sig995.sd <- (attr(vwnd, which = "standard deviation"))
flights_weather <- cbind(flights_weather,vwnd.sig995,vwnd.sig995.sd)



#Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
uwnd10 <- NCEP.interp(variable = "uwnd.10m", level = "gaussian",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
uwnd.10m <- (as.numeric(uwnd10))
uwnd.10m.sd <- (attr(uwnd10, which = "standard deviation"))
flights_weather <- cbind(flights_weather,uwnd.10m,uwnd.10m.sd)


#Wind Speed in N-S direction 'vwnd.10m' (ms^-1) '10 m'
vwnd10 <- NCEP.interp(variable = "vwnd.10m", level = "gaussian",
                    lat = flights$start_lat, lon = flights$start_long,  
                    dt = flights$start_time,
                    reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                    interp = 'linear')

#Add values to flights_weather table
vwnd.10m <- (as.numeric(vwnd10))
vwnd.10m.sd <- (attr(vwnd10, which = "standard deviation"))
flights_weather <- cbind(flights_weather,vwnd.10m,vwnd.10m.sd)




flights_weather <- as.data.frame(flights_weather)

str(flights_weather)

flights_weather <- cbind(flights$flight_id, flights$start_time, flights$device_info_serial, flights_weather)
str(flights_weather)

new.names <- names(flights_weather)
new.names <- new.names[4:length(new.names)]
new.names <- c("flight_id", "start_time", "device_info_serial", new.names)

names(flights_weather) <- new.names


#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights_weather, tablename = "lund_flights_weather",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL, varTypes = 
          c(start_time = "datetime"))






