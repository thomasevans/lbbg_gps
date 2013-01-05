#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.


#collect weather information, do for each GPS point (or flight?)
#wind direction, speed
#perhaps cloudcover, rain - look at what's available
#then output to new table with device_info_serial and date_time for primary keys


#Database queries #################################
library(RODBC) 

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)

#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
FROM lund_flights AS f
ORDER BY f.flight_id ASC;")

#str(flights)  #check structure


#Testing##########################
#make subset of the data
flights_original <- flights
flights <- flights_original
flights <- flights[1:10,]


#Get weather data #################################
library(RNCEP)  #package to extract weather data

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
  flights_weather <- cbind(flights_weather,tcdc.eatm,tcdc.eatm.sd)


  #Air tempreture at surface 'air.sig995' (Air tempreture at surface in deg K)
  air.temp <- NCEP.interp(variable = "air.sig995", level = "surface",
                       lat = flights$start_lat, lon = flights$start_long,  
                       dt = flights$start_time,
                       reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                       interp = 'linear')

  #Add values to flights_weather table
  air.sig995 <- (as.numeric(air.temp))
  air.sig995.sd <- (attr(air.temp, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,air.sig995,air.sig995.sd)



#Precipitation 'pr_wtr.eatm' (Precipitation kg/m^2)
ppt <- NCEP.interp(variable = "pr_wtr.eatm", level = "surface",
                        lat = flights$start_lat, lon = flights$start_long,  
                        dt = flights$start_time,
                        reanalysis2 = FALSE, keep.unpacking.info = TRUE,
                        interp = 'linear')

  #Add values to flights_weather table
  pr_wtr.eatm <- (as.numeric(ppt))
  pr_wtr.eatm.sd <- (attr(ppt, which = "standard deviation"))
  flights_weather <- cbind(flights_weather,pr_wtr.eatm,pr_wtr.eatm.sd)

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



