# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps


# Extract more weather vairables for flight points

# 1. Read in flight paramaters start, end time etc... ------
#Datbase functions
#Get the flight data from the db.
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


#Get a copy of the flights DB table.
flights <- sqlQuery(gps.db, query = "SELECT DISTINCT f.*
                    FROM lund_flights_commuting AS f
                    ORDER BY f.flight_id ASC;", as.is = TRUE)


flights$start_time <- as.POSIXct(flights$start_time,
                            tz="GMT",
                            format="%Y-%m-%d %H:%M:%S")

flights$end_time <- as.POSIXct(flights$end_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

flights$mid_dist_time <- as.POSIXct(flights$mid_dist_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

# flights$mid_dist_time[1]

# for testing
# flights <- flights[1:20,]

# str(flights)

# 2. Get GPS points for all flights and merge into single table ----


# Get GPS points for flights ####

# Parallel foreach loop to get GPS data for each trip
library(doParallel)
library(foreach)

# cl <- makeCluster(1)
# registerDoParallel(cl)

# clusterExport(cl, c("flights"))  
# ?time
# str(points)
str(flights)

# flights$start_time[i]

points <- NULL
# str(flights.combined)
# Get points.old for all flights - probably ca. 40 minutes
system.time({
  points <- foreach (i = 1:length(flights$device_info_serial), .combine = rbind) %do% {
#     i <- 8
# ?foreach
    # Get gps_extract function
    source("gps_extract.R")
    x <- NA
    x <- gps.extract(flights$device_info_serial[i],
                     flights$start_time[i],
                     flights$end_time[i],
                     simple = TRUE)
    
    x <- cbind(x,i,flights$flight_id[i])
    return(x)
    #   points.old <- rbind(points.old,x)
  }
})

# warnings()

# stopCluster(cl)

# str(points)
# summary(is.na(points))
# summary(is.na(points2))

# Remove those points that are missing long/ lat
points <- points[!is.na(points$longitude),]

# Save to file
save(points, file = "points.com.flights.20131212.RData")

names(points) <- c("device_info_serial",  "date_time", "longitude",
                   "latitude", "i",          
                   "flight_id")



# 3. Get weather data - tempreture, maybe others? (air.2m) ----


#Writing a new version which will make 20 instances dividing the data evenly between each
n <- length(points$date_time)
s <- floor(n/20)
v <- seq(1,20*s,s)
vt <- v-1
vt <- c(vt[-1],n)


# Make a list of available devices
# devices <- sort(unique(points$device_info_serial))


# Make cluster of number of devices instances
cl <- makeCluster(20)

# start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


# export the gps data and trip list
clusterExport(cl, c("points","v","vt"))  

#make a list object to recieve the data
lst <- list()

#Use system.time to time how long this takes.
system.time({lst <- foreach(i = 1:20 ) %dopar%{
  
#   i <- 4
  
  # Package to extract weather data from NOAA
  require(RNCEP)
  
  sub01 <- points[v[i]:vt[i],]
  #   sub01[1,]
#   sub01 <- points[v[4]:vt[4],]
  
  #free-up some memory
  rm("points")
  

#   ?NCEP.interp
  #   Wind Speed in E-W direction 'uwnd.10m' (ms^-1) '10 m'
  air.2m.val <- NCEP.interp(
    variable = "air.2m",
    level = "gaussian",
    lat = sub01$latitude,
    lon = sub01$longitude,
    dt  = sub01$date_time,
    reanalysis2 = FALSE,
    keep.unpacking.info = TRUE,
    interp = 'linear',
    status.bar = FALSE
  )
  
  
  #Add values to points.weather table
  air.2m <- (as.numeric(air.2m.val))
  air.2m.sd <- (attr(air.2m.val, which = "standard deviation"))
  #       points.weather <- cbind(uwnd.10m,uwnd.10m.sd)
  
  
  x <- cbind(sub01$device_info_serial,sub01$date_time, air.2m, air.2m.sd)
  
  #   x <- cbind(1,3,4)
  #   paste("data_uwnd10m_", d, ".Rdata", sep = "")
  x.df <- as.data.frame(x)
  #   ?save
  names(x.df) <- c("device_info_serial","date_time","air.2m", "air.2m.sd")
  save(x.df,
       file = paste("testdata_air2m_", i, ".Rdata", sep = ""))
  
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
names(weather.data) <- c("device_info_serial","date_time","air_2m", "air_2m_sd")
row.names(weather.data) <- NULL
weather.data  <- weather.data[order(weather.data$device_info_serial, weather.data$date_time),]


# Get variables into correct format

weather.data$device_info_serial <- as.numeric(as.character(weather.data$device_info_serial))
weather.data$date_time  <- as.POSIXct(as.character(weather.data$date_time),
                                    tz="GMT",
                                    format="%Y-%m-%d %H:%M:%S")
weather.data$air_2m <- as.numeric(as.character(weather.data$air_2m))
weather.data$air_2m_sd <- as.numeric(as.character(weather.data$air_2m_sd))


str(weather.data)
# 4. Output to new DB table, flights.com.points.weather

odbcClose(gps.db)
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


#Output weather data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, weather.data, tablename = "lund_flights_com_points_weather",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(device_info_serial = "integer",
                      date_time = "datetime",
                      air_2m = "double", air_2m_sd = "double"))