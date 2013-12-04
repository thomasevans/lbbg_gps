# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script produces a table of commuting flights with start and finish times, this will be based on a function to recognise when the flight ceases to be in 'commuting' phase.

# Get data - database  ####

# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# See what tables are available
# sqlTables(gps.db)

#Get a copy of the flights DB table.
flights.all <- sqlQuery(gps.db, as.is = TRUE, query="SELECT DISTINCT f.*
                    FROM lund_flights AS f
                    ORDER BY f.flight_id ASC;")

# Hack to set time zone back to UTC rather than system locale.
# See: http://stackoverflow.com/questions/7484880/how-to-read-utc-timestamps-from-sql-server-using-rodbc-in-r

flights.all$start_time <- as.POSIXct(flights.all$start_time,
                                 tz="GMT",
                                 format="%Y-%m-%d %H:%M:%S")

flights.all$end_time <- as.POSIXct(flights.all$end_time,
                               tz="GMT",
                               format="%Y-%m-%d %H:%M:%S")

# Make flight type a factor rather than character
flights.all$trip_flight_type <- as.factor(flights.all$trip_flight_type)
# summary(flights.all$trip_flight_type)

# Subsetting flight data ####
flights.com <- subset(flights.all, trip_flight_type == "inward"  | trip_flight_type == "outward")

#testing script, take sample
# i <- sample(length(flights.com$trip_flight_type),100)
# flights.com <- flights.com[i,]

# Index vector for flights.com
flights.com.ind <- 1:length(flights.com$flight_id)



#Run in parallel########
require(foreach)
require(doParallel)

#use x cores, general solution for any windows machine.
cl <- makeCluster(parallel::detectCores())     

# cl <- makeCluster(16)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)   


#this maybe neccessary so that the clustered instances or R have the
#required vairables/ functions in their scope, i.e. those functions
#and vairables which are referred to within the 'foreach' function.
clusterExport(cl, c("flights.com"))   

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#make a list object to recieve the data
lst <- list()

# i <- 6
  system.time({lst <- foreach(i = seq(along = flights.com$trip_flight_type)) %dopar%{
#    for(i in 1:10){
  
  # Get GPS points for these flights
  source("gps_extract.R")
  require(RODBC)
  require(fossil)
  
  gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')
  
  # Get flight id
  id <- flights.com$flight_id[i]
  
  
  # Get GPS points for flight
  points <- gps.extract(
    flights.com$device_info_serial[i],
    flights.com$start_time[i],
    flights.com$end_time[i])
  
  # Correct data_time format
  points$date_time <- as.POSIXct(points$date_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S")
  
  
  # Calculate distance from island centre
  # ?deg.dist
  karlso.cen.long   <-  17.972088
  karlso.cen.lat    <-  57.284804
  
  island.dist <- deg.dist(karlso.cen.long, karlso.cen.lat,
                          points$longitude, points$latitude)
  
  
  
  # Categorisation ####
  
  # Get direction - outward or inward
  dir <- 1
  if(flights.com$trip_flight_type[i] == "inward") dir <- -1
  
  n <- length(points$device_info_serial)
  # str(flights.com)
    if(n < 7){
      data.flight <- cbind(
        flights.com$flight_id[i],
        points$device_info_serial[1],
        flights.com$start_time[i],
        flights.com$end_time[i])
  #     data.all <- rbind(data.all,data.flight)
    } else {
#       print("hi")}
      
      # Calculate speed relative to displacement from island
      d.dist <- points$nest_gc_dist[2:n] - points$nest_gc_dist[1:(n-1)]
      d.dist <- d.dist *1000   # Get to metres
      d.speed <- d.dist/ points$time_interval_s[2:n]
      d.speed.cor <- d.speed*dir
      d.speed.cor <- c(0,d.speed.cor) #add a value for first point
      
      # Change in speed relative to island relative to last 3 points
      # Reverse point list if inward flight (work from island out)
      if(dir == -1) {
        ds <- rev(d.speed.cor)} else {
          ds <- d.speed.cor
        }
      
      
      # Find final point within island buffer
      # Reverse point list if inward flight (work from island out)
      if(dir == -1) {
        is.dis <- rev(island.dist)} else {
          is.dis <- island.dist
        }
      
      # If distance from island centre is more than 1.5 km break - i.e. keep index of first point outside of buffer.
      for(ix in 1:length(is.dis)){
        if(is.dis[ix] > 1.5) break
      }
      
      # Get index for last point inside buffer.
      if(ix > 1) ix <- ix - 1
      
      ds <- ds[ix:length(ds)]
      
      # If we have less than 7 values left, we must stop
      if(length(ds) < 6){ #print("ok")}
        data.flight <- cbind(flights.com$flight_id[i],
                             points$device_info_serial[1])
                             if(dir == 1){
                               data.flight2 <- cbind(
                                 points$date_time[ix],
                                 flights.com$end_time[i])
                             } else {
                                data.flight2 <- cbind(
                               flights.com$start_time[i],
                               points$date_time[n-ix])                             
                             }
        data.flight <- cbind(data.flight, data.flight2)
        } else{
        
        #Change in speed from previous points
        d.dif <- function(ia, ds = ds){
          mean(ds[(ia ):(ia + 2)]) / mean(ds[(ia - 1):(ia - 3)])
        }
#         z <- 6
        
        thresh <- 0.3
        
        #apply function
        x <- sapply(c(3:(length(ds)-2)), d.dif, ds = ds)
        if (x[1] < thresh) {z <- thresh}else{ z <- x[1]}
        x <- c(rep(z,1),x,rep(x[length(x)],2))   #include first 3 points and final points
        
#         s <- TRUE
        p.stop <- NULL
        for(it in 1:length(x)){
          if(x[it] < thresh ){
#             s <- FALSE
            p.stop <- it
            break
          }
        }
        # i
        
        
        if(dir == 1) {
#             print(paste(i,"Out flight    "))
            # If outward
            time.start <- points$date_time[ix]
            if(is.null(p.stop)){
              time.end <- points$date_time[n]
            } else time.end <- points$date_time[p.stop]
        }else{    
#           print(paste(i,"In flight    "))
          # If inward (need to reverse order again)
            time.end <- points$date_time[n - ix + 1]
#             points$date_time[n]
            if(is.null(p.stop)){
              time.start <- points$date_time[1]
            } else {
                time.start <- points$date_time[n - p.stop + 1]
              } 
        }
        
          data.flight <- cbind(
            id, points$device_info_serial[1],
            time.start, time.end)
          
     #     data.all <- rbind(data.all,data.flight)
#         as.POSIXct(data.flight[3],
#                    tz="GMT",
#                    format="%Y-%m-%d %H:%M:%S",
#                    origin = "1970-01-01 00:00:00")
#         
#         as.POSIXct(data.flight[4],
#                    tz="GMT",
#                    format="%Y-%m-%d %H:%M:%S",
#                    origin = "1970-01-01 00:00:00")
# #         
#         flights.com[i,3]
#         flights.com[i,4]
        
    #     data.flight <- cbind(flights.com$trip_id[i],points$device_info_serial[1],flights.com$start_time[i],flights.com$end_time[i])
    }
    }
  
  # If for some reason the end time is earlier than the start time, use original classification values
  if(data.flight[3] > data.flight[4]){
    data.flight <- cbind(
      flights.com$flight_id[i],
      points$device_info_serial[1],
      flights.com$start_time[i],
      flights.com$end_time[i])
  }
  
#   data.flight$start_time[i]
#   flights.com$end_time[i]
  # Get mid-time
#   pointsx$date_time ==data.flight[3]
  # Find mid-point of flight by distance
  pointsx <- subset(points, date_time >= data.flight[3] &
                      date_time <=   data.flight[4])
  n <- length(pointsx$device_info_serial)
  
  final.dist <- deg.dist(pointsx$longitude, pointsx$latitude,
                          points$longitude[n], points$latitude[n])
  dist.rat <- final.dist/final.dist[1]
  dist.val <- dist.rat < 0.5
  
  dist.close <- which((dist.val))
  first.close <- min(dist.close)
  
  mid.time <- pointsx$date_time[first.close]
  data.flight <- cbind(data.flight, mid.time)
  
#   lst[[i]] <- list(data.flight)  
  list(data.flight)  
#   warnings()
  }
}) #end of system.time

             
#close cluster
stopCluster(cl)
      

# flight.info <-  data.frame(matrix(unlist(lst), nrow = 10, byrow = T))

flight.info <-  data.frame(matrix(unlist(lst), nrow = length(flights.com$trip_flight_type), byrow = T))
names(flight.info) <- c("flight_id","device_info_serial","start_time","end_time","mid_dist_time")             
        
flight.info$start_time <- as.POSIXct(flight.info$start_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S",
                                     origin = "1970-01-01 00:00:00")

flight.info$end_time <- as.POSIXct(flight.info$end_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S",
                                     origin = "1970-01-01 00:00:00")

flight.info$mid_dist_time <- as.POSIXct(flight.info$mid_dist_time,
                                   tz="GMT",
                                   format="%Y-%m-%d %H:%M:%S",
                                   origin = "1970-01-01 00:00:00")

# flights.com[1:10,]
# flight.info.back <- flight.info

# str(flight.info)

# Re-order before writing to database
flight.info  <- flight.info[order(flight.info$device_info_serial,flight.info$flight_id),]
# Remove row numbers
row.names(flight.info) <- NULL


# Then output to database

odbcClose(gps.db)
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')


#Output data to database #####
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flight.info, tablename = "lund_flights_commuting",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(device_info_serial = "integer",
                      flight_id = "integer",
                      start_time = "datetime",
                      end_time   =  "datetime"))

beep <- function(n = 9){
  x <- c(1,1,3,1,1,3,1,1,3,1,1)
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(x[i])
  }
}
beep()
