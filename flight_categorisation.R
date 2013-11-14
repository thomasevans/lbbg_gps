# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script produces a table of commuting flights with start and finish times, this will be based on a function to recognis when the flight ceases to be in 'commuting' phase.

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

# i <- 5
# for all flights    i in length flights.com
# data.all <- NULL    # initialisation
# for(i in 1:length(flights.com.ind)){




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

# i <- 4
system.time({lst <- foreach(i = seq(along = flights.com$trip_flight_type)) %dopar%{
# for(i in 1:10){

# Get GPS points for these flights
source("gps_extract.R")
# library(RODBC)

# gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

# Get flight id
id <- flights.com$flight_id[i]


# Get GPS points for flight
points <- gps.extract(flights.com$device_info_serial[i],flights.com$start_time[i],flights.com$end_time[i])

# Correct data_time format
points$date_time <- as.POSIXct(points$date_time,
                                   tz="GMT",
                                   format="%Y-%m-%d %H:%M:%S")


# Categorisation ####

# Get direction - outward or inward
dir <- 1
if(flights.com$trip_flight_type[i] == "inward") dir <- -1

n <- length(points$device_info_serial)

  if(n < 5){
    data.flight <- cbind(flights.com$trip_id[i],points$device_info_serial[1],flights.com$start_time[i],flights.com$end_time[i])
#     data.all <- rbind(data.all,data.flight)
  } else {
    
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
    
    #Change in speed from previous points
    d.dif <- function(i, ds = ds){
      ds[i]/mean(ds[(i-1):(i-3)])
    }
    
    #apply function
    x <- sapply(c(4:length(ds)),d.dif,ds=ds) 
    x <- c(1,1,1,x)   #include first 3 points
    
    s <- TRUE
    p.stop <- NULL
    for(it in 1:length(x)){
      if(x[it] < 0.2 & s){
        s <- FALSE
        p.stop <- it
      }
    }
    # i
    
    
    if(dir == 1) {
        # If outward
        time.start <- points$date_time[1]
        if(is.null(p.stop)){
          time.end <- points$date_time[n]
        } else time.end <- points$date_time[p.stop]
    }else{    
        # If inward (need to reverse order again)
        time.end <- points$date_time[n]
        if(is.null(p.stop)){
          time.start <- points$date_time[1]
        } else {
            time.start <- points$date_time[(n+1) - p.stop]
          } 
    }
    
    
    data.flight <- cbind(id,points$device_info_serial[1],time.start,time.end)
#     data.all <- rbind(data.all,data.flight)
    
#     data.flight <- cbind(flights.com$trip_id[i],points$device_info_serial[1],flights.com$start_time[i],flights.com$end_time[i])
  }

list(data.flight)  

}
}) #end of system.time

             
#close cluster
stopCluster(cl)
             
flight.info <-  data.frame(matrix(unlist(lst), nrow = length(flights.com$trip_flight_type), byrow = T))
names(flight.info) <- c("trip_id","device_info_serial","start_time","end_time")             
        
flight.info$start_time <- as.POSIXct(flight.info$start_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S",
                                     origin = "1970-01-01 00:00:00")

flight.info$end_time <- as.POSIXct(flight.info$end_time,
                                     tz="GMT",
                                     format="%Y-%m-%d %H:%M:%S",
                                     origin = "1970-01-01 00:00:00")



# Then output to database