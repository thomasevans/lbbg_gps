#********************************
#This script extracts data from the database, then for each trip, produces a summary, for example, greatest distance and bearing at that point, trip duration, start time, end time, number of points, gps interval


#To link to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)

#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query="SELECT DISTINCT c.*, g.longitude, g.latitude, g.altitude
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY c.device_info_serial ASC, c.date_time ASC ;"
                ,as.is=TRUE)

#for testing purposes we only take the first x lines
gps <- gps[1:100000,]

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

#if we want to check which columns are present
names(gps)

#produce a vector of trip numbers
trip_id <- sort(unique(gps$trip_id))

#get a list of available devices:
devices <- sort(unique(gps$device_info_serial))




#***********start of function: trip.info
#Function 'trip.info' produces a lists of lists of information on trips
#t - trip_id, the trip number
#gps - the gps dataframe
trip.info <- function(t, gps=gps){
  
  #make a subset of 'gps' containing just data for device, d, away from the nest.
  sub01 <- subset(gps,loc_type != 0 & device_info_serial == d,select=c(loc_type,trip_id))
  
  n <- length(sub01$loc_type) #the number gps positions
  
  
  #a windows progress bar to monitor progress
  pb <- winProgressBar(title = paste( "progress bar for device",d), min = 0,max = n, width = 300)
  
  x <- 0   #x will keep note of trip number, we start at zero.
  #loop through all gps points, labelling them with trip id (x)
  for(i in 1:n){
    setWinProgressBar(pb, i, title=paste( round(i/n*100, 0),"% done for ", d)) #refresh the progress bar, so that we can keep note of progress.
    if(sub01$loc_type[i] == 1) x <- x+1      #if start of a trip, increment x by one
    sub01$trip_id[i] <- x                    #allocated value of x for trip_id for position 'i'.
  }
  
  close(pb)    #close the windows progress bar
  return(sub01$trip_id)            #output a vector for the bird of trip id
}
#**********End of this function: trip.lab










require(foreach)
require(doParallel)
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.
registerDoParallel(cl)   #start the parellel session of R; the 'slaves', which will run the analysis.

clusterExport(cl, c("trip.lab", "devices", "gps"))   #this maybe neccessary so that the clustered instances or R have the required vairables/ functions in their scope, i.e. those functions and vairables which are referred to within the 'foreach' function.

#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.

#work out trip numbers for each device id, then add these to a list of lists (lst)
#Use system.time to time how long this takes - so far has taken around 10-15 minutes on 8 thread machine.
system.time({lst <- foreach(i = seq(along = devices )) %dopar%{
  #calculate the trip numbers for the device i. i.e. the function which we wish to run for each device.     
  x <- trip.lab(devices[i],gps)
  list(x) #output x as list
} #end of foreach functions
}) #end of things being timed by system.time

#close cluster
stopCluster(cl)


