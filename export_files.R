#Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
#You are welcome to use parts of this code, but please give credit when using it extensively.
#NB Some of following code is based on a script by Michael Kemp for a course held in Amsterdam in 2010

#Set the working directory
setwd("F:/Documents/Work/LBBG_GPS/GIS/GPS_track_files")

#To link to database
library(RODBC)

#To get spatial functions
library(fossil)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('F:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)


#for all of data_base, except pre-deployment and null records
gps <- sqlQuery(gps.db, query="SELECT DISTINCT g.device_info_serial, g.date_time, g.longitude, g.latitude,c.calculated_speed,c.nest_gc_dist,c.nest_bear,c.inst_ground_speed,c.p2p_dist,c.time_interval_s,c.turning_angle,g.altitude
  FROM gps_uva_tracking_limited AS g, cal_mov_paramaters AS c
  WHERE g.device_info_serial = c.device_info_serial
    AND g.date_time = c.date_time
    ORDER BY g.device_info_serial ASC, g.date_time ASC ;"
                ,as.is=TRUE)

#for testing purposes we only take the first 100 lines
#gps <- gps[1:100,]

#a hack/fix to make the date_time a POSIX object (i.e. R will now recognise this as a date-time object.
gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

#for testing
#gps.original <- gps
#gps <- gps[1:100,]

## Split the date and time variables into two separate vectors, for date and time sepprately (code from M. Kemp)
gps$date <- unlist(strsplit(as.character(gps$date_time), split=' ')) [seq(1,((length(gps$date_time)*2)-1), by=2)]
gps$time <- unlist(strsplit(as.character(gps$date_time), split=' ')) [seq(2,((length(gps$date_time)*2)), by=2)]

#if we want to check which columns are present
names(gps)

#code to recognise trips, and adds this column, labelling with 0 if 'at the nest' and 1 if over 500 m from nest location
gps$trip <- ifelse(gps$nest_gc_dist < 0.5, 0,1)

#*We want to label the positions for each trip with a unique trip id
#first we make some vectors of next, previous point etc, to find start and end points of trips
trip1 <- gps$trip +1
#make vector of next point value
trip2 <- (2* c(gps$trip[2:length(gps$trip)],0))+1
#make vector of prev point value
trip3 <- (3* c(0,gps$trip[1:(length(gps$trip)-1)]))+1


#label by type of point: 0 - trip, 1 - start, 2 - end, 3 - nest
gps$loc_type <- trip1*trip2*trip3   #product of above three vectors
loc_calc <- gps$loc_type        #keep a copy of above calculation
#Reduce to the four possibilties
gps$loc_type[(gps$loc_type == 1)  ] <- 0
gps$loc_type[gps$loc_type == 3 | (gps$loc_type == 12)] <- 1
gps$loc_type[(gps$loc_type == 24) | (gps$loc_type == 6) | (gps$loc_type == 8) | (gps$loc_type == 2)]<- 3
gps$loc_type[gps$loc_type == 4] <- 2

#make column for trip id, start with value 0, which will be null value - i.e. not a trip (points at the nest)
gps$trip_id <- 0

#summary(loc_calc==2)

#***********start of function: trip.lab
#Function 'trip.lab' will produce a vector of trip number for each device
#d - device_info_serial
#gps - the gps dataframe
trip.lab <- function(d, gps=get("gps", envir=environment(trip.lab))){
    
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


#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))

#*Calculate trip id for each device
#do this in parallel
#first load neccessary packages
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

#make a vector to which the trip numbers will be added for all individuals.
all.points <- c(1:length(gps$device_info_serial))
all.points <- 0*all.points   #make this all zero (i.e. the default value - where there is no trip)
z <- 0    #a vairable which will store the maximum trip number from the previous device
x <- 0    #a new vector of trip numbers

#Loop through all devices, making trip numbers unique, by adding number of highest numbered #existing trip, then adding this to the all.points vector, this being filtered, so that only #points in real trips get value.
for(i in seq(along = devices )){
  z <- max(x)    #highest current trip number
  x <- unlist(lst[[i]]) + z   #add z to vector of current device trip numbers
  d <- devices[i]       #device id
  all.points[gps$loc_type != 0 & gps$device_info_serial == d] <- x    #add vector x to the vector of all gps points
  }

#then add the vector 'all.points' to the 'gps' dataframe.
gps$trip_id <- all.points




#next need to add the gps$trip_id, gps$loc_type to some of the various outputs
#then should start a new script which makes a table of trips, and calculates varios summary functions for these, e.g. perhaps labelling migration trips too.
#could also move all functions into a new R file, and then source this at the beggining of each script - would clean things up here a bit.



#output the new data columns to the 'cal_mov_paramaters' database table
#put it all together in data_frame, with device_info_serial and date_time for primary keys.
export_table <- as.data.frame(cbind(gps$device_info_serial,gps$date_time,gps$trip,gps$loc_type,gps$trip_id))
#give names for columns
names(export_table) <- c("device_info_serial","date_time","pos_type","loc_type","trip_id")
#add date_time to dataframe
export_table$date_time <- gps$date_time
#export these calculated values to the database, updating existing 'cal_mov_paramaters' table.
#first added the three new columns to the table useing access
sqlUpdate(gps.db, export_table, tablename = "cal_mov_paramaters",index = c("device_info_serial","date_time"),fast=TRUE)








#***************************************
#for each bird create a subset of the table 'gps'

#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))

#Give each gps position a unique id
gps$index <- seq(along = gps$device_info_serial)



#function to output range of data files
#produce three files, a shape file for Arcgis including a spatial reference. A csv file of all the data for that device (all data columns). A second csv file in the unicsv format of GPS Babel; this file can later be converted to various other files, such as kml.
out.data <- function(xi,gps=get("gps", envir=environment(out.data)),devices=get("devices", envir=environment(out.data))){
  require(sp)
  require(rgdal)
  
  #first make a subset of the gps dataframe, containing data for device xi only.
  gps_sub <- subset(gps, gps$device_info_serial == devices[xi])
  
  #name for output file
  outfile <- paste("track_",devices[xi],sep = "")
  
  #write to a shape file for ArcGIS
  #first cbind long and lat, for a spatial object
  xy <- cbind(gps_sub$longitude,gps_sub$latitude)
  #make a spatial points object
  coords <- SpatialPoints(cbind(gps_sub$longitude,gps_sub$latitude))
  
  #now use above spatial points object (coords) and subset of gps to make a spatial points dataframe
  sp_df <- SpatialPointsDataFrame(coords,gps_sub,proj4string=CRS("+proj=longlat +datum=WGS84"))
  #a bit of a hack (because I think the above function should achieve this, but I can't get it to work!) - give it a spatial reference
  proj4string(sp_df)  <- CRS("+proj=longlat +datum=WGS84")
  
  # write out dataframe to a new shapefile (including projection, .prj, component)
  writeOGR(sp_df, ".", outfile, driver="ESRI Shapefile")
  
  #  write to a csv file, for use in Excel etc.
  write.csv(gps_sub,file=paste(outfile,".csv",sep=""))
  
  #write a second csv file to be input to GPSbabel, using 'unicsv' column names and format (see: http://www.gpsbabel.org/htmldoc-development/fmt_unicsv.html)
  #first produce a dataframe in appropriate order
  unicsv <- cbind(gps_sub$altitude, gps_sub$device_info_serial,gps_sub$date,gps_sub$time,gps_sub$calculated_speed,gps_sub$latitude,gps_sub$longitude,gps_sub$index,gps_sub$nest_gc_dist,gps_sub$inst_ground_speed)
  unicsv <- as.data.frame(unicsv)   #as dataframe
  names(unicsv) <- c("alt","comment","utc_d","utc_t","geschw","lat","lon","name","prox","speed")
  #to avoid errors in kml conversion, remove NAs and replace with zeroes
  unicsv[is.na(unicsv[,,])] <- 0
  write.csv(unicsv,file=paste(outfile,"_gpsbabel",".csv",sep=""))
    
  return()
}
#*end of out.data function
#****************************************************


#to run the 'out.data' function in parallel, to speed things up
#first neccessary packages
require(foreach)
require(doParallel)
#use x cores, general solution for any windows machine.
cl <- makeCluster(parallel::detectCores()) 

#register the cluster
registerDoParallel(cl)

#run out.data for each individual and time how long this takes.
system.time({foreach(i = seq(along = devices )) %dopar%
  out.data(i,gps=gps,devices=devices)}, gcFirst = TRUE)

#close the cluster
stopCluster(cl)
