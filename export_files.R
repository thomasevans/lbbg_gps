#NB Some of following code is based on a script by Michael Kemp for a course held in Amsterdam in 2010

#Set the work directory
setwd("F:/Documents/Work/LBBG_GPS/GIS/GPS_track_files")

#To link to database
library(RODBC)

#to get spatial functions
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
gps$loc_calc <- gps$loc_type        #keep a copy of above calculation
#Reduce to the four possibilties
gps$loc_type[(gps$loc_type == 1)  | (gps$loc_type == 12)] <- 0
gps$loc_type[gps$loc_type == 3] <- 1
gps$loc_type[gps$loc_type == 4] <- 2
gps$loc_type[(gps$loc_type == 24) | (gps$loc_type == 6) | (gps$loc_type == 8) | (gps$loc_type == 2)]<- 3

#make column for trip id, start with value 0, which will be null value - i.e. not a trip (points at the nest)
gps$trip_id <- 0

#unique(gps$device_info_serial)  #device numbers
#d<- 1   #for testing


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
    setWinProgressBar(pb, i, title=paste( round(i/n*100, 0),"% done")) #refresh the progress bar, so that we can keep note of progress.
    if(sub01$loc_type[i] == 1) x <- x+1      #if start of a trip, increment x by one
    sub01$trip_id[i] <- x                    #allocated value of x for trip_id for position 'i'.
  }

  close(pb)    #close the windows progress bar
  return(sub01$trip_id)            #output a vector for the bird of trip id
}
#**********End of this function: trip.lab



#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))


#*Calculate trip id for each device
#do this tack in parallel, here we have 8 threads, so I make a cluster of 8 threads with which to run the analysis
require(foreach)
require(doParallel)
#cl <- makeCluster(8) #use x cores
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.
registerDoParallel(cl)
#i <- 1
#x <- c(1:20)
#as.character("519")
#lst <- as.list(devices)    #to initiate the list, we make a list of n devices vectors
clusterExport(cl, c("trip.lab", "devices", "gps","lst"))   #this maybe neccessary so that the clusetered instances or R have the required vairables/ functions in their scope, i.e. those functions and vairables which are referred to within the 'foreach' function.
#lst[[1]] <- x
#for(i in seq(along = devices )lst[[i]] <- as.character(devices[i]) = 0
#NB see: http://stackoverflow.com/questions/9404881/writing-to-global-variables-in-using-dosnow-and-doing-parallelization-in-r
#There a solution is offered for exporting vairables from foreach to the global environment.
system.time({lst <- foreach(i = seq(along = devices )) %dopar%{
 # require(foreach)   #seems that this package must be called within foreach, to ensure that it is loaded in the parallel instancies of R! See: 
             
  #make a vector of trip_id called 'deviceX', where X is the device number
 # i <- d <- 8
 # a <- paste("device", d, sep="")
  x <- trip.lab(devices[i],gps)
 # assign(paste("device", d, sep=""),0)
         #    x <- c(1:20)
  #lst[[i]] <- x
  list(x) #output x as list
 # names(lst[[i]]) <- paste("device", devices[i], sep="")} #this doesn't behave as expected. Instead of labelling list 'i', it seems to make a second list (list of lists) under i.
             #perhaps it is then neccessary to somehow pass this list back to the global environment lst list? Maybe need to define the environment from which lst is e.g. somthing like: globaleviron.lst[[i]]
}
             })
#  })
#run for each individual and time how long this takes.

#close cluster
stopCluster(cl)





#get above data, and put it into vector a
#a <- get(paste("device", d, sep=""))    


#pseudocode
#for each bird add max trip id from previous bird to current trip ids
#then make normal length vector (will have to use filter thing: "gps$loc_type != 0 & gps$device_info_serial == d")
#add this vector to the global vector (other values are zero - so won't lose anything)
#add this labelled id back to the gps dataframe
#then add this as an output column in below file outputs

all.points <- c(1:length(gps$device_info_serial))
z <- 0
x <- 0
for(i in seq(along = devices )){
  z <- max(x)
  x <- unlist(lst[[i]]) + z  
  d <- devices[i]
  all.points[gps$loc_type != 0 & gps$device_info_serial == d] <- x
  }

gps$tripn <- all.points

unique(all.points)
  









#***************************************
#for each bird create a subset of the table 'gps'

#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))


gps$index <- seq(along = gps$device_info_serial)

#for testing, set value xi
#xi <- 1 

out.data <- function(xi,gps=get("gps", envir=environment(out.data)),devices=get("devices", envir=environment(out.data))){
  require(sp)
  require(rgdal)
  
  gps_sub <- subset(gps, gps$device_info_serial == devices[xi])
  # names(gps_sub)
  #long_lat <- cbind(gps_sub$longitude,gps_sub$latitude)
  
  #for testing
  # gps_sub <- gps_sub[1:20,]
  #gps_sub <- 
  
  #just take the first x lines of this for testing - faster!
  outfile <- paste("track_",devices[xi],sep = "")
  # ?SpatialPointsDataFrame
  #write to a shape file for ArcGIS
  xy <- cbind(gps_sub$longitude,gps_sub$latitude)
  #   names(xy) <- c("x","y")
  coords <- SpatialPoints(cbind(gps_sub$longitude,gps_sub$latitude))
  
  
  
  sp_df <- SpatialPointsDataFrame(coords,gps_sub,proj4string=CRS("+proj=longlat +datum=WGS84"))
  #a bit of a hack - give it a spatial reference
  proj4string(sp_df)  <- CRS("+proj=longlat +datum=WGS84")
  # write out a new shapefile (including .prj component)
  writeOGR(sp_df, ".", outfile, driver="ESRI Shapefile")
  names(sp_df)
  #  write to a csv file, for use in Excel etc.
  write.csv(gps_sub,file=paste(outfile,".csv",sep=""))
  
  #write a second csv file to be input to GPSbabel, using 'unicsv' column names and format (see: http://www.gpsbabel.org/htmldoc-development/fmt_unicsv.html)
  #first produce a dataframe in appropriate order
  #names(gps_sub)
  unicsv <- cbind(gps_sub$altitude, gps_sub$device_info_serial,gps_sub$date,gps_sub$time,gps_sub$calculated_speed,gps_sub$latitude,gps_sub$longitude,gps_sub$index,gps_sub$nest_gc_dist,gps_sub$inst_ground_speed)
  unicsv <- as.data.frame(unicsv)   #as dataframe
  names(unicsv) <- c("alt","comment","utc_d","utc_t","geschw","lat","lon","name","prox","speed")
  #to avoid errors in kml conversion, remove NAs and replace with zeroes
  unicsv[is.na(unicsv[,,])] <- 0
  #summary(is.na(unicsv[,,]))
  write.csv(unicsv,file=paste(outfile,"_gpsbabel",".csv",sep=""))
  
  
  return()
}


require(foreach)
require(doParallel)
cl <- makeCluster(8) #use x cores
registerDoParallel(cl)

system.time({foreach(i = seq(along = devices )) %dopar%
  out.data(i,gps=gps,devices=devices)}, gcFirst = TRUE)
#run for each individual and time how long this takes.


stopCluster(cl)