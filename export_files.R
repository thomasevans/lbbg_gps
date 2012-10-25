#NB Some of following code is copied/ edited from a script by Michael Kemp for a course held in Amsterdam in 2010

#random edit

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


gps$date_time <- as.POSIXct(gps$date_time, tz="GMT",format="%Y-%m-%d %H:%M:%S")

## Split the date and time variables into two separate vectors (code from M. Kemp)
gps$date <- unlist(strsplit(as.character(gps$date_time), split=' ')) [seq(1,((length(gps$date_time)*2)-1), by=2)]
gps$time <- unlist(strsplit(as.character(gps$date_time), split=' ')) [seq(2,((length(gps$date_time)*2)), by=2)]

names(gps)

#code to recognise trips, and add this column
gps$trip <- ifelse(gps$nest_gc_dist < 0.5, 0,1)    #label points more than 500 m from nest location
trip1 <- gps$trip +1
#make vector of next point value
trip2 <- (2* c(gps$trip[2:length(gps$trip)],0))+1
#make vector of prev point value
trip3 <- (3* c(0,gps$trip[1:(length(gps$trip)-1)]))+1
gps$loc_type <- trip1*trip2*trip3   #label points, 0 - trip, 1 - start, 2 - end, 3 - nest
gps$loc_calc <- gps$loc_type
gps$loc_type[(gps$loc_type == 1)  | (gps$loc_type == 12)] <- 0
gps$loc_type[gps$loc_type == 3] <- 1
gps$loc_type[gps$loc_type == 4] <- 2
gps$loc_type[(gps$loc_type == 24) | (gps$loc_type == 6) | (gps$loc_type == 8) | (gps$loc_type == 2)]<- 3
#summary(factor(gps$loc_type))
#summary(factor(gps$loc_calc))

total <- length(gps$loc_type[1:50])


#could improve by running for each individual seppratly, and parallelising that task. Would no longer have unique numbers for trips, but might still be useable.
#could also cut out non trip points
#at simplest, could filter out the start points of trips, number these
#then somehow add this number to the rest of a trip
names(gps)
gps$trip_id <- 0
unique(gps$device_info_serial)

#?subset
#summary(gps$loc_type != 0  & gps$device_info_serial == 519)
d<- 1
gps$trip_id <- 0
#globalenv()

trip.lab <- function(d, gps=get("gps", envir=environment(trip.lab))){
  #d is the device id
  pb <- winProgressBar(title = paste( "progress bar for device",d), min = 0,max = n, width = 300)   #a progress bar to monitor progress
  sub01 <- subset(gps,loc_type != 0 & device_info_serial == d,select=c(loc_type,trip_id))
  n <- length(sub01$loc_type)
  x <- 0
  for(i in 1:n){
    setWinProgressBar(pb, i, title=paste( round(i/n*100, 0),"% done"))
    if(sub01$loc_type[i] == 1) x <- x+1
    sub01$trip_id[i] <- x
  }
  #  summary(gps$loc_type != 0 & gps$device_info_serial == 531)
  # length(sub01$trip_id)
  assign(paste("device", d, sep=""),sub01$trip_id)
  a <- paste("device", d, sep="")
  close(pb)
  return(get(a))
}

trip.lab(531)


sub01 <- subset(gps,loc_type != 0 & device_info_serial == 531,select=c(loc_type,trip_id))
n <- length(sub01$loc_type)
#n <- n/100
pb <- winProgressBar(title =  "progress bar", min = 0,max = n, width = 300)

x <- 0
gps$trip_id <- 0
for(i in 1:n){
  setWinProgressBar(pb, i, title=paste( round(i/n*100, 0),"% done"))
  if(sub01$loc_type[i] == 1) x <- x+1
  sub01$trip_id[i] <- x
}
close(pb)


require(foreach)
require(doParallel)
cl <- makeCluster(8) #use x cores
registerDoParallel(cl)

#first make a list of available devices
devices <- sort(unique(gps$device_info_serial))


system.time({foreach(i = seq(along = devices )) %dopar%
  out.data(i,gps=gps,devices=devices)}, gcFirst = TRUE)
#run for each individual and time how long this takes.


stopCluster(cl)


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
