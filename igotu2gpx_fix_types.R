# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to produce a new database table to
# annotate the GPS fix data from the IGU loggers
# labelling points by whether they are vallid
# GPS fixes, plus weather they include diving etc.

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Get GPS data
points <- sqlQuery(gps.db,
  query = "SELECT DISTINCT f.*
          FROM guillemots_gps_points_igu AS f
          ORDER BY f.device_info_serial ASC, f.date_time;",
  as.is = TRUE)


# Label by whether vallid GPS fix -----

# Has location?
gps_val <- (points$long != 0) & (points$lat != 0)
summary(gps_val)

# Is good quality GPS fix?
f0 <- gps_val & (points$timeout <= 100)
f1 <- gps_val & (points$ehpe <= 100)
# f2 <- gps_val & (points$timeout <= 150)
gps_ok <- f0 & f1
summary(gps_ok)
# gps_ok2 <- f0 & f2
# summary(gps_ok2)

# 
# map.trip(points = points)
# points(points$lat[!gps_ok]~points$long[!gps_ok],
#        col = "magenta", pch = 8, cex = 1.2)
# points(points$lat[!gps_ok2]~points$long[!gps_ok2],
#        col = "green", pch = 8, cex = 1.2)





# Label by 'behaviour' ------
#* 1. Flight
#* 2. Colony
#* 3. Diving (apparent)
#* 4. Water surface (swimming etc)
#* 5. Other (i.e. uncategorised)

# 1 . Flight ----
# label as flight if instantaneous speed exceeds
# 5 ms-1
flight <- points$speed_ms > 5
summary(flight)


# 2. Collony ----

# For each GPS position get distance from colony
# If GPS location is 0,0 give NA
fun.dist <- function(lat,long){
  # Define collony location
  lat.c  <- 57.289848
  long.c <- 17.958252
  
  # Get function to calculate distances
  source("deg.dist.R")
  
  if(lat == 0 | long == 0) {x <- NA} else {
    x <- deg.dist(long.c,lat.c, long,lat) 
  }
  return(x)
}

# Get distance for each point
col.dist <- mapply(fun.dist,points$lat,points$long)

# Convert distance from km to m
col.dist <- col.dist * 1000

# Have a look at this data
hist(col.dist)
hist(col.dist, breaks = 40)
hist(col.dist, breaks = 5000, xlim = c(0, 500))
# Appears that distances between ca. 0 and 100
# correspond to murre-lab - i.e. nesting - and
# distances from ca. 100 - 200 are the office (i.e.
# non-deployed loggers)
# Non-deployed logger locations should later be 
# removed by only including data from during the
# the deployment period.

# Locations within 100 m of central location in colony
col.loc <- col.dist < 100
summary(col.loc)


# 3. Diving (apparent) ----
diving <- ((points$lat == 0 ) | (points$long == 0 ) )& (points$timeout == 12)
summary(diving)

# View number of diving fixes by hour of day
hours <- as.POSIXlt(points$date_time[diving],
                    tz = "UTC")
hours.loc <- as.POSIXlt(hours, tz = "Europe/Stockholm")


hist(((as.numeric(as.POSIXlt(hours.loc)$hour)+2) %%24), xlim = c(0,24), breaks = 24,
     xlab = "Hour of day (UTC)",
     ylab = "Diving GPS fixes",
     main = "",
     col = "grey60")


# Some labelling of previous/ next points? -----
# Could be good for example to label if previous point was diving
# so as to plot points according to this.
# When plotting could get average/ intermediate position for diving
# events

# Label points where next fix is diving
# Index thing
n <- length(points$lat)
# Make an index
ind <- c(1:n)

# Diving points
ind.dive <- ind[diving]

# Previous point
dive.prev.point <- ind.dive -1

# True/ false
dive.next <- rep(FALSE,n)
dive.next[dive.prev.point] <- TRUE


# Next points where previous point is diving
# Next point
dive.next.point <- ind.dive +1

# True/ false
dive.prev <- rep(FALSE,n)
dive.prev[dive.next.point] <- TRUE
# summary(dive.next)



# Combine information to label points ----
type <- rep("unclassified", n) 

# No location
# diving
type[diving] <- "dive"

# no loc, not diving
summary(!gps_val)
summary(diving)
points.test <- points[(!gps_val & !diving),]
type[(!gps_val & !diving)] <- "no_location"

# View these points on map
    map.trip(points = points[gps_ok,])
    
    #missing points, not diving
    ind.miss <- ind[(!gps_val & !diving)]
    
    # Previous point
    ind.miss.prev <- ind.miss -1
    
    points2plot <- ind.miss.prev[points$long[ind.miss.prev] != 0]
    
    points(points$lat[points2plot]~points$long[points2plot],
           col = "green", pch = 8)
    # Appear to be nearly universally colony based locations.
    # Removing these then won't matter too much - not parts of
    # foraging trips.

summary(as.factor(type))

# With a location
# With a location but bad
bad_loc <- (gps_val & !gps_ok)
points(points$lat[bad_loc]~points$long[bad_loc],
       col = "magenta", pch = 8)
type[bad_loc] <- "bad_location"

# Flight
flight.points <- flight & !bad_loc
summary(flight.points)
type[flight.points] <- "flight"

summary(as.factor(type))


# Colony
col.points <- col.loc & !bad_loc
summary(col.points)

type[col.points] <- "colony"


# Map these points
points(points$lat[col.points]~points$long[col.points],
       col = "orange", pch = 8)


# Remaining points - both water surface and pre/ post deployment
# points - will have to remove those later (by deployment period)
# These points will be 'unclassified'
# Later can classify into 'surface' and just filter out the 
# non-deployment points



# Output to new DB table ----

# Assemble vairables to new data frame
export.table <- cbind(points$device_info_serial,
                      points$date_time)

# Add colony distance
export.table <- cbind(export.table, col.dist)

# dive next and previous
export.table <- cbind(export.table, dive.prev, dive.next)

# add points classification
export.table <- cbind(export.table, type)

# Format as data frame
export.table <- as.data.frame(export.table)
str(export.table)

# Change structure
str(col.dist)
export.table$col.dist <- col.dist


names(export.table)[1] <- "device_info_serial"
names(export.table)[2] <- "date_time"

export.table$date_time <- as.POSIXct(export.table$date_time, tz = "UTC")

str(export.table)

# Output annotation to new DB table include device_info_serial
# and date_time for primary key data





# Write to database
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.table,
        tablename = "guillemots_gps_points_igu_class",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
