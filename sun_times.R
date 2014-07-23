# Sunrise/ set test script

install.packages("maptools")

library("maptools")


?sunriset

sunriset(as.POSIXct(Sys.time(), "GMT"),
         matrix((c(17.972088, 57.284804)), nrow = 1),
         direction = "sunrise", POSIXct.out = TRUE)


date_start <- as.POSIXct("2011-01-01", tz="UTC")
date_seq <- seq(from = date_start,
                length.out = 365*4, by = "days")

pos <- matrix(c(17.972088, 57.284804), nrow = 1)
pos.sp <- SpatialPoints(pos,
                        proj4string = 
                          CRS("+proj=longlat +datum=WGS84"))



up <- sunriset(pos.sp, date_seq,
               direction = "sunrise", POSIXct.out = TRUE)

down <- sunriset(pos.sp, date_seq,
               direction = "sunset", POSIXct.out = TRUE)

str(up)
