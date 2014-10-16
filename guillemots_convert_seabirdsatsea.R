# This script loads in the csv file from Vidare's program
# It then processes the file and outputs an R binnary data file (*.RData)
# and a csv file.
# Date-time is converted into an R date-time format, and geographical
# corrdinates are converted into decimal degrees.

# Read in data -----
raw.dat <- read.csv("D:/Dropbox/Guillemots/Jonas_Toms_seabird_distribution/vidare_seabird/2014101515.CSV", header = FALSE)

# Name variables
names(raw.dat)[7:9] <- c("date","lat","long")

names(raw.dat)[c(22,23,25,27,31)] <- c("time","species",
                                       "number","type","flight_dir")

# Date_time -----
# Create three columns, date, time, date-time
# Add leading zeros to times (e.g. 05:06:23 is currently 50623, to be converted
# it should be 050623)
time.fix <- sprintf("%06d", raw.dat$time)
date_time.new <- as.POSIXct(paste(raw.dat$date, time.fix), format="%d%m%Y %H%M%S", tz = "UTC")

# Check this looks ok + see when they collected data!
plot(date_time.new)

date_only <- format(date_time.new, "%Y-%m-%d")
time_only <- format(date_time.new, "%H:%M:%S")

# Lat/ long format -----

# Define a function to convert to decimal degrees
coord.fun <- function(x){
  p1 <- as.numeric(substr(x, 1, 2))
  p2 <- substr(x, 3, 6)
  p3 <- as.numeric(substr(p2, 1, 2))/60
  p4 <- as.numeric(substr(p2, 3, 4))
  
  coord <- p1 + (p3)+(p4/(100*60))
  return(coord)
}

# Convert lat
lat.new <- sapply(raw.dat$lat, coord.fun)

# Convert long
long.new <- sapply(raw.dat$long, coord.fun)

# Check this looks plausible
plot(long.new,lat.new)


# Re-format variables ----
species.new <- as.factor(raw.dat$species)
flight_dir.new <- as.factor(raw.dat$flight_dir)

type.fun <- function(x){
  if(is.na(x)){return(NA)
               }else if(x == 10) return("fly") else if(x == 20){
                 return("surf")
                 } else return(NA)
}

type.new <- sapply(raw.dat$type, type.fun)

# Combine to data frame and output to file ----
seabirds_at_sea <- cbind.data.frame(date_time.new,
                                    date_only,
                                    time_only,
                                    lat.new,
                                    long.new,
                                    species.new,
                                    flight_dir.new,
                                    raw.dat$number,
                                    type.new)

names(seabirds_at_sea) <- c(
  "date_time", "date", "time", "lat", "long",
  "species", "flight_dir", "number", "type"
  )

save(seabirds_at_sea, file = "seabirds_at_sea.RData")

write.csv(seabirds_at_sea, file = "seabirds_at_sea.csv",
          row.names = FALSE)

