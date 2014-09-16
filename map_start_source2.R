source("gps_extract.R")

gps.data <- NULL
for(i in 1:length(flight.numbers)){
  x <- gps.extract(trips$device_info_serial[t.f][i],trips$start_time[t.f][i],trips$end_time[t.f][i], simple = TRUE)
  gps.data <- rbind(gps.data, x)  
}


library(maps)

#plot base map
# Set map limits
# ?range
c.xlim <- range(gps.data$longitude, na.rm = TRUE)
dif    <- c.xlim[2] - c.xlim[1]
dif    <- dif *.12
c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))

c.ylim <- range(gps.data$latitude, na.rm = TRUE)
dif    <- c.ylim[2] - c.ylim[1]
dif    <- dif *.1
c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Plot base map
load("SWE_adm0.RData")

par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))
#   par(bg = 'white')

plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="grey70", bg = "gray90",
#      xlab = "Latitude"
     )
# ?par

col.vec <- rainbow(length(trip_ids))
