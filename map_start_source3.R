source("gps_extract.R")

# gps.data <- NULL
# for(i in 1:length(flights.in$device_info_serial)){
#   x <- gps.extract(flights.in$device_info_serial[i],
#                    flights.in$start_time[i],
#                    flights.in$end_time[i], simple = TRUE)
#   gps.data <- rbind(gps.data, x)  
# }


library(maps)

#plot base map
# Set map limits
# ?range
# c.xlim <- range(gps.data$longitude, na.rm = TRUE)
# dif    <- c.xlim[2] - c.xlim[1]
# dif    <- dif *.12
# c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
# 
# c.ylim <- range(gps.data$latitude, na.rm = TRUE)
# dif    <- c.ylim[2] - c.ylim[1]
# dif    <- dif *.1
# c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))

# Plot base map
load("SWE_adm0.RData")



c.xlim <- c(17.0,18.5)
c.ylim <- c(56.4,57.6)
par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))
#   par(bg = 'white')

plot(gadm, xlim = c.xlim,
     ylim = c.ylim, col="white", bg = "grey")

# plot(gadm, xlim = c.xlim,
#      ylim = c.ylim, col="grey70", bg = "gray90",
#           xlab = "Latitude"
# )
# ?par
# ?rainbow
col.vec <- rainbow(length(flights.in$device_info_serial),
                   alpha = 0.4)
