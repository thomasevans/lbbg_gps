
# Distance to horizone

# Based on calculation here:
# http://blogs.discovermagazine.com/badastronomy/2009/01/15/how-far-away-is-the-horizon/#.VgOfzvmqpBc

x <- 0:100

y <- sqrt((x*x) + 2*6365000*x)/1000

# d2 = h2 + 2Rh

png("flight_heigh_horizon.png")
par(mar = c(6,6,2,2))
plot(y~x, ylim = c(0,35), type = "l",
     ylab = "Distance (km)",
     xlab = "Flight altitude (m)",
     cex = 2,
     cex.lab = 2.2,
     cex.axis = 1.7,
     lwd = 3)
dev.off()
