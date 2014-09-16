

# Required package
library(maps)


install.packages("mapdata")

library("mapdata")

# Plot base map
load("SWE_adm0.RData")
data(world2HiresMapEnv)


pdf("overview_map_Biologging.pdf",
    width = 4,
    height = 4,
    , colormodel='cmyk')

# ?pdf

# plot(x = c(0,1))

par(mfrow=c(1,1))
par( mar = c(5, 4, 4, 2))
#   par(bg = 'white')
c.xlim <- c(-4,30)
c.ylim = c(45,65)
# data(worldMapEnv)
# ?map
data(worldHiresMapEnv)

map('worldHires', xlim = c.xlim,
    ylim = c.ylim, col="grey", bg = "white",
    fill = TRUE)
col.red <- col2rgb("red")/255
col.red <- rgb(col.red[1],col.red[2],col.red[3],alpha = 0.4)
points(17.5,57, col = "red", cex = 2,
       pch = 21, bg = col.red)
convertColor(col.red,from = "sRGB", to = "cmyk")

??cmyk
?rgb
# Scale bar and axis
# x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
# y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
# map.scale(x,y,ratio = FALSE)
#   ?map.scale
box()
axis(side=(1),las=1)
axis(side=(2),las=1)

dev.off()


# plot(gadm, xlim = c.xlim,
#      ylim = c.ylim, col="white", bg = "grey")
