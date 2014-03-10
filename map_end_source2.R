# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE, col="grey50",col.lab="grey50")
# ?map.scale
#   ?map.scale
box(,col="grey50",lwd=2)
axis(side=(1), las=1, col="grey50", col.axis="grey50")
axis(side=(2), las=1, col="grey50", col.axis="grey50")
# ?axis