# Scale bar and axis
x <- c.xlim[1] + (c.xlim[2] - c.xlim[1])/20
y <- c.ylim[1] + (c.ylim[2] - c.ylim[1])/10
map.scale(x,y,ratio = FALSE)
#   ?map.scale
box(,col="white",lwd=2)
axis(side=(1),las=1,col="white",col.axis="white")
axis(side=(2),las=1,col="white",col.axis="white")
# ?axis