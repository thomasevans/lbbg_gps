# Required packages ------
# library("RODBC")
library("sp")
# library("rgdal")
# library("rgeos")
library("raster")

# Make empty raster ----

# Distance function
source("deg.dist.R")

# Raster extent (to calculate number of rows and columns)
x1 <- 14
x2 <- 20.5
y1 <- 58
y2 <- 62

# Length of sides
left.side <- deg.dist(x1,y1,x1,y2, km = FALSE)
right.side <- deg.dist(x2,y1,x2,y2, km = FALSE)
top.side <- deg.dist(x1,y2,x2,y2, km = FALSE)
bottom.side <- deg.dist(x1,y1,x2,y1, km = FALSE)

# Number of rows and columns to get aproximate 500 m cell sizes
num.rows <- round(((left.side + right.side)/2)/500)
num.cols <- round(((top.side + bottom.side)/2)/500)

base_raster <- raster(nrows = num.rows, ncols = num.cols,
                      xmn = 14, xmx = 20.5,
                      ymn = 58, ymx = 62
                      )



# Package to operate conversions ------
# Use package 'geoconv' from: http://sourceforge.net/projects/geoconv/
# Requires 'akima' package, so first install this.
# install.packages('akima')
# 
# library('geoconv')
# ?polygonize
# base_raster_polyg <- polygonize(base_raster)
?xyFromCell
n <- length(base_raster)
ras_xy <- xyFromCell(base_raster, c(1:n))

?rasterToPolygons
ras_poly <- rasterToPolygons(base_raster, n = 4, na.rm = FALSE)

plot(ras_poly, xlim = c(17.85,17.95), ylim = c(60.55,60.58))


head(ras_poly)

str(ras_poly)



ras_xy.df <- as.data.frame(ras_xy)
row.names(ras_xy.df) <- NULL
str(ras_xy.df)
n <- length(ras_xy.df$x)
ras_xy.df$cell_id <- c(1:n)

head(ras_xy.df)

# ?save
names(ras_xy.df) <- c("lon","lat","name")
row.names(ras_xy.df) <- NULL
row.names(ras_xy.df)
?write.csv
write.csv(ras_xy.df, file = "test.csv", row.names = FALSE)
range(ras_xy.df$longitude)
range(ras_xy.df$latitude)
