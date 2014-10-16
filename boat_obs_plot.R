# Gridded (rasterized) boat-based seabird observation data

# Load required packages ----
#libraries
library(raster)
library(maps)


# Load required data -----
# Observations
load("seabirds_at_sea.RData")

# Raster (for grid)
load('bsbd_raster.RData')

# Swedish coast-line data (for map)
load("SWE_adm0.RData")

# Raster bathymetry data (high-res)
bath_raster <- raster("bsbd-0.9.3.grd")



# Make raster layers ----

# Coordinates
obs.xy <- cbind(seabirds_at_sea$long,seabirds_at_sea$lat)

# Weigting criterion (number of observations)
ob.weight <- seabirds_at_sea$number

# All observations raster
all.obs <- rasterize(obs.xy,
                     bsbd_raster,
                     ob.weight,
                     fun = sum)

# All auks
# Filter observations
summary(seabirds_at_sea$species)
f <- (seabirds_at_sea$species == "URAAL") | (seabirds_at_sea$species == "ALTOR") |
  (seabirds_at_sea$species == "ALURS")

coords <- obs.xy[f,]
weights <- ob.weight[f]
auks.all <- rasterize(coords,
                     bsbd_raster,
                     weights,
                     fun = sum)

# All auks - surface
f <- ((seabirds_at_sea$species == "URAAL") |
        (seabirds_at_sea$species == "ALTOR") |
        (seabirds_at_sea$species == "ALURS")) & (
          seabirds_at_sea$type == "surf")

coords <- obs.xy[f,]
weights <- ob.weight[f]
auks.all.surf <- rasterize(coords,
                      bsbd_raster,
                      weights,
                      fun = sum)

# All auks - flight
f <- ((seabirds_at_sea$species == "URAAL") |
        (seabirds_at_sea$species == "ALTOR") |
        (seabirds_at_sea$species == "ALURS")) & (
          seabirds_at_sea$type == "fly")

coords <- obs.xy[f,]
weights <- ob.weight[f]
auks.all.fly <- rasterize(coords,
                           bsbd_raster,
                           weights,
                           fun = sum)



# Guillemots - all
f <- ((seabirds_at_sea$species == "URAAL")) 

coords <- obs.xy[f,]
weights <- ob.weight[f]
guil.all <- rasterize(coords,
                      bsbd_raster,
                      weights,
                      fun = sum)


# Guillemots - surface
f <- ((seabirds_at_sea$species == "URAAL")) & (
          seabirds_at_sea$type == "surf")

coords <- obs.xy[f,]
weights <- ob.weight[f]
guil.surf <- rasterize(coords,
                           bsbd_raster,
                           weights,
                           fun = sum)

# Guillemots - fly
f <- ((seabirds_at_sea$species == "URAAL")) & (
  seabirds_at_sea$type == "fly")

coords <- obs.xy[f,]
weights <- ob.weight[f]
guil.fly <- rasterize(coords,
                      bsbd_raster,
                      weights,
                      fun = sum)


# Razorbills - all
f <- ((seabirds_at_sea$species == "ALTOR")) 

coords <- obs.xy[f,]
weights <- ob.weight[f]
razo.all <- rasterize(coords,
                      bsbd_raster,
                      weights,
                      fun = sum)


# Razorbills - surface
f <- ((seabirds_at_sea$species == "ALTOR")) & (
  seabirds_at_sea$type == "surf")

coords <- obs.xy[f,]
weights <- ob.weight[f]
razo.surf <- rasterize(coords,
                       bsbd_raster,
                       weights,
                       fun = sum)

# Razorbills - fly
f <- ((seabirds_at_sea$species == "ALTOR")) & (
  seabirds_at_sea$type == "fly")

coords <- obs.xy[f,]
weights <- ob.weight[f]
razo.fly <- rasterize(coords,
                      bsbd_raster,
                      weights,
                      fun = sum)




# Mapping function ------
# Function for mapping the raster layers
map.obs.fun <- function(obs.ras, main = ""){
  library(RColorBrewer)
  library(maps)
  
  
  # Code for transparent colours ----
  # Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
  # Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
  addalpha <- function(colors, alpha=1.0) {
    r <- col2rgb(colors, alpha=T)
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(rgb(r[1,], r[2,], r[3,], r[4,]))
  }
  
  col.obs.transp <- addalpha((brewer.pal(9,"OrRd")), alpha = 0.70)
  
  # Batymetry colours
  bath.break.points <- c(0, -20, seq(-30,-70,-10),-90,-110,-250)
  bath.col <- rev(brewer.pal(9,"PuBu"))
  bath.col <- addalpha(bath.col, alpha = 0.5)
  
#   break.points <- c(seq(0,4,0.5),5)
#   par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 5))
  plot(gadm, col=NA, bg = NA,xlim = c(16.8, 18.2), ylim = c(56.8, 57.8))
  plot(bath_raster, colNA = NA, col = bath.col, add = TRUE ,
       breaks = bath.break.points)

  plot(obs.ras, add = T,
       col = col.obs.transp,
#        breaks = break.points,
       horizontal = TRUE)


  title(main = main, line = 3)
  map.scale(x= 17.1, y = 56.9, ratio = FALSE)
  plot(gadm, col="grey", bg = NA, add = T)
  box(,col="grey50",lwd=2)
  axis(side=(2), las=1, col="grey50", col.axis="grey50")
  axis(side=(3), las=1, col="grey50", col.axis="grey50")
  mtext("Depth (m)",
        side = 4,
        line = 1,
        las = 2,
        at = 57.65)  
  title(xlab = "Number of birds observed", line = 0)

}


# Map data and ouput to pdf -----
cairo_pdf("observations_all.pdf", onefile = TRUE)
par(mfrow=c(1,1))
map.obs.fun(obs.ras = auks.all, main = "Auks - all")
map.obs.fun(obs.ras = auks.all.fly, main = "Auks - all - flight")
map.obs.fun(obs.ras = auks.all.surf, main = "Auks - all - surface")
map.obs.fun(obs.ras = guil.all, main = "Guillemots - all")
map.obs.fun(obs.ras = guil.surf, main = "Guillemots - surface")
map.obs.fun(obs.ras = guil.fly, main = "Guillemots - flight")
map.obs.fun(obs.ras = razo.all, main = "Razorbills - all")
map.obs.fun(obs.ras = razo.surf, main = "Razorbills - surface")
map.obs.fun(obs.ras = razo.fly, main = "Razorbills - flight")
dev.off()

cairo_pdf("observations_guil_razo_comp.pdf", width = 14, height = 7,
          onefile = TRUE)
par(mfrow=c(1,2))
map.obs.fun(obs.ras = razo.surf, main = "Razorbills - surface")
map.obs.fun(obs.ras = guil.surf, main = "Guillemots - surface")
dev.off()
