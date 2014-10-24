color_legend_png <- function(legend.file,pointsize = 14,
                             col = SAGA_pal[[1]],
                             ras.val = NA,
                             zval = c(0.1,0.2,0.3,0.5,1,2,5,10,15,
                                      25,50),
                             main = "% use",
                             dig = 1){
  
  require(shape)
      
  z.min <- 10^(min(ras.val, na.rm = TRUE) - 0.1)
  if(z.min == 0){
    z.min <- 0.0001
  }
  z.max <- 10^(max(ras.val, na.rm = TRUE) + 0.1)
  
  colorlegend.c <- function (col = femmecol(100), zlim, zlevels = 5, dz = NULL, 
                             zval = NULL, log = FALSE, posx = c(0.9, 0.93), posy = c(0.05, 
                                                                                     0.9), main = NULL, main.cex = 1, main.col = "black", 
                             lab.col = "black", digit = 0, left = FALSE, ...) 
  {
    ncol <- length(col)
    par(new = TRUE)
    omar <- nmar <- par("mar")
    nmar[c(2, 4)] <- 0
    par(mar = nmar)
    emptyplot()
    pars <- par("usr")
    dx <- pars[2] - pars[1]
    xmin <- pars[1] + posx[1] * dx
    xmax <- pars[1] + posx[2] * dx
    dy <- pars[4] - pars[3]
    ymin <- pars[3] + posy[1] * dy
    ymax <- pars[3] + posy[2] * dy
    if (!is.null(zval)) {
      zz <- zval
      dz <- NULL
    }
    if (is.null(dz) & is.null(zval)) 
      if (!is.null(zlevels)) {
        if (log) {
          zz <- 10^(pretty(log10(zlim), n = (zlevels + 
                                               1)))
        }
        else zz <- pretty(zlim, n = (zlevels + 1))
      }
    else zz <- NULL
    if (!is.null(dz)) {
      if (log) 
        zz <- 10^(seq(log10(zlim[1]), log10(zlim[2]), by = dz))
      if (!log) 
        zz <- seq(zlim[1], zlim[2], by = dz)
    }
    if (log) {
      zlim <- log10(zlim)
      if (!is.null(zz)) 
        zz <- log10(zz)
    }
    zmin <- zlim[1]
    zmax <- zlim[2]
    Y <- seq(ymin, ymax, length.out = ncol + 1)
    rect(xmin, Y[-(ncol + 1)], xmax, Y[-1], col = col, border = NA)
    #   ?rect
    rect(xmin, ymin, xmax, ymax, border = lab.col)
    
    if (!is.null(zz)) {
      dx <- (xmax - xmin)
      dy <- (ymax - ymin)
      if (left) {
        Dx <- -dx
        pos <- 2
        xpos <- xmin + Dx * 0.5
      }
      else {
        Dx <- +dx
        pos <- 4
        xpos <- xmax + Dx * 0.5
      }
      Ypos <- ymin + (zz - zmin)/(zmax - zmin) * dy
      segments(xmin, Ypos, xmax, Ypos, col = lab.col)
      segments(xpos + Dx * 0.25, Ypos, xmin, Ypos, col = lab.col)
      text(xpos, Ypos, formatC(zval, digits = digit, format = "f"), 
           pos = pos, col = lab.col, ...)
    }
    
    if (!is.null(main)) {
      for (i in length(main):1) text(x = mean(c(xmin, xmax)), 
                                     y = ymax + 0.05 * (length(main) - i + 1), labels = main[i], 
                                     adj = c(0.5, 0.5), cex = main.cex, col = main.col)
    }
    par(new = FALSE)
    par(mar = omar)
  }
  
  
  
      zlim <- c(z.min,z.max)
           width <- 120
           height <- 240
      png(filename = legend.file, width = 120, height = 240, 
          bg = "transparent", pointsize = pointsize)
      par(mar = c(0.5, 0, 0.5, 4))
      plot(x = 0:5, y = 0:5, asp = 3, type = "n", axes = FALSE, 
           xlab = "", ylab = "")
      colorlegend.c(col = col, zlim = zlim, zlevels = 5, dz = NULL,
                  zval = zval, log = TRUE, posx = c(0, 0.3), 
                  posy = c(0.1, 0.7), main = main, main.cex = 1.0, 
                  main.col = "white", lab.col = "white", 
                  digit = dig, left = FALSE)
  
  
      dev.off()

}