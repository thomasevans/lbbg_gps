# Code for interpolation by time.


# Library
# install.packages("rJava")

library(xlsx)

# Apparently neccessary on Windows to get rJava working, which is required for the xlsx package.
# Sys.getenv("JAVA_HOME")
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7\\")

# Some sample data (attached)
# dat <- read.xlsx(file.choose(), 1)
dat <- read.xlsx("sampledata.xlsx",1)

# Time sequence by minute
full <- seq(dat[1,3], dat[3,3], by ="min")

# Merge with data, create missing slots where interpolation is needed
dat2 <- merge(dat, full, by.x = 3, by.y = 1, all.y = TRUE)
N <- length(dat2[,1])


# Interpolate linearly (separate for lat and long (?))
ap.x <- approx(x = dat2[,2], y = dat2[,3], n = N)$x
ap.y <- approx(x = dat2[,2], y = dat2[,3], n = N)$y


# See how this looks, black points for new points, and red asterisks for
# original points
plot(ap.x, ap.y)
points(dat[,c(1:2)], pch = 8, col = "red")

