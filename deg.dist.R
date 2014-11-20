# Function to calculate grand-circle distance. Copied from package 'fossil'. As this is the only function required from this package, it seems better to include it in a single script.
# Vavrek (2012) 'fossil' v0.3.7. Palaeoecological and Palaeogeographical Analysis Tools. http://matthewvavrek.com/programs-and-code/fossil/
deg.dist <- function (long1, lat1, long2, lat2, km = TRUE) 
{
  # Returns distance in km. Metres if km is false
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 40041.47/(2 * pi)
  d <- R * c
  if(km == FALSE){d <- (d*1000)}
  
  return(d)
}
