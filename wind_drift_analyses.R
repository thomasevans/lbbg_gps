

#Copied from 'flight_movement_calculations'






#Wind effect/ drift analysis########################

#Wind drift analysis:
#Equation:  y = (w.sin(b))/Va
#y  - angle between track and heading (drift)
#w  - wind speed
#b  - angle between track and wind (with 0 a tail wind)
#Va - air speed (need to assume this)

#need to calculate 'b', and look up 'Va'.

# names(flights.characteristics)
# names(flights.weather)
# names(flights)

# Airspeed - we can try a range from 10, 12, 14 perhaps, covering likely value range.
# Values for Karlsö, according to current analysis appear a bit low.


# Calculate angle between Track and Wind vector (beta)

#Difference between wind direction and flight direction
beta  <- (flights.characteristics$wind.dir - flights$bearing_a_b)
hist(beta)

# Absolute difference
beta  <- abs(beta)
hist(beta)

angle.dif <- function(x){
  #Function to find difference between flight and wind direction
  #Range from 0 - 180
  if(is.na(x)) return (NA)
  else{
    if(x > 180) {return (360 - x)}
    else return (x)
  }
}

angle.dif(NA)



#Use 'angle.dif' function to calculate actual alpha value
beta  <- sapply(beta, angle.dif)
hist(beta)

#y = (w.sin(b))/Va
#Air speed
Va <- 12

y  <- (flights.characteristics$wind.10m.flt.ht * sin(rad(beta))) / Va
y.rad <- y
y.deg <- deg(y)
alpha <- y.deg

hist(y.deg)

wind.vec <-  sapply(flights$bearing_a_b, angle.dif)
hist(wind.vec)

track.vec <-  sapply(flights.characteristics$wind.dir, angle.dif)
hist(track.vec)


# Output to database #####
plot(alpha , track.vec)
points(alpha, wind.vec, col = "red")
reg1 <- lm(alpha ~ track.vec)
reg2 <- lm(alpha ~ wind.vec)
abline(reg1)
abline(reg2, col = "red")


#Inward flights
plot(alpha[inward] , flights$bearing_a_b[inward])
points(alpha[inward], beta[inward], col = "red")





#Fast directional flight classificiation############
#**Perhaps save this for a sepperate script doing direct comparisons.

#Sepperate out fast directional flight from not obviously
#directional flight. Required for drift analysis where we
#only want to analyse directional flight.
