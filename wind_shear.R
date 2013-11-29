# Wind shear -------

# Making aproximate calculation of wind speed at median flight height
# using equation one from:
#
# Hsu SA, Meindl EA, Gilhousen DB (1994) Determining the power-law
# wind-profile exponent under near-neutral stability conditions at
# sea. Journal of Applied Meteorology 33:757–772.
#
# Formular:
# u2 = u1 (Z2 / Z1) ^ P
#
# They suggest using a value of 0.11 for P
# u1 and u2 are the reference and desired wind speed measure
# Z1 and Z2 are the reference and desired heights respectively.
#
# Here we will use the values at 10m (uwnd.10m and vwnd.10m)
# Calculating first uwnd.10m.flt.ht and vwn.10m.flt.ht
# Then with simple Pythagoras theorem, the wind velocity
# (square root of sum of the squared values for u and v)
# (3^0.11)*5.69
# 
# ((43.5/10)^0.11)
# ((17.5/10)^0.11)

wind.shear <- function(uwind10, vwind10, ht.med){
  
  # Remove negative altitude values, if x is 1 m or less, replace with 1 m
  # Leave NA values as is.
  rem.neg <- function(x){
    if(is.na(x) == FALSE){
      if(x < 1) x <- 1
    }
    return(x)
  }
  
  #   rem.neg(-1)
  
  
  #For median flight height, remove values less than 1, and replace with
  # 1. See function 'rem.neg' above.
  ht.med <- sapply(ht.med, rem.neg)
  
  #Calculate the new wind speeds for both u and v wind vectors
  uwind.new <- uwind10 * ((ht.med / 10) ^ 0.11)
  vwind.new <- vwind10 * ((ht.med / 10) ^ 0.11)
  
  
  #New wind speed, using Pythagoras theorem to calculate wind speed
  wind.new  <- sqrt((uwind.new * uwind.new) + (vwind.new * vwind.new))
  
  #Vairables to export
  vairables <- cbind(uwind.new, vwind.new, wind.new)
  
  return(vairables)
}        #end of wind.shear function
