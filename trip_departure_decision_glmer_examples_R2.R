# Load data ------
# Read in the text file
# Note inclusing of 'na.strings', this makes sure that '#N/A' are
# recognised as missing values by R
trips <-  read.delim(file = "trips.txt", header = TRUE, dec = ",",
           sep = "\t",na.strings = "#N/A" )

# Load library -----
library("lme4")

# Re-scale the 'sea_level_pressure' variable -----

# First re-scale around 0, with low pressures negative and 
# higher relative pressures positive
# value - mean(sea_level_pressure)
sea_level_pressure_rel <- trips$sea_level_pressure - mean(trips$sea_level_pressure)

# Re-scale by dividing by 1000
sea_level_pressure_new <- sea_level_pressure_rel/1000


# Re-run analysis using the transformed variable for sea_level_pressure -----
mod.pressure.uplift.new <-glmer(
  gotland_fac~sea_level_pressure_new*thermal_uplift_bi +
    (1|ring_number), family = binomial, data = trips)



# Get R-square -----
# First install required library/ package
install.packages("MuMIn")

# Load package
library("MuMIn")

# R-square
r.squaredGLMM(mod.pressure.uplift.new)
# It gives two values
# R2m and R2c
# One is the 'marginal R-square', the other the
# 'conditional R-square'
# See explanation here
?r.squaredGLMM
# and associated paper here:
browseURL("http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210x.2012.00261.x/abstract")

r.squaredGLMM(mod.pressure.uplift.new)
# In this example the 'conditional R-square' (R2c)
# is much greater than the 'model R-square' (R2m)
# this suggests that individual birds are quite
# different, with individual explaining much of
# the variation in whether to forage on Gotland
# or not. The main-effects (here sea-level pressure
# and thermal-uplift) then explain only a small
# proportion of variance.
# Oh yeah - BTW, values are between 0 - 1, giving
# proportion of overal variance explained. If you
# multiply by 100 - you can view it has a %