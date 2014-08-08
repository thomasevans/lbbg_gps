# Load data ------
# Read in the text file
# Note inclusing of 'na.strings', this makes sure that '#N/A' are
# recognised as missing values by R
trips <-  read.delim(file = "trips.txt", header = TRUE, dec = ",",
           sep = "\t",na.strings = "#N/A" )

# Load library -----
library("lme4")


# Run model first ------
mod.pressure.uplift <- glmer(
  gotland_fac~sea_level_pressure*thermal_uplift_bi +
    (1|ring_number), family = binomial, data = trips)
# Get error message about 'Some predictor variables are
# on very different scales: consider rescaling'


# Re-scale the 'sea_level_pressure' variable -----
# Histogram of original variable - values are very large, ca. 100,000
hist(trips$sea_level_pressure)

# First re-scale around 0, with low pressures negative and 
# higher relative pressures positive
# value - mean(sea_level_pressure)
sea_level_pressure_rel <- trips$sea_level_pressure - mean(trips$sea_level_pressure)
hist(sea_level_pressure_rel)
# Still have relatively large and small values (-2000 to + 2000)

# Re-scale by dividing by 1000
sea_level_pressure_new <- sea_level_pressure_rel/1000
hist(sea_level_pressure_new)


# Re-run analysis using the transformed variable for sea_level_pressure -----
mod.pressure.uplift.new <-glmer(
  gotland_fac~sea_level_pressure_new*thermal_uplift_bi +
    (1|ring_number), family = binomial, data = trips)
# Now no errors!

# View details of model
summary(mod.pressure.uplift)


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



# Run mod.month.year ------
mod.month.year <- glmer(gotland_fac ~ month * year +
                          (1|ring_number), family = binomial,
                        data = trips)
# Still has errors

# Check variable structures ----
# 'gotland_fac' looks ok
summary(trips$gotland_fac)

# Month is coded as an integer (whole values), should be factor probably
str(trips$month)

# Year is also coded as an integer
str(trips$year)

# Re-code month and year as factors ----
month_fac <- as.factor(as.character(trips$month))
summary(month_fac)

year_fac <- as.factor(as.character(trips$year))
summary(year_fac)


# Re-run model with re-coded variables ----
mod.month.year.new <- glmer(gotland_fac ~ month_fac * year_fac +
                              (1|ring_number), family = binomial,
                            data = trips)
# Now no errors!

# View model details
summary(mod.month.year.new)








