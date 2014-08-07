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








