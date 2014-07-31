
# Read in data -----
# First read in the data-table
# Best to use the file ending 'RData', as this is already
# an R object, so you shouldn't need to change data types
# etc.

# Set the data directory where the data resides
# Remember that R doesn't like back-slashes, either
# replace with forward-slashes(/), or a doulble back-slash (\\)
setwd("D:/Dropbox/LBBG_agriculture/departure_decision")

# Now read in the data file
load("foraging_trip_info_filtered.RData")

# After reading this file in a new object 'trips.f' should
# be available in your workspace

str(trips.f)

# Decide on a cut-off/ threshold value for amount of time
# on Gotland to be identified as a Gotland foraging trip
# See distribution of data
hist(trips.f$gotland_time_prop)

# Increase resolution, and limit y-axis
hist(trips.f$gotland_time_prop, breaks = 20, ylim = c(0,200))

# Somewhere between 20 and 50 % would look ok. Majority of
# non-Gotland trips are <10 %, and majority of Gotland trips
# are >50 %. So setting the threshold anywhere between these
# values should be ok.


# Create new variable for trip type (Gotland or not)
gotland_trip <- trips.f$gotland_time_prop > 0.2
summary(gotland_trip)

# Depending how you are using the variable, you
# might need to convert it to a factor
gotland_trip_fac <- as.factor(gotland_trip)
summary(gotland_trip_fac)


# You can also add these two new variables to the existing
# data frame (trips.f)
trips.f <- cbind(trips.f, gotland_trip, gotland_trip_fac)


# Some example plots
# Proportion of trips to Gotland by month
plot(gotland_trip_fac ~ as.factor(month), data = trips.f)

# By year
plot(gotland_trip_fac ~ as.factor(year), data = trips.f)


# Sea level pressure
plot(sea_level_pressure ~ (gotland_trip_fac) , data = trips.f)
# Looks like Gotland trips are more likely at lower
# air pressures.

# Sex
plot((gotland_trip_fac) ~ sex_tentative,  data = trips.f)
# There doesn't seem to be a big difference between the proportion of trips to Gotland by sex

