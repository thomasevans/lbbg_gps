# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.


# Explanation text -----
# This is script is to filter the trip_departure_decision
# data to retain only relevant trips. For example it will
# only include trips of a minimum time duration and distance
# plus will exclude trips of too great a time duration.
# Further it filters by periods, so we only retain data
# for relevant months to our analysis (May, June, July),
# months during the core breeding season.

# Read in data -----
# First read in the data-table
# Best to use the file ending 'RData', as this is already
# an R object, so you shouldn't need to change data types
# etc.

# Set the data directory where the data resides
# Remember that R doesn't like back-slashes, either
# replace with forward-slashes(/), or a doulble back-slash (\\)
setwd("D:/Dropbox/LBBG_agriculture/departure_desision")

# Now read in the data file
load("foraging_trip_info.RData")

# After reading this file in a new object 'trips' should
# be available in your workspace

# Inspect data ----
# Check that this looks right
# See vairables and their data types
str(trips)

# See table itself (note upper case V in view)
View(trips)

# Filter data ----

# *trip_type ----
# 1/0, 1 - migratory, 0 - non-migratory
# First filter out migratory trips.
# Note double equals sign to indicate 'is equal to'
f0 <- trips$trip_type == 0
# 8 trips are now excluded 'FALSE'
summary(f0)


# *fix_n ----
# Fix number - Number of GPS fixes for a trip
# First see the distribution and don't include
# migratory trips (use the filter we made above)
hist(trips$fix_n[f])

# Just to see trips with relatively few GPS locations
hist(trips$fix_n[f & trips$fix_n < 1000])
hist(trips$fix_n[f & trips$fix_n < 100])
hist(trips$fix_n[f & trips$fix_n < 15])


# There's no obvious cut-off (bi-modal distribution etc)
# Therefor I suggest just using a 'reasonable' value,
# perhaps 8 fixes, which should be adequate for this
# analysis where we want to know things like - did they go
# to Gotland, aproximatly how much time on Gotland. What
# was the furthest point they reached?

# Create filter to exclude trips with less than 8 points
# Retain trips with 8 or more GPS locations (fixes)
f1 <- trips$fix_n >= 8
summary(f1)

# We can combine with the previous filter using 'AND'
# operator (&)
f <- f0 & f1
summary(f)


# *duration_s ----
# The duration of a trip in seconds
# For a trip to be a 'foraging trip' it should have
# some minimum duration, plus also a maximum.
# We only want to include 'central-place foraging'
# originating from the breeding colony at Stora
# Karlsö.

# First have a look at the data to get an idea of what
# might be sensible thresholds.

# Maximum duration
hist(trips$duration_s[f])

# One-week is almost certainly too much, but we can have a
# look at durations of trips of one-week or less.
# First calculate 1-week in seconds
# 7 days, 24 hours, 60 mins, 60 s
week <- 7 * 24 * 60 * 60
day <- 24 * 60 * 60
hist(trips$duration_s[f & trips$duration_s < week])
# Add lines for days
abline(v = c(day, day *2, day * 3, day *4, day *5),
       lty = 2, lwd = 2, col = "red")

# View 3-days
hist(trips$duration_s[f & trips$duration_s < 3*day],
     ylim = c(0,200))

# 110000 appears a potentially good threshold
abline(v = 110000,
       lty = 2, lwd = 2, col = "red")
# What is this in days?
110000/day
# Just over 1-day. Perhaps unlikely for a chick-rearing trip
# but possible for an incubation stint. We'll take this for
# the maximum time duration.
f3 <- trips$duration_s < 110000
summary(f3)
f <- f & f3
# We've now exluced one-third of trips!
summary(f)

# Minimum time duration
hist(trips$duration_s[f])
# View trips less than 5 hours
hour <- 60 * 60
hist(trips$duration_s[f & trips$duration_s < 5 * hour])
# Lets reduce the bin width
hist(trips$duration_s[f & trips$duration_s < 5 * hour],
     breaks = 40)
# There's not a really obvious cut-off. However there is
# a drop in the number of trips per a time duration below
# ca. 3500 seconds.
abline(v = 3500, lty = 2, lwd = 2, col = "red")
# What is this in hours?
3500 / hour
# Just under an hour - that seems a reasonable time duration,
# probably enough to travel a short distance forage a bit,
# then return

# Minimum time duration filter
f4 <- trips$duration_s > 3500
summary(f4)
f <- f & f4
summary(f)


# *dist_max ----
# The maximum distance (km) reached from the nest
# It's seems sensible to have a minimum distance threshold.
# If the bird only travels say 1 km, perhaps it is roosting
# on the beach further down the coast.
# Let's first have a look at what this looks like
hist(trips$dist_max[f])
hist(trips$dist_max[f], breaks = 20)

# Look at trips of less than 150 km
hist(trips$dist_max[f & trips$dist_max < 150], breaks = 20)

# Less than 20 km
hist(trips$dist_max[f & trips$dist_max < 20], breaks = 20)
# Here there is a fairly clear potential cut-off. There are
# quite a number of trips of 0-1 km and 1 - 2 km, but few
# 2 - 4 km, then a gradual increase.
# Let's then suggest a threshold of 3 km
abline(v = 3, lty = 2, lwd = 2, col = "red")

f5 <- trips$dist_max > 3
summary(f5)

f <- f & f5
summary(f)


# *interval_max ----
# Maximum time interval between GPS locations
# We don't want to include trips with long data gaps.
# If there is a very long gap there could potentially
# be two trips included within one. Given that the 
# algorithm to dettect trips works simply on starting 
# a trip when the bird leaves the island to when it
# returns to the island.

# First view this paramater
hist(trips$interval_max[f])
# Cut-down to less than 10 hours (using the hour that we
# defined earlier - the number of seconds in an hour)
hist(trips$interval_max[f & trips$interval_max < 10 * hour])
# 5 hours
hist(trips$interval_max[f & trips$interval_max < 5 * hour])

# 1 hour
hist(trips$interval_max[f & trips$interval_max < hour],
     breaks = 20)
# Indicated half an hour (red)
abline(v = 0.5 * hour, lty = 2, lwd = 2, col = "red")
# 15 minutes (blue)
abline(v = 0.25 * hour, lty = 2, lwd = 2, col = "blue")

# I think for this analysis 30 minutes should be adequate,
# for most trips this shouldn't miss too much.
f6 <- trips$interval_max < 0.5 * hour
summary(f6)

f <- f & f6
summary(f)


# *month-----
# Months are stored as text, as a factor. They are numbered.
# 01 - January
# 02 - February, etc. ...

# Filter to include only trips during May (05), June (06), and July (07)
f7 <- trips$month == "05" |  trips$month == "06" |  trips$month == "07"

summary(f7)
# Add to existing filter
f <- f & f7
# See how many trips we now have (TRUE)
summary(f)

# Summary ----
# We applied the following 7 filters:
# Non-migration trips
f0 <- trips$trip_type == 0  

# At least 8 GPS locations
f1 <- trips$fix_n >= 8  

# Trips of less than ca. 31 hours
f3 <- trips$duration_s < 110000  

# Trips of at least ca. 1 hour
f4 <- trips$duration_s > 3500  

# Trips where the furthest point reached is at least 3 km
f5 <- trips$dist_max > 3   

# Trips with no more than 30 minutes between GPS locations
f6 <- trips$interval_max < 0.5 * hour

# Trips during months of May, June, and July only
f7 <- trips$month == "05" |  trips$month == "06" |  trips$month == "07"

length(trips$fix_n)
# We started with 3283 trips ...
summary(f)
# ... and ended up with 1427 trips that met the above criteria

# Sample sizes ----
# First install package 'reshape2' which makes this
# quite easy to look at
install.packages("reshape2")
library(reshape2)

# Number of trips per individual
aggregate(fix_n ~ ring_number,
          data = trips,
          FUN = length)

# Number of trips per year
aggregate(fix_n ~ year,
          data = trips,
          FUN = length)

# Number of trips per month
aggregate(fix_n ~ month,
          data = trips,
          FUN = length)

# Number of trips per individual by year
aggregate(fix_n ~ year + ring_number ,
          data = trips,
          FUN = length)

# Number of individuals
length(unique(trips$ring_number))

# Output filtered data ----
# For later analysis we can save the filtered data

# Output to an R binnary file
# Filter the trips data frame
trips.f <- trips[f,]

# Save as an R data file
save(trips.f, file = "foraging_trip_info_filtered.RData")

# Output to a csv file
write.csv(trips.f, file = "foraging_trip_info_filtered.csv")

# Output to an Excel file
# First install required library
install.packages("xlsx")
library("xlsx")

# Now output the file
write.xlsx(trips.f, file = "foraging_trip_info_filtered.xlsx")
