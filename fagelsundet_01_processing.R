# Calculate various paramaters for the GPS tracking data for
# all Fågelsundet gulls for 2014
# Including:
# - Distance from nest
# - Whether on foraging trip (distance from nest threshhold)
# - Type of point ('flight', 'non-flight') using speed threshold
# - Distance to nearest coast
# - Whether on land/ sea
# - Combined, signed distance to coast
# Then output this data to DB with combined primary key of
# device_info_serial and date_time

# Required packages ------
library("RODBC")


# Read in data from DB -----
# GPS data

# Nest location data (combine device_info_serial with ring_number)

# Calculate various paramaters ----

# Distance from nest -----

# Whether on trip (distance threshold) ----

# Type of point (flight/ non-flight, speed threshold) ----

# Distance from coast -----

# On land/ sea -----

# Signed distance from coast ----

# Combine into data frame ------

# Output to Database ------
