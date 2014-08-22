# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# Classification of guillemot GPS data into foraging trips
# Will base this largely on the script used to do this for the LBBG data,
# adapting slightly the various thresholds etc.
# Purpose of this script is simply to identify foraging trips,
# Then get start-time, end-time, and number each trip with unique ID
# Summary statistics for each foraging trip will be extracted in a
# second sepperate script.


# DB function to extract data for each device_ID for deployment
# period.