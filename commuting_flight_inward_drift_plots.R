# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Description -----
# This script makes plots to look at levels of wind drift during inward flights on foraging trips - a filter specifies only marine foraging trips.


# Database data downloand ----

# Connect to database
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Documents/Work/GPS_DB/GPS_db.accdb')

#See what tables are available
sqlTables(gps.db)


# Get flight drift data + weather data (point level)
sql...


# Get commuting flight summary data
sql...



# Filter flights ------
# Only include inward flights - use same filter as in stats analysis (for consistency etc)
# Filter flights table

# Then use above filter %in% ... to filter the points table too.



# Plot some example flights for drift by distance etc, perhaps 10 in different colours (same for each graph...)

# Plot all data (maybe small points?)
# Add spline, and 95% CI? Loess etc
# See some of options found from a bit of Googling
