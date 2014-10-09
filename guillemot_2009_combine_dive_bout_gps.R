# Analysis of dive-bouts from 2009 guillemot GPS + TDR study



# Import required data ------

# From DB:
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# Dive bouts
dive.bouts <- sqlQuery(gps.db,
                        query = "SELECT DISTINCT d.*
          FROM guillemots_dive_bouts_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time_start ASC;",
                        as.is = TRUE)

# GPS data
gps.points <- sqlQuery(gps.db,
                       query = "SELECT DISTINCT d.*
          FROM guillemots_gps_points_igu_2009 AS d
          ORDER BY d.ring_number ASC, d.date_time ASC;",
                       as.is = TRUE)

# Bathymetry
load('bsbd_raster.RData')


# Combine two data - for loop to go through all bouts -----





