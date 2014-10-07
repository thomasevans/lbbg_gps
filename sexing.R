# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to look at the morphometric data for
# the lesser black-backed gulls to determine how/if
# males and females can be sepperated, and by which
# measures.



#Datbase functions#########
library(RODBC)

#Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

#See what tables are available
#sqlTables(gps.db)


# Load in DB tables:
birds <- sqlQuery(gps.db, query="SELECT DISTINCT b.*
                     FROM lund_birds_morph as b
                     ORDER BY device_info_serial ASC;")


# Plot head-bill size in order or size
plot(sort(birds$head_bill_mm), ylab = "head+bill (mm)",
     xlab = "Cumulative number")
# Rough cut-off point. With supposed males above the line,
# and females below the line
abline(h = 113.5, lwd = 2, lty = 2)
