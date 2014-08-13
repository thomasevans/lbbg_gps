# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to produce a new database table to
# annotate the GPS fix data from the IGU loggers
# labelling points by whether they are vallid
# GPS fixes, plus weather they include diving etc.

# Read in DB data -----


# Label by whether vallid GPS fix -----


# Label by 'behaviour' ------
#* 1. Flight
#* 2. Diving (apparent)
#* 3. Colony
#* 4. Water surace (swimming etc)
#* 5. Other (i.e. uncategorised)


# Some labelling of previous/ next points? -----
# Could be good for example to label if prio point was diving
# so as to plot points according to this.
# When plotting could get average/ intermediate position for diving
# events

# Output to new DB table ----
# Output annotation to new DB table include device_info_serial
# and date_time for primary key data

