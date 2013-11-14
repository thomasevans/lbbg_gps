# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# This script is to test different methods to categorise flight to sepperate out commuting (directed) flight from other types of flight (mainly expected to be searching).


#'  1. Get a subset of flights to use in testing - get 50 of each
#'   i.  For outward flights
#'   ii. For inwward flights
#'  
#'  2. Plot maps for non categorised outward and inward flights
#'  
#'  3. Make categorisation alogrithm
#'  
#'  4. Test alorithm on a few individual flights
#'  
#'  5. Test algorithm on full sample (50 of each flight type)
#'  
#'  6. When satisfied with algorithm apply to all 'commuting' flights, flights classified as outward or inward.
#'  
#'  7. Re-analyse all these flights to get summary statistics
#'  
#'  8. Re-run previous statistical analyses on newly analysed data.
