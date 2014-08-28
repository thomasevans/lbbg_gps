# Comparing different bathymetric data to choose grid for
# analysis.

# Load GPS point data for guillemots -----

# 'surface_points' contains all GPS locations for 2014 where
# speed is < 5 ms-1 (not flying), and where distances are
# > 300 m from the colony. In addition there is limited
# GPS quality filtering to remove the most innacurate
# GPS locations.
load("...surface_points")

# 'surface_points2' is above but with one indivdual bird
# removed, that which appeared highly disturbed by the device
# spending ca. 5 days at sea, mostly apparently swimming on
# the sea surface
load("...surface_points2")


# 'dive_points' contains a subset of 'surface_points', only
# including locations from the 'homemade' (not Amsterdam)
# GPS tags. For these devices it contains locations where
# the previous GPS fix attempt was during a dive.
load("...dive_points")


# Bathymetric data examples ----

# 1. BSBD data -----
# The first example is of data from ....
