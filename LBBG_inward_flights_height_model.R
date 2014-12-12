# Developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.
# Code available at https://github.com/thomasevans/lbbg_gps

# Description ------
# In this script we perform a statistical analysis of the environmental
# factors determining lesser black-backed gull flight height, focussing
# principally on meteorological variables.



# Read in data ------

# Datbase functions
# Required library
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
#sqlTables(gps.db)


# Get a copy of the inward flights DB table. -------
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*
                    FROM lund_flight_com_lbbg AS f
                    ORDER BY f.device_info_serial ASC, f.start_time ASC;")


str(flights)  #check structure

# Inspect data -----
range(flights$alt_new_median, na.rm = TRUE)
hist(flights$alt_new_median, breaks = 200, xlim = c(-50,50))


hist(flights$alt_new_median, breaks = 500, xlim = c(-20,20) )


# Flights to include (-20 - 150)
hist(flights$alt_new_median, breaks = 200, )
abline(v = c(-20, 150), lwd = 3, lty = 2)

hist(flights$alt_new_median, breaks = 200, xlim = c(-40,20) )
abline(v = c(-20, 150), lwd = 3, lty = 2)

# Data filter criteria ----
flights.sub <- flights$alt_new_median > -20 &
  flights$alt_new_median < 150 &
  !is.na(flights$alt_new_median)

summary(flights.sub)
# Exclude 11 flights where altitude data is missing, or where altitude falls outside of expected range.

flights.df <- flights[flights.sub,]




# Variable preparation - transformation etc -----

# Response variable:
# Altitude
hist(flights.df$alt_new_median, breaks = 100)
hist(flights.df$alt_new_median + 20, breaks = 100)

# Transform with natural log
hist(log(flights.df$alt_new_median + 10), breaks = 100)

# Explanatory variables:
# To include:
#'  temp_2m
#'  cloud_total
#'  cloud_low
#'  wind - head-tail
#'  wind - side
#'  distance

names(flights.df)


# temp_2m
temp_2m <- flights.df$temperature_2mmean
# Histogram of temp (C)
hist(temp_2m - 273)
# Scaled 'normalized' temperature
hist(scale(temp_2m))
temp_z <- scale(flights.df$temperature_2mmean)


# cloud
# How correlated are total and low level cloud?
mod1 <- lm(flights.df$cloud_cover_totalmean ~ flights.df$cloud_cover_low_altitudemean)
anova(mod1)
plot(flights.df$cloud_cover_totalmean ~ flights.df$cloud_cover_low_altitudemean)

hist(flights.df$cloud_cover_totalmean)
hist(flights.df$cloud_cover_low_altitudemean)
hist(flights.df$cloud_cover_low_altitudemean)
res_mod1 <- resid(mod1)
plot(flights.df$cloud_cover_totalmean~scale(res_mod1))
# Low and tot cloud are strongly correlated, thus it 


# Total atomospheric cloud cover
cloud_total <- flights.combined$cloud_cover_totalmean[f3]
hist(cloud_total)
hist(logit(cloud_total))
cloud_total_logit <- logit(cloud_total)

# Low level cloud (may reflect atmospheric visibility better)
cloud_low <- flights.combined$cloud_cover_low_altitudemean[f3]
hist(cloud_low)
cloud_low_logit <- sapply(cloud_low,logit)
hist(cloud_low_logit)



# Model 1 - specification + random effects + autoregressive thing -----

# Model 2 - Simplify (drop variables - AIC selection) -----

# Model 3 - Check assumptions -----

# Model 4 - Summarise (R2, p values etc) ------

# Model 5 - 'Forest plot' ------


# Figures to illustrate ------
# Flight altitude vs. wind (head-tail)

# Flight altitude vs. wind (side-component)

