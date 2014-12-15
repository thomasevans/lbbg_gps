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
hist(log(flights.df$alt_new_median + 20), breaks = 100)
alt_med_ln <- log(flights.df$alt_new_median + 20)
alt_med_ln_z <- scale(alt_med_ln)
hist(alt_med_ln_z)

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
mod1 <- lm(flights.df$cloud_cover_low_altitudemean ~
             flights.df$cloud_cover_totalmean)
anova(mod1)
plot(flights.df$cloud_cover_low_altitudemean ~
       flights.df$cloud_cover_totalmean)

hist(flights.df$cloud_cover_totalmean)
hist(flights.df$cloud_cover_low_altitudemean)
hist(flights.df$cloud_cover_low_altitudemean)
# Potentially could use the residual variation - like excess
# cloud at low altitude once total cloud taken into account
res_mod1 <- resid(mod1)
plot(scale(res_mod1)~scale(flights.df$cloud_cover_totalmean))
# Low and tot cloud are strongly correlated, thus it 
# is probably best to only include one of these variables
cloud_tot_z <- scale(flights.df$cloud_cover_totalmean)
hist(cloud_tot_z)


# Wind - side
side.type <- as.factor(sign(flights.df$wind_side_mean_10))
summary(side.type)

side_abs <- abs(flights.df$wind_side_mean_10)
hist(side_abs)

# I think in the model it's best to use the real wind values, and not the scaled value
side_abs_z <- scale(side_abs)
hist(side_abs_z)

# Wind - head-tail
head_tail.type <- as.factor(sign(flights.df$wind_head_tail_mean_10))
summary(head_tail.type)

hist(flights.df$wind_head_tail_mean_10)
head_tail_abs <- abs(flights.df$wind_head_tail_mean_10)
hist(head_tail_abs)
head_tail_abs_z <- scale(head_tail_abs)
hist(head_tail_abs_z)



# Flight distance
dist_km <- flights.df$dist_a_b/1000
hist(dist_km)  # distances in km
dist_km_z <- scale(dist_km)
hist(dist_km_z)


# Combine data (and exlude all NAs)
alt_data_df <- cbind.data.frame(
  dist_km,
  dist_km_z,
  head_tail.type,
  head_tail_abs,
  side.type,
  side_abs,
  temp_z,
  cloud_tot_z,
  alt_med_ln,
  alt_med_ln_z,
  flights.df$device_info_serial)

names(alt_data_df) <- c(
  "dist_km",
  "dist_km_z",
  "head_tail.type",
  "head_tail_abs",
  "side.type",
  "side_abs",
  "temp_z",
  "cloud_tot_z",
  "alt_med_ln",
  "alt_med_ln_z",
  "device_info_serial"
  )

# Exclude any NA rows
anyNA(alt_data_df)
# Already removed - so no need to filter

# Model 1 - specification + random effects + autoregressive thing -----
# Variable list:
#' dist_km
#' dist_km_z
#' head_tail.type
#' head_tail.abs
#' side.type
#' side_abs
#' temp_z
#' cloud_tot_z
#' alt_med_lm
#' alt_med_lm_z

library(nlme)


# full model with all 2-way interactions included
mod_01  <-  lme(alt_med_ln ~
                    (dist_km_z + head_tail.type + head_tail_abs +
                       side.type + side_abs +
                       temp_z + cloud_tot_z)^2,
                  random = ~1|device_info_serial,
                  data = alt_data_df,
                method = "ML"
)

anova(mod_01)



mod_02  <-  lme(alt_med_ln ~
                  (dist_km_z) + (head_tail.type + head_tail_abs)^2 +
                     (side.type + side_abs)^2 +
                     (temp_z + cloud_tot_z)^2,
                random = ~1|device_info_serial,
                data = alt_data_df,
                method = "ML"
)




# Model simplification by AIC, looking at all candidate models
# including upto 2-way interactions.

library(MuMIn)
require(foreach)
require(doParallel)

#Make cluster of number of devices instances
cl <- makeCluster(8)

#start the parellel session of R; the 'slaves', which will run the analysis.
registerDoParallel(cl)  


clusterExport(cl, "alt_data_df")
# ?clusterExport
clusterEvalQ(cl = cl, library("nlme"))

# Limit to no more than 10 possible terms
dd <- pdredge(mod_01, m.max = 10, cluster = cl)

# ?dredge
# #close cluster
stopCluster(cl)

# dd <- dredge(mod_01)


dd_sub4 <- subset(dd, delta < 4)
# Visualize the model selection table:
if(require(graphics))
  plot(dd)

# Model average models with delta AICc < 4
model.avg(dd, subset = delta < 4)
#or as a 95% confidence set:
model.avg(dd, subset = cumsum(weight) <= .95) # get averaged coefficients
#'Best' model
#'
#'
summary(get.models(dd, 1)[[1]])

model.sel(dd)




library(MuMIn)
r.squaredGLMM(mod_01)


# Check for whether to fit an autocorrelation function
plot(ACF(mod_01, maxLag = 50), alpha = 0.01)
# Autocorrelation does not appear to be significant at any lag,
# therefor we do not fit an autocorrelation function in the model,
# at least at this stage.


install.packages("coefplot2",
                 repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")

library(coefplot2)
coefplot2(mod_01)




# Model simplification # 3 - glmulti ------
install.packages("glmulti")


# Has some silly java error when ran on my computer - if you get that warning
# (on windows at least) the following should fix it


library(glmulti)



# Model 2 - Simplify (drop variables - AIC selection) -----

# Model 3 - Check assumptions -----

# Model 4 - Summarise (R2, p values etc) ------

# Model 5 - 'Forest plot' ------


# Figures to illustrate ------
# Flight altitude vs. wind (head-tail)

# Flight altitude vs. wind (side-component)

