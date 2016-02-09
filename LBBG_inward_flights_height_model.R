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

va <- flights.df$head_speed_median
  
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
  flights.df$device_info_serial,
  va,
  flights.df$ground_speed_median,
  flights.df$alt_new_median)

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
  "device_info_serial",
  "va",
  "vg",
  "alt_med"
  )

# Exclude any NA rows
anyNA(alt_data_df)
# Already removed - so no need to filter


# Check data structure
str(alt_data_df)

# correct format for device_info_serial
alt_data_df$device_info_serial <- as.factor(as.character(alt_data_df$device_info_serial))

# Check structure again
str(alt_data_df)


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
summary(mod_02)



# Some new stuff for flight lab meeting -------


vg vs va
plot(alt_data_df$va~alt_data_df$vg,
     xlab = "Vg", ylab = "Va")
abline(lm(alt_data_df$va~alt_data_df$vg), lwd = 3, col = "blue")
abline(h = mean(alt_data_df$va), lwd = 3, lty = 2, col = "black")
lw1 <- loess(alt_data_df$va~alt_data_df$vg)
j <- order(alt_data_df$vg)
lines(alt_data_df$vg[j],lw1$fitted[j],col="red",lwd=3)
# lines(loess(alt_data_df$va~alt_data_df$vg, span = 0.4 ))
# ?loess
library(lme4)
# mod.7<-g<lmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)


mod.1  <-  glmer(alt_med ~
                  (dist_km_z) + (head_tail.type + head_tail_abs)^2 +
                  (side.type + side_abs)^2 +
                (1|device_info_serial),
                data = alt_data_df
)
summary(mod.1)

str(alt_data_df$side.type)

# Illustrate this model
# First make sure to use the standardized model to allow for comparison
library(arm)
stdz.mod.7<-standardize(mod.1, standardize.y=FALSE)
# Check this looks sensible
summary(stdz.mod.7)

# Get confidence intervals for coeficients
stdz.mod.7.ci.Wald <- confint(stdz.mod.7, method="Wald")

# Need this package for plots:
library(lattice)

# I worked this out based on code here:
# http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/

# Make a data.frame of the CI
ci_dat <-stdz.mod.7.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef = row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')

# View current coeficient names:
ci_df$coef

# Make a new vector of coeficient names (need to change these to what is
# sensible based on the model)
ci_df$coef_new <- c("(intercept)", "Distance", "Head-tail - type (tail)",
                    "Head-tail - abs", "Side - type (left)", "Side - abs",
                    "Head-tail abs*type (tail)", "Side abs*type (left)")

# If you want the coeficients displayed in a different order to the current
# Here we sort them in order of the coeficient value
ci_df_sort <- ci_df[order(ci_df$mean),]
ci_df_sort$coef_new <- factor(ci_df_sort$coef_new, levels = unique(ci_df_sort$coef_new))


# Plot the figure
lattice::dotplot(coef_new ~ mean, ci_df_sort,
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect (altitude)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df_sort$lwr, y, ci_df_sort$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)

library(MuMIn)
r1<-r.squaredGLMM(stdz.mod.7)
r1



# Same for Va ----
library(lme4)
# mod.7<-g<lmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)


mod.1  <-  glmer(va ~
                   (dist_km_z) + (head_tail.type + head_tail_abs)^2 +
                   (side.type + side_abs)^2 +
                   (1|device_info_serial),
                 data = alt_data_df
)
summary(mod.1)

# Illustrate this model
# First make sure to use the standardized model to allow for comparison
library(arm)
stdz.mod.7<-standardize(mod.1, standardize.y=FALSE)
# Check this looks sensible
summary(stdz.mod.7)

# Get confidence intervals for coeficients
stdz.mod.7.ci.Wald <- confint(stdz.mod.7, method="Wald")

# Need this package for plots:
library(lattice)

# I worked this out based on code here:
# http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/

# Make a data.frame of the CI
ci_dat <-stdz.mod.7.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef = row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')

# View current coeficient names:
ci_df$coef

# Make a new vector of coeficient names (need to change these to what is
# sensible based on the model)
ci_df$coef_new <- c("(intercept)", "Distance", "Head-tail - type (tail)",
                    "Head-tail - abs", "Side - type (left)", "Side - abs",
                    "Head-tail abs*type (tail)", "Side abs*type (left)")

# If you want the coeficients displayed in a different order to the current
# Here we sort them in order of the coeficient value
ci_df_sort <- ci_df[order(ci_df$mean),]
ci_df_sort$coef_new <- factor(ci_df_sort$coef_new, levels = unique(ci_df_sort$coef_new))


# Plot the figure
lattice::dotplot(coef_new ~ mean, ci_df_sort, #xlim = c(-3,2),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect on (Va)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df_sort$lwr, y, ci_df_sort$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)

library(MuMIn)
r1<-r.squaredGLMM(stdz.mod.7)
r1



# Old stuff below here ------





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
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7\\")
library(glmulti)

# ?glmulti

test <- glmulti(mod_01, level = 2, data = alt_data_df,
                marginality = TRUE,
                maxsize = 10,
                method = "g",
                crit = "aic",
#                 fitfunc = lme.glmulti)
                plotty = TRUE,
                report = TRUE,
            )
                

mod_01  <-  lme(alt_med_ln ~
                  (dist_km_z + head_tail.type + head_tail_abs +
                     side.type + side_abs +
                     temp_z + cloud_tot_z)^2,
                random = ~1|device_info_serial,
                data = alt_data_df,
                method = "ML"
)


#########
library("lme4")
library("glmulti")
# dd <- read.table("SO_glmulti.dat",header=TRUE)
m1 <- lmer(alt_med_ln ~
             (dist_km_z + head_tail.type + head_tail_abs +
                side.type + side_abs +
                temp_z + cloud_tot_z)^2+
             (1|device_info_serial),
           data = alt_data_df)

summary(m1)
r.squaredGLMM(m1)
aic(m1)

coefplot2(m1)


setMethod('getfit', 'merMod', function(object, ...) {
  summ <- coef(summary(object))
  summ1 <- summ[,1:2,drop=FALSE]
  ## if (length(dimnames(summ)[[1]])==1) {
  ##     summ1 <- matrix(summ1, nr=1,
  ##                     dimnames=list(c("(Intercept)"),
  ##                     c("Estimate","Std. Error")))
  ## }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

lmer.glmulti<-function(formula,data,random="",...){
  lmer(paste(deparse(formula),random),data=data,REML=F,...)
}




glmulti_lmm_10_run_01 <- glmulti(formula(m1,fixed.only=TRUE),
                       random = "+(1|device_info_serial)",
                       data = alt_data_df, method = "h",
                       deltaM = 0.5, 
                       fitfunc = lmer.glmulti,
                       intercept = TRUE,
                       marginality = TRUE,
                       maxsize = 10,
                       level = 2)

# ?glmulti

summary(glmulti_lmm_10_run_01)
weightable(glmulti_lmm_10_run_01)


# Takes a long time to run - probably best to run overnight/ on Amazon server
glmulti_lmm_10_h <- glmulti(formula(m1,fixed.only=TRUE),
                          random = "+(1|device_info_serial)",
                          data = alt_data_df,
                          method = "h",
#                           deltaM = 0.5, 
                          fitfunc = lmer.glmulti,
                          intercept = TRUE,
                          marginality = TRUE,
                          minsize = 10,
                          level = 2)


weightable(glmulti_lmm_10_h)



# glmulti_lmm_10_g_run01 <- glmulti_lmm_10_g
# aic(glmulti_lmm_10_g_run03)
glmulti_lmm_10_g_run03 <- glmulti(formula(m1,fixed.only=TRUE),
                            random = "+(1|device_info_serial)",
                            data = alt_data_df,
                            method = "g",
                            deltaM = 0.5, 
                            fitfunc = lmer.glmulti,
                            intercept = TRUE,
                            marginality = TRUE,
                            maxsize = 10,
                            level = 2)


summary(glmulti_lmm_10_g_run01)


weightable(glmulti_lmm_10_g_run03)


glmulti_lmm_8_g_run01 <- glmulti(formula(m1,fixed.only=TRUE),
                                  random = "+(1|device_info_serial)",
                                  data = alt_data_df,
                                  method = "g",
                                  deltaM = 0.5, 
                                  fitfunc = lmer.glmulti,
                                  intercept = TRUE,
                                  marginality = TRUE,
                                  maxsize = 8,
                                  level = 2)


str(alt_data_df)


# Model 2 - Simplify (drop variables - AIC selection) -----

# Model 3 - Check assumptions -----

# Model 4 - Summarise (R2, p values etc) ------

# Model 5 - 'Forest plot' ------


# Figures to illustrate ------
# Flight altitude vs. wind (head-tail)

# Flight altitude vs. wind (side-component)

