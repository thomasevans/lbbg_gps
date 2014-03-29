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
flight.points <- sqlQuery(gps.db, query="SELECT DISTINCT g.*
                    FROM lund_flight_com_in_drift_points AS g
                    ORDER BY g.device_info_serial ASC, g.date_time ASC;")


# Get commuting flight summary data
flights <- sqlQuery(gps.db, query="SELECT DISTINCT f.*, l.trip_flight_n,l.trip_id, l.trip_flight_type, w.* 
                    FROM lund_flights_commuting_par AS f, lund_flights as l, lund_flight_com_wind_par_ecmwf AS w
                    WHERE f.flight_id = l.flight_id
                    AND f.flight_id = w.flight_id
                    ORDER BY f.flight_id ASC;")


# Get trip info data
#Query the gull db to extract trip information
trips <- sqlQuery(gps.db, query="SELECT DISTINCT t.*
                  FROM lund_trips AS t
                  ORDER BY t.trip_id ASC;")


# Filter flights ------
# Only include inward flights - use same filter as in stats analysis (for consistency etc)
# Filter flights table

# following from 'commuting_flight_comparisons_categorised_stats.R'
# Label trip type and duration -----
trip_type     <- 0
trip_duration <- 0
trip_gotland  <- 0
trip_distmax  <- 0

# Go through all trips, labelling by trip type duration etc.
# Later to be used for filtering criteria
for(i in seq(along = flights$trip_id)){
  trip_type[i] <- trips$trip_type[trips$trip_id ==
                                    flights$trip_id[i]][1]
  trip_duration[i] <- trips$duration_s[trips$trip_id ==
                                         flights$trip_id[i]][1]
  trip_gotland[i] <- trips$gotland[trips$trip_id ==
                                     flights$trip_id[i]][1]
  trip_distmax[i] <- trips$dist_max[trips$trip_id ==
                                      flights$trip_id[i]][1]
}

# Make to factors - not numeric
trip_type    <- as.factor(trip_type)
trip_gotland <- as.factor(trip_gotland)
# 
# summary(trip_type)
# summary(trip_gotland)


# Filter for inward flights meeting criteria for inclusion ------
inward  <- (flights$trip_flight_type == "inward")  & (trip_gotland == 0) & (flights$interval_mean < 800) & (trip_distmax > 4) & (trip_distmax < 400) & flights$points > 4  & flights$dist_a_b > 2000

summary(inward)





# Then use above filter %in% ... to filter the points table too.
f <- flight.points$flight_id %in% flights$flight_id[inward]
summary(f)
flight.points.f <- flight.points[f,]





# Plot some example flights for drift by distance etc, perhaps 10 in different colours (same for each graph...)
plot(flight.points.f$drift_prop ~ flight.points.f$dist_prop_to_goal,
     ylim = c(-1,1), xlim = c(0,1))
abline(lm(flight.points.f$drift_prop[flight.points.f$drift_prop < 2 &
                                       flight.points.f$drift_prop > -2] ~
            flight.points.f$dist_prop_to_goal[flight.points.f$drift_prop < 2 &
                                                flight.points.f$drift_prop > -2]),
       col = "red",
       lwd = 2)

abline(h = 0,
       col = "blue",
       lwd = 2,
       lty = 2)


plot(flight.points.f$drift_prop ~ flight.points.f$dist_prop_to_goal,
     ylim = c(-1000,1000), xlim = c(0,1))
abline(lm(flight.points.f$drift_prop[flight.points.f$drift_prop < 2 &
                                       flight.points.f$drift_prop > -2] ~
            flight.points.f$dist_prop_to_goal[flight.points.f$drift_prop < 2 &
                                                flight.points.f$drift_prop > -2]),
       col = "red",
       lwd = 2)

abline(h = 0,
       col = "blue",
       lwd = 2,
       lty = 2)

hist(flight.points.f$drift_prop, xlim = c(-10,10), breaks = 10000)

hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal < 0.2],
     breaks = 10000, xlim = c(-10,10))

hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal>0.4
                                       & flight.points.f$dist_prop_to_goal < 0.6],xlim = c(0,1.5), breaks = 100)
hist(flight.points.f$drift_prop[flight.points.f$dist_prop_to_goal>0.8],xlim = c(0,1.5), breaks = 100)

names(flight.points.f)

# Plot all data (maybe small points?)
# Add spline, and 95% CI? Loess etc
# See some of options found from a bit of Googling


names(flight.points.f)


# Figures for drift by segment analysis -----

plot(flight.points.f$drift_relative_segment ~ flight.points.f$goal_dist,
     ylim = c(-2,3), xlim = c(0,50000))


library("ggplot2")
library(ggthemes)


# mean(flight.points.f$drift_relative_segment, na.rm = TRUE)


# Altitude vs. wind: head-tail ----
# pdf(file = "BLAM_2014_altitude_wind_head2.pdf", width = 6.472136,
#     height = 4, colormodel = "cmyk")
# Golden ratio dimensions!
# ?pdf
qplot(goal_dist, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point", "smooth"),
      span = 0.3,
      ylim = c(-2, 2),
      xlim = c(50000,0),
      alpha = I(1 / 2),
      
) + labs(x = "Distance from island (m)", y = "Drift")  +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 
#   + geom_point(colour = "red")
#   theme_wsj()
# dev.off()







qplot(prop_time, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point", "smooth"),
      span = 0.3,
      ylim = c(-2, 2),
      xlim = c(0,1),
      alpha = I(1 / 2),
      
) + labs(x = "Time proportion", y = "Drift")  +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 

qplot(prop_time, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point", "smooth"),
      span = 0.3,
      ylim = c(-2, 2),
      xlim = c(0,1),
      alpha = I(1 / 2),
      
) + labs(x = "Time proportion", y = "Drift")  +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 



qplot(dist_prop_to_goal, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point"),
      span = 0.05,
      ylim = c(-2, 2),
      xlim = c(0,1),
      alpha = I(1 / 2),
      
) + labs(x = "Distance from start of flight (relative)", y = "Drift") +
  geom_hline(yintercept = 0, colour = "grey80",
             lwd = 1) +
  geom_smooth(lwd = 1) +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 




hist(flight.points.f$bear_dif_seg[
  flight.points.f$bear_dif_seg > -3 &
    flight.points.f$bear_dif_seg < 3], xlim = c(-3,3),
  breaks = 60)


ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_line()

ggplot(flight.points.f, aes(goal_dist, drift_relative_segment,
                            group = flight_id,
                            )) + 
  ylim(c(-2, 2)) +
xlim(c(30000,0)) + 
#        ylim = c(-2, 2),
#        xlim = c(50000,0),
#        alpha = I(1 / 3)) +
  geom_line(alpha = I(1 / 3)) +
  geom_smooth(aes(group = 1), method="lm", size = 2, se = F)

?ggplot

names(flight.points.f)
qplot(goal_dist, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point", "smooth"),
      span = 0.6,
      ylim = c(-2, 2),
      xlim = c(50000,0),
      alpha = I(1 / 3),
      
) + labs(x = expression(paste("Wind speed: head-tail component (ms",""^{-1}, ")")), y = paste("Flight altitude (m)"))  +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 






# Get average drift per flight, and per flight half ------
flight_ids <- unique(flight.points.f$flight_id)

# i <- 5

mean.drift.whole <- 
      mean.side_wind.whole <- 
      mean.drift.half1 <- 
      mean.side_wind.half1 <- 
      mean.drift.half2 <- 
      mean.side_wind.half2 <- NULL


for(i in 1: length(flight_ids)){
  id <- flight_ids[i]
  f <- flight.points.f$flight_id == id
  
  goal.dist.start <- flight.points.f$goal_dist[f][1]
  
  half.1 <- flight.points.f$goal_dist[f] < goal.dist.start*0.5
  half.2 <- !half.1
  
  
  mean.drift.whole[i] <- mean(flight.points.f$drift_relative_segment[f],
                              na.rm = TRUE)
  mean.side_wind.whole[i] <- mean(flight.points.f$side_wind_segment[f],
                                  na.rm = TRUE)
  
  mean.drift.half1[i] <- mean(
    flight.points.f$drift_relative_segment[f][half.1], na.rm = TRUE)
  mean.side_wind.half1[i] <- mean(
    flight.points.f$side_wind_segment[f][half.1], na.rm = TRUE)
  
  mean.drift.half2[i] <- mean(
    flight.points.f$drift_relative_segment[f][half.2], na.rm = TRUE)
  mean.side_wind.half2[i] <- mean(
    flight.points.f$side_wind_segment[f][half.2], na.rm = TRUE)
    
}

names(flight.points.f)


flight.drift <- cbind(flight_ids, mean.drift.whole , 
                        mean.side_wind.whole , 
                        mean.drift.half1 , 
                        mean.side_wind.half1 , 
                        mean.drift.half2 , 
                        mean.side_wind.half2)
flight.drift <- as.data.frame(flight.drift)


mean.side_wind.whole.abs <- abs(flight.drift$mean.side_wind.whole)
mean.side_wind.half1.abs <- abs(flight.drift$mean.side_wind.half1)
mean.side_wind.half2.abs <- abs(flight.drift$mean.side_wind.half2)

flight.drift <- cbind(flight.drift,mean.side_wind.whole.abs,
                      mean.side_wind.half1.abs,
                      mean.side_wind.half2.abs)


hist(mean.drift.half2[mean.drift.half2 > -10 & 
                        mean.drift.half2 < 10],
     breaks = 20)

hist(mean.drift.half1[mean.drift.half1 > -10 & 
                        mean.drift.half1 < 10],
     breaks = 20)

hist(mean.drift.whole[mean.drift.whole > -5 & 
                        mean.drift.whole < 5],
     breaks = 40)


qplot(mean.drift.whole[mean.drift.whole > -3 & 
                         mean.drift.whole < 3], geom="histogram",
      binwidth = 0.1) +   theme_igray() +
  geom_vline(xintercept = 1, colour = "grey80",
             lwd = 1)  +
  geom_vline(xintercept = 0, colour = "grey40",
             lwd = 1) +
  labs(x = "Drift (proportion)", y = paste("N flights in 0.1 intervals of drift"))

qplot(mean.side_wind.half1.abs, mean.drift.half1, 
      data = flight.drift,
      geom = c("point"),
      span = 0.05,
      ylim = c(-2, 2),
#       xlim = c(0,1),
      alpha = I(1 / 2),
      
) + labs(x = "Wind speed", y = "Mean drift") +
  geom_hline(yintercept = 0, colour = "grey80",
             lwd = 1) +
  geom_smooth(lwd = 1) +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 




qplot(mean.side_wind.half2.abs, mean.drift.half2, 
      data = flight.drift,
      geom = c("point"),
      span = 0.05,
      ylim = c(-2, 2),
      #       xlim = c(0,1),
      alpha = I(1 / 2),
      
) + labs(x = "Wind speed", y = "Mean drift") +
  geom_hline(yintercept = 0, colour = "grey80",
             lwd = 1) +
  geom_smooth(lwd = 1) +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 

drifts <- c(flight.drift$mean.drift.half1, flight.drift$mean.drift.half2)

flight.part <- c(rep("Half 1",length(flight.drift$mean.drift.half1)),
                 rep("Half 2",length(flight.drift$mean.drift.half1))
)

boxplot(drifts~flight.part, ylim = c(-5,5))

plot(drifts~as.factor(flight.part), ylim = c(-2,2))
abline(h = 0, lwd = 2, lty = 2, col = "blue")

# ?boxplot





# Figures for Seabird group poster -------
library("ggplot2")
library(ggthemes)


pdf(file = "Seabird_2014_drift_dist_goal.pdf", width = 4,
    height = 4, colormodel = "cmyk")
png(file = "BTO_2014_drift_dist_goal2.png")
# tiff(file = "BTO_2014_drift_dist_goal.tiff")
# ?tiff
qplot(dist_prop_to_goal, drift_relative_segment, 
      data = flight.points.f,
      geom = c("point"),
#       size = 0.1,
      span = 0.05,
      ylim = c(-2, 2),
      xlim = c(0,1),
      alpha = I(1 / 2),
      size=I(.5)
      
) + labs(x = "Distance from start of flight (normalized)", y = "Drift") +
  geom_hline(yintercept = 0, colour = "red", lwd = 2, alpha = I(2 / 3)) +
  geom_hline(yintercept = 1, colour = "grey80", alpha = I(2 / 3)) +
  geom_smooth( fullrange = TRUE, lwd = 2) +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray(base_size = 20)
# ?geom_smooth
dev.off()

# ?theme_igray

pdf(file = "Seabird_2014_drift_flights_hist.pdf", width = 4,
    height = 4, colormodel = "cmyk")

png(file = "BTO_2014_drift_flights_hist.png")

qplot(mean.drift.whole[mean.drift.whole > -3 & 
                         mean.drift.whole < 3], geom="histogram",
            binwidth = 0.1) +
  theme_igray(base_size = 20)+
  geom_vline(xintercept = 1, colour = "grey80", alpha = I(1 / 3))  +
  geom_vline(xintercept = 0, colour = "grey80", alpha = I(1 / 3)) +
  labs(x = "Drift (proportion)", y = paste("N flights in 0.1 intervals of drift"))
dev.off()





pdf(file = "Seabird_2014_drift_wind.pdf", width = 4,
    height = 4, colormodel = "cmyk")
png(file = "BTO_2014__drift_wind.png")

qplot(mean.side_wind.whole.abs, mean.drift.whole, 
      data = flight.drift,
      geom = c("point"),
      #       size = 0.1,
      span = 0.05,
      ylim = c(-2, 2),
      xlim = c(0,8),
      alpha = I(1 / 2),
      size=I(1)
      
) + labs(x = expression(paste("Wind speed: side component (ms",""^{-1}, ")")), y = "Drift") +
  geom_hline(yintercept = 0, colour = "grey80", alpha = I(2 / 3)) +
  geom_hline(yintercept = 1, colour = "grey80", alpha = I(2 / 3)) +
  geom_smooth( fullrange = TRUE) +
  #   theme_solarized_2(light = FALSE)
  #   theme_solarized_2()
  #   theme_solarized(light = FALSE)
  theme_igray() 
# ?geom_smooth
dev.off()

