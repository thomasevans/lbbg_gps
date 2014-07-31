
# Look at last communication dates for Stora Karlsö in 2013

# Following file created with this SQL query
# SELECT e.device_info_serial, e.date_time
# FROM ee_comm_limited AS e
# WHERE e.date_time > '2013-06-01'
# AND e.date_time < '2013-12-01'
# ORDER BY e.device_info_serial ASC, e.date_time ASC ;

com <- read.csv("D:/Dropbox/R_projects/lbbg_gps/communication_2013_all_history.csv")


com$date_time_x <- as.character(com$date_time)
com$date_time_x <- as.POSIXct(com$date_time)

# Stora Karlsö devices up to and including 2013:
k_devices <- c(602, 603,596,598,628,624,
               531,532,527,519,522,523,
               521,524,528,530,803,875,
               874,673,869,687)

x <- com$device_info_serial %in% k_devices
summary(x)

com.f <- com[x,]

library(reshape2)
# Number of trips per individual
aggregate(date_time_x ~ device_info_serial,
          data = com.f,
          FUN = max)
